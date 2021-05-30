#include "pci.h"
#include <vector>

#include "assert.h"
#include "io.h"

constexpr u16 CONFIG_ADDRESS = 0xCF8;
constexpr u16 CONFIG_DATA = 0xCFC;

using namespace std;

namespace pci {

  u32 read_config_u32(Address addr, u32 func, u8 offset) {
    u32 enable = 1 << 31;
    assert((offset & 0b11) == 0, "Offset must be dword-aligned");
    u32 address = enable | (addr.bus << 16) | (addr.device << 11) | (func << 8) | offset;
    io::out32(CONFIG_ADDRESS, address);
    return io::in32(CONFIG_DATA);
  }

  u16 read_config_u16(Address addr, u32 func, u8 offset) {
    assert((offset & 1) == 0, "Offset must be word-aligned");
    bool word_off = offset & 0b10;
    offset &= 0b11111100;
    u32 dword_config = read_config_u32(addr, func, offset);
    return (u16) ((dword_config >> (16*word_off)) & 0xffff);
  }

  vector<PCIDevice> enumerate_devices() {
    vector<PCIDevice> devices;
    u32 bus = 0; // TODO: recurse down PCI bridges
    for (u32 device = 0; device < 32; ++device) {
      Address addr{.bus = bus, .device = device};
      u16 vendor_id = read_config_u16(addr, 0, VENDOR_ID);
      if (vendor_id == 0xffff) continue;
      u16 device_id = read_config_u16(addr, 0, DEVICE_ID);
      u16 class_subclass = get_class_code(addr);
      u8 class_code = (u8)(class_subclass >> 8);
      u8 subclass_code = (u8)(class_subclass & 0xff);
      devices.push_back(PCIDevice{
          .addr = addr,
          .vendor_id = vendor_id, .device_id = device_id,
          .class_code = class_code, .subclass_code = subclass_code
        });
    }
    return devices;
  }

  const char* get_vendor_string(u16 vid) {
    if (vid == 0x8086) {
      return "Intel";
    }
    else {
      return "Unknown";
    }
  }

  u16 get_class_code(Address addr) {
    return read_config_u16(addr, 0, CLASS_CODE);
  }

  u32 get_BAR0(Address addr) {
    return read_config_u32(addr, 0, BAR0);
  }
  u32 get_BAR1(Address addr) {
    return read_config_u32(addr, 0, BAR1);
  }
  u32 get_BAR2(Address addr) {
    return read_config_u32(addr, 0, BAR2);
  }
  u32 get_BAR3(Address addr) {
    return read_config_u32(addr, 0, BAR3);
  }
  u32 get_BAR4(Address addr) {
    return read_config_u32(addr, 0, BAR4);
  }

}
