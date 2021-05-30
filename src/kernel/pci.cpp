#include "pci.h"
#include <vector>

#include "assert.h"
#include "io.h"

constexpr u16 CONFIG_ADDRESS = 0xCF8;
constexpr u16 CONFIG_DATA = 0xCFC;

using namespace std;

namespace pci {

  u16 read_config_u16(u32 bus, u32 device, u32 func, u8 offset) {
    u32 enable = 1 << 31;
    assert((offset & 1) == 0, "Offset must be word-aligned");
    bool word_off = offset & 0b10;
    offset &= 0b11111100;
    u32 address = enable | (bus << 16) | (device << 11) | (func << 8) | offset;
    io::out32(CONFIG_ADDRESS, address);
    return (u16) ((io::in32(CONFIG_DATA) >> (16*word_off)) & 0xffff);
  }

  vector<PCIDevice> enumerate_devices() {
    vector<PCIDevice> devices;
    u32 bus = 0; // TODO: recurse down PCI bridges
    for (u32 device = 0; device < 32; ++device) {
      u16 vendor_id = read_config_u16(bus, device, 0, 0);
      if (vendor_id == 0xffff) continue;
      u16 device_id = read_config_u16(bus, device, 0, 2);
      devices.push_back(PCIDevice{
          .bus = bus, .device = device,
          .vendor_id = vendor_id, .device_id = device_id
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

}
