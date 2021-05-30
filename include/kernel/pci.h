#ifndef PCI_H
#define PCI_H

#include <types.h>
#include <vector>

namespace pci {

  constexpr u8 VENDOR_ID = 0x0;
  constexpr u8 DEVICE_ID = 0x2;
  constexpr u8 CLASS_CODE = 0xa;
  constexpr u8 BAR0 = 0x10;
  constexpr u8 BAR1 = 0x14;
  constexpr u8 BAR2 = 0x18;
  constexpr u8 BAR3 = 0x1c;
  constexpr u8 BAR4 = 0x20;
  constexpr u8 BAR5 = 0x24;

  struct Address {
    u32 bus;
    u32 device;
  };

  struct PCIDevice {
    Address addr;
    u16 vendor_id;
    u16 device_id;
    u8 class_code;
    u8 subclass_code;
  };

  u32 read_config_u32(Address addr, u32 func, u8 offset);
  u16 read_config_u16(Address addr, u32 func, u8 offset);
  std::vector<PCIDevice> enumerate_devices();
  const char* get_vendor_string(u16 vid);
  u16 get_class_code(Address addr);
  u32 get_BAR0(Address addr);
  u32 get_BAR1(Address addr);
  u32 get_BAR2(Address addr);
  u32 get_BAR3(Address addr);
  u32 get_BAR4(Address addr);

}

#endif
