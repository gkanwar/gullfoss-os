#ifndef PCI_H
#define PCI_H

#include <types.h>
#include <vector>

namespace pci {

  struct PCIDevice {
    u32 bus;
    u32 device;
    u16 vendor_id;
    u16 device_id;
  };

  u16 read_config_u16(u32 bus, u32 device, u32 func, u8 offset);
  std::vector<PCIDevice> enumerate_devices();
  const char* get_vendor_string(u16 vid);

}

#endif
