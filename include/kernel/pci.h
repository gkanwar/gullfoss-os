#ifndef PCI_H
#define PCI_H

namespace pci {

u16 read_config_u16(u32 bus, u32 device, u32 func, u8 offset);

}

#endif
