#ifndef IDE_CONTROLLER_H
#define IDE_CONTROLLER_H

#include "pci.h"

// TODO: upgrade to singleton container, in case of multiple IDE controllers?
class IDEController {
 public:
  IDEController(const pci::PCIDevice& dev);
  static IDEController& get();
  static bool initialized();

 private:
  pci::PCIDevice dev;
  
  struct IDEChannelRegisters {
    u16 cmd;   // Command Base
    u16 ctrl;  // Control Base
    u16 bmide; // Bus Master IDE
    u8  nIEN;  // nIEN (No Interrupt);
  } channels[2];
  
  u8 ide_buf[2048] = {0};
  u8 ide_irq_invoked = 0;
  u8 atapi_packet[12] = {0xA8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  
  struct ide_device {
    u8  Reserved;    // 0 (Empty) or 1 (This Drive really exists).
    u8  Channel;     // 0 (Primary Channel) or 1 (Secondary Channel).
    u8  Drive;       // 0 (Master Drive) or 1 (Slave Drive).
    u16 Type;        // 0: ATA, 1:ATAPI.
    u16 Signature;   // Drive Signature
    u16 Capabilities;// Features.
    u32 CommandSets; // Command Sets Supported.
    u32 Size;        // Size in Sectors.
    u8  Model[41];   // Model in string.
  } ide_devices[4];

  u8 ide_read(u8 channel, u8 reg);
  void ide_write(u8 channel, u8 reg, u8 data);
  void ide_init();
};

#endif
