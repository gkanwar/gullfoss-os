#ifndef INTERRUPT_MANGER_H
#define INTERRUPT_MANAGER_H

#include <stdint.h>

struct InterruptGate {
  uint16_t offset_1;
  uint16_t segment_selector;
  uint8_t ist;
  uint8_t flags;
  uint16_t offset_2;
  uint32_t offset_3;
  uint32_t zero;
} __attribute__((packed));

class InterruptManager {
 public:
  InterruptManager();
  static InterruptManager& get();
  void init_interrupts();
  static void pic_send_eoi(uint8_t irq);
  static const uint8_t irq_offset = 0x20;
 private:
  void reprogram_pics();
  void load_idt();
  InterruptGate interrupt_desc_table[256];
};

#endif
