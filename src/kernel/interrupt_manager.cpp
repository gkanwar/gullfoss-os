#include <cstring>
#include <stdint.h>

#include "debug_serial.h"
#include "interrupt_impl.h"
#include "interrupt_manager.h"
#include "io.h"


static InterruptManager* inst;

InterruptManager::InterruptManager() {
  inst = this;
  load_idt();
}
InterruptManager& InterruptManager::get() { return *inst; }

static inline InterruptGate make_interrupt_gate(void f(interrupt::int_frame*)) {
  uint64_t offset = (uint64_t)f;
  const uint8_t type = 0b1110;
  const uint8_t p = 1;
  const uint8_t dpl = 0;
  return {
    .offset_1 = (uint16_t) (offset & 0xffff),
    .segment_selector = 0x8,
    .ist = 0,
    .flags = (p << 7) | (dpl << 5) | type,
    .offset_2 = (uint16_t) ((offset >> 16) & 0xffff),
    .offset_3 = (uint32_t) ((offset >> 32) & 0xffffffff),
    .zero = 0
  };
}

struct IDTRegister {
  uint16_t limit;
  uint64_t addr;
} __attribute__((packed));

void InterruptManager::load_idt() {
  std::memset(interrupt_desc_table, 0, sizeof(interrupt_desc_table));
  auto assert_no_handler = make_interrupt_gate(interrupt::assert_no_handler);
  auto id_handler = make_interrupt_gate(interrupt::id_handler);
  interrupt_desc_table[0x00] = make_interrupt_gate(interrupt::handle_div0);
  interrupt_desc_table[0x01] = make_interrupt_gate(interrupt::handle_debug);
  interrupt_desc_table[0x02] = make_interrupt_gate(interrupt::handle_nmi);
  interrupt_desc_table[0x03] = make_interrupt_gate(interrupt::handle_breakpoint);
  interrupt_desc_table[0x04] = make_interrupt_gate(interrupt::handle_overflow);
  interrupt_desc_table[0x05] = make_interrupt_gate(interrupt::handle_bound);
  interrupt_desc_table[0x06] = make_interrupt_gate(interrupt::handle_undef_opcode);
  interrupt_desc_table[0x07] = make_interrupt_gate(interrupt::handle_fpu_unavail);
  interrupt_desc_table[0x08] = make_interrupt_gate(interrupt::handle_double_fault);
  interrupt_desc_table[0x09] = make_interrupt_gate(interrupt::handle_fpu_overrun);
  interrupt_desc_table[0x0a] = make_interrupt_gate(interrupt::handle_invalid_tss);
  interrupt_desc_table[0x0b] = make_interrupt_gate(interrupt::handle_segfault);
  interrupt_desc_table[0x0c] = make_interrupt_gate(interrupt::handle_stack_segfault);
  interrupt_desc_table[0x0d] = make_interrupt_gate(interrupt::handle_gp_fault);
  interrupt_desc_table[0x0e] = make_interrupt_gate(interrupt::handle_page_fault);
  interrupt_desc_table[0x0f] = assert_no_handler;
  interrupt_desc_table[0x10] = make_interrupt_gate(interrupt::handle_fpu_fault);
  interrupt_desc_table[0x11] = make_interrupt_gate(interrupt::handle_align_check);
  interrupt_desc_table[0x12] = make_interrupt_gate(interrupt::handle_machine_check);
  interrupt_desc_table[0x13] = make_interrupt_gate(interrupt::handle_sse_fault);
  interrupt_desc_table[0x14] = make_interrupt_gate(interrupt::handle_virt_fault);
  interrupt_desc_table[0x15] = make_interrupt_gate(interrupt::handle_cp_fault);
  for (unsigned i = 0x16; i < 0x20; ++i) {
    interrupt_desc_table[i] = assert_no_handler;
  }
  interrupt_desc_table[0x20] = make_interrupt_gate(interrupt::handle_timer);
  interrupt_desc_table[0x21] = make_interrupt_gate(interrupt::handle_keyboard);
  // TODO: enable hardware interrupt handlers
  for (unsigned i = 0x22; i < 0x2c; ++i) {
    interrupt_desc_table[i] = id_handler;
  }
  interrupt_desc_table[0x2c] = make_interrupt_gate(interrupt::handle_ps2_mouse);
  interrupt_desc_table[0x2d] = id_handler;
  interrupt_desc_table[0x2e] = id_handler;
  interrupt_desc_table[0x2f] = id_handler;
  for (unsigned i = 0x30; i < 0x100; ++i) {
    interrupt_desc_table[i] = assert_no_handler;
  }
}


/** PIC stuff **/
// Ports
static const unsigned pic1 = 0x20;
static const unsigned pic1_cmd = pic1;
static const unsigned pic1_data = pic1+1;
static const unsigned pic2 = 0xa0;
static const unsigned pic2_cmd = pic2;
static const unsigned pic2_data = pic2+1;
// Init control words (ICW)
static const uint8_t icw1_use_icw4 = 1 << 0;
static const uint8_t icw1_init = 1 << 4;
static const uint8_t icw4_8086_mode = 1 << 0;

void InterruptManager::pic_send_eoi(uint8_t irq) { // end-of-interrupt
  const unsigned eoi_signal = 0x20;
  // due to PIC cascading:
  // lower PIC gets the signal if it owns the IRQ
  if (irq >= 8) io::out8(pic2_cmd, eoi_signal);
  // upper PIC gets the signal no matter what
  io::out8(pic1_cmd, eoi_signal);
}

void InterruptManager::reprogram_pics() {
  uint8_t mask1, mask2;
  mask1 = io::in8(pic1_data);
  mask2 = io::in8(pic2_data);
  debug::serial_printf("PIC1 mask %02x, PIC2 mask %02x\n", mask1, mask2);

  io::out8(pic1_cmd, icw1_init | icw1_use_icw4);
  io::out8(pic2_cmd, icw1_init | icw1_use_icw4);
  io::io_wait();
  io::out8(pic1_data, irq_offset);
  io::out8(pic2_data, irq_offset+8);
  io::io_wait();
  // TODO: explain these magic consts to let upper/lower PIC communicate?
  io::out8(pic1_data, 4);
  io::out8(pic2_data, 2);
  io::io_wait();
  io::out8(pic1_data, icw4_8086_mode);
  io::out8(pic2_data, icw4_8086_mode);
  io::io_wait();


  // TODO: more principled enable/disable masking
  mask1 &= ~(1 << 1); // enable keyboard interrupts
  io::out8(pic1_data, mask1);
  io::out8(pic2_data, mask2);
}

void InterruptManager::init_interrupts() {
  reprogram_pics(); // TODO: switch over to APIC?
  IDTRegister reg = {
    .limit = sizeof(interrupt_desc_table)-1,
    .addr = (uint64_t)interrupt_desc_table
  };
  asm volatile ("lidt %0" :: "m"(reg));
  asm volatile ("sti"::);
}

