/**
 * Implementations of all interrupt stubs registered by the system. Note: this
 * file should be compiled with -mgeneral-regs-only to ensure special register
 * state is not accidentally overwritten.
 */

#include "assert.h"
#include "debug_serial.h"
#include "interrupt_impl.h"
#include "interrupt_manager.h"
#include "io.h"
#include "keyboard_state.h"

namespace interrupt {

struct int_frame {

};

// panic if called (we expect no interrupts on these vectors)
__attribute__((interrupt))
void assert_no_handler(int_frame*) {
  panic("Unsupported interrupt called!");
}
// ignore if called (unhandled interrupts for now)
__attribute__((interrupt)) void id_handler(int_frame*) {}

__attribute__((interrupt)) void handle_div0(int_frame*) {
  debug::serial_printf("EXCEPTION: div 0\n");
}
__attribute__((interrupt)) void handle_debug(int_frame*) {}
__attribute__((interrupt)) void handle_nmi(int_frame*) {}
__attribute__((interrupt)) void handle_breakpoint(int_frame*) {}
__attribute__((interrupt)) void handle_overflow(int_frame*) {}
__attribute__((interrupt)) void handle_bound(int_frame*) {}
__attribute__((interrupt)) void handle_undef_opcode(int_frame*) {}
__attribute__((interrupt)) void handle_fpu_unavail(int_frame*) {}

__attribute__((interrupt))
void handle_double_fault(int_frame*, uword_t err_code) {
  debug::serial_printf("DOUBLE FAULT\n", err_code);
}

__attribute__((interrupt)) void handle_fpu_overrun(int_frame*) {}
__attribute__((interrupt)) void handle_invalid_tss(int_frame*) {}

__attribute__((interrupt))
void handle_segfault(int_frame*, uword_t err_code) {
  debug::serial_printf("EXCEPTION: seg fault %08x\n", err_code);
}

__attribute__((interrupt))
void handle_stack_segfault(int_frame*, uword_t err_code) {
  debug::serial_printf("EXCEPTION: stack seg fault %08x\n", err_code);
}

__attribute__((interrupt))
void handle_gp_fault(int_frame*, uword_t err_code) {
  debug::serial_printf("EXCEPTION: gp fault %08x\n", err_code);
}

__attribute__((interrupt))
void handle_page_fault(int_frame*, uword_t err_code) {
  debug::serial_printf("EXCEPTION: page fault %08x\n", err_code);
}

__attribute__((interrupt)) void handle_fpu_fault(int_frame*) {}
__attribute__((interrupt)) void handle_align_check(int_frame*) {}
__attribute__((interrupt)) void handle_machine_check(int_frame*) {}
__attribute__((interrupt)) void handle_sse_fault(int_frame*) {}
__attribute__((interrupt)) void handle_virt_fault(int_frame*) {}
__attribute__((interrupt)) void handle_cp_fault(int_frame*) {}


// IRQs from PICs
__attribute__((interrupt)) void handle_timer(int_frame*) {
  // TODO
  InterruptManager::pic_send_eoi(0x0);
}

__attribute__((interrupt)) void handle_keyboard(int_frame*) {
  const uint8_t ps2_data = 0x60;
  uint8_t c = io::in8(ps2_data);
  KeyboardState::get().handle_scan_code(c);
  debug::serial_printf("INTERRUPT: key scan code %02x\n", c);
  InterruptManager::pic_send_eoi(0x1);
}

__attribute__((interrupt)) void handle_ps2_mouse(int_frame*) {
  // TODO: qemu PS2 mouse doesn't trigger this for some reason, fix it!
  debug::serial_printf("INTERRUPT: mouse\n");
  InterruptManager::pic_send_eoi(0xc);
}

}
