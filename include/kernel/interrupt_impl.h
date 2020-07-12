#ifndef INTERRUPT_IMPL_H
#define INTERRUPT_IMPL_H

namespace interrupt {

#ifdef __x86_64__
typedef unsigned long long int uword_t;
#else
typedef unsigned int uword_t;
#endif

struct int_frame {
  uword_t xip;
  uword_t cs;
  uword_t xflags;
  uword_t xsp;
  uword_t ss;
};

// panic if called (we expect no interrupts on these vectors)
__attribute__((interrupt)) void assert_no_handler(int_frame*);
// ignore if called (unhandled interrupts for now)
__attribute__((interrupt)) void id_handler(int_frame*);

__attribute__((interrupt)) void handle_div0(int_frame*);
__attribute__((interrupt)) void handle_debug(int_frame*);
__attribute__((interrupt)) void handle_nmi(int_frame*);
__attribute__((interrupt)) void handle_breakpoint(int_frame*);
__attribute__((interrupt)) void handle_overflow(int_frame*);
__attribute__((interrupt)) void handle_bound(int_frame*);
__attribute__((interrupt)) void handle_undef_opcode(int_frame*);
__attribute__((interrupt)) void handle_fpu_unavail(int_frame*);
__attribute__((interrupt)) void handle_double_fault(int_frame*, uword_t);
__attribute__((interrupt)) void handle_fpu_overrun(int_frame*);
__attribute__((interrupt)) void handle_invalid_tss(int_frame*);
__attribute__((interrupt)) void handle_segfault(int_frame*, uword_t);
__attribute__((interrupt)) void handle_stack_segfault(int_frame*, uword_t);
__attribute__((interrupt)) void handle_gp_fault(int_frame*, uword_t);
__attribute__((interrupt)) void handle_page_fault(int_frame*, uword_t);
__attribute__((interrupt)) void handle_fpu_fault(int_frame*);
__attribute__((interrupt)) void handle_align_check(int_frame*);
__attribute__((interrupt)) void handle_machine_check(int_frame*);
__attribute__((interrupt)) void handle_sse_fault(int_frame*);
__attribute__((interrupt)) void handle_virt_fault(int_frame*);
__attribute__((interrupt)) void handle_cp_fault(int_frame*);
__attribute__((interrupt)) void handle_timer(int_frame*);
__attribute__((interrupt)) void handle_keyboard(int_frame*);
__attribute__((interrupt)) void handle_ps2_mouse(int_frame*);

}

#endif
