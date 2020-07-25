#include "debug_serial.h"
#include "syscalls.h"

extern "C" {
  
  void yield() {
    // FORNOW: just hlt
    asm volatile("hlt"::);
  }

  void exit(u8 code) {
    // FORNOW: do nothing
    debug::serial_printf("syscall: exit(%d)\n", (int)code);
  }

}