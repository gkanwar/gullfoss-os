#include "debug_serial.h"
#include "interrupt_manager.h"
#include "task_manager.h"
#include "syscalls.h"

extern "C" {

  void yield() {
    ScopedInterruptGuard guard;
    TaskManager::get().yield();
  }

  void exit(u8 code) {
    // FORNOW: do nothing
    debug::serial_printf("syscall: exit(%d)\n", (int)code);
  }

}
