#ifndef TEST_H
#define TEST_H

#include "debug_serial.h"

namespace test {

#define ESC_RED "\033[31m"
#define ESC_GREEN "\033[32m"
#define ESC_END "\033[0m"

template<typename Test>
inline void pretty_print_test(const char* name, Test t) {
  if (t()) {
    debug::serial_printf(ESC_GREEN "[PASS %s]\n" ESC_END, name);
  }
  else {
    debug::serial_printf(ESC_RED "[FAIL %s]\n" ESC_END, name);
  }
}

}

void test_kernel_early_main();
void test_kernel_stage1();

#endif
