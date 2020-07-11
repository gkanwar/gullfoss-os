#ifndef TEST_H
#define TEST_H

#include "vga.h"

using Color = VGATerminal::Color;

namespace test {

template<typename Test>
void pretty_print_test(const char* name, VGATerminal& term, Test t) {
  auto color = term.get_color();
  if (t()) {
    term.set_color(Color::green, Color::black);
    term.write_string("[PASS ");
    term.write_string(name);
    term.write_string("]\n");
  }
  else {
    term.set_color(Color::red, Color::black);
    term.write_string("[FAIL ");
    term.write_string(name);
    term.write_string("]\n");
  }
  term.set_color(color);
}

}

#endif
