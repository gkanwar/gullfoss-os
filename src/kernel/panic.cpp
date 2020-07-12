#include "assert.h"
#include "debug_serial.h"
#include "kernel.h"

[[noreturn]] void panic(const char* msg) {
  debug::serial_printf("\033[31mKernel panic: %s\033[0m\n", msg);
  // TODO: Drop to VGA text mode and display this message?
  // VGATerminal terminal;
  // terminal.set_color(Color::white, Color::magenta);
  // terminal.clear();
  // terminal.write_string("\n\n\n\n\n\n\n\n");
  // const char title[] = "== " PROJ_NAME " kernel panic ==";
  // unsigned off = (VGATerminal::WIDTH - sizeof(title) - 2)/2;
  // for (unsigned i = 0; i < off; ++i) {
  //   terminal.write_string(" ");
  // }
  // terminal.write_string(title);
  // terminal.set_indent(4);
  // terminal.write_string("\n\n");
  // terminal.write_string("We're having too much Fun, something broke!\n\n");
  // terminal.write_string("Error: ");
  // terminal.write_string(msg);
  while (true) { asm volatile ("cli; hlt"::); }
}
