#include "debug_serial.h"
#include "keyboard_state.h"
#include "shell.h"

using ModifierBit = KeyboardState::ModifierBit;

namespace app {

void Shell::key_down(KeyCode code, uint8_t mod_state, uint8_t lock_state) {
  char c = key_map.to_ascii(code, mod_state, lock_state);
  debug::serial_printf("key_down %c\n", c);
  term.putc_at(c, 0, 0);
  // if (code == KeyCode::backspace) {
  //   term.retreat_char();
  //   term.putc(' ', false);
  // }
  // else if ((mod_state >> ModifierBit::ctrl) & 0x1) {
  //   // TODO: ctrl codes
  // }
  // else if ((mod_state >> ModifierBit::alt) & 0x1) {
  //   // TODO: alt codes
  // }
  // else if (c) {
  //   term.putc(c);
  // }
  // term.set_cursor_to_pos();
}
void Shell::key_up(KeyCode, uint8_t, uint8_t) {}

void Shell::main() {
  // FORNOW: just hlt for keyboard input
  while (true) { asm volatile ("hlt"::); }
}

}
