#ifndef SHELL_H
#define SHELL_H

#include "keyboard_state.h"
#include "vga.h"

using Color = VGATerminal::Color;

namespace app {

class Shell : public KeyboardSubscriber {
 public:
  // TODO: Not sure about giving a term here to copy, seems mucky
  Shell(const KeyMap& key_map, VGATerminal& term) : key_map(key_map), term(term) {
    this->term.set_pos(0, 0);
    this->term.clear();
    this->term.set_color(Color::light_grey, Color::black);
    this->term.write_string("~~ Welcome to shell (type some stuff) ~~\n");
    this->term.set_cursor(1, 0);
  }
  void key_down(KeyCode code, uint8_t mod_state, uint8_t lock_state) override;
  void key_up(KeyCode code, uint8_t mod_state, uint8_t lock_state) override;
  void main();

 private:
  const KeyMap& key_map;
  VGATerminal term;
};

}

#endif
