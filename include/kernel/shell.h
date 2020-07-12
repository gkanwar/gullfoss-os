#ifndef SHELL_H
#define SHELL_H

#include "keyboard_state.h"
#include "framebuffer.h"
#include "terminal.h"

namespace app {

class Shell : public KeyboardSubscriber {
 public:
  Shell(const KeyMap& key_map, FBTerminal& term) : key_map(key_map), term(term) {
    this->term.set_fg_color(FG_COLOR);
    this->term.set_bg_color(BG_COLOR);
    this->term.clear();
    // this->term.write_string("~~ Welcome to shell (type some stuff) ~~\n");
    this->term.putc_at('!', 0, 0);
  }
  void key_down(KeyCode code, uint8_t mod_state, uint8_t lock_state) override;
  void key_up(KeyCode code, uint8_t mod_state, uint8_t lock_state) override;
  void main();
  static const pixel_t FG_COLOR = 0xffcccccc;
  static const pixel_t BG_COLOR = 0xff000000;
 private:
  const KeyMap& key_map;
  FBTerminal term;
};

}

#endif
