#ifndef SHELL_H
#define SHELL_H

#include "keyboard_state.h"
#include "framebuffer.h"
#include "terminal.h"

namespace app {

class Shell : public KeyboardSubscriber {
 public:
  Shell(const KeyMap& key_map, FBTerminal& term)
      : key_map(key_map), term(term), pos_r(0), pos_c(0) {
    this->term.set_fg_color(FG_COLOR);
    this->term.set_bg_color(BG_COLOR);
    this->term.clear();
    write_string("~~ Welcome to shell (type some stuff) ~~\n");
    this->term.putc_at('!', pos_r, pos_c);
  }
  void cr() { pos_c = 0; }
  void crlf() { // TODO: paging
    cr();
    if (++pos_r >= term.height) {
      pos_r = 0;
    }
  }
  void advance() {
    if (++pos_c >= term.width) {
      crlf();
    }
  }
  void retreat() {
    if (pos_c == 0) {
      pos_c = term.width-1;
      if (pos_r == 0) {
        pos_r = term.height-1;
      }
      else { --pos_r; }
    }
    else { --pos_c; }
  }
  void putc_advance(char c) {
    if (c == '\n') {
      crlf();
    }
    else if (c == '\r') {
      cr();
    }
    else {
      term.putc_at(c, pos_r, pos_c);
      advance();
    }
  }
  void write_string(const char* s) {
    for (char c = *s; c != '\0'; ++s, c = *s) {
      putc_advance(c);
    }
  }
  void key_down(KeyCode code, uint8_t mod_state, uint8_t lock_state) override;
  void key_up(KeyCode code, uint8_t mod_state, uint8_t lock_state) override;
  void main();
  static const pixel_t FG_COLOR = 0xffcccccc;
  static const pixel_t BG_COLOR = 0xff000000;
 private:
  const KeyMap& key_map;
  FBTerminal term;
  size_t pos_r, pos_c;
};

}

#endif
