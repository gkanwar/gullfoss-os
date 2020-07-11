#ifndef KEYBOARD_STATE_H
#define KEYBOARD_STATE_H

#include <stdint.h>

/**
 * One-byte codes for all key scan codes we could get. We use the standard
 * one- byte make codes whenever it is possible (0x0 - 0x7f). The multi-byte
 * codes are translated to fill in the upper half of the one-byte space (0x80
 * - 0xff).
 */
enum KeyCode {
  // base codes
  esc = 0x01,
  n1 = 0x02, exclamation = n1,
  n2 = 0x03, at = n2,
  n3 = 0x04, hash = n3,
  n4 = 0x05, dollar = n4,
  n5 = 0x06, percent = n5,
  n6 = 0x07, caret = n6,
  n7 = 0x08, ampersand = n7,
  n8 = 0x09, asterisk = n8,
  n9 = 0x0a, left_paren = n9,
  n0 = 0x0b, right_paren = n0,
  dash = 0x0c, underscore = dash,
  equal = 0x0d, plus = equal,
  backspace = 0x0e,
  tab = 0x0f,
  q = 0x10, w = 0x11, e = 0x12, r = 0x13, t = 0x14, y = 0x15, u = 0x16,
  i = 0x17, o = 0x18, p = 0x19,
  left_sq = 0x1a, left_curly = left_sq,
  right_sq = 0x1b, right_curly = right_sq,
  enter = 0x1c,
  left_ctrl = 0x1d,
  a = 0x1e, s = 0x1f, d = 0x20, f = 0x21, g = 0x22, h = 0x23, j = 0x24,
  k = 0x25, l = 0x26,
  semicolon = 0x27, colon = semicolon,
  quote = 0x28,
  backtick = 0x29, tilde = backtick,
  left_shift = 0x2a,
  bwd_slash = 0x2b, pipe = bwd_slash,
  z = 0x2c, x = 0x2d, c = 0x2e, v = 0x2f, b = 0x30, n = 0x31, m = 0x32,
  comma = 0x33, left_angle = comma,
  period = 0x34, right_angle = period,
  fwd_slash = 0x35, question = fwd_slash,
  right_shift = 0x36,
  keypad_asterisk = 0x37,
  left_alt = 0x38,
  spacebar = 0x39,
  caps_lock = 0x3a,
  f1 = 0x3b, f2 = 0x3c, f3 = 0x3d, f4 = 0x3e, f5 = 0x3f, f6 = 0x40, f7 = 0x41,
  f8 = 0x42, f9 = 0x43, f10 = 0x44,
  num_lock = 0x45, scroll_lock = 0x46,
  keypad_7 = 0x47, keypad_8 = 0x48, keypad_9 = 0x49,
  keypad_dash = 0x4a,
  keypad_4 = 0x4b, keypad_5 = 0x4c, keypad_6 = 0x4d,
  keypad_plus = 0x4e,
  keypad_1 = 0x4f, keypad_2 = 0x50, keypad_3 = 0x51,
  keypad_0 = 0x52, keypad_period = 0x53,
  f11 = 0x57, f12 = 0x58,
  del = 0x59,
  // keypad names without numlock
  keypad_print_screen = keypad_asterisk,
  keypad_home = keypad_7,
  keypad_up = keypad_8,
  keypad_page_up = keypad_9,
  keypad_left = keypad_4,
  keypad_right = keypad_6,
  keypad_end = keypad_1,
  keypad_down = keypad_2,
  keypad_page_down = keypad_3,
  keypad_del = keypad_period,
  keypad_insert = keypad_0,
  // TODO: translated codes
  _last = del
};

class KeyboardSubscriber {
 public:
  virtual void key_down(KeyCode code, uint8_t mod_state, uint8_t lock_state) = 0;
  virtual void key_up(KeyCode code, uint8_t mod_state, uint8_t lock_state) = 0;
};

class KeyboardState {
 public:
  KeyboardState();
  static KeyboardState& get();

  void set_subscriber(KeyboardSubscriber& sub) {
    subscriber = &sub;
  }


  void handle_scan_code(uint8_t c);
  void key_set_state(KeyCode code);
  void key_unset_state(KeyCode code);
  void key_update_state(KeyCode code, bool make);

  enum LockBit {
    caps = 0,
    num = 1,
    scroll = 2,
  };
  enum ModifierBit {
    shift = 0,
    ctrl = 1,
    alt = 2,
    super = 3,
  };
  
 private:
  KeyboardSubscriber* subscriber = nullptr;
  uint8_t key_states[(KeyCode::_last+1)/8] = {0};
  uint8_t lock_state = 0;
  uint8_t mod_state = 0;
  uint8_t scan_buf[3]; // TODO (long extended codes)
  enum ScanState {ready, e0_b1}; // TODO: more extended codes?
  ScanState scan_state = ScanState::ready;
};

class KeyMap {
 public:
  virtual bool is_printable(KeyCode code) const = 0;
  virtual char to_ascii(KeyCode code, uint8_t mod_state, uint8_t lock_state) const = 0;
};
class USKeyMap : public KeyMap {
 public:
  bool is_printable(KeyCode code) const override;
  char to_ascii(KeyCode code, uint8_t mod_state, uint8_t lock_state) const override;
};


#endif
