#include "assert.h"
#include "debug_serial.h"
#include "keyboard_state.h"

using ModifierBit = KeyboardState::ModifierBit;

static KeyboardState* inst;
KeyboardState::KeyboardState() {
  inst = this;
  debug::serial_printf("KeyboardState() scan_state = %08x\n", static_cast<unsigned>(scan_state));
  debug::serial_printf("KeyboardState() &scan_state = %08x\n", (uint32_t)&scan_state);
  debug::serial_printf("KeyboardState() &key_states = %08x\n", (uint32_t)key_states);
  debug::serial_printf("KeyboardState() &lock_state = %08x\n", (uint32_t)&lock_state);
  debug::serial_printf("KeyboardState() &scan_buf = %08x\n", (uint32_t)scan_buf);
}
KeyboardState& KeyboardState::get() { return *inst; }

void KeyboardState::key_set_state(KeyCode code) {
  uint8_t state_bitfield = key_states[code / 8];
  state_bitfield |= 1 << (code % 8);
  key_states[code / 8] = state_bitfield;
  if (code == KeyCode::left_shift || code == KeyCode::right_shift) {
    mod_state |= (1 << ModifierBit::shift);
  }
  else if (code == KeyCode::left_ctrl /*|| code == KeyCode::right_ctrl*/) {
    mod_state |= (1 << ModifierBit::ctrl);
  }
  else if (code == KeyCode::left_alt /*|| code == KeyCode::right_alt*/) {
    mod_state |= (1 << ModifierBit::alt);
  }
  if (subscriber) {
    subscriber->key_down(code, mod_state, lock_state);
  }
}
void KeyboardState::key_unset_state(KeyCode code) {
  uint8_t state_bitfield = key_states[code / 8];
  state_bitfield &= ~(1 << (code % 8));
  key_states[code / 8] = state_bitfield;
  if (code == KeyCode::left_shift || code == KeyCode::right_shift) {
    mod_state &= ~(1 << ModifierBit::shift);
  }
  else if (code == KeyCode::left_ctrl /*|| code == KeyCode::right_ctrl*/) {
    mod_state &= ~(1 << ModifierBit::ctrl);
  }
  else if (code == KeyCode::left_alt /*|| code == KeyCode::right_alt*/) {
    mod_state &= ~(1 << ModifierBit::alt);
  }
  if (subscriber) {
    subscriber->key_up(code, mod_state, lock_state);
  }
}
void KeyboardState::key_update_state(KeyCode code, bool make) {
  if (make) key_set_state(code);
  else key_unset_state(code);
}


static inline uint8_t break_bit_set(uint8_t c) {
  return (c >> 7) & 0x1;
}
static inline uint8_t unset_break_bit(uint8_t c) {
  return c & ~(1 << 7);
}

void KeyboardState::handle_scan_code(uint8_t c) {
  switch (scan_state) {
    case ready: {
      if (c == 0xe0) {
        scan_state = e0_b1;
      }
      else if (c == 0xe1) {
        // FORNOW: skip
        scan_state = ready;
      }
      else if (break_bit_set(c)) {
        uint8_t code = unset_break_bit(c);
        if (code <= KeyCode::_last) {
          key_unset_state(static_cast<KeyCode>(code));
        }
      }
      else {
        uint8_t code = c;
        if (c <= KeyCode::_last) {
          key_set_state(static_cast<KeyCode>(code));
        }
      }
      break;
    }
    case e0_b1: {
      bool break_bit = break_bit_set(c);
      uint8_t code;
      if (break_bit) code = unset_break_bit(c);
      else code = c;
      switch (code) {
        // FORNOW: these codes just map to the corresponding keypad keys
        case KeyCode::enter:
        case KeyCode::left_ctrl:
        case KeyCode::left_shift:
        case KeyCode::fwd_slash:
        case KeyCode::right_shift:
        case KeyCode::left_alt:
        case KeyCode::scroll_lock:
        case KeyCode::keypad_print_screen:
        case KeyCode::keypad_home:
        case KeyCode::keypad_up:
        case KeyCode::keypad_page_up:
        case KeyCode::keypad_left:
        case KeyCode::keypad_right:
        case KeyCode::keypad_end:
        case KeyCode::keypad_down:
        case KeyCode::keypad_page_down:
        case KeyCode::keypad_insert:
        case KeyCode::keypad_del:
          key_update_state(static_cast<KeyCode>(code), !break_bit);
          scan_state = ready;
          break;
        // TODO: Microsoft keyboard (left window, right window, menu)
        // case 0x5b:
        // case 0x5c:
        // case 0x5d:
      }
      break;
    }
    default: panic("bad keyboard scan state\n");
  }
}



static char us_lower_ascii_table[KeyCode::_last+1] = {
  0, 0, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', 0, '\t',
  'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n', 0,
  'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' , ';', '\'', '`', 0, '\\',
  'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',
  0, '*', 0, ' ', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  '7', '8', '9', '-', '4', '5', '6', '+', '1', '2', '3', '0', '.',
  0, 0, 0,
};
static char us_upper_ascii_table[KeyCode::_last+1] = {
  0, 0, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', 0, '\t',
  'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', '\n', 0,
  'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L' , ':', '"', '~', 0, '|',
  'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?',
  0, '*', 0, ' ', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  '7', '8', '9', '-', '4', '5', '6', '+', '1', '2', '3', '0', '.',
  0, 0, 0,
};

bool USKeyMap::is_printable(KeyCode code) const {
  char lower = us_lower_ascii_table[code];
  return lower != 0;
}

char USKeyMap::to_ascii(KeyCode code, uint8_t mod_state, uint8_t) const {
  bool upper = (mod_state >> ModifierBit::shift) & 0x1;
  // TODO: deal with keypad numlock stuff
  if (upper) {
    return us_upper_ascii_table[code];
  }
  else {
    return us_lower_ascii_table[code];
  }
}


