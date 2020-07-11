#ifndef VGA_H
#define VGA_H

#include <stddef.h>
#include <stdint.h>
#include "kernel.h"

#define TERM_BUFFER 0xB8000
#define VIRT_TERM_BUFFER (TERM_BUFFER + KERNEL_BASE)
#define TERM_BUFFER_LEN 0x1000

// TODO: Some of the advance and retreat semantics are weird
class VGATerminal {
 public:
  static const size_t WIDTH = 80;
  static const size_t HEIGHT = 25;

  enum Color {
    black = 0,
    blue = 1,
    green = 2,
    cyan = 3,
    red = 4,
    magenta = 5,
    brown = 6,
    light_grey = 7,
    dark_grey = 8,
    light_blue = 9,
    light_green = 10,
    light_cyan = 11,
    light_red = 12,
    light_magenta = 13,
    light_brown = 14,
    white = 15,
  };

  VGATerminal();
  void clear();
  uint8_t get_color() const;
  void set_color(Color fg, Color bg);
  void set_color(uint8_t color);
  void set_indent(size_t indent);
  void putc(char);
  void putc(char, bool advance);
  void write(const char* data, size_t size);
  void write_string(const char* data);
  void advance_row();
  void advance_char();
  void retreat_char();
  void set_pos(size_t r, size_t c);
  void set_cursor(size_t r, size_t c);
  void set_cursor_to_pos();
  void cr();
  void crlf();
 private:
  void putc(char c, uint8_t color, size_t x, size_t y);
  size_t row, column, indent;
  uint8_t color;
  uint16_t* buffer;
};

#endif
