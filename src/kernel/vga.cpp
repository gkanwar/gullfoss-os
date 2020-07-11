#include <stddef.h>
#include <cstring>
#include "assert.h"
#include "io.h"
#include "vga.h" // TODO: rename to TTY? I don't really understand what TTY means...

using namespace std;
using Color = VGATerminal::Color;

static inline uint8_t vga_entry_color(Color fg, Color bg)
{
  return fg | bg << 4;
}
static inline uint16_t vga_entry(unsigned char uc, uint8_t color)
{
  return (uint16_t) uc | (uint16_t) color << 8;
}

uint8_t VGATerminal::get_color() const { return color; }
void VGATerminal::set_color(uint8_t color) { color = color; }
void VGATerminal::set_color(Color fg, Color bg) {
  set_color(vga_entry_color(fg, bg));
}
void VGATerminal::set_indent(size_t indent) { indent = indent; }

void VGATerminal::clear() {
  for (size_t y = 0; y < VGATerminal::HEIGHT; y++) {
    for (size_t x = 0; x < VGATerminal::WIDTH; x++) {
      const size_t index = y * VGATerminal::WIDTH + x;
      buffer[index] = vga_entry(' ', color);
    }
  }
}

VGATerminal::VGATerminal() :
    row(0), column(0), indent(0),
    color(vga_entry_color(Color::light_grey, Color::black)),
    buffer((uint16_t*) VIRT_TERM_BUFFER) {
  clear();
}

void VGATerminal::putc(char c, uint8_t color, size_t x, size_t y) {
  const size_t index = y * VGATerminal::WIDTH + x;
  buffer[index] = vga_entry(c, color);
}

void VGATerminal::advance_row() {
  if (++row == VGATerminal::HEIGHT) {
    row = 0;
  }
}

void VGATerminal::cr() { column = indent; }
void VGATerminal::crlf() { cr(); advance_row(); }

void VGATerminal::advance_char() {
  if (++column == VGATerminal::WIDTH) {
    crlf();
  }
}
void VGATerminal::retreat_char() {
  if (column == indent) {
    column = VGATerminal::WIDTH-1;
    if (row == 0) row = VGATerminal::HEIGHT-1;
    else row--;
  }
  else {
    column--;
  }
}
void VGATerminal::set_pos(size_t r, size_t c) {
  assert(r < VGATerminal::HEIGHT, "bad pos row coord");
  assert(c < VGATerminal::WIDTH, "bad pos col coord");
  row = r;
  column = c;
}
void VGATerminal::set_cursor(size_t r, size_t c) {
  assert(r < VGATerminal::HEIGHT, "bad cursor row coord");
  assert(c < VGATerminal::WIDTH, "bad cursor col coord");
  const size_t index = r * VGATerminal::WIDTH + c;
  io::out8(0x3d4, 0x0f);
  io::out8(0x3d5, (uint8_t)(index & 0xff));
  io::out8(0x3d4, 0x0e);
  io::out8(0x3d5, (uint8_t)((index >> 8) & 0xff));
}
void VGATerminal::set_cursor_to_pos() {
  set_cursor(row, column);
}

void VGATerminal::putc(char c, bool advance) {
  if (c == '\n') {
    crlf();
  }
  else if (c == '\r') {
    cr();
  }
  /* TODO: other special chars? */
  else {
    putc(c, color, column, row);
    if (advance) advance_char();
  }
}
void VGATerminal::putc(char c) { putc(c, true); }

void VGATerminal::write(const char* data, size_t size) {
  for (size_t i = 0; i < size; i++)
    putc(data[i]);
}

void VGATerminal::write_string(const char* data) {
  write(data, strlen(data));
}
