#ifndef TERMINAL_H
#define TERMINAL_H

#include <stddef.h>
#include <stdint.h>
#include "debug_serial.h"
#include "framebuffer.h"
#include "psffont.h"

/**
 * Uses provided glyphs to offer a monospace terminal interface through
 * framebuffer rendering.
 */

class FBTerminal {
 public:
  FBTerminal(Framebuffer* fb, PSFFont font)
      : charheight(font.charsize), framebuffer(fb), font(font) {
    width = fb->cols / charwidth;
    height = fb->rows / charheight;
  }
  void clear() {
    debug::serial_printf("FBTerminal clear %08x\n", bg_color);
    debug::serial_printf("Buffer = %p\n", framebuffer->buffer);
    framebuffer->clear(bg_color);
  }
  void set_fg_color(pixel_t fg_color) {
    this->fg_color = fg_color;
  }
  void set_bg_color(pixel_t bg_color) {
    this->bg_color = bg_color;
  }
  void putc_at(char, size_t r, size_t c);
  void putc_at(char, pixel_t fg_color, size_t r, size_t c);
  // write a NUL-terminated string from current location with wrapping
  // void write_string(const char*, size_t& r, size_t& c);
  size_t width, height, charheight;
  const size_t charwidth{8};
 private:
  pixel_t fg_color, bg_color;
  Framebuffer* framebuffer;
  PSFFont font;
};

#endif
