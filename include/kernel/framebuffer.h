#ifndef FRAMEBUFFER_H
#define FRAMEBUFFER_H

#include <stddef.h>
#include <stdint.h>
#include "debug_serial.h"

typedef uint32_t pixel_t;

class Framebuffer {
 public:
  Framebuffer(pixel_t framebuffer[], size_t length, size_t rows,
              size_t cols, size_t scanline)
      : length(length), rows(rows), cols(cols), scanline(scanline),
        buffer(framebuffer) {}
  void clear(pixel_t color) {
    debug::serial_printf("Clearing framebuffer (size %llu)\n", length);
    for (unsigned i = 0; i < length/sizeof(pixel_t); ++i) {
      buffer[i] = color;
    }
  }
  size_t length, rows, cols, scanline;
  pixel_t* buffer;
};

#endif
