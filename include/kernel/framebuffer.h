#ifndef FRAMEBUFFER_H
#define FRAMEBUFFER_H

#include <stddef.h>
#include <stdint.h>
#include <types.h>
#include "debug_serial.h"

typedef uint32_t pixel_t;

class Framebuffer {
 public:
  Framebuffer(pixel_t framebuffer[], lsize_t length, uint rows,
              uint cols, lsize_t scanline)
      : length(length), scanline(scanline), rows(rows), cols(cols), 
        buffer(framebuffer) {}
  void clear(pixel_t color) {
    debug::serial_printf("Clearing framebuffer (size %llu)\n", length);
    for (uint i = 0; i < length/sizeof(pixel_t); ++i) {
      buffer[i] = color;
    }
  }
  lsize_t length, scanline;
  uint rows, cols;
  pixel_t* buffer;
};

#endif
