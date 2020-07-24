#ifndef SYSCALLS_H
#define SYSCALLS_H

#include <types.h>

extern "C" {

  /// kernel
  void yield();
  void exit(u8 code);

  /// TODO: proper driver API registration
  /// graphics
  struct framebuffer_t {
    u32* pixels;
    uint width;
    uint height;
    // TODO: scanline
  };

  framebuffer_t get_framebuffer();

}

#endif
