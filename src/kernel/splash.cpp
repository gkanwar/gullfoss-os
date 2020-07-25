#include <cstring>
#include "debug_serial.h"
#include "framebuffer.h"
#include "splash.h"

#define SPLASH_FILENAME "waterfall.bmp"

struct BitmapHeader {
  uint8_t magic[2]; // should be "BM"
  uint32_t size;
  uint8_t reserved_1[2];
  uint8_t reserved_2[2];
  uint32_t data_offset;
} __attribute__((packed));

void show_splash(const BOOTBOOT& info, pixel_t* framebuffer, Tarball& initrd) {
  if (info.fb_type != FB_ARGB) {
    // TODO: pixel reordering
    // FORNOW: splash a constant color
    pixel_t fill_color;
    switch (info.fb_type) {
      case FB_RGBA: { fill_color = 0xffcc55ff; break; }
      case FB_ABGR: { fill_color = 0xff55ccff; break; }
      case FB_BGRA: { fill_color = 0x55ccffff; break; }
      default: { return; }
    }
    for (unsigned i = 0; i < info.fb_size/sizeof(pixel_t); ++i) {
      framebuffer[i] = fill_color;
    }
    return;
  }

  // Normal splash display (FB_ARGB, matching bitmap file format)
  debug::serial_printf("Framebuffer format is good, loading bitmap\n");
  tar_file_t splash_file = initrd.find_file(SPLASH_FILENAME);
  if (!splash_file.buffer) {
    debug::serial_printf("Failed to find splash file\n");
    return;
  }
  debug::serial_printf("Found splash file %p\n", splash_file.buffer);
  const BitmapHeader* splash_header = (const BitmapHeader*)splash_file.buffer;
  if (splash_header->magic[0] != 'B' ||
      splash_header->magic[1] != 'M') {
    debug::serial_printf("Corrupt splash file\n");
    return;
  }
  debug::serial_printf("Showing splash into %p\n", framebuffer);
  const pixel_t* pixel_data = (const pixel_t*)
      ((const uint8_t*)splash_file.buffer + splash_header->data_offset);
  const lsize_t width = 800, height = 600;
  for (unsigned row = 0; row < height; ++row) {
    const pixel_t* row_data = pixel_data + (height-row-1)*width; // bitmap stores bottom up
    pixel_t* framebuffer_row_data = framebuffer + row*info.fb_scanline/sizeof(pixel_t);
    std::memcpy(framebuffer_row_data, row_data, width*sizeof(pixel_t));
  }
}
