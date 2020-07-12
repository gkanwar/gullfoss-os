#include "terminal.h"
#include "util.h"

void FBTerminal::putc_at(char c, size_t row, size_t col) {
  putc_at(c, fg_color, row, col);
}
void FBTerminal::putc_at(char c, pixel_t fg_color, size_t row, size_t col) {
  uint16_t ucs_char = (uint16_t)c;
  const uint8_t* glyph = font.to_glyph(ucs_char);
  pixel_t* base = framebuffer->buffer
      + row*charheight*framebuffer->scanline/sizeof(pixel_t)
      + col*charwidth;
  for (unsigned glyph_r = 0; glyph_r < charheight;
       ++glyph_r, base += framebuffer->scanline/sizeof(pixel_t)) {
    for (unsigned i = 0; i < charwidth; ++i) {
      if (util::get_bit(glyph[glyph_r], charwidth-i)) {
        base[i] = fg_color;
      }
      else {
        base[i] = bg_color;
      }
    }
  }
}
