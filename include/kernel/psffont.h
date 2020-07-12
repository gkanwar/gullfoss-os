#ifndef PSFFONT_H
#define PSFFONT_H

#include <stdint.h>
#include <stddef.h>

#define MAX_CHAR_MAP 64

class PSFFont {
 public:
  PSFFont(const uint8_t* psf_buffer);
  const uint8_t* to_glyph(uint16_t ucs_char) const;
  uint8_t charsize;
 private:
  struct CharMap {
    uint16_t ucs_char;
    const uint8_t* glyph;
  };
  const uint8_t* glyphs;
  size_t num_glyphs;
  // TODO: when this array is too large we get a HeapAllocator::get() crash?!
  CharMap char_map[MAX_CHAR_MAP+1];
  uint8_t* undef_glyph;
};

#endif
