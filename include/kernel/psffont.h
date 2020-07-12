#ifndef PSFFONT_H
#define PSFFONT_H

#include <stdint.h>
#include <stddef.h>

class PSFFont {
 public:
  PSFFont(const uint8_t* psf_buffer);
  const uint8_t* to_glyph(uint16_t ucs_char) const;
  uint8_t charsize;
  struct CharMap {
    uint16_t ucs_char;
    const uint8_t* glyph;
  };
 private:
  const uint8_t* glyphs;
  size_t num_glyphs;
  CharMap* char_map;
  size_t char_map_count;
  uint8_t* undef_glyph;
};

#endif
