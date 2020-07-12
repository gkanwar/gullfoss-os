#include "assert.h"
#include "debug_serial.h"
#include "psffont.h"
#include "util.h"

#define PSF1_MAGIC0     0x36
#define PSF1_MAGIC1     0x04

#define PSF1_MODE512    0
#define PSF1_MODEHASTAB 1
#define PSF1_MODEHASSEQ 2

#define PSF1_SEP   0xFFFF
#define PSF1_SEQ   0xFFFE

struct psf1_header {
  unsigned char magic[2];
  unsigned char mode;
  unsigned char charsize;
};

using CharMap = PSFFont::CharMap;

static void fill_undef_glyph(uint8_t* undef_glyph, uint8_t charsize) {
  // 1px margin all around
  undef_glyph[0] = 0;
  undef_glyph[charsize-1] = 0;
  for (uint8_t i = 1; i < charsize-1; ++i) {
    undef_glyph[i] = 0b01111110;
  }
}

// FORNOW: parse the base char mappings, skip composites
static unsigned parse_unicode_map(
    const uint16_t* map_info, CharMap* out,
    const uint8_t* glyphs, unsigned charsize, unsigned num_glyphs) {
  unsigned count = 0;
  unsigned glyph_i = 0;
  bool scanning_base_chars = true;
  while (glyph_i < num_glyphs) {
    if (*map_info == PSF1_SEP) {
      ++glyph_i;
      scanning_base_chars = true;
    }
    else if (*map_info == PSF1_SEQ) {
      // after PSF1_SEQ we get composite unicodes (I think?)
      scanning_base_chars = false;
    }
    else if (scanning_base_chars) {
      // it's a base unicode char we want to translate
      if (out) {
        out[count] = {
          .ucs_char = *map_info,
          .glyph = &glyphs[glyph_i*charsize]
        };
      }
      ++count;
    }      
    ++map_info;
  }
  return count;
}

PSFFont::PSFFont(const uint8_t* psf_buffer) {
  const psf1_header* header = (const psf1_header*)psf_buffer;
  assert(header->magic[0] == PSF1_MAGIC0 &&
         header->magic[1] == PSF1_MAGIC1, "psf font corrupted");
  assert(util::get_bit(header->mode, PSF1_MODEHASTAB),
         "psf font needs a unicode table");
  charsize = header->charsize;
  num_glyphs = util::get_bit(header->mode, PSF1_MODE512) ? 512 : 256;
  glyphs = (const uint8_t*)(header+1);
  const uint16_t* map_info = (const uint16_t*)(glyphs + num_glyphs*charsize);
  // Stage 1: count how many characters we have mapped
  char_map_count = parse_unicode_map(map_info, nullptr, glyphs, charsize, num_glyphs);
  char_map = new CharMap[char_map_count];
  // Stage 2: write mappings into instantiated array
  parse_unicode_map(map_info, char_map, glyphs, charsize, num_glyphs);
  // make sure to display something for undefined characters
  undef_glyph = new uint8_t[charsize];
  fill_undef_glyph(undef_glyph, charsize);
}

const uint8_t* PSFFont::to_glyph(uint16_t ucs_char) const {
  // FORNOW dumb linear scan
  for (unsigned i = 0; i < char_map_count; ++i) {
    if (char_map[i].ucs_char == ucs_char) {
      return char_map[i].glyph;
    }
  }
  return undef_glyph;
}
