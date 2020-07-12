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

static void fill_undef_glyph(uint8_t* undef_glyph, uint8_t charsize) {
  // 1px margin all around
  undef_glyph[0] = 0;
  undef_glyph[charsize-1] = 0;
  for (uint8_t i = 1; i < charsize-1; ++i) {
    undef_glyph[i] = 0b01111110;
  }
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
  unsigned j = 0;
  for (unsigned i = 0; i < num_glyphs; ++i) {
    const uint8_t* cur_glyph = &glyphs[i*charsize];
    bool skip = false;
    while (*map_info != PSF1_SEP) {
      if (*map_info == PSF1_SEQ) {
        // TODO: handle sequenced unicode chars
        ++map_info;
        skip = true;
        continue;
      }
      if (!skip) {
        char_map[j] = {
          .ucs_char = *map_info,
          .glyph = cur_glyph
        };
        ++j;
      }
      ++map_info;
      debug::serial_printf("... map info done (j = %u)\n", j);
      // FORNOW HACK!!!
      if (j > MAX_CHAR_MAP) break;
      debug::serial_printf("... map info continue\n", j);
    }
    ++map_info;
    // FORNOW HACK!!!
    if (j > MAX_CHAR_MAP) break;
  }
  // Last CharMap is a special "undefined" glyph
  undef_glyph = new uint8_t[charsize];
  fill_undef_glyph(undef_glyph, charsize);
  char_map[MAX_CHAR_MAP] = {
    .ucs_char = 0,
    .glyph = undef_glyph
  };
}

const uint8_t* PSFFont::to_glyph(uint16_t ucs_char) const {
  // FORNOW dumb linear scan
  for (unsigned i = 0; i < MAX_CHAR_MAP; ++i) {
    if (char_map[i].ucs_char == ucs_char) {
      return char_map[i].glyph;
    }
  }
  return char_map[MAX_CHAR_MAP].glyph;
}
