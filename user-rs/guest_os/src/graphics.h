/**
 * Mock interface to graphics driver. For now we only support a very simple
 * linear framebuffer interface.
 */

#pragma once

struct pixel_t {
  uint8_t r;
  uint8_t g;
  uint8_t b;
  uint8_t a;
} __attribute__((packed));

class Graphics {
 public:
  Graphics(unsigned width, unsigned height);
  ~Graphics();
  static Graphics& get();
  void draw() const;
  void blit(unsigned, unsigned, unsigned, unsigned) const;
  pixel_t* buffer;
  unsigned width, height;
 private:
  // framebuffer is maintained as a GL texture, and drawn using a simple quad
  // with texture shader
  GLuint fb_tex;
  GLuint vert_arr_obj, vert_buf_obj;
  GLuint shader_prog;
};

struct framebuffer_t {
  pixel_t* pixels;
  unsigned width, height;
} __attribute((packed));

extern "C" framebuffer_t get_framebuffer();
