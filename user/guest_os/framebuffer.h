#pragma once

class Framebuffer {
 public:
  Framebuffer(unsigned width, unsigned height);
  static Framebuffer& get();
  
};
