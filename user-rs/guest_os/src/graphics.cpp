#include <cassert>
#include <iostream>
#include "gl_generic.h"
#include "graphics.h"

static bool compile_shader(const char* src, GLuint shader) {
  glShaderSource(shader, 1, &src, NULL);
  glCompileShader(shader);
  GLint success;
  char compile_log[512];
  glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
  if (!success) {
    glGetShaderInfoLog(shader, sizeof(compile_log), NULL, compile_log);
    std::cout << "Error on shader compile:" << std::endl
              << compile_log << std::endl;
  }
  return success;
}

static bool link_shader_prog(GLuint shader_prog, GLuint vert_shader, GLuint frag_shader) {
  glAttachShader(shader_prog, vert_shader);
  glAttachShader(shader_prog, frag_shader);
  glLinkProgram(shader_prog);
  GLint success;
  char compile_log[512];
  glGetProgramiv(shader_prog, GL_LINK_STATUS, &success);
  if (!success) {
    glGetProgramInfoLog(shader_prog, sizeof(compile_log), NULL, compile_log);
    std::cout << "Error on shader compile:" << std::endl
              << compile_log << std::endl;
  }
  return success;
}

static Graphics* inst = nullptr;
Graphics& Graphics::get() {
  assert(inst);
  return *inst;
}

Graphics::Graphics(unsigned width, unsigned height) : width(width), height(height) {
  assert(!inst); inst = this;
  
  const char* vert_shader_src = "#version 330 core\n"
      "layout (location = 0) in vec3 aPos;\n"
      "layout (location = 1) in vec2 aTexCoord;\n"
      "out vec2 TexCoord;\n"
      "void main() {\n"
      "  gl_Position = vec4(aPos, 1.0);\n"
      "  TexCoord = aTexCoord;\n"
      "}";
  const char* frag_shader_src = "#version 330 core\n"
      "in vec2 TexCoord;\n"
      "out vec4 frag_color;\n"
      "uniform sampler2D ourTexture;\n"
      "void main() {\n"
      "  frag_color = texture(ourTexture, TexCoord);\n"
      "}";

  GLuint vert_shader = glCreateShader(GL_VERTEX_SHADER);
  GLuint frag_shader = glCreateShader(GL_FRAGMENT_SHADER);
  std::cout << "Compiling vert shader" << std::endl;
  if (!compile_shader(vert_shader_src, vert_shader)) {
    return;
  }
  std::cout << "Compiling frag shader" << std::endl;
  if (!compile_shader(frag_shader_src, frag_shader)) {
    return;
  }
  shader_prog = glCreateProgram();
  std::cout << "Linking shader program" << std::endl;
  if (!link_shader_prog(shader_prog, vert_shader, frag_shader)) {
    return;
  }
  glDeleteShader(vert_shader);
  glDeleteShader(frag_shader);

  #define NVERTS 6
  GLfloat quad_verts_and_tex[] = {
    // verts
    -1.0f, -1.0f, 0.0f,
    1.0f, -1.0f, 0.0f,
    -1.0f, 1.0f, 0.0f,
    -1.0f, 1.0f, 0.0f,
    1.0f, -1.0f, 0.0f,
    1.0f, 1.0f, 0.0f,
    // tex coords
    0.0f, 1.0f,
    1.0f, 1.0f,
    0.0f, 0.0f,
    0.0f, 0.0f,
    1.0f, 1.0f,
    1.0f, 0.0f
  };
  glGenVertexArrays(1, &vert_arr_obj);
  glGenBuffers(1, &vert_buf_obj);
  glBindVertexArray(vert_arr_obj);
  glBindBuffer(GL_ARRAY_BUFFER, vert_buf_obj);
  glBufferData(GL_ARRAY_BUFFER, sizeof(quad_verts_and_tex), quad_verts_and_tex, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3*sizeof(float), 0);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 2*sizeof(float), (void*)(3*sizeof(float)*NVERTS));
  glEnableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  glGenTextures(1, &fb_tex);
  // some random gradient
  buffer = new pixel_t[width*height];
  for (unsigned i = 0; i < height; ++i) {
    for (unsigned j = 0; j < width; ++j) {
      buffer[i*width + j] = {
        .r = (uint8_t)((0xff*i)/height),
        .g = 0,
        .b = 0,
        .a = 0xff,
      };
    }
  }
  glBindTexture(GL_TEXTURE_2D, fb_tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA,
               GL_UNSIGNED_BYTE, buffer);
  glBindTexture(GL_TEXTURE_2D, 0);

  GLenum err;
  while ((err = glGetError()) != GL_NO_ERROR) {
    std::cout << "GL error: " << err << std::endl;
  }
  std::cout << "Graphics constructor done, fb_tex = " << fb_tex << std::endl;
}

Graphics::~Graphics() {
  delete[] buffer;
  glDeleteVertexArrays(1, &vert_arr_obj);
  glDeleteBuffers(1, &vert_buf_obj);
  glDeleteProgram(shader_prog);
  inst = nullptr;
}

void Graphics::draw() const {
  // FIXME: don't load entire texture if we don't need to
  blit(0, 0, width, height);
  glUseProgram(shader_prog);
  glBindTexture(GL_TEXTURE_2D, fb_tex);
  glBindVertexArray(vert_arr_obj);
  glDrawArrays(GL_TRIANGLES, 0, NVERTS);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindVertexArray(0);
}

void Graphics::blit(
    unsigned row, unsigned col, unsigned width, unsigned height) const {
  assert(row + height <= this->height);
  assert(col + width <= this->width);
  glBindTexture(GL_TEXTURE_2D, fb_tex);
  glTexSubImage2D(GL_TEXTURE_2D, 0, col, row, width, height,
                  GL_RGBA, GL_UNSIGNED_BYTE, buffer); 
  glBindTexture(GL_TEXTURE_2D, 0);
}


extern "C" {
  framebuffer_t get_framebuffer() {
    return {
      .pixels = Graphics::get().buffer,
      .width = Graphics::get().width,
      .height = Graphics::get().height,
    };
  }
}
