#define GL_SILENCE_DEPRECATION
#define GLFW_INCLUDE_GLCOREARB
#include <GLFW/glfw3.h>
#include <chrono>
#include <iostream>
#include <thread>
#include <stdint.h>

// FORNOW: pull in haskell FFIs, we statically link against the lib
#include "Graphics/Wayland_stub.h"

using namespace std::chrono_literals;
typedef std::chrono::high_resolution_clock hr_clock;
typedef std::chrono::milliseconds ms;

#define WIDTH 800
#define HEIGHT 600

struct pixel_t {
  uint8_t a;
  uint8_t b;
  uint8_t g;
  uint8_t r;
} __attribute__((packed));

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

bool compile_shader(const char* src, GLuint shader) {
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

bool link_shader_prog(GLuint shader_prog, GLuint vert_shader, GLuint frag_shader) {
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

int main(int argc, char** argv) {
  hs_init(&argc, &argv);
  
  if (!glfwInit()) return 1;

  #ifdef __APPLE__
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  #endif

  GLFWwindow* window = glfwCreateWindow(WIDTH, HEIGHT, "gullfoss VM", NULL, NULL);
  if (!window) {
    glfwTerminate();
    return 2;
  }

  glfwMakeContextCurrent(window);

  GLuint vert_shader = glCreateShader(GL_VERTEX_SHADER);
  GLuint frag_shader = glCreateShader(GL_FRAGMENT_SHADER);
  std::cout << "Compiling vert shader" << std::endl;
  if (!compile_shader(vert_shader_src, vert_shader)) {
    return 3;
  }
  std::cout << "Compiling frag shader" << std::endl;
  if (!compile_shader(frag_shader_src, frag_shader)) {
    return 3;
  }
  GLuint shader_prog = glCreateProgram();
  std::cout << "Linking shader program" << std::endl;
  if (!link_shader_prog(shader_prog, vert_shader, frag_shader)) {
    return 3;
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
  GLuint vbo, vao;
  glGenVertexArrays(1, &vao);
  glGenBuffers(1, &vbo);
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(quad_verts_and_tex), quad_verts_and_tex, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3*sizeof(float), 0);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 2*sizeof(float), (void*)(3*sizeof(float)*NVERTS));
  glEnableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  GLuint tex;
  glGenTextures(1, &tex);
  // some random gradient
  pixel_t buf[WIDTH*HEIGHT];
  for (unsigned i = 0; i < HEIGHT; ++i) {
    for (unsigned j = 0; j < WIDTH; ++j) {
      buf[i*WIDTH + j] = {
        .a = 0xff,
        .b = 0,
        .g = 0,
        .r = (uint8_t)((0xff*i)/HEIGHT),
      };
    }
  }
  glBindTexture(GL_TEXTURE_2D, tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, WIDTH, HEIGHT, 0, GL_RGBA,
               GL_UNSIGNED_INT_8_8_8_8, buf);
  glBindTexture(GL_TEXTURE_2D, 0);

  // give Wayland the buffer
  HsStablePtr wayland_handle = waylandStartThread(WIDTH, HEIGHT);

  auto last_frame = hr_clock::now();
  unsigned ind = 0;
  while (!glfwWindowShouldClose(window)) {
    auto now = hr_clock::now();
    if ((now - last_frame) >= 16ms) {
      last_frame = now;
      uint8_t* new_screen = (uint8_t*)waylandGetScreen(wayland_handle);
      // blit in damage (TODO: smart blit?)
      glBindTexture(GL_TEXTURE_2D, tex);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, WIDTH, HEIGHT, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, new_screen);
      glUseProgram(shader_prog);
      glBindTexture(GL_TEXTURE_2D, tex);
      glBindVertexArray(vao);
      glDrawArrays(GL_TRIANGLES, 0, NVERTS);
      glfwSwapBuffers(window);
    }
    glfwPollEvents();
    std::this_thread::sleep_for(10ms);
  }

  glDeleteVertexArrays(1, &vao);
  glDeleteBuffers(1, &vbo);
  glDeleteProgram(shader_prog);

  glfwTerminate();
  hs_exit();
  return 0;
}
