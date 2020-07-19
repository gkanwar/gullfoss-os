#include <chrono>
#include <dlfcn.h>
#include <iostream>
#include <thread>

#include "gl_generic.h"
#include "graphics.h"
#include "input_events.h"

#define WIDTH 800
#define HEIGHT 600

using namespace std::chrono_literals;
typedef std::chrono::high_resolution_clock hr_clock;
typedef std::chrono::milliseconds ms;
typedef unsigned uint;

int init_glfw(GLFWwindow* &window, int& width, int& height) {
  if (!glfwInit()) return 1;
  #ifdef __APPLE__
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  #endif
  window = glfwCreateWindow(WIDTH, HEIGHT, "gullfoss VM", NULL, NULL);
  if (!window) {
    glfwTerminate();
    return 2;
  }
  glfwMakeContextCurrent(window);
  glfwGetWindowSize(window, &width, &height);

  #ifndef __APPLE__
  if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {
    std::cout << "Failed to load GL using GLAD" << std::endl;
    return 3;
  }
  #endif
  return 0;
}
void term_glfw() {
  glfwTerminate();
}

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <main_prog>" << std::endl;
    return 1;
  }
  const char* main_dylib = argv[1];
  void* handle = dlopen(main_dylib, RTLD_LAZY | RTLD_GLOBAL);
  void (*entry)(pixel_t*,uint,uint) = (void (*)(pixel_t*,uint,uint)) dlsym(handle, "main");

  GLFWwindow* window;
  int width, height;
  int ret = init_glfw(window, width, height);
  if (ret) return ret;

  {
    Graphics graphics(width, height);
    InputEvents input_events;

    // start up user-mode entry
    std::thread user_main(entry, graphics.buffer, width, height);

    auto last_frame = hr_clock::now();
    while (!glfwWindowShouldClose(window)) {
      auto now = hr_clock::now();
      if ((now - last_frame) >= 16ms) {
        std::cout << "frame! ("
                  << std::chrono::duration_cast<ms>(now-last_frame).count()
                  << "ms)" << std::endl;
        last_frame = now;
        graphics.draw();
        glfwSwapBuffers(window);
      }
      glfwPollEvents();
    }

    user_main.join();
  }

  term_glfw();
  return 0;
}
