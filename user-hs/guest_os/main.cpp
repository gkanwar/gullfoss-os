#define GL_SILENCE_DEPRECATION
#define GLFW_INCLUDE_GLCOREARB
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <chrono>
#include <iostream>
#include <thread>
#include <stdint.h>

#include "graphics.h"
#include "input_events.h"
// TODO: dynamically load instead of static include/linking
#include "Graphics/Wayland_stub.h"

#define WIDTH 800
#define HEIGHT 600

using namespace std::chrono_literals;
typedef std::chrono::high_resolution_clock hr_clock;
typedef std::chrono::milliseconds ms;

int main(int argc, char** argv) {
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
  int width, height;
  glfwGetWindowSize(window, &width, &height);

  if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {
    std::cout << "Failed to load GL using GLAD" << std::endl;
    return 3;
  }

  {
    Graphics graphics(width, height);
    InputEvents input_events;

    // start up user-mode Haskell runtime
    hs_init(&argc, &argv);
    std::cout << "waylandStart with buf " << graphics.buffer << std::endl;
    waylandStart(width, height, graphics.buffer);
    std::cout << "waylandStart complete" << std::endl;
  
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
    
    hs_exit();
  }

  glfwTerminate();
  return 0;
}
