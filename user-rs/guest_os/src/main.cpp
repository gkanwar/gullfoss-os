#include <chrono>
#include <iostream>
#include <vector>

#include "gl_generic.h"
#include "graphics.h"
#include "input_events.h"
#include "process.h"

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

static std::vector<Process*> user_procs;

// Some set of "syscalls" available to user-mode programs
// TODO: move this out into a formal interface
extern "C" void spawn(const char* path, size_t path_len) {
  char* path_cstr = new char[path_len+1];
  memcpy(path_cstr, path, path_len);
  path_cstr[path_len] = '\0';
  Process* p = new Process(path_cstr);
  if (!*p) {
    std::cout << "Failed to start app: " << path << std::endl;
    delete p;
  }
  else {
    user_procs.push_back(p);
    p->start();
  }
  delete[] path_cstr;
}


int main(int argc, char** argv) {
  if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <main_prog>" << std::endl;
    return 1;
  }
  const char* main_path = argv[1];

  GLFWwindow* window;
  int width, height;
  int ret = init_glfw(window, width, height);
  if (ret != 0) return ret;

  do {
    Graphics graphics(width, height);
    InputEvents input_events;

    // start up user-mode entry program
    user_procs.push_back(new Process(main_path));
    if (!*user_procs.back()) {
      std::cout << "Main process failed to load" << std::endl;
      delete user_procs.back();
      break;
    }
    user_procs.back()->start();

    auto last_fps_check = hr_clock::now();
    auto last_frame = hr_clock::now();
    int n_frames = 0;
    while (!glfwWindowShouldClose(window)) {
      auto now = hr_clock::now();
      if ((now - last_frame) >= 16ms) {
        last_frame = now;
        graphics.draw();
        glfwSwapBuffers(window);
        n_frames++;
      }
      if ((now - last_fps_check) >= 1s) {
        double fps = 1000 * n_frames / (double) std::chrono::duration_cast<ms>(now-last_fps_check).count();
        std::cout << "FPS = " << fps << std::endl;
        n_frames = 0;
        last_fps_check = now;
      }
      glfwPollEvents();
    }

    for (const Process* p : user_procs) {
      p->send_signal(Signal::INT);
    }
    for (Process* p : user_procs) {
      p->join();
      delete p;
    }
  } while (false);

  term_glfw();
  return 0;
}
