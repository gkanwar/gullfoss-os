#include <dlfcn.h>
#include <iostream>

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <main_prog>" << std::endl;
    return 1;
  }
  const char* main_dylib = argv[1];
  void* handle = dlopen(main_dylib, RTLD_LAZY | RTLD_GLOBAL);
  void (*entry)(void) = (void (*)(void)) dlsym(handle, "main");
  entry();
  return 0;
}
