#include <cassert>
#include <dlfcn.h>
#include <iostream>
#include "process.h"

Process::Process(const char* path) {
  handle = dlopen(path, RTLD_LAZY | RTLD_GLOBAL);
  if (!handle) {
    std::cerr << "dlopen failed: " << dlerror() << std::endl;
    return;
  }
  entry = (void (*)(void)) dlsym(handle, "main");
  signal = (void (*)(Signal)) dlsym(handle, "handle_signal");
}

Process::~Process() {
  if (handle) {
    dlclose(handle);
  }
}

void Process::start() {
  assert(entry);
  thread = std::thread(entry);
}

void Process::send_signal(Signal s) const {
  assert(signal);
  signal(s);
}

void Process::join() {
  thread.join();
}
