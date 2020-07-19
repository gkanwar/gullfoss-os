/**
 * Utilities to load and hold information about a user process.
 */

#pragma once

#include <thread>

enum class Signal {
  INT = 2
};

class Process {
 public:
  Process(const char* path);
  Process(Process&&) = default;
  ~Process();
  void start();
  void send_signal(Signal) const;
  void join();
  explicit operator bool() const { return handle; }
 private:
  std::thread thread;
  void (*entry)(void);
  void (*signal)(Signal);
  void* handle;
};
