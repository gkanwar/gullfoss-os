#include "assert.h"
#include "debug_serial.h"
#include "ipc.h"
#include "task_manager.h"

static InterProcessComm* inst;
InterProcessComm::InterProcessComm() { assert_make_inst(inst, this); }
InterProcessComm& InterProcessComm::get() { return assert_get_inst(inst); }

void* InterProcessComm::poll(u16 port) {
  for (auto&& msg : messages) {
    if (msg.port == port) return msg.data;
  }
  return nullptr;
}

void* InterProcessComm::accept(u16 port) {
  // TODO: sleep/wake on message
  while (true) {
    void* data = poll(port);
    if (data) {
      debug::serial_printf("accept got data on port %u = %p\n", (uint)port, data);
      return data;
    }
    TaskManager::get().yield();
  }
}

void InterProcessComm::send(u16 port, void* data) {
  debug::serial_printf("send data %p on port %u\n", data, (uint)port);
  messages.push_front({.port = port, .data = data});
}
