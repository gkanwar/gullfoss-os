#ifndef IPC_H
#define IPC_H

#include <forward_list>
#include "types.h"

using namespace std;

class InterProcessComm {
 public:
  InterProcessComm();
  static InterProcessComm& get();
  void* accept(u16 port); // blocks until data is available
  void* poll(u16 port); // non-blocking, returns nullptr if no msg
  void send(u16 port, void* data);
 private:
  struct Message {
    u16 port;
    void* data;
  };
  forward_list<Message> messages;
};

#endif
