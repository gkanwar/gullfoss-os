#ifndef PROCESS_H
#define PROCESS_H

#include <vector>
#include "virt_mem_allocator.h"

struct Process {
  unsigned pid;
  ProcessPageTables page_tables;
};

class ProcessManager {
 public:
  ProcessManager();
  static ProcessManager& get();
 private:
  std::vector<Process> processes;
};

#endif
