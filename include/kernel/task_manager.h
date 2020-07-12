#ifndef TASK_MANAGER_H
#define TASK_MANAGER_H

struct TaskNode {
  TaskNode* next;
  TaskNode* prev;
  struct State {
    void* stack_ptr;
  } state;
};

class TaskManager {
 public:
  TaskManager();
  static TaskManager& get();
  void yield();
  void exit();
  // TODO: fork()-like primitive
 private:
  TaskNode* cur_task{nullptr};
};

#endif
