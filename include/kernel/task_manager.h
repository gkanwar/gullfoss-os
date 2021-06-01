#ifndef TASK_MANAGER_H
#define TASK_MANAGER_H

enum class ThreadState {
  ACTIVE, // actively executing, wants to be scheduled
  ZOMBIE // has exited, needs to be cleaned up
};
struct TaskNode {
  TaskNode* next;
  TaskNode* prev;
  struct State {
    void* stack_ptr;
  } state;
  struct Resources {
    void* kernel_stack;
  } resources;
  ThreadState thread_state;
};

class TaskManager {
 public:
  TaskManager();
  static TaskManager& get();
  void yield();
  void spawn(void entry(void*), void* arg);
  [[noreturn]] void exit();
 private:
  TaskNode* cur_task{nullptr};
  TaskNode* schedule_next_task() const;
  [[noreturn]] friend void reaper_thread(void*);
  void reap_threads();
};

#endif
