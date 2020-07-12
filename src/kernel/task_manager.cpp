#include "assert.h"
#include "debug_serial.h"
#include "task_manager.h"

static TaskManager* inst;
TaskManager::TaskManager() {
  inst = this;
  // initializing thread is the only task
  cur_task = new TaskNode;
  cur_task->next = cur_task;
  cur_task->prev = cur_task;
}
TaskManager& TaskManager::get() { return *inst; }

void TaskManager::yield() {
  debug::serial_printf("yield()\n");
  TaskNode::State& cur_state = cur_task->state;
  const TaskNode::State& next_state = cur_task->next->state;
  cur_task = cur_task->next;
  debug::serial_printf("advanced cur_task, trying to switch stacks\n");
  asm volatile("mov %%rsp,%0" : "=m"(cur_state.stack_ptr) :);
  debug::serial_printf("saved current rsp = %p\n", cur_state.stack_ptr);
  debug::serial_printf("loading next next rsp = %p\n", next_state.stack_ptr);
  asm volatile("mov %0,%%rsp" :: "m"(next_state.stack_ptr));
  debug::serial_printf("switched!\n");
}

void TaskManager::exit() {
  assert(cur_task->next != cur_task &&
         cur_task->prev != cur_task, "last kernel task exited!");
  TaskNode* prev = cur_task->prev;
  TaskNode* next = cur_task->next;
  prev->next = next;
  next->prev = prev;
  delete cur_task;
  cur_task = next;
  asm volatile("mov %0,%%rsp" :: "m"(next->state.stack_ptr));
}
