#include <cstring>
#include "assert.h"
#include "debug_serial.h"
#include "interrupt_impl.h"
#include "interrupt_manager.h"
#include "phys_mem_allocator.h"
#include "task_manager.h"
#include "util.h"
#include "virt_mem_allocator.h"

static TaskManager* inst;
TaskManager::TaskManager() {
  assert_make_inst(inst, this);
  // initializing thread is the only task
  cur_task = new TaskNode;
  cur_task->next = cur_task;
  cur_task->prev = cur_task;
}
TaskManager& TaskManager::get() { return assert_get_inst(inst); }

#ifdef __LP64__
#define NUM_GP_REGS 12
#else
#error "32 bit not supported"
#endif

void TaskManager::yield() {
  asm volatile(
      "push %%rbx; push %%rbp; push %%r12; "
      "push %%r13; push %%r14; push %%r15" :::); // 6 callee-saved registers
  TaskNode::State& cur_state = cur_task->state;
  const TaskNode::State& next_state = cur_task->next->state;
  cur_task = cur_task->next;
  asm volatile("mov %%rsp,%0":"=m"(cur_state.stack_ptr):);
  asm volatile("mov %0,%%rsp"::"m"(next_state.stack_ptr):);
  asm volatile(
      "pop %%r15; pop %%r14; pop %%r13; "
      "pop %%r12; pop %%rbp; pop %%rbx" :::); // 6 callee-saved registers
}

extern "C" void task_entry();

struct reg_state_t {
  u64 callee_saved_regs[6];
} __attribute__((packed));

void TaskManager::spawn(void entry(void*), void* arg) {
  // Allocate new kernel stack = 1 L1 block
  // TODO: Don't map full stack to physical memory
  void* new_stack = VirtMemAllocator::get().alloc_free_l1_block();
  uint8_t stack_map_flags = 0;
  util::set_bit(stack_map_flags, MapFlag::Writeable);
  map_block(
      PhysMemAllocator::get(), VirtMemAllocator::get(),
      new_stack, LEVEL1_BLOCK_SIZE, stack_map_flags);
  // init stack ptr
  uint8_t* top_of_stack = (uint8_t*)new_stack + LEVEL1_BLOCK_SIZE;
  // push task_entry address, real entry arg, real entry address, so yield
  // returns to generic task entry setup, which in turn calls into real entry
  // after unlocking scheduling and setting up args in regs.
  top_of_stack -= sizeof(void*);
  *(void**)top_of_stack = arg;
  top_of_stack -= sizeof(void*);
  *(void**)top_of_stack = (void*)entry;
  top_of_stack -= sizeof(void*);
  *(void**)top_of_stack = (void*)task_entry;
  // push initial callee-saved register state, for yield to pop
  top_of_stack -= sizeof(reg_state_t);
  *(reg_state_t*)top_of_stack = {0};

  {
    ScopedInterruptGuard guard;
    TaskNode* new_task = new TaskNode {
      .next = cur_task->next,
      .prev = cur_task,
      .state = { .stack_ptr = top_of_stack }};
    cur_task->next->prev = new_task;
    cur_task->next = new_task;
  }
}
