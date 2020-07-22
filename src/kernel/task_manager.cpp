#include <cstring>
#include "assert.h"
#include "debug_serial.h"
#include "interrupt_impl.h"
#include "interrupt_manager.h"
#include "phys_mem_allocator.h"
#include "task_manager.h"
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
  // WARNING: this assumes the calling code saves all relevant registers on the
  // stack so we can simply swap stacks and return. Interrupts do this for GP
  // registers, but special registers need to be handled.
  TaskNode::State& cur_state = cur_task->state;
  const TaskNode::State& next_state = cur_task->next->state;
  cur_task = cur_task->next;
  asm volatile("mov %%rsp,%0":"=m"(cur_state.stack_ptr):);
  asm volatile("mov %0,%%rsp"::"m"(next_state.stack_ptr));
}

// TODO: Better scheduler locking system
static void task_entry() {
  asm volatile("sti"::);
}

void TaskManager::start(void entry(void)) {
  // Allocate new kernel stack
  // TODO: Don't map full stack to physical memory
  void* new_stack = VirtMemAllocator::get().alloc_free_l1_block();
  constexpr auto chunk_pages = PhysMemAllocator::NUM_PAGES_BIG;
  for (unsigned i = 0; i < NUM_PT_ENTRIES/chunk_pages; ++i) {
    void* phys_chunk = PhysMemAllocator::get().allocBig();
    for (unsigned j = 0; j < chunk_pages; ++j) {
      void* phys_page = (void*)((uint8_t*)phys_chunk + j*PAGE_SIZE);
      void* virt_page = (void*)((uint8_t*)new_stack + (chunk_pages*i+j)*PAGE_SIZE);
      [[maybe_unused]] void* res = VirtMemAllocator::get().map_page(virt_page, phys_page);
      assert(res, "mapping new stack page failed");
    }
  }
  // init stack ptr
  uint8_t* top_of_stack = (uint8_t*)new_stack + NUM_PT_ENTRIES*PAGE_SIZE;
  // push task_entry address and real entry address, so yield returns to generic
  // task entry setup, which in turn returns to the real entry after unlocking
  // scheduling, etc
  top_of_stack -= sizeof(void*);
  *(void**)top_of_stack = (void*)entry;
  top_of_stack -= sizeof(void*);
  *(void**)top_of_stack = (void*)task_entry;

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
