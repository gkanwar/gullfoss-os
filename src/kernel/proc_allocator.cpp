#include "assert.h"
#include "proc_allocator.h"

static ProcAllocator* inst;

ProcAllocator::ProcAllocator(
    PhysMemAllocator& physMemAlloc, VirtMemAllocator& virtMemAlloc)
    : physMemAlloc(physMemAlloc), virtMemAlloc(virtMemAlloc) {
  assert_make_inst(inst, this);
}

ProcAllocator& ProcAllocator::get() { return assert_get_inst(inst); }

void* ProcAllocator::alloc_proc_segments(size_t) {
  PANIC_NOT_IMPLEMENTED("alloc_proc_segments");
}
