#include <memory>
#include "assert.h"
#include "proc_allocator.h"

static ProcAllocator* inst;

ProcAllocator::ProcAllocator(
    PhysMemAllocator& physMemAlloc, VirtMemAllocator& virtMemAlloc)
    : physMemAlloc(physMemAlloc), virtMemAlloc(virtMemAlloc) {
  assert_make_inst(inst, this);
  void* mem = virtMemAlloc.alloc_free_l2_block();
  assert(mem, "not enough mem for ProcAllocator");
  segment_alloc = std::make_unique<LinkedBlockAllocator>(mem, LEVEL2_BLOCK_SIZE);
}

ProcAllocator& ProcAllocator::get() { return assert_get_inst(inst); }

void* ProcAllocator::alloc_proc_segments(lsize_t size) {
  return segment_alloc->malloc(size);
}
