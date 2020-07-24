#include <memory>
#include "assert.h"
#include "debug_serial.h"
#include "proc_allocator.h"

static ProcAllocator* inst;

ProcAllocator::ProcAllocator(
    PhysMemAllocator& physMemAlloc, VirtMemAllocator& virtMemAlloc)
    : physMemAlloc(physMemAlloc), virtMemAlloc(virtMemAlloc) {
  assert_make_inst(inst, this);
  void* mem = virtMemAlloc.alloc_free_l2_block();
  debug::serial_printf("ProcAllocator initialized with mem block at %p\n", mem);
  assert(mem, "not enough mem for ProcAllocator");
  segment_alloc = std::make_unique<LinkedPageAllocator>(mem, LEVEL2_BLOCK_SIZE);
}

ProcAllocator& ProcAllocator::get() { return assert_get_inst(inst); }

void* ProcAllocator::reserve_proc_segments(lsize_t size) {
  void* mem = segment_alloc->reserve(size);
  debug::serial_printf("Reserved proc mem in [%p, %p]\n", mem, mem+size);
  return mem;
}

void ProcAllocator::map_segment(void* base, lsize_t size, uint8_t flags) {
  return map_block(physMemAlloc, virtMemAlloc, base, size, flags);
}
