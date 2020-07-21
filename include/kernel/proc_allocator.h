#ifndef PROC_ALLOCATOR_H
#define PROC_ALLOCATOR_H

#include <stddef.h>
#include "phys_mem_allocator.h"
#include "virt_mem_allocator.h"

// TODO: need to determine the right interface for this guy
#define CHUNK_PAGES 32
#define PROC_PAGES NUM_PT_ENTRIES // 2MiB proc

/**
 * Page-granularity allocator for user-space process memory. Offers two possible
 * interfaces for memory requests:
 * 
 * 1. Unassigned memory: virtual pages are allocated and registered to the
 *    process, but are not pinned to physical memory. If reads are performed it
 *    is simply an error (reading from uninitialized memory), but if a write is
 *    performed, the page fault is handled by allocated physical memory to back
 *    the virtual page.
 * 2. Assigned memory: virtual pages are allocated and mapped to physical pages.
 *    This supports the use case of needing to immediately load data.
 */
class ProcAllocator {
 public:
  ProcAllocator(PhysMemAllocator&, VirtMemAllocator&);
  static ProcAllocator& get();
  void* alloc_proc_segments(size_t image_size);
 private:
  PhysMemAllocator& physMemAlloc;
  VirtMemAllocator& virtMemAlloc;
};

#endif
