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
  void* malloc(size_t);
  union ProcBlock;
  ProcBlock* proc;
 private:
  void* slab8_malloc();
  void* slab16_malloc();
  void* slab32_malloc();
  void* block_malloc(size_t);
  void* phys_proc_chunks[PROC_PAGES / 32];
  // Avoid really tiny mallocs hitting the main proc, using "slab"
  // suballocators for primitive small types. Each suballoc is given one chunk.
  struct Slab8 { uint8_t data[8]; };
  struct Slab16 { uint8_t data[16]; };
  struct Slab32 { uint8_t data[32]; };
  Slab8* slab8;
  Slab16* slab16;
  Slab32* slab32;
  uint8_t slab8_bitmap[PAGE_SIZE * CHUNK_PAGES / (sizeof(Slab8)*8)];
  uint8_t slab16_bitmap[PAGE_SIZE * CHUNK_PAGES / (sizeof(Slab16)*8)];
  uint8_t slab32_bitmap[PAGE_SIZE * CHUNK_PAGES / (sizeof(Slab32)*8)];
};

#endif
