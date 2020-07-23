#ifndef HEAP_ALLOCATOR_H
#define HEAP_ALLOCATOR_H

/**
 * Kernel heap allocator. NOT for use in spooling up user-space programs.
 * FIXME: HeapAllocator is not thread-safe.
 */

#include <memory>
#include <stddef.h>
#include "linked_block_allocator.h"
#include "phys_mem_allocator.h"
#include "virt_mem_allocator.h"

#define HEAP_PAGES NUM_PT_ENTRIES // 2MiB heap

class HeapAllocator {
  static constexpr auto CHUNK_PAGES = PhysMemAllocator::NUM_PAGES_BIG;
 public:
  HeapAllocator();
  static HeapAllocator& get();
  // NOTE: We need initialize code for kernel_early_main (prior to ctors)
  void initialize(PhysMemAllocator&, VirtMemAllocator&);
  void* malloc(lsize_t);
 private:
  void* slab8_malloc();
  void* slab16_malloc();
  void* slab32_malloc();
  void* block_malloc(lsize_t);
  LinkedBlockAllocator linked_block_alloc;
  // Avoid really tiny mallocs hitting the main heap, using "slab"
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
