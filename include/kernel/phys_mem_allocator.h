#ifndef PHYS_MEM_ALLOCATOR_H
#define PHYS_MEM_ALLOCATOR_H

#include <stdint.h>
#include "bootboot.h"
#include "kernel.h"

class PhysMemAllocator {
 public:
  PhysMemAllocator();
  static PhysMemAllocator& get();
  void init_mmap(const MMapEnt* mmap, unsigned count);
  void* alloc1();
  void* alloc8();
  void* alloc32();
  uint8_t mem_bitmap[NUM_PAGES / 8];
  unsigned last_alloc;
};

#endif
