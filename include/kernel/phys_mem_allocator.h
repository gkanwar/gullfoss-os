#ifndef PHYS_MEM_ALLOCATOR_H
#define PHYS_MEM_ALLOCATOR_H

#include <stdint.h>
#include <stddef.h>
#include "bootboot.h"
#include "kernel.h"

class PhysMemAllocator {
 public:
  static constexpr unsigned NUM_PAGES_BIG = 64;
  static constexpr unsigned NUM_PAGES_MED = 8;
  PhysMemAllocator();
  static PhysMemAllocator& get();
  void init_mmap(const MMapEnt* mmap, unsigned count);
  void* alloc1();
  void* allocMed();
  void* allocBig();
  void dealloc1(void*);
  uint8_t mem_bitmap[NUM_PHYS_PAGES / 8];
  unsigned last_alloc;
};

#endif
