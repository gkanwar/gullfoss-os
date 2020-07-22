#ifndef LINKED_BLOCK_ALLOCATOR_H
#define LINKED_BLOCK_ALLOCATOR_H

#include <stddef.h>
#include <types.h>

class LinkedBlockAllocator {
 public:
  LinkedBlockAllocator();
  LinkedBlockAllocator(void* mem, lsize_t mem_size);
  // Initializer for pre-heap instance
  void initialize(void* mem, lsize_t mem_size);
  void* malloc(lsize_t);
  // TODO: free
 private:
  union Block;
  Block* head;
};

#endif