#include <stdint.h>
#include "assert.h"
#include "debug_serial.h"
#include "kernel.h"
#include "linked_block_allocator.h"

#define LINKED_BLOCK_ALIGN 64
union alignas(LINKED_BLOCK_ALIGN) LinkedBlockAllocator::Block {
  struct {
    Block* next;
    lsize_t size;
    bool allocated;
  };
  // header align 16
  uint8_t align[16];
  // data implicitly follows
} __attribute__((packed));

LinkedBlockAllocator::LinkedBlockAllocator() {}
LinkedBlockAllocator::LinkedBlockAllocator(void* mem, lsize_t memsize) {
  initialize(mem, memsize);
}
void LinkedBlockAllocator::initialize(void* mem, lsize_t memsize) {
  // set up heap blocks
  head = (Block*)mem;
  head->next = nullptr;
  head->size = memsize;
  head->allocated = false;
}

void* LinkedBlockAllocator::malloc(lsize_t size) {
  assert(head, "cannot malloc from empty LinkedBlockAllocator");
  for (Block* node = head; node != nullptr; node = node->next) {
    if (!node->allocated && node->size - sizeof(Block) >= size) {
      node->allocated = true;
      lsize_t leftover = node->size - sizeof(Block) - size;
      leftover -= leftover % LINKED_BLOCK_ALIGN;
      if (leftover > 0) {
        node->size -= leftover;
        Block* next = (Block*)((uint8_t*)node + node->size);
        next->next = node->next;
        next->size = leftover;
        next->allocated = false;
        node->next = next;
      }
      return (void*)(node+1); // data is just after header
    }
  }
  return nullptr;
}

struct LinkedPageAllocator::Block {
  std::unique_ptr<Block> next;
  void* start;
  lsize_t size;
  bool allocated;
  Block(Block* next, void* start, lsize_t size, bool allocated) :
      next(next), start(start), size(size), allocated(allocated) {} 
};

LinkedPageAllocator::LinkedPageAllocator(void* mem, lsize_t mem_size) {
  assert(mem_size % PAGE_SIZE == 0, "LinkedPageAllocator needs whole multiple of pages");
  head = std::make_unique<Block>(nullptr, mem, mem_size, false);
}

void* LinkedPageAllocator::reserve(lsize_t size) {
  assert(size % PAGE_SIZE == 0, "LinkedPageAllocator can only give out whole pages");
  for (Block* node = head.get(); node != nullptr; node = node->next.get()) {
    if (!node->allocated && node->size >= size) {
      node->allocated = true;
      lsize_t leftover = node->size - size;
      if (leftover > 0) {
        node->size -= leftover;
        void* next_start = node->start + node->size;
        Block* next = new Block(node->next.release(), next_start, leftover, false);
        node->next = std::unique_ptr<Block>(next);
      }
      return node->start; // data is just after header
    }
  }
  return nullptr;
}
