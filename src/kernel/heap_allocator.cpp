#include <memory>
#include "assert.h"
#include "debug_serial.h"
#include "heap_allocator.h"
#include "virt_mem_allocator.h"

static HeapAllocator* inst;

HeapAllocator::HeapAllocator() { assert_make_inst(inst, this); }
HeapAllocator& HeapAllocator::get() { return assert_get_inst(inst); }

void HeapAllocator::initialize(
    PhysMemAllocator& physMemAlloc, VirtMemAllocator& virtMemAlloc) {
  // grab a contiguous region of kernel virtual memory
  assert(HEAP_PAGES == LEVEL1_PAGES, "heap must be one L1 block");
  void* mem = virtMemAlloc.alloc_free_l1_block();
  assert(mem, "not enough mem for VirtMemAllocator");

  map_block(virtMemAlloc, physMemAlloc, mem, LEVEL1_PAGES*PAGE_SIZE);

  // parcel out suballocators
  assert(HEAP_PAGES % CHUNK_PAGES == 0, "heap must be a multiple of CHUNK_PAGES");
  constexpr auto n_chunks = HEAP_PAGES / CHUNK_PAGES;
  
  linked_block_alloc.initialize(mem, (LEVEL1_PAGES-3) * PAGE_SIZE);
  slab8 = (Slab8*)(mem + (n_chunks-3) * CHUNK_PAGES * PAGE_SIZE);
  slab16 = (Slab16*)(mem + (n_chunks-2) * CHUNK_PAGES * PAGE_SIZE);
  slab32 = (Slab32*)(mem + (n_chunks-1) * CHUNK_PAGES * PAGE_SIZE);

  debug::serial_printf("heap reserved at v %p\n", mem);
  debug::serial_printf("slab8 reserved at v %p\n", slab8);
  debug::serial_printf("slab16 reserved at v %p\n", slab16);
  debug::serial_printf("slab32 reserved at v %p\n", slab32);
}

void* HeapAllocator::slab8_malloc() {
  // TODO: abstract this code somehow
  for (unsigned i = 0; i < sizeof(slab8_bitmap); ++i) {
    uint8_t bitmap = slab8_bitmap[i];
    if (bitmap != 0xff) {
      unsigned j = 0;
      while (bitmap & 0x1) {
        bitmap >>= 1;
        j++;
      }
      slab8_bitmap[i] |= 1 << j;
      return &slab8[8*i+j];
    }
  }
  return nullptr;
}

void* HeapAllocator::slab16_malloc() {
  for (unsigned i = 0; i < sizeof(slab16_bitmap); ++i) {
    uint8_t bitmap = slab16_bitmap[i];
    if (bitmap != 0xff) {
      unsigned j = 0;
      while (bitmap & 0x1) {
        bitmap >>= 1;
        j++;
      }
      slab16_bitmap[i] |= 1 << j;
      return &slab16[8*i+j];
    }
  }
  return nullptr;
}

void* HeapAllocator::slab32_malloc() {
  for (unsigned i = 0; i < sizeof(slab32_bitmap); ++i) {
    uint8_t bitmap = slab32_bitmap[i];
    if (bitmap != 0xff) {
      unsigned j = 0;
      while (bitmap & 0x1) {
        bitmap >>= 1;
        j++;
      }
      slab32_bitmap[i] |= 1 << j;
      return &slab32[8*i+j];
    }
  }
  return nullptr;
}

void* HeapAllocator::malloc(lsize_t size) {
  if (size > HEAP_PAGES * PAGE_SIZE) {
    return nullptr;
  }
  // attempt slab allocators first, but fall through if needed
  if (size <= 8) {
    void* out = slab8_malloc();
    if (out) return out;
  }
  if (size <= 16) {
    void* out = slab16_malloc();
    if (out) return out;
  }
  if (size <= 32) {
    void* out = slab32_malloc();
    if (out) return out;
  }
  return linked_block_alloc.malloc(size);
}
