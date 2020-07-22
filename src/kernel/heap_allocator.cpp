#include <memory>
#include "assert.h"
#include "debug_serial.h"
#include "heap_allocator.h"

static HeapAllocator* inst;

HeapAllocator::HeapAllocator() { assert_make_inst(inst, this); }
HeapAllocator& HeapAllocator::get() { return assert_get_inst(inst); }

void HeapAllocator::initialize(
    PhysMemAllocator& physMemAlloc, VirtMemAllocator& virtMemAlloc) {
  // grab a contiguous region of kernel virtual memory
  assert(HEAP_PAGES == LEVEL1_PAGES, "heap must be one L1 block");
  void* mem = virtMemAlloc.alloc_free_l1_block();
  assert(mem, "not enough mem for VirtMemAllocator");

  assert(HEAP_PAGES % CHUNK_PAGES == 0, "heap must be allocated in 'big' chunks");
  const unsigned n_chunks = HEAP_PAGES / CHUNK_PAGES;
  for (unsigned i = 0; i < n_chunks; ++i) {
    phys_heap_chunks[i] = physMemAlloc.allocBig();
    assert(phys_heap_chunks[i], "heap chunk alloc failed");
    for (unsigned j = 0; j < CHUNK_PAGES; ++j) {
      void* heap_page = (void*)((uint8_t*)phys_heap_chunks[i] + j*PAGE_SIZE);
      void* virt_page = (void*)((uint8_t*)mem + (CHUNK_PAGES*i+j)*PAGE_SIZE);
      [[maybe_unused]] void* res = virtMemAlloc.map_page(virt_page, heap_page);
      assert(res, "mapping heap page failed");
      // save addresses of suballocators
      if (j == 0) {
        if (i == n_chunks-3) {
          slab8 = (Slab8*)virt_page;
        }
        else if (i == n_chunks-2) {
          slab16 = (Slab16*)virt_page;
        }
        else if (i == n_chunks-1) {
          slab32 = (Slab32*)virt_page;
        }
      }
    }
  }

  debug::serial_printf("Making LBA(%p) %p, %llu\n", &linked_block_alloc,
                       mem, (LEVEL1_PAGES-3) * PAGE_SIZE);
  linked_block_alloc.initialize(mem, (LEVEL1_PAGES-3) * PAGE_SIZE);
  debug::serial_printf("LBA created\n");

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
