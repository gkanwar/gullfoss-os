#include "phys_mem_allocator.h"

#include <climits>
#include <cstring>
#include "assert.h"
#include "bootboot.h"
#include "kernel.h"

static PhysMemAllocator* inst;

PhysMemAllocator::PhysMemAllocator() : last_alloc(0) { assert_make_inst(inst, this); }
PhysMemAllocator& PhysMemAllocator::get() { return assert_get_inst(inst); }

static unsigned page_addr_to_bit(uint64_t addr) {
  return (uint32_t) (addr >> PAGE_ADDR_NBITS);
}
static uint64_t bit_to_page_addr(unsigned i) {
  return (uint64_t) (i << PAGE_ADDR_NBITS);
}
#include "debug_serial.h"
static void mark_unavail_pages(const MMapEnt* mmap, uint8_t *mem_bitmap) {
  if (MMapEnt_IsFree(mmap)) return;
  // TODO: could be more efficient by holding byte (or word) in tmp and making
  // all changes before writeback.
  for (uint64_t addr = mmap->ptr & PAGE_MASK;
       addr < mmap->ptr + MMapEnt_Size(mmap);
       addr += PAGE_SIZE) {
    unsigned i = page_addr_to_bit(addr);
    mem_bitmap[i/8] |= 1 << (i%8);
  }
}

void PhysMemAllocator::init_mmap(const MMapEnt* mmap, unsigned count) {
  std::memset(mem_bitmap, 0, sizeof(mem_bitmap));
  for (unsigned i = 0; i < count; ++i) {
    const MMapEnt* mmap_ent = mmap+i;
    debug::serial_printf("mmap %p (len %016x) type = %\n",
                         mmap_ent->ptr, MMapEnt_Size(mmap_ent),
                         MMapEnt_Type(mmap_ent));
    mark_unavail_pages(mmap_ent, mem_bitmap);
  }
  
  // TODO: do we still need this?
  // mark the kernel code allocated, otherwise BAD THINGS can happen
  // uint32_t kernel_start_addr = (uint32_t)&_kernel_start;
  // uint32_t kernel_end_addr = (uint32_t)&_kernel_end;
  // multiboot_memory_map_t kernel_mmap = {
  //   .size = 0,
  //   .addr = KERNEL_VIRT_TO_PHYS(kernel_start_addr),
  //   .len = kernel_end_addr - kernel_start_addr,
  //   .type = MULTIBOOT_MEMORY_RESERVED
  // };
  // mark_unavail_pages(kernel_mmap, mem_bitmap);
}

void* PhysMemAllocator::alloc1() {
  auto try_alloc1 = [&](unsigned i)->void* {
    uint8_t bitmap = mem_bitmap[i];
    if (bitmap < 0xff) {
      unsigned j = 0;
      while ((bitmap & 0x1) > 0) {
        bitmap >>= 1;
        j++;
      }
      mem_bitmap[i] |= 1 << j;
      last_alloc = 8*i + j;
      return (void*)bit_to_page_addr(8*i + j);
    }
    else {
      return nullptr;
    }
  };
  for (unsigned i = last_alloc / 8; i < sizeof(mem_bitmap); ++i) {
    void* out = try_alloc1(i);
    if (out) return out;
  }
  for (unsigned i = 0; i < last_alloc / 8; ++i) {
    void* out = try_alloc1(i);
    if (out) return out;
  }
  return nullptr;
}

void* PhysMemAllocator::allocMed() {
  assert(NUM_PAGES_MED == 8, "allocMed wrong chunk size");
  auto try_med_alloc = [&](unsigned i)->void* {
    uint8_t bitmap = mem_bitmap[i];
    if (bitmap == 0) {
      mem_bitmap[i] = 0xff;
      last_alloc = 8*i;
      return (void*)bit_to_page_addr(8*i);
    }
    else {
      return nullptr;
    }
  };
  for (unsigned i = last_alloc / 8; i < sizeof(mem_bitmap); ++i) {
    void* out = try_med_alloc(i);
    if (out) return out;
  }
  for (unsigned i = 0; i < last_alloc / 8; ++i) {
    void* out = try_med_alloc(i);
    if (out) return out;
  }
  return nullptr;
}

void* PhysMemAllocator::allocBig() {
  assert(NUM_PAGES_BIG == 64, "allocBig wrong chunk size");
  const unsigned b64 = sizeof(uint64_t)/sizeof(uint8_t);
  auto try_big_alloc = [&](unsigned i)->void* {
    assert(i % b64 == 0, "i must be 64-page aligned");
    uint64_t bitmap = ((uint64_t*)mem_bitmap)[i/b64];
    if (bitmap == 0) {
      ((uint64_t*)mem_bitmap)[i/b64] = UINT64_MAX;
      last_alloc = 8*i;
      return (void*)bit_to_page_addr(8*i);
    }
    else {
      return nullptr;
    }
  };
  unsigned last_alloc_chunked = last_alloc/8 - (last_alloc/8)%b64;
  for (unsigned i = last_alloc_chunked; i < sizeof(mem_bitmap); i += b64) {
    void* out = try_big_alloc(i);
    if (out) return out;
  }
  for (unsigned i = 0; i < last_alloc_chunked; i += b64) {
    void* out = try_big_alloc(i);
    if (out) return out;
  }
  return nullptr;
}
