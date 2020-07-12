#include "phys_mem_allocator.h"

#include <cstring>
#include "assert.h"
#include "bootboot.h"
#include "kernel.h"
#include "vga.h"

static PhysMemAllocator* inst;

PhysMemAllocator::PhysMemAllocator() : last_alloc(0) { inst = this; }
PhysMemAllocator& PhysMemAllocator::get() { return *inst; }

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

void* PhysMemAllocator::alloc8() {
  auto try_alloc8 = [&](unsigned i)->void* {
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
    void* out = try_alloc8(i);
    if (out) return out;
  }
  for (unsigned i = 0; i < last_alloc / 8; ++i) {
    void* out = try_alloc8(i);
    if (out) return out;
  }
  return nullptr;
}

void* PhysMemAllocator::alloc32() {
  const unsigned b32 = sizeof(uint32_t)/sizeof(uint8_t);
  auto try_alloc32 = [&](unsigned i)->void* {
    assert(i % b32 == 0, "i must be 32-page aligned");
    uint32_t bitmap = ((uint32_t*)mem_bitmap)[i/b32];
    if (bitmap == 0) {
      ((uint32_t*)mem_bitmap)[i/b32] = 0xffffffff;
      last_alloc = 8*i;
      return (void*)bit_to_page_addr(8*i);
    }
    else {
      return nullptr;
    }
  };
  unsigned last_alloc_b32 = last_alloc/8 - (last_alloc/8)%b32;
  for (unsigned i = last_alloc_b32; i < sizeof(mem_bitmap); i += b32) {
    void* out = try_alloc32(i);
    if (out) return out;
  }
  for (unsigned i = 0; i < last_alloc_b32; i += b32) {
    void* out = try_alloc32(i);
    if (out) return out;
  }
  return nullptr;
}
