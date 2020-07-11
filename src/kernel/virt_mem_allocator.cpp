#include "assert.h"
#include "kernel.h"
#include "virt_mem_allocator.h"
#include "debug_serial.h"

#define PD_PRESENT 0x1
#define PT_PRESENT 0x1

// TODO: when exactly do we need to flush?
// static void flush_tlb() {
//   asm volatile ("movl %%cr3,%%eax; movl %%eax,%%cr3"::);
// }

static VirtMemAllocator* inst;
VirtMemAllocator::VirtMemAllocator() { inst = this; }
VirtMemAllocator& VirtMemAllocator::get() { return *inst; }

void VirtMemAllocator::init_page_dir(uint32_t page_dir[], uint32_t page_tables[]) {
  this->page_dir = page_dir;
  this->page_tables = page_tables;
}

void VirtMemAllocator::clear_ident_map() {
  unsigned ident_pd_index = VIRT_TO_PD_INDEX(KERNEL_VIRT_TO_PHYS(&_kernel_start));
  page_dir[ident_pd_index] = 0;
}

uint32_t* VirtMemAllocator::get_page_table(unsigned pd_index) {
  return (uint32_t*)((uint32_t)page_dir[pd_index] & PAGE_MASK);
}
void VirtMemAllocator::load_page_table(unsigned pd_index) {
  uint32_t pd_entry = KERNEL_VIRT_TO_PHYS(&page_tables[1024*pd_index]);
  pd_entry |= PD_FLAGS; // TODO: more detailed flags
  page_dir[pd_index] = pd_entry;
}

void* VirtMemAllocator::find_free_virt_page() {
  // FORNOW first pass linear scan allocator
  uint32_t page = ((uint32_t)&_kernel_start) & PAGE_MASK;
  // scan until ptr wrap at 4GiB boundary
  while (page != (uint32_t)nullptr) {
    unsigned pd_index = VIRT_TO_PD_INDEX(page);
    // next 4MiB block boundary
    uint32_t next_block = page + PAGE_BLOCK_SIZE;
    next_block -= page % PAGE_BLOCK_SIZE;
    // totally free block
    if (!(page_dir[pd_index] & PD_PRESENT)) {
      load_page_table(pd_index);
    }

    uint32_t* page_table = get_page_table(pd_index);
    for (; page != next_block; page += PAGE_SIZE) {
      unsigned pt_index = VIRT_TO_PT_INDEX(page);
      if (!(page_table[pt_index] & PT_PRESENT)) {
        return (void*)page;
      }
    }
  }
  return nullptr;
}

void* VirtMemAllocator::map_page(void* virt_page, void* phys_page) {
  if (!virt_page) {
    virt_page = find_free_virt_page();
  }
  assert((uint32_t)virt_page % PAGE_SIZE == 0, "virt page must be aligned");
  assert((uint32_t)phys_page % PAGE_SIZE == 0, "phys page must be aligned");
  unsigned pd_index = VIRT_TO_PD_INDEX(virt_page);
  unsigned pt_index = VIRT_TO_PT_INDEX(virt_page);
  uint32_t* page_table = get_page_table(pd_index);
  uint32_t pt_entry = ((uint32_t)phys_page) & PAGE_MASK;
  pt_entry |= PT_FLAGS;
  page_table[pt_index] = pt_entry;
  return virt_page;
}

void* VirtMemAllocator::reserve_block() {
  uint32_t page = ((uint32_t)&_kernel_start) & PAGE_MASK;
  // round up to nearest page block
  if (page % PAGE_BLOCK_SIZE != 0) {
    page += PAGE_BLOCK_SIZE - page % PAGE_BLOCK_SIZE;
  }
  // scan until ptr wrap at 4GiB boundary
  for (; page != (uint32_t)nullptr; page += PAGE_BLOCK_SIZE) {
    unsigned pd_index = VIRT_TO_PD_INDEX(page);
    if (!(page_dir[pd_index] & PD_PRESENT)) {
      load_page_table(pd_index);
      return (void*)page;
    }
  }
  return nullptr;
}
