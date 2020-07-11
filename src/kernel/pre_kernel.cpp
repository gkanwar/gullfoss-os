/**
 * Prepaging code, linked into .pretext section. Everything here is focused on
 * bringing up paging so we can call into higher-half kernel code.
 */

#include <stddef.h>
#include <stdint.h>
#include "kernel.h"
#include "multiboot.h"
#include "virt_mem_allocator.h"
#include "vga.h"

// Just a raw hang on pre-paging issues
#define assert(x) if (!(x)) { asm volatile ("cli; hlt"::); }

extern uint32_t init_page_dir[1024];
extern uint32_t init_page_tables[1024*1024];

extern "C" {

// pre-paging helper to set up paging
// WARNING: should not call into any .text code, since that is not yet mapped.
// WARNING: .bss, etc memory references must be preprocessed with
//          KERNEL_VIRT_TO_PHYS since they are not yet mapped.
__attribute__((section(".pretext")))
void map_page(uint32_t virt_addr, uint32_t phys_addr, uint32_t *p_page_table) {
  size_t page_pt_index = VIRT_TO_PT_INDEX(virt_addr);
  uint32_t page_pt_entry = phys_addr | PT_FLAGS;
  p_page_table[page_pt_index] = page_pt_entry;
}

__attribute__((fastcall))
__attribute__((section(".pretext")))
void setup_paging(const multiboot_info_t* info) {
  uint32_t* p_init_page_dir = (uint32_t*)KERNEL_VIRT_TO_PHYS((uint32_t)init_page_dir);
  uint32_t* p_init_page_tables = (uint32_t*)KERNEL_VIRT_TO_PHYS((uint32_t)init_page_tables);
  
  // map initial 4MiB page table block with both identity mapping and higher
  // half mapping, so we can keep executing but soon jump into higher half addrs
  uint32_t kernel_pd_entry = (uint32_t)p_init_page_tables | PD_FLAGS;
  size_t ident_pd_index = VIRT_TO_PD_INDEX(KERNEL_VIRT_TO_PHYS(&_kernel_start));
  size_t highhalf_pd_index = VIRT_TO_PD_INDEX(&_kernel_start);
  p_init_page_dir[ident_pd_index] = kernel_pd_entry;
  p_init_page_dir[highhalf_pd_index] = kernel_pd_entry;

  // scan kernel pages and insert into common 4MiB page table blocks
  uint32_t kernel_page = KERNEL_VIRT_TO_PHYS(&_kernel_start) & PAGE_MASK;
  uint32_t* cur_p_page_table = p_init_page_tables;
  size_t cur_ident_pd_index = ident_pd_index;
  size_t cur_highhalf_pd_index = highhalf_pd_index;
  for (; kernel_page < KERNEL_VIRT_TO_PHYS(&_kernel_end); kernel_page += PAGE_SIZE) {
    size_t new_ident_pd_index = VIRT_TO_PD_INDEX(kernel_page);
    if (new_ident_pd_index > cur_ident_pd_index) {
      kernel_pd_entry += PAGE_SIZE;
      cur_p_page_table += PAGE_SIZE / (sizeof(uint32_t)/sizeof(uint8_t));
      cur_ident_pd_index = new_ident_pd_index;
      cur_highhalf_pd_index = highhalf_pd_index + cur_ident_pd_index - ident_pd_index;
      p_init_page_dir[cur_ident_pd_index] = kernel_pd_entry;
      p_init_page_dir[cur_highhalf_pd_index] = kernel_pd_entry;
    }
    map_page(kernel_page, kernel_page, cur_p_page_table);
  }

  // map BIOS buffers into higher half memory
  size_t vga_pd_index = VIRT_TO_PD_INDEX(VIRT_TERM_BUFFER);
  assert(vga_pd_index == highhalf_pd_index); // need to reuse PT
  map_page(VIRT_TERM_BUFFER, TERM_BUFFER, p_init_page_tables);

  // map multiboot info and mmap info into higher half memory
  uint32_t multiboot_addr = (uint32_t)info;
  size_t multiboot_pd_index = VIRT_TO_PD_INDEX(KERNEL_BASE + multiboot_addr);
  assert(multiboot_pd_index == highhalf_pd_index); // need to reuse PT
  uint32_t multiboot_page = multiboot_addr & PAGE_MASK;
  for (; multiboot_page < multiboot_addr + sizeof(multiboot_info_t);
       multiboot_page += PAGE_SIZE) {
    map_page(KERNEL_BASE + multiboot_page, multiboot_page, p_init_page_tables);
  }
  multiboot_pd_index = VIRT_TO_PD_INDEX(KERNEL_BASE + info->mmap_addr);
  assert(multiboot_pd_index == highhalf_pd_index); // need to reuse PT
  uint32_t mmap_page = info->mmap_addr & PAGE_MASK;
  for (; mmap_page < info->mmap_addr + info->mmap_length;
       mmap_page += PAGE_SIZE) {
    map_page(KERNEL_BASE + mmap_page, mmap_page, p_init_page_tables);
  }
}

}
