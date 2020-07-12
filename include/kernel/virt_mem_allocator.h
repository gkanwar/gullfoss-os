#ifndef VIRT_MEM_ALLOCATOR_H
#define VIRT_MEM_ALLOCATOR_H

/**
 * The virtual memory allocator for the kernel assigns pages of virtual memory
 * to pages of physical memory. It owns and maintains the full paging structure.
 */

#define PAGING_32 1
#define PAGING_PAE 2
#define PAGING_PML4 3
#define PAGING_PML5 4
#define PAGING PAGING_PML4

#define NUM_PT_ENTRIES 512

#include <stdint.h>
#include "kernel.h"
#include "phys_mem_allocator.h"

struct PageTable;
// NOTE: (bool)PageVirtualStatus = whether page is free
enum class PageVirtualStatus {
  Present = 0,
  PageMissing = 1,
  PageTableMissing = 2,
  PageDirMissing = 3,
  PageDirPTMissing = 4,
  PML4Missing = 5,
};

class VirtMemAllocator {
 public:
  // Takes ownership of the paging structures handed off from bootloader.
  VirtMemAllocator();
  static VirtMemAllocator& get();
  // NOTE: We need initialize code for kernel_early_main (prior to ctors)
  void initialize(PhysMemAllocator*);
  // Removes identity map leftover from early boot
  void clear_ident_map();
  // Map `virt_page` to point to `phys_page`, returning the mapped virt page.
  // If `virt_page` is unavailable, returns `nullptr`. If `virt_page` is
  // `nullptr`, finds an arbitrary free page above `_kernel_start` and maps
  // there.
  void* map_page(void* virt_page, void* phys_page);
  // Find unmapped page before `_kernel_start`, returning the virt start
  // page. If unavailable, returns `nullptr`.
  void* find_free_page();
  // Find unmapped page dir entry (4MiB block) before `_kernel_start`, returning
  // the virt start page. If unavailable, returns `nullptr`.
  void* find_free_block();
 private:
  void do_map_page(void*, void*);
  PageVirtualStatus page_map_status(void*);
  PageTable* pml4_table;
  PhysMemAllocator* physMemAlloc;
};

#endif
