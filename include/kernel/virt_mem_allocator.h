#ifndef VIRT_MEM_ALLOCATOR_H
#define VIRT_MEM_ALLOCATOR_H

#include <stdint.h>
#include <types.h>
#include "kernel.h"
#include "phys_mem_allocator.h"

/**
 * The virtual memory allocator for the kernel assigns pages of virtual memory
 * to pages of physical memory. It owns and maintains the full paging structure.
 */

#define PAGING_32 1
#define PAGING_PAE 2
#define PAGING_PML4 3
#define PAGING_PML5 4
#define PAGING PAGING_PML4

constexpr lsize_t NUM_PT_ENTRIES = 512;
// level1: page table = 2MiB
// level2: page dir = 1GiB
// level3: pdpt = 512GiB
constexpr lsize_t LEVEL1_PAGES = NUM_PT_ENTRIES;
constexpr lsize_t LEVEL2_PAGES = NUM_PT_ENTRIES*NUM_PT_ENTRIES;
constexpr lsize_t LEVEL3_PAGES = NUM_PT_ENTRIES*NUM_PT_ENTRIES*NUM_PT_ENTRIES;
constexpr lsize_t LEVEL1_BLOCK_SIZE = LEVEL1_PAGES*PAGE_SIZE;
constexpr lsize_t LEVEL2_BLOCK_SIZE = LEVEL2_PAGES*PAGE_SIZE;
constexpr lsize_t LEVEL3_BLOCK_SIZE = LEVEL3_PAGES*PAGE_SIZE;

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
enum class PagingLevel {
  Page = 1,
  PageTable = 2,
  PageDir = 3,
  PageDirPT = 4,
  PML4 = 5,
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
  // Find and reserve unmapped entry at a given level of the paging structure.
  // NOTE: reservation does not MAP anything, but uses higher page table bits
  // to indicate that the consumer plans to map things there.
  void* alloc_free_page();
  void* alloc_free_l1_block();
  void* alloc_free_l2_block();
  void* alloc_free_l3_block();
 private:
  void* find_free_block(const lsize_t, PageVirtualStatus);
  void do_map_page(void*, void*);
  void reserve_entry(void*, PagingLevel);
  PageVirtualStatus page_map_status(void*);
  PageTable* pml4_table;
  PhysMemAllocator* physMemAlloc;
};

#endif
