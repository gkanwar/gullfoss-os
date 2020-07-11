#ifndef VIRT_MEM_ALLOCATOR_H
#define VIRT_MEM_ALLOCATOR_H

/**
 * The virtual memory allocator for the kernel assigns pages of virtual memory
 * to pages of physical memory. It maintains the page directory and page tables
 * for the kernel.
 */

#define PD_FLAGS 0x1
#define PT_FLAGS 0x1

#include <stdint.h>
#include "kernel.h"

class VirtMemAllocator {
 public:
  VirtMemAllocator();
  static VirtMemAllocator& get();
  // Takes ownership of the page directory, page tables, clears identity
  // mapping and prepares to map more pages on demand.
  void init_page_dir(uint32_t page_dir[], uint32_t page_tables[]);
  // Removes identity map leftover from early boot
  void clear_ident_map();
  // Loads pre-alloc'd page table into page dir (lazy mapping)
  void load_page_table(unsigned pd_index);
  // Map `virt_page` to point to `phys_page`, returning the mapped virt page.
  // If `virt_page` is unavailable, returns `nullptr`. If `virt_page` is
  // `nullptr`, finds an arbitrary free page above `_kernel_start` and maps
  // there.
  void* map_page(void* virt_page, void* phys_page);
  // Find unmapped page dir entry after `_kernel_start` and reserve the entire
  // 4MiB block, returning the virt start page. If unavailable, returns
  // `nullptr`.
  void* reserve_block();
  uint32_t *page_dir;
  uint32_t *page_tables;
 private:
  uint32_t* get_page_table(unsigned pd_index);
  void* find_free_virt_page();
};

#endif
