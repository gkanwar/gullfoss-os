#include <stddef.h>
#include "assert.h"
#include "debug_serial.h"
#include "kernel.h"
#include "virt_mem_allocator.h"
#include "util.h"

#if (PAGING == PAGING_PML4)

// Generalized entry structure for use in all paging structures
union PageTableEntry {
  struct {
    uint8_t flags;
    uint8_t _pad[6];
    uint8_t prot_key;
  };
  uint64_t addr;
} __attribute__((packed));
#  define MAXPHYADDR 52 // Intel notation
#  define PHYS_ADDR_MASK 0xffffffffff000
#  define PT_ENTRY_MASK 0x1ff
#  define PTE_ADDR(pte) (pte.addr & PHYS_ADDR_MASK)
#  define PT1_INDEX(addr) ((addr >> 12) & PT_ENTRY_MASK)
#  define PT2_INDEX(addr) ((addr >> 21) & PT_ENTRY_MASK)
#  define PT3_INDEX(addr) ((addr >> 30) & PT_ENTRY_MASK)
#  define PT4_INDEX(addr) ((addr >> 39) & PT_ENTRY_MASK)

#else
#  error "only PML4 supported"
#endif

enum class PageTableFlags {
  Present = 0,
  ReadWrite = 1,
  UserSuper = 2,
  WriteThrough = 3,
  CacheDisable = 4,
  Accessed = 5,
  Dirty = 6,
  PageSize = 7,
};

const unsigned num_pt_entries = 512;
struct PageTable {
  PageTableEntry entries[num_pt_entries];
} __attribute__((packed));

// Resolve an entry to a physical address (if the entry is present) or nullptr
static void* resolve_entry(const PageTable& table, unsigned i) {
  assert(i < num_pt_entries, "entry index too large");
  const PageTableEntry& entry = table.entries[i];
  // debug::serial_printf("resolve_entry (i=%d) entry %016llx\n", i, entry.addr);
  if (util::get_bit(entry.flags, PageTableFlags::Present)) {
    return (void*)PTE_ADDR(entry);
  }
  return nullptr;
}
// Add an entry to a physical address, with given extra flags (Present is automatic)
static void add_entry(PageTable& table, unsigned i, const void* addr, uint8_t flags) {
  assert(i < num_pt_entries, "entry index too large");
  PageTableEntry& entry = table.entries[i];
  entry.addr = (uint64_t)addr;
  entry.flags = flags;
  util::set_bit(entry.flags, PageTableFlags::Present);
}

// TODO: when exactly do we need to flush?
// static void flush_tlb() {
//   asm volatile ("movl %%cr3,%%eax; movl %%eax,%%cr3"::);
// }

static VirtMemAllocator* inst;
VirtMemAllocator::VirtMemAllocator() { inst = this; }
VirtMemAllocator& VirtMemAllocator::get() { return *inst; }

void VirtMemAllocator::initialize(PhysMemAllocator* physMemAlloc) {
  this->physMemAlloc = physMemAlloc;
  PageTable* pml4_ptr;
  asm ("movq %%cr3,%0" : "=g"(pml4_ptr) : );
  pml4_table = pml4_ptr;
}

void VirtMemAllocator::clear_ident_map() {
  PANIC_NOT_IMPLEMENTED("clear_ident_map");
}

void* VirtMemAllocator::find_free_page() {
  // FORNOW first pass linear scan allocator
  // scan from kernel start downwards
  size_t page = ((size_t)&_kernel_start) & PAGE_MASK;
  for (; page != (size_t)nullptr; page -= PAGE_SIZE) {
    if ((bool)page_map_status((void*)page)) {
      return (void*)page;
    }
  }
  return nullptr;
}

void* VirtMemAllocator::find_free_block() {
  // FORNOW first pass linear scan allocator
  // scan from kernel start downwards
  size_t page = ((size_t)&_kernel_start) & PAGE_MASK;
  debug::serial_printf("find_free_block starting from %p\n",
                       &_kernel_start);
  // round down to nearest page block
  page -= page % PAGE_BLOCK_SIZE;
  for (; page != (size_t)nullptr; page -= PAGE_BLOCK_SIZE) {
    auto status = page_map_status((void*)page);
    debug::serial_printf("page %p status %d\n", (void*)page, status);
    if (status >= PageVirtualStatus::PageTableMissing) {
      debug::serial_printf("... found %p\n", page);
      return (void*)page;
    }
  }
  return nullptr;
}


#if (PAGING == PAGING_PML4)
PageVirtualStatus VirtMemAllocator::page_map_status(void* page) {
  // WARNING: Assumes identity mapping, probably should remap a page table
  // section somewhere into the kernel and write translation code.
  uint64_t addr = (uint64_t)page;
  const PageTable* pml3_table = (const PageTable*)resolve_entry(*pml4_table, PT4_INDEX(addr));
  debug::serial_printf("Page map status of %p (PT4 index %d) -> pml3_table = %p\n",
                       page, PT4_INDEX(addr), pml3_table);
  if (!pml3_table) {
    return PageVirtualStatus::PageDirPTMissing;
  }
  const PageTable* pml2_table = (const PageTable*)resolve_entry(*pml3_table, PT3_INDEX(addr));
  if (!pml2_table) {
    return PageVirtualStatus::PageDirMissing;
  }
  const PageTable* pml1_table = (const PageTable*)resolve_entry(*pml2_table, PT2_INDEX(addr));
  if (!pml1_table) {
    return PageVirtualStatus::PageTableMissing;
  }
  if (!resolve_entry(*pml1_table, PT1_INDEX(addr))) {
    return PageVirtualStatus::PageMissing;
  }
  return PageVirtualStatus::Present;
}

void VirtMemAllocator::do_map_page(void* virt_page, void* phys_page) {
  assert(physMemAlloc, "VirtMemAllocator needs to bind a PhysMemAllocator "
         "before mapping pages");
  uint8_t extra_flags = 0;
  uint64_t addr = (uint64_t)virt_page;
  PageTable* pml3_table = (PageTable*)resolve_entry(*pml4_table, PT4_INDEX(addr));
  if (!pml3_table) {
    pml3_table = (PageTable*)physMemAlloc->alloc1();
    add_entry(*pml4_table, PT4_INDEX(addr), (void*)pml3_table, extra_flags);
  }
  PageTable* pml2_table = (PageTable*)resolve_entry(*pml3_table, PT3_INDEX(addr));
  if (!pml2_table) {
    pml2_table = (PageTable*)physMemAlloc->alloc1();
    add_entry(*pml3_table, PT3_INDEX(addr), (void*)pml2_table, extra_flags);
  }
  PageTable* pml1_table = (PageTable*)resolve_entry(*pml2_table, PT2_INDEX(addr));
  if (!pml1_table) {
    pml1_table = (PageTable*)physMemAlloc->alloc1();
    add_entry(*pml2_table, PT2_INDEX(addr), (void*)pml1_table, extra_flags);
  }
  PageTableEntry& pml1_entry = pml1_table->entries[PT1_INDEX(addr)];
  assert(!util::get_bit(pml1_entry.flags, PageTableFlags::Present),
         "attempting to map an already-mapped page");
  add_entry(*pml1_table, PT1_INDEX(addr), phys_page, extra_flags);
}

#else
#error "only PML4 supported"
#endif

void* VirtMemAllocator::map_page(void* virt_page, void* phys_page) {
  if (!virt_page) {
    virt_page = find_free_page();
  }
  assert((size_t)virt_page % PAGE_SIZE == 0, "virt page must be aligned");
  assert((size_t)phys_page % PAGE_SIZE == 0, "phys page must be aligned");
  do_map_page(virt_page, phys_page);
  return virt_page;
}
