#include <cstring>
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
#  define MAXPHYADDR 52
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

constexpr uint64_t RESERVE_MAGIC = 0xcafecafecafec000 & PHYS_ADDR_MASK;
constexpr uint64_t POISON_MAGIC  = 0xabadbadbadbad000 & PHYS_ADDR_MASK;

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

struct PageTable {
  PageTableEntry entries[NUM_PT_ENTRIES];
} __attribute__((packed));


// Resolve an entry to a physical address (if the entry is present) or nullptr
static void* resolve_entry(const PageTable& table, unsigned i) {
  assert(i < NUM_PT_ENTRIES, "entry index too large");
  const PageTableEntry& entry = table.entries[i];
  if (util::get_bit(entry.flags, PageTableFlags::Present)) {
    return (void*)PTE_ADDR(entry);
  }
  return nullptr;
}
// Set entry to point to physical address, with given flags
static void set_entry(PageTable& table, unsigned i, const void* addr, uint8_t flags) {
  assert(i < NUM_PT_ENTRIES, "entry index too large");
  PageTableEntry& entry = table.entries[i];
  entry.addr = (uint64_t)addr;
  entry.flags = flags;
}
// Add an entry to a physical address, with given extra flags (Present is automatic)
static void add_entry(PageTable& table, unsigned i, const void* addr, uint8_t flags) {
  util::set_bit(flags, PageTableFlags::Present);
  set_entry(table, i, addr, flags);
}

// TODO: when exactly do we need to flush?
// static void flush_tlb() {
//   asm volatile ("movl %%cr3,%%eax; movl %%eax,%%cr3"::);
// }

static VirtMemAllocator* inst;
VirtMemAllocator::VirtMemAllocator() { assert_make_inst(inst, this); }
VirtMemAllocator& VirtMemAllocator::get() { return assert_get_inst(inst); }

void VirtMemAllocator::initialize(PhysMemAllocator* physMemAlloc) {
  this->physMemAlloc = physMemAlloc;
  PageTable* pml4_ptr;
  asm ("movq %%cr3,%0" : "=g"(pml4_ptr) : );
  pml4_table = pml4_ptr;
}

void* VirtMemAllocator::find_free_block(
    const lsize_t block_size, PageVirtualStatus min_status) {
  // FORNOW first pass linear scan allocator
  // scan from kernel start downwards
  lsize_t page = ((lsize_t)&_kernel_start) & PAGE_MASK;
  debug::serial_printf("find_free_block starting from %p\n",
                       &_kernel_start);
  // round down to nearest page block
  page -= page % block_size;
  for (; page != (lsize_t)nullptr; page -= block_size) {
    auto status = page_map_status((void*)page);
    if (status >= min_status) {
      debug::serial_printf("... found %p\n", page);
      return (void*)page;
    }
  }
  return nullptr;
}

void* VirtMemAllocator::alloc_free_page() {
  void* out = find_free_block(PAGE_SIZE, PageVirtualStatus::PageMissing);
  if (!out) return out;
  reserve_entry(out, PagingLevel::Page, RESERVE_MAGIC);
  return out;
}

void* VirtMemAllocator::alloc_free_l1_block() {
  void* out = find_free_block(LEVEL1_BLOCK_SIZE, PageVirtualStatus::PageTableMissing);
  if (!out) return out;
  reserve_entry(out, PagingLevel::PageTable, RESERVE_MAGIC);
  return out;
}

void* VirtMemAllocator::alloc_free_l2_block() {
  void* out = find_free_block(LEVEL2_BLOCK_SIZE, PageVirtualStatus::PageDirMissing);
  if (!out) return out;
  reserve_entry(out, PagingLevel::PageDir, RESERVE_MAGIC);
  return out;
}

void* VirtMemAllocator::alloc_free_l3_block() {
  void* out = find_free_block(LEVEL3_BLOCK_SIZE, PageVirtualStatus::PageDirPTMissing);
  if (!out) return out;
  reserve_entry(out, PagingLevel::PageDirPT, RESERVE_MAGIC);
  return out;
}

void VirtMemAllocator::poison_page(void* page) {
  assert((uint64_t)page % PAGE_SIZE == 0, "page must be page-aligned");
  reserve_entry(page, PagingLevel::Page, POISON_MAGIC);
}

#if (PAGING == PAGING_PML4)
PageVirtualStatus VirtMemAllocator::page_map_status(void* page) {
  // WARNING: Assumes identity mapping, probably should remap a page table
  // section somewhere into the kernel and write translation code.
  uint64_t addr = (uint64_t)page;
  const PageTable* pml3_table = (const PageTable*)resolve_entry(*pml4_table, PT4_INDEX(addr));
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

void VirtMemAllocator::do_map_page(void* virt_page, void* phys_page, uint8_t extra_flags) {
  assert(physMemAlloc,
         "VirtMemAllocator needs to bind a PhysMemAllocator before mapping pages");
  uint64_t addr = (uint64_t)virt_page;
  PageTable* pml3_table = (PageTable*)resolve_entry(*pml4_table, PT4_INDEX(addr));
  if (!pml3_table || pml3_table == (void*)RESERVE_MAGIC) {
    pml3_table = (PageTable*)physMemAlloc->alloc1();
    std::memset(pml3_table, 0, sizeof(PageTable));
    add_entry(*pml4_table, PT4_INDEX(addr), (void*)pml3_table, extra_flags);
  }
  PageTable* pml2_table = (PageTable*)resolve_entry(*pml3_table, PT3_INDEX(addr));
  if (!pml2_table || pml2_table == (void*)RESERVE_MAGIC) {
    pml2_table = (PageTable*)physMemAlloc->alloc1();
    std::memset(pml2_table, 0, sizeof(PageTable));
    add_entry(*pml3_table, PT3_INDEX(addr), (void*)pml2_table, extra_flags);
  }
  PageTable* pml1_table = (PageTable*)resolve_entry(*pml2_table, PT2_INDEX(addr));
  if (!pml1_table || pml1_table == (void*)RESERVE_MAGIC) {
    pml1_table = (PageTable*)physMemAlloc->alloc1();
    std::memset(pml1_table, 0, sizeof(PageTable));
    add_entry(*pml2_table, PT2_INDEX(addr), (void*)pml1_table, extra_flags);
  }
  [[maybe_unused]]
  PageTableEntry& pml1_entry = pml1_table->entries[PT1_INDEX(addr)];
  assert(!util::get_bit(pml1_entry.flags, PageTableFlags::Present),
         "attempting to map an already-mapped page");
  add_entry(*pml1_table, PT1_INDEX(addr), phys_page, extra_flags);
}

void VirtMemAllocator::reserve_entry(
    void* virt_page, PagingLevel level, uint64_t magic_value) {
  assert(level < PagingLevel::PageDirPT, "paging level too high for reservation");
  uint8_t extra_flags = 0;
  uint64_t addr = (uint64_t)virt_page;
  PageTable* pml3_table = (PageTable*)resolve_entry(*pml4_table, PT4_INDEX(addr));
  if (!pml3_table) {
    pml3_table = (PageTable*)physMemAlloc->alloc1();
    std::memset(pml3_table, 0, sizeof(PageTable));
    add_entry(*pml4_table, PT4_INDEX(addr), (void*)pml3_table, extra_flags);
  }
  PageTable* pml2_table = (PageTable*)resolve_entry(*pml3_table, PT3_INDEX(addr));
  if (level == PagingLevel::PageDir) {
    assert(!pml2_table, "attempting to reserve already-reserved mem");
    set_entry(*pml3_table, PT3_INDEX(addr), (void*)magic_value, extra_flags);
    return;
  }
  else if (!pml2_table) {
    pml2_table = (PageTable*)physMemAlloc->alloc1();
    std::memset(pml2_table, 0, sizeof(PageTable));
    add_entry(*pml3_table, PT3_INDEX(addr), (void*)pml2_table, extra_flags);
  }
  PageTable* pml1_table = (PageTable*)resolve_entry(*pml2_table, PT2_INDEX(addr));
  if (level == PagingLevel::PageTable) {
    assert(!pml1_table, "attempting to reserve already-reserved mem");
    set_entry(*pml2_table, PT2_INDEX(addr), (void*)magic_value, extra_flags);
    return;
  }
  else if (!pml1_table) {
    pml1_table = (PageTable*)physMemAlloc->alloc1();
    std::memset(pml1_table, 0, sizeof(PageTable));
    add_entry(*pml2_table, PT2_INDEX(addr), (void*)pml1_table, extra_flags);
  }
  assert(level == PagingLevel::Page, "unknown paging level");
  // TODO: this lets us reserve already-mapped null page. Not relevant in
  // practice, but dissatisfying.
  [[maybe_unused]]
  void* page = resolve_entry(*pml1_table, PT1_INDEX(addr));
  assert(!page, "attempting to reserve already-reserved mem");
  set_entry(*pml1_table, PT1_INDEX(addr), (void*)magic_value, extra_flags);
}

#else
#error "only PML4 supported"
#endif

void* VirtMemAllocator::map_page(void* virt_page, void* phys_page, uint8_t flags) {
  assert((lsize_t)virt_page % PAGE_SIZE == 0, "virt page must be aligned");
  assert((lsize_t)phys_page % PAGE_SIZE == 0, "phys page must be aligned");

  uint8_t extra_flags = 0;
  
  if (util::get_bit(flags, MapFlag::Executable)) {
    util::unset_bit(flags, MapFlag::Executable);
    // TODO: kernel support for NX, FORNOW just keep user-readable
    util::set_bit(extra_flags, PageTableFlags::UserSuper);
  }
  if (util::get_bit(flags, MapFlag::Writeable)) {
    util::unset_bit(flags, MapFlag::Writeable);
    util::set_bit(extra_flags, PageTableFlags::ReadWrite);
  }
  if (util::get_bit(flags, MapFlag::UserReadable)) {
    util::unset_bit(flags, MapFlag::UserReadable);
    util::set_bit(extra_flags, PageTableFlags::UserSuper); // user-readable
  }
  assert(flags == 0, "map_page did not handle all flags");

  do_map_page(virt_page, phys_page, extra_flags);

  return virt_page;
}

void map_block(
    PhysMemAllocator& physMemAlloc, VirtMemAllocator& virtMemAlloc,
    void* mem, lsize_t size, uint8_t flags) {
  assert(size % PAGE_SIZE == 0, "block must be multiple of PAGE_SIZE");
  assert((uint64_t)mem % PAGE_SIZE == 0, "mem must be page-aligned");
  void* end = mem + size;

  // alloc physical blocks in descending size for efficiency
  constexpr lsize_t big_alloc_size = PhysMemAllocator::NUM_PAGES_BIG * PAGE_SIZE;
  for (; mem <= end - big_alloc_size; mem += big_alloc_size) {
    void* phys_mem = physMemAlloc.allocBig();
    assert(phys_mem, "not enough memory to map block");
    for (unsigned i = 0; i < PhysMemAllocator::NUM_PAGES_BIG; ++i) {
      virtMemAlloc.map_page(mem + i*PAGE_SIZE, phys_mem + i*PAGE_SIZE, flags);
    }
  }

  constexpr lsize_t med_alloc_size = PhysMemAllocator::NUM_PAGES_MED * PAGE_SIZE;
  for (; mem <= end - med_alloc_size; mem += med_alloc_size) {
    void* phys_mem = physMemAlloc.allocMed();
    assert(phys_mem, "not enough memory to map block");
    for (unsigned i = 0; i < PhysMemAllocator::NUM_PAGES_MED; ++i) {
      virtMemAlloc.map_page(mem + i*PAGE_SIZE, phys_mem + i*PAGE_SIZE, flags);
    }
  }

  for (; mem < end; mem += PAGE_SIZE) {
    void* phys_mem = physMemAlloc.alloc1();
    assert(phys_mem, "not enough memory to map block");
    virtMemAlloc.map_page(mem, phys_mem, flags);
  }
}
