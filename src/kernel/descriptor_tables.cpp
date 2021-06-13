#include "descriptor_tables.h"

#include "assert.h"
#include "util.h"
#include "virt_mem_allocator.h"
#include <types.h>


static DescTablesManager* inst;
DescTablesManager::DescTablesManager() { assert_make_inst(inst, this); }
DescTablesManager& DescTablesManager::get() { return assert_get_inst(inst); }


#ifndef __x86_64__
#error "Descriptor tables code highly specialized to x86_64"
#endif

constexpr u64 ACCESS_KERN_CODE = ((u64)0b10011010) << 40;
constexpr u64 ACCESS_KERN_DATA = ((u64)0b10010010) << 40;
constexpr u64 ACCESS_USER_CODE = ((u64)0b11111010) << 40;
constexpr u64 ACCESS_USER_DATA = ((u64)0b11110010) << 40;
constexpr u64 CODE_64BIT = ((u64)1) << 53;

struct __attribute__((__packed__)) TSS {
  u32 _res0 = 0;
  // stack pointers for privilege escalation
  u64 rsp0;
  u64 rsp1;
  u64 rsp2;
  u32 _res1 = 0;
  u32 _res2 = 0;
  u64 ist1;
  u64 ist2;
  u64 ist3;
  u64 ist4;
  u64 ist5;
  u64 ist6;
  u64 ist7;
  u32 _res3 = 0;
  u32 _res4 = 0;
  u16 _res5 = 0;
  u16 iopb_offset;
};

struct __attribute__((__packed__)) TSSDescriptor {
  u16 limit;
  u16 base0;
  u8 base1;
  u16 flags;
  u8 base2;
  u32 base3;
  u32 _res = 0;
};

struct __attribute__((__packed__)) DescTables {
  struct __attribute__((__packed__)) {
    u64 _null_entry = 0;
    u64 kernel_code_desc = ACCESS_KERN_CODE | CODE_64BIT;
    u64 kernel_data_desc = ACCESS_KERN_DATA;
    u64 user_code_desc = ACCESS_USER_CODE | CODE_64BIT;
    u64 user_data_desc = ACCESS_USER_DATA;
    TSSDescriptor tss_desc = {};
  } gdt;
  TSS tss = {};
};

struct __attribute__((__packed__)) GDTDescriptor {
  u16 limit;
  u64 base;
};

void DescTablesManager::initialize(void* tables_page) {
  u64 tables_offset = (u64)tables_page;
  tables = (DescTables*)tables_offset; // NOTE: depends on BOOTBOOT identity mapping
  // default init
  *tables = DescTables{};

  // make kernel syscall stacks and set up TSS
  void* kernel_stack = VirtMemAllocator::get().alloc_free_l1_block();
  u8 flags = 0;
  util::set_bit(flags, MapFlag::Writeable);
  map_block(PhysMemAllocator::get(), VirtMemAllocator::get(),
            (void*)((u64)kernel_stack + PAGE_SIZE),
            LEVEL1_BLOCK_SIZE - PAGE_SIZE, flags);
  VirtMemAllocator::get().poison_page(kernel_stack); // poison end of stack
  tables->tss.rsp0 = (u64)kernel_stack;
  // set up TSS desc
  u64 tss_addr = (u64)&(tables->tss);
  assert(sizeof(tables->tss) <= 0xffff, "TSS overflows 16-bit limit field");
  tables->gdt.tss_desc = TSSDescriptor{
    .limit = sizeof(tables->tss)-1,
    .base0 = (u16)(tss_addr & 0xffff),
    .base1 = (u8)((tss_addr >> 16) & 0xff),
    .flags = 0b10001001,
    .base2 = (u8)((tss_addr >> 24) & 0xff),
    .base3 = (u32)((tss_addr >> 32) & 0xffffffff)
  };
  // TODO: set up a "known good" stack for some interrupts using ISTs

  // load GDT
  GDTDescriptor desc{
    .limit = sizeof(tables->gdt)-1,
    .base = tables_offset
  };
  asm volatile("lgdt %[gdt]" :: [gdt] "m" (desc));
  // NOTE: normally we would have to reload segment registers, but we simply
  // recreated the same kernel segments as before so there are no issues.
}
