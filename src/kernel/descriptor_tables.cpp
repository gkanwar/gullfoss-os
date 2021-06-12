#include "descriptor_tables.h"

#include "assert.h"
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

struct __attribute__((__packed__)) DescTables {
  struct __attribute__((__packed__)) {
    u64 _null_entry = 0;
    u64 kernel_code_segment = ACCESS_KERN_CODE | CODE_64BIT;
    u64 kernel_data_segment = ACCESS_KERN_DATA;
    u64 user_code_segment = ACCESS_USER_CODE | CODE_64BIT;
    u64 user_data_segment = ACCESS_USER_DATA;
  } gdt;
};

struct __attribute__((__packed__)) GDTDescriptor {
  u16 limit;
  u64 base;
};

void DescTablesManager::initialize(void* tables_page) {
  u64 tables_offset = (u64)tables_page;
  tables = (DescTables*)tables_offset; // NOTE: depends on BOOTBOOT identity mapping
  *tables = DescTables{};
  GDTDescriptor desc{
    .limit = sizeof(tables->gdt)-1,
    .base = tables_offset
  };
  asm volatile("lgdt %[gdt]" :: [gdt] "m" (desc));
}
