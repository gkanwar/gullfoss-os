#ifndef KERNEL_H
#define KERNEL_H

#define PROJ_NAME "gullfoss"
#define KERNEL_BASE 0xc0000000
// #ifndef ASM_FILE
// #define KERNEL_VIRT_TO_PHYS(addr) (((uint32_t)addr) - KERNEL_BASE)
// #else
// #define KERNEL_VIRT_TO_PHYS(addr) ((addr) - KERNEL_BASE)
// #endif
#define PAGE_MASK (~(0xfff))
#define PAGE_SIZE 0x1000
#define PAGE_BLOCK_SIZE (512 * PAGE_SIZE)
#define PAGE_ADDR_NBITS 12
#define NUM_PAGES (1 << 20)

#define PAGE_DIR_SHIFT 22
#define PAGE_TABLE_SHIFT PAGE_ADDR_NBITS
#define PAGE_TABLE_MASK 0x3ff

// #ifndef ASM_FILE // C++ specific stuff

#include <stdint.h>

#define VIRT_TO_PT_INDEX(addr) ((((uint32_t)addr) >> PAGE_TABLE_SHIFT) & PAGE_TABLE_MASK)
#define VIRT_TO_PD_INDEX(addr) (((uint32_t)addr) >> PAGE_DIR_SHIFT)

// Just memory locations but we want void pointer semantics so mark as uint8_t
extern uint8_t _kernel_start;
extern uint8_t _kernel_end;

// #endif

#endif
