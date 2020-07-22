#ifndef KERNEL_H
#define KERNEL_H

#include <stdint.h>

#define PROJ_NAME "gullfoss"
#define PAGE_MASK (~(0xfff))
#define PAGE_SIZE 0x1000
#define PAGE_ADDR_NBITS 12
// TODO: We're grabbing 4GiB of BOOTBOOT's 16GiB, we could extend
#define NUM_PHYS_PAGES (1 << 22)

// Just memory locations but we want void pointer semantics so mark as uint8_t
extern uint8_t _kernel_start;
extern uint8_t _kernel_end;

// Very common utils
template <typename T, typename U> inline
T round_up(T addr, U size) {
  if (addr % size != 0) {
    addr += size - addr % size;
  }
  return addr;
}
template <typename T, typename U> inline
T round_down(T addr, U size) {
  return addr - addr % size;
}

#endif
