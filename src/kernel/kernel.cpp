/**
 * Note: This will be a freestanding C code, i.e. with no access to the C
 * runtime. However, freestanding code has access to header-only defns.
 */

#include <cstring>
#include <new>

#include "assert.h"
#include "debug_serial.h"
#include "heap_allocator.h"
#include "interrupt_manager.h"
#include "keyboard_state.h"
#include "kernel.h"
#include "multiboot.h"
#include "phys_mem_allocator.h"
#include "shell.h"
#include "test.h"
#include "vga.h"
#include "virt_mem_allocator.h"

using namespace std;
using Color = VGATerminal::Color;

// Put these checks in once to ensure linux or x64 targets crash out
#if defined(__linux__)
#error "Must use a cross-compiler!"
#endif
#if !defined(__i386__)
#error "Kernel only supports 32-bit ix86 targets!"
#endif


extern uint32_t init_page_dir[1024];
extern uint32_t init_page_tables[1024*1024];

// PMA, VMA, HA held globally because they exist before the heap exists
PhysMemAllocator physMemAlloc;
VirtMemAllocator virtMemAlloc;
HeapAllocator heapAlloc;

extern "C" {
  
__attribute__((fastcall))
int kernel_early_main(const multiboot_info_t *info) {
  debug::init_serial();
  debug::serial_printf("begin kernel_early_main\n");
  debug::serial_printf("init page dir:\n");
  for (int i = 0; i < 1024; ++i) {
    debug::serial_printf("%08x ", init_page_dir[i]);
  }
  debug::serial_printf("\n");
  uint32_t* page_table = (uint32_t*)(init_page_dir[0] & 0xfffffc00);
  debug::serial_printf("init page table:\n");
  for (int i = 0; i < 1024; ++i) {
    debug::serial_printf("%08x ", page_table[i]);
  }
  debug::serial_printf("\n");
  
  unsigned long flags = info->flags;
  if (!(flags & MULTIBOOT_INFO_MEM_MAP)) {
    debug::serial_printf("multiboot mmap structures required to boot\n");
    return 1;
  }
  uint32_t mmap_length = info->mmap_length;
  uint32_t mmap_addr = info->mmap_addr + KERNEL_BASE;
  physMemAlloc.init_mmap(mmap_length, mmap_addr);
  debug::serial_printf("phys mem bitmap:\n");
  for (int i = 0; i < (1 << 8); ++i) {
    debug::serial_printf("%08x ", ((uint32_t*)physMemAlloc.mem_bitmap)[i]);
  }
  debug::serial_printf("...\n");
  debug::serial_printf("init page dir...\n");
  virtMemAlloc.init_page_dir(init_page_dir, init_page_tables);
  debug::serial_printf("done\n");
  debug::serial_printf("init heap pages...\n");
  heapAlloc.init_heap_pages(physMemAlloc, virtMemAlloc);
  debug::serial_printf("done\n");
  debug::serial_printf("end kernel_early_main\n");
  return 0;
}
  
[[noreturn]]
void kernel_main(void) 
{
  debug::serial_printf("start kernel_main\n");
  VirtMemAllocator::get().clear_ident_map();
  debug::serial_printf("ident map cleared\n");

  new KeyboardState;
  new InterruptManager;
  InterruptManager::get().init_interrupts();
  debug::serial_printf("interrupts enabled!\n");

  VGATerminal term;
  term.set_color(Color::white, Color::black);
  term.write_string("Welcome to the " PROJ_NAME " kernel!\n");
  term.set_color(Color::light_grey, Color::black);
  term.write_string("More features to come, tests for some specific ones run below.\n");
  term.write_string("Use alt-2 (or equivalent) to get to QEMU console and shutdown.\n");
  debug::serial_printf("terminal works!\n");
  
  test::pretty_print_test("malloc(128)", term, [&]()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(128);
    if (!mem_chunk) return false;
    memset((void*)mem_chunk, 0xff, 128);
    uint8_t byte = mem_chunk[77];
    return byte == 0xff;
  });
  test::pretty_print_test("malloc(8)", term, []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(8);
    if (!mem_chunk) return false;
    memset((void*)mem_chunk, 0xab, 8);
    uint8_t byte = mem_chunk[5];
    return byte == 0xab;
  });
  test::pretty_print_test("malloc(16)", term, []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(16);
    if (!mem_chunk) return false;
    memset((void*)mem_chunk, 0xcd, 16);
    uint8_t byte = mem_chunk[13];
    return byte == 0xcd;
  });
  test::pretty_print_test("malloc(32)", term, []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(32);
    if (!mem_chunk) return false;
    memset((void*)mem_chunk, 0x88, 32);
    uint8_t byte = mem_chunk[27];
    return byte == 0x88;
  });
  debug::serial_printf("malloc all good\n");

  // Run our first real (kernel mode) app! For now it just takes ownership of
  // the whole kernel :)
  USKeyMap key_map;
  app::Shell shell(key_map, term);
  KeyboardState::get().set_subscriber(shell);
  shell.main();

  panic("kernel exited!\n");
}

}
