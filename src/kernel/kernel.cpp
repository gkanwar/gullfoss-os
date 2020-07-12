/**
 * Note: This will be a freestanding C code, i.e. with no access to the C
 * runtime. However, freestanding code has access to header-only defns.
 */

// Check compiler once and for all
#if defined(__linux__)
#error "Must use a cross-compiler!"
#endif
#if !defined(__x86_64__)
#error "Kernel only supports x86_64 targets!"
#endif


#include <cstring>
#include <new>

#include "assert.h"
#include "bootboot.h"
#include "debug_serial.h"
#include "framebuffer.h"
#include "heap_allocator.h"
#include "interrupt_manager.h"
#include "keyboard_state.h"
#include "kernel.h"
#include "phys_mem_allocator.h"
#include "psffont.h"
#include "shell.h"
#include "tar.h"
#include "terminal.h"
#include "test.h"
#include "util.h"
// #include "vga.h"
#include "virt_mem_allocator.h"

// using Color = VGATerminal::Color;

// Need for purely virtual functions
extern "C" void __cxa_pure_virtual() { ASSERT_NOT_REACHED; }

// PMA, VMA, HA held globally because they exist before the heap exists
PhysMemAllocator physMemAlloc;
VirtMemAllocator virtMemAlloc;
HeapAllocator heapAlloc;

struct BitmapHeader {
  uint8_t magic[2]; // should be "BM"
  uint32_t size;
  uint8_t reserved_1[2];
  uint8_t reserved_2[2];
  uint32_t data_offset;
} __attribute__((packed));

void show_splash(const BOOTBOOT& info, pixel_t framebuffer[], const Tarball& initrd) {
  if (info.fb_type != FB_ARGB) {
    // TODO: pixel reordering
    // FORNOW: splash a constant color
    pixel_t fill_color;
    switch (info.fb_type) {
      case FB_RGBA: { fill_color = 0xffcc55ff; break; }
      case FB_ABGR: { fill_color = 0xff55ccff; break; }
      case FB_BGRA: { fill_color = 0x55ccffff; break; }
      default: { return; }
    }
    for (unsigned i = 0; i < info.fb_size/sizeof(pixel_t); ++i) {
      framebuffer[i] = fill_color;
    }
    return;
  }

  // Normal splash display (FB_ARGB, matching bitmap file format)
  debug::serial_printf("Framebuffer format is good, loading bitmap\n");
  const void* splash_file = initrd.find_file("waterfall.bmp");
  if (!splash_file) {
    debug::serial_printf("Failed to find splash file\n");
    return;
  }
  debug::serial_printf("Found splash file %p\n", splash_file);
  const BitmapHeader* splash_header = (const BitmapHeader*)splash_file;
  if (splash_header->magic[0] != 'B' ||
      splash_header->magic[1] != 'M') {
    debug::serial_printf("Corrupt splash file\n");
    return;
  }
  debug::serial_printf("Showing splash into %p\n", framebuffer);
  const pixel_t* pixel_data = (const pixel_t*)
      ((const uint8_t*)splash_file + splash_header->data_offset);
  const size_t width = 800, height = 600;
  for (unsigned row = 0; row < height; ++row) {
    const pixel_t* row_data = pixel_data + (height-row-1)*width; // bitmap stores bottom up
    pixel_t* framebuffer_row_data = framebuffer + row*info.fb_scanline/sizeof(pixel_t);
    std::memcpy(framebuffer_row_data, row_data, width*sizeof(pixel_t));
  }
}

int kernel_early_main(const BOOTBOOT& info, pixel_t framebuffer[]) {
  Tarball initrd((const void*)info.initrd_ptr, info.initrd_size);
  show_splash(info, framebuffer, initrd);
  
  const MMapEnt* mmap = &info.mmap;
  unsigned count = (info.size - 128)/sizeof(MMapEnt);
  physMemAlloc.init_mmap(mmap, count);
  virtMemAlloc.initialize(&physMemAlloc);
  debug::serial_printf("init heap pages...\n");
  heapAlloc.initialize(physMemAlloc, virtMemAlloc);
  debug::serial_printf("done\n");
  debug::serial_printf("heap ptr %p\n", heapAlloc.heap);
  return 0;
}
  
[[noreturn]]
void kernel_main(const BOOTBOOT& info, pixel_t framebuffer[]) 
{
  // VirtMemAllocator::get().clear_ident_map();
  // debug::serial_printf("ident map cleared\n");

  new KeyboardState;
  new InterruptManager;
  InterruptManager::get().init_interrupts();
  debug::serial_printf("interrupts enabled!\n");

  // TEST: Try to trigger div0 exception
  // int j = 0;
  // int i = 1/j;
  // debug::serial_printf("i = %d\n", i);
    
  // VGATerminal term;
  // term.set_color(Color::white, Color::black);
  // term.write_string("Welcome to the " PROJ_NAME " kernel!\n");
  // term.set_color(Color::light_grey, Color::black);
  // term.write_string("More features to come, tests for some specific ones run below.\n");
  // term.write_string("Use alt-2 (or equivalent) to get to QEMU console and shutdown.\n");
  // debug::serial_printf("terminal works!\n");
  
  test::pretty_print_test("malloc(128)", [&]()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(128);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xff, 128);
    uint8_t byte = mem_chunk[77];
    return byte == 0xff;
  });
  test::pretty_print_test("malloc(8)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(8);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xab, 8);
    uint8_t byte = mem_chunk[5];
    return byte == 0xab;
  });
  test::pretty_print_test("malloc(16)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(16);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xcd, 16);
    uint8_t byte = mem_chunk[13];
    return byte == 0xcd;
  });
  test::pretty_print_test("malloc(32)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(32);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0x88, 32);
    uint8_t byte = mem_chunk[27];
    return byte == 0x88;
  });
  debug::serial_printf("malloc all good\n");

  // Run our first real (kernel mode) app! For now it just takes ownership of
  // the whole kernel :)
  debug::serial_printf("BOOTBOOT info %p\n", &info);
  debug::serial_printf("...fb_size = %llu, (%llu x %llu)\n", info.fb_size,
                       info.fb_width, info.fb_height);
  Tarball initrd((const void*)info.initrd_ptr, info.initrd_size);
  const uint8_t* psf_buffer = (const uint8_t*)
      initrd.find_file("texgyrecursor-regular.psf");
  PSFFont font(psf_buffer);
  USKeyMap key_map;
  Framebuffer fb(framebuffer, info.fb_size, info.fb_height, info.fb_width, info.fb_scanline);
  FBTerminal term(&fb, font);
  app::Shell shell(key_map, term);
  KeyboardState::get().set_subscriber(shell);
  shell.main();

  panic("kernel exited!\n");
}


extern "C" {
  extern BOOTBOOT _bootboot;
  extern char _bootboot_environment;
  extern uint8_t _bootboot_fb;
  extern uint8_t _bootboot_mmio;

  extern void _init();

  [[noreturn]] void _start() {
    // TODO: why does this value not come in correctly from BOOTBOOT?
    _bootboot.fb_size = sizeof(pixel_t)*_bootboot.fb_scanline*_bootboot.fb_height;
    // Set up kernel memory management
    debug::serial_printf("begin kernel_early_main\n");
    debug::serial_printf("bootboot info fb_size %llu\n", _bootboot.fb_size);
    kernel_early_main(_bootboot, (pixel_t*)&_bootboot_fb);
    debug::serial_printf("end kernel_early_main\n");
    // Run global ctors
    debug::serial_printf("start global ctors\n");
    _init();
    debug::serial_printf("end global ctors\n");
    // Enter usual C++ happy land, where we can use new, etc.
    debug::serial_printf("start kernel_main\n");
    kernel_main(_bootboot, (pixel_t*)&_bootboot_fb);
  }
}
