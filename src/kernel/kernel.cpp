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
#include "elf_loader.h"
#include "framebuffer.h"
#include "heap_allocator.h"
#include "interrupt_manager.h"
#include "keyboard_state.h"
#include "kernel.h"
#include "phys_mem_allocator.h"
#include "psffont.h"
#include "shell.h"
#include "syscalls.h"
#include "tar.h"
#include "task_manager.h"
#include "terminal.h"
#include "test.h"
#include "util.h"
#include "virt_mem_allocator.h"

// Need for purely virtual functions
#define ATEXIT_FUNC_MAX 128
extern "C" {
  void __cxa_pure_virtual() { ASSERT_NOT_REACHED; }

  struct dtor_entry_t {
    void (*dtor)(void*) = nullptr;
    void* obj = nullptr;
    void* dso_handle = nullptr;
  };
  dtor_entry_t dtor_funcs[ATEXIT_FUNC_MAX];
  unsigned dtor_func_count = 0;
  unsigned __cxa_atexit(void (*dtor)(void*), void* obj, void* dso_handle) {
    if (dtor_func_count >= ATEXIT_FUNC_MAX) {
      return -1;
    }
    dtor_funcs[dtor_func_count++] = {
      .dtor = dtor, .obj = obj, .dso_handle = dso_handle
    };
    return 0;
  }

  void __cxa_finalize(void* f) {
    if (f == nullptr) {
      for (unsigned i = 0; i < dtor_func_count; ++i) {
        if (dtor_funcs[i].dtor) {
          (*dtor_funcs[i].dtor)(dtor_funcs[i].obj);
        }
      }
    }
    else {
      for (unsigned i = 0; i < dtor_func_count; ++i) {
        if (dtor_funcs[i].dtor == f) {
          (*dtor_funcs[i].dtor)(dtor_funcs[i].obj);
          dtor_funcs[i] = {0};
        }
      }
    }
  }
}

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

void show_splash(const BOOTBOOT& info, pixel_t* framebuffer, Tarball& initrd) {
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
  tar_file_t splash_file = initrd.find_file("waterfall.bmp");
  if (!splash_file.buffer) {
    debug::serial_printf("Failed to find splash file\n");
    return;
  }
  debug::serial_printf("Found splash file %p\n", splash_file.buffer);
  const BitmapHeader* splash_header = (const BitmapHeader*)splash_file.buffer;
  if (splash_header->magic[0] != 'B' ||
      splash_header->magic[1] != 'M') {
    debug::serial_printf("Corrupt splash file\n");
    return;
  }
  debug::serial_printf("Showing splash into %p\n", framebuffer);
  const pixel_t* pixel_data = (const pixel_t*)
      ((const uint8_t*)splash_file.buffer + splash_header->data_offset);
  const lsize_t width = 800, height = 600;
  for (unsigned row = 0; row < height; ++row) {
    const pixel_t* row_data = pixel_data + (height-row-1)*width; // bitmap stores bottom up
    pixel_t* framebuffer_row_data = framebuffer + row*info.fb_scanline/sizeof(pixel_t);
    std::memcpy(framebuffer_row_data, row_data, width*sizeof(pixel_t));
  }
}

int kernel_early_main(const BOOTBOOT& info, pixel_t* framebuffer) {
  Tarball initrd((void*)info.initrd_ptr, info.initrd_size);
  show_splash(info, framebuffer, initrd);

  const MMapEnt* mmap = &info.mmap;
  unsigned count = (info.size - 128)/sizeof(MMapEnt);
  physMemAlloc.init_mmap(mmap, count);
  virtMemAlloc.initialize(&physMemAlloc);
  virtMemAlloc.poison_page(nullptr);
  debug::serial_printf("init heap pages...\n");
  heapAlloc.initialize(physMemAlloc, virtMemAlloc);
  debug::serial_printf("done\n");
  return 0;
}

// Our first real (kernel mode) app!
// FORNOW: Just forward-declare these globals again, need to figure out args to
// started tasks.
extern "C" {
  extern BOOTBOOT _bootboot;
  extern uint8_t _bootboot_fb;
}
[[noreturn]] void shell_task() {
  debug::serial_printf("shell_task start\n");
  const BOOTBOOT& info = _bootboot;
  pixel_t* framebuffer = (pixel_t*)&_bootboot_fb;
  debug::serial_printf("BOOTBOOT info %p\n", &info);
  debug::serial_printf("...fb_size = %llu, (%llu x %llu)\n", info.fb_size,
                       info.fb_width, info.fb_height);
  Tarball initrd((void*)info.initrd_ptr, info.initrd_size);
  tar_file_t psf = initrd.find_file("texgyrecursor-regular.psf");
  assert(psf.buffer, "PSF not found for shell");
  PSFFont font(psf.buffer);
  USKeyMap key_map;
  Framebuffer fb(framebuffer, info.fb_size, info.fb_height, info.fb_width, info.fb_scanline);
  FBTerminal term(&fb, font);
  app::Shell shell(key_map, term);
  KeyboardState::get().set_subscriber(shell);
  shell.main();
  ASSERT_NOT_REACHED;
}

[[noreturn]] void elf_user_task() {
  debug::serial_printf("elf_user_task start\n");
  const BOOTBOOT& info = _bootboot;
  debug::serial_printf("BOOTBOOT info %p\n", &info);
  Tarball initrd((void*)info.initrd_ptr, info.initrd_size);
  tar_file_t user_elf = initrd.find_file("apps/wallpaper");
  assert(user_elf.buffer, "User ELF apps/wallpaper not found");
  // ELFLoader elf_loader(unique_ptr<BlockSource>(new InMemorySource(
  //     (const uint8_t*)user_elf.buffer, user_elf.size)));
  ELFLoader elf_loader((const uint8_t*)user_elf.buffer);
  debug::serial_printf("First 4 chars of ELF: %c %c %c %c\n",
                       user_elf.buffer[0], user_elf.buffer[1],
                       user_elf.buffer[2], user_elf.buffer[3]);
  ELFLoader::Status ret;
  ret = elf_loader.parse_header();
  if (ret != ELFLoader::Status::SUCCESS) {
    debug::serial_printf("Failed to parse ELF header: %d\n", ret);
    panic("bad!");
  }
  ProcAllocator alloc(PhysMemAllocator::get(), VirtMemAllocator::get());
  ret = elf_loader.load_process_image(alloc);
  if (ret != ELFLoader::Status::SUCCESS) {
    debug::serial_printf("Failed to load ELF: %d\n", ret);
    panic("bad!");
  }
  else {
    debug::serial_printf("Load process image success!\n");
  }
  ret = elf_loader.dynamic_link();
  if (ret != ELFLoader::Status::SUCCESS) {
    debug::serial_printf("Failed to dynamic link ELF: %d\n", ret);
    panic("bad!");
  }
  else {
    debug::serial_printf("Dynamic link success!\n");
  }
  elf_loader.exec_process();
  ASSERT_NOT_REACHED;
}

[[noreturn]]
void kernel_main()
{
  // VirtMemAllocator::get().clear_ident_map();
  // debug::serial_printf("ident map cleared\n");
  debug::serial_printf("LEVEL3_BLOCK_SIZE check: %llu\n", (void*)LEVEL3_BLOCK_SIZE);
  assert(LEVEL3_BLOCK_SIZE, "LEVEL3_BLOCK_SIZE should not overflow!");

  new KeyboardState;
  new InterruptManager;
  InterruptManager::get().init_interrupts();
  InterruptManager::get().toggle_irq(PICMask1::Keyboard, true);
  debug::serial_printf("interrupts enabled!\n");

  // TEST: Try to trigger div0 exception
  // int j = 0;
  // int i = 1/j;
  // debug::serial_printf("i = %d\n", i);

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

  // Multitasking
  new TaskManager;
  InterruptManager::get().toggle_irq(PICMask1::PITimer, true);

  // Fire Stage 2 booting (for now, just run "shell")
  // TaskManager::get().start(shell_task);
  // FORNOW: attempt to load user-space ELF exe
  TaskManager::get().start(elf_user_task);

  // Kernel idle loop (maybe we should nuke this task?)
  while (true) { asm volatile("hlt"::); }
  ASSERT_NOT_REACHED;
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
    kernel_main();
  }

  // FIXME: huge hack to just get userspace graphics going
  framebuffer_t get_framebuffer() {
    return {
      .pixels = (u32*)&_bootboot_fb,
      .width = _bootboot.fb_width,
      .height = _bootboot.fb_height
    };
  }
}
