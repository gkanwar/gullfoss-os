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
#include "prog_int_timer.h"
#include "psffont.h"
#include "shell.h"
#include "splash.h"
#include "syscalls.h"
#include "tar.h"
#include "task_manager.h"
#include "terminal.h"
#include "test.h"
#include "util.h"
#include "virt_mem_allocator.h"

// PMA, VMA, HA held globally because they exist before the heap exists
PhysMemAllocator physMemAlloc;
VirtMemAllocator virtMemAlloc;
HeapAllocator heapAlloc;

int kernel_early_main(const BOOTBOOT& info, pixel_t* framebuffer) {
  Tarball initrd((void*)info.initrd_ptr, info.initrd_size);
  show_splash(info, framebuffer, initrd);

  const MMapEnt* mmap = &info.mmap;
  unsigned count = (info.size - 128)/sizeof(MMapEnt);
  physMemAlloc.init_mmap(mmap, count);
  virtMemAlloc.initialize(&physMemAlloc);
  virtMemAlloc.poison_page(nullptr);
  heapAlloc.initialize(physMemAlloc, virtMemAlloc);

  test_kernel_early_main();
  
  return 0;
}

// Our first real (kernel mode) apps:
[[noreturn]] void shell_task();
[[noreturn]] void elf_user_task();

[[noreturn]]
void kernel_main()
{
  debug::serial_printf("LEVEL3_BLOCK_SIZE check: %llu\n", (void*)LEVEL3_BLOCK_SIZE);
  assert(LEVEL3_BLOCK_SIZE, "LEVEL3_BLOCK_SIZE should not overflow!");

  // Interrupts
  new KeyboardState;
  new TaskManager;
  new InterruptManager;
  InterruptManager::get().init_interrupts();
  InterruptManager::get().toggle_irq(PICMask1::Keyboard, true);
  pit::set_channel0_divisor(PIT_DIVISOR);
  InterruptManager::get().toggle_irq(PICMask1::PITimer, true);

  test_kernel_main();

  // Fire Stage 2 booting (for now, just run "shell")
  // TaskManager::get().start(shell_task);
  // FORNOW: attempt to load user-space ELF exe
  TaskManager::get().start(elf_user_task);

  // Kernel idle loop (TODO: exit() from this task?)
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
    kernel_early_main(_bootboot, (pixel_t*)&_bootboot_fb);
    // Run global ctors
    _init();
    // Enter usual C++ happy land, where we can use new, etc.
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


// TESTING: some kernel mode tasks
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
  ELFLoader elf_loader((const uint8_t*)user_elf.buffer);
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
