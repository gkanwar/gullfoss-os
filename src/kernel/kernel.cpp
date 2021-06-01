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
#include "ide_controller.h"
#include "interrupt_manager.h"
#include "ipc.h"
#include "keyboard_state.h"
#include "kernel.h"
#include "pci.h"
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
#include "virt_file_sys.h"
#include "virt_mem_allocator.h"


void spawn_elf(const char* path);
[[noreturn]] void shell_task(void*);
[[noreturn]] void test_task(void*);
void kernel_init_stage1(const BOOTBOOT& info);
[[noreturn]] void kernel_init_stage2(void*);


// PMA, VMA, HA held globally because they exist before the heap exists
PhysMemAllocator physMemAlloc;
VirtMemAllocator virtMemAlloc;
HeapAllocator heapAlloc;

/// Kernel early bootup. Does the bare minimum needed to set up memory.
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


struct KernelConfig {
  const char* rootfs;
};
KernelConfig kernel_config = {0};

void parse_kernel_config(const char* ro_env_string) {
  // TODO: Handle UTF-8 encoding
  char* env_string = new char[strlen(ro_env_string)+1];
  strcpy(env_string, ro_env_string);
  debug::serial_printf("kernel env string:\n%s\n", ro_env_string);

  bool tokens_left = true;
  while (tokens_left) {

    char* line = env_string;
    char* newline = strchr(env_string, '\n');
    if (newline != nullptr) {
      *newline = '\0';
      env_string = newline+1;
    }
    else {
      tokens_left = false;
    }

    char* delim = strchr(line, '=');
    assert(delim != nullptr, "Invalid kernel config environment");
    *delim = '\0';
    const char* key = line;
    const char* value = delim+1;
    debug::serial_printf("kernel config: %s = %s\n", key, value);
    if (strcmp(key, "root") == 0) {
      char* buf = new char[strlen(value)+1];
      strcpy(buf, value);
      kernel_config.rootfs = buf;
      debug::serial_printf("rootfs set to %s\n", buf);
    }

  }

  delete[] env_string;
}

/// Kernel main bootup. Stage 1 boot sets up interrupts and multitasking. Stage
/// 2 boot is established initially as the only kernel thread.
[[noreturn]]
void kernel_main(const BOOTBOOT& info, const char* env_string)
{
  parse_kernel_config(env_string);
  kernel_init_stage1(info);
  test_kernel_stage1();

  TaskManager::get().spawn(kernel_init_stage2, nullptr);

  // Kernel idle loop (TODO: exit() from this task?)
  while (true) { TaskManager::get().yield(); }
  ASSERT_NOT_REACHED;
}

void kernel_init_stage1(const BOOTBOOT&) {
  debug::serial_printf("[BEGIN kernel_init_stage1]\n");
  // Interrupts
  new KeyboardState;
  new TaskManager;
  new InterruptManager;
  InterruptManager::get().init_interrupts();
  InterruptManager::get().toggle_irq(PICMask1::Keyboard, true);
  pit::set_channel0_divisor(PIT_DIVISOR);
  InterruptManager::get().toggle_irq(PICMask1::PITimer, true);
  debug::serial_printf("[END kernel_init_stage1]\n");
}

[[noreturn]]
void kernel_init_stage2(void*) {
  debug::serial_printf("[BEGIN kernel_init_stage2]\n");

  // PCI
  vector<pci::PCIDevice> devices = pci::enumerate_devices();
  debug::serial_printf("PCI enumeration found %d devices\n", devices.size());
  for (auto &dev : devices) {
    const char* vendor_string = pci::get_vendor_string(dev.vendor_id);
    debug::serial_printf(
        "... (%04x:%02x.0 vid=%04x[%s] did=%04x class=[%02x:%02x])\n",
        dev.addr.bus, dev.addr.device, dev.vendor_id, vendor_string, dev.device_id,
        dev.class_code, dev.subclass_code);
    // Storage
    if (dev.class_code == 0x01 && dev.subclass_code == 0x01) {
      assert(!IDEController::initialized(), "Multiple IDE controllers not supported");
      new IDEController(dev);
    }
  }
  
  // VFS
  new VirtFileSystem;
  // TODO: Use rootfs config to find partition and mount "/"

  // Userspace multi-processing
  new ProcAllocator(PhysMemAllocator::get(), VirtMemAllocator::get());
  new InterProcessComm;

  debug::serial_printf("[END kernel_init_stage2] (handoff to stage 3)\n");

  // TEST: run two tasks simultaneously
  TaskManager::get().spawn(test_task, nullptr);
  TaskManager::get().spawn(test_task, nullptr);
  
  // TODO: Fire Stage 3 booting
  // FORNOW: Just put up our dummy "compositor" userspace process
  // spawn_elf("/apps/compositor");

  TaskManager::get().exit();
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
    kernel_main(_bootboot, &_bootboot_environment);
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
[[noreturn]] void shell_task(void*) {
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
[[noreturn]] void test_task(void*) {
  for (int i = 0; i < 10; ++i) {
    debug::serial_printf("test_task %d\n", i);
    TaskManager::get().yield();
  }
  debug::serial_printf("test_task exiting\n");
  TaskManager::get().exit();
}

void spawn_elf(const char* path) {
  debug::serial_printf("spawn_elf start\n");
  // const BOOTBOOT& info = _bootboot;
  // debug::serial_printf("BOOTBOOT info %p\n", &info);
  unique_ptr<File> user_elf = VirtFileSystem::get().open(path);
  assert(user_elf, "User ELF not found");
  ELFLoader elf_loader(user_elf->buffer);
  ELFLoader::Status ret;
  ret = elf_loader.parse_header();
  if (ret != ELFLoader::Status::SUCCESS) {
    debug::serial_printf("Failed to parse ELF header: %d\n", ret);
    panic("bad!");
  }
  ProcAllocator& alloc = ProcAllocator::get();
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
  void (*entry)(void*) = (void(*)(void*))elf_loader.get_entry();
  TaskManager::get().spawn(entry, nullptr);
}
