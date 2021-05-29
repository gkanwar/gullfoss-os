#include "debug_serial.h"
#include "elf_loader.h"
#include "interrupt_manager.h"
#include "ipc.h"
#include "syscalls.h"
#include "task_manager.h"
#include "virt_file_sys.h"

extern "C" {

  void spawn(const char* path, size_t path_len) {
    char* path_cstr = new char[path_len+1];
    std::memcpy(path_cstr, path, path_len);
    path_cstr[path_len] = '\0';

    unique_ptr<File> exe = VirtFileSystem::get().open(path_cstr);
    if (!exe) {
      debug::serial_printf("spawn failed to find %s\n", path_cstr);
      delete[] path_cstr;
      return;
    }

    ELFLoader::Status ret;
    ELFLoader elf_loader(exe->buffer);
    ret = elf_loader.parse_header();
    if (ret != ELFLoader::Status::SUCCESS) {
      debug::serial_printf("Failed to parse ELF header: %d\n", ret);
      delete[] path_cstr;
      return;
    }

    ret = elf_loader.load_process_image(ProcAllocator::get());
    if (ret != ELFLoader::Status::SUCCESS) {
      debug::serial_printf("Failed to load ELF: %d\n", ret);
      delete[] path_cstr;
      return;
    }

    ret = elf_loader.dynamic_link();
    if (ret != ELFLoader::Status::SUCCESS) {
      debug::serial_printf("Failed to dynamic link ELF: %d\n", ret);
      delete[] path_cstr;
      return;
    }

    // TODO: init args
    TaskManager::get().spawn((void(*)(void*))elf_loader.get_entry(), nullptr);

    delete[] path_cstr;
  }

  void yield() {
    ScopedInterruptGuard guard;
    TaskManager::get().yield();
  }

  void exit(u8 code) {
    // FORNOW: do nothing
    debug::serial_printf("syscall: exit(%d)\n", (int)code);
  }

  void* accept(u16 port) {
    return InterProcessComm::get().accept(port);
  }

  void send(u16 port, void* data) {
    InterProcessComm::get().send(port, data);
  }

}
