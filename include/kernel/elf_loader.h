#ifndef ELF_LOADER_H
#define ELF_LOADER_H

#include <memory>
#include "block_source.h"
#include "proc_allocator.h"

using namespace std;

struct Elf64_Ehdr;
struct Elf64_Shdr;
struct Elf64_Phdr;
typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;
typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef  int32_t Elf64_Sword;
typedef uint64_t Elf64_Xword;
typedef  int64_t Elf64_Sxword;

class SymbolMap {
 public:
  virtual void* resolve_symbol(const char*) const = 0;
};

class EmptySymbolMap : public SymbolMap {
 public:
  EmptySymbolMap() {}
  void* resolve_symbol(const char*) const override { return nullptr; }
};

// FORNOW: dumb linked list version
class CompositeSymbolMap : public SymbolMap {
 public:
  CompositeSymbolMap(unique_ptr<SymbolMap>, const char*, void*);
  void* resolve_symbol(const char*) const override;
 private:
  unique_ptr<SymbolMap> inner_map;
  const char* symbol;
  void* ptr;
};

class ELFLoader {
 public:
  enum class Status {
    SUCCESS,
    E_BAD_MAGIC,
    E_UNSUPP_CLASS,
    E_UNSUPP_DATA,
    E_BAD_VERSION,
    E_UNSUPP_ABI,
    E_UNSUPP_TYPE,
    E_UNSUPP_PHDR,
    E_MAX_LOAD_SEGMENTS,
    E_BAD_PTPHDR,
    E_BAD_PTDYN,
    E_DYNLINK_FAIL,
  };
  
  ELFLoader(const uint8_t* src);
  Status parse_header();
  // NOTE: limited to PIC only, because of the shared memory space
  Status load_process_image(ProcAllocator&);
  Status dynamic_link();
  [[noreturn]] void exec_process();
  
 private:
  Status load_dylib(const char*);
  // unique_ptr<BlockSource> src;
  const uint8_t* src; // TODO: load from a generic block source
  const char* interp; // TODO: split dynamic linking out into this exe
  const Elf64_Ehdr* elf_header;
  const Elf64_Shdr* sec_header_base;
  const Elf64_Phdr* prog_header_base;
  Elf64_Off base_addr;
  Elf64_Addr entry;
  Elf64_Addr dynamic;
  Elf64_Off dynamic_len;
  unique_ptr<SymbolMap> symbol_map;
};

#endif
