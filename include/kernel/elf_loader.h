#ifndef ELF_LOADER_H
#define ELF_LOADER_H

#include <memory>
#include "block_source.h"
#include "proc_allocator.h"

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
  };
  
  ELFLoader(const uint8_t* src);
  Status parse_header();
  Status load_process_image(ProcAllocator&);
  
 private:
  // UniquePtr<BlockSource> src;
  const uint8_t* src; // TODO: load from a generic block source
  const Elf64_Ehdr* elf_header;
  const Elf64_Shdr* sec_header_base;
  const Elf64_Phdr* prog_header_base;
  Elf64_Addr entry;
};

#endif
