#include <climits>
#include "assert.h"
#include "debug_serial.h"
#include "elf_loader.h"
#include "kernel.h"

using Status = ELFLoader::Status;

#define ELF_NIDENT 16
struct Elf64_Ehdr {
  unsigned char e_ident[ELF_NIDENT];
  Elf64_Half e_type;
  Elf64_Half e_machine;
  Elf64_Word e_version;
  Elf64_Addr e_entry;
  Elf64_Off e_phoff;
  Elf64_Off e_shoff;
  Elf64_Word e_flags;
  Elf64_Half e_ehsize;
  Elf64_Half e_phentsize;
  Elf64_Half e_phnum;
  Elf64_Half e_shentsize;
  Elf64_Half e_shnum;
  Elf64_Half e_shstrndx;
};

enum Elf_Ident {
  EI_MAG0 = 0,
  EI_MAG1 = 1,
  EI_MAG2 = 2,
  EI_MAG3 = 3,
  EI_CLASS = 4,
  EI_DATA = 5,
  EI_VERSION = 6,
  EI_OSABI = 7,
  EI_ABIVERSION = 8,
  EI_PAD = 9,
};

enum Elf_Class {
  ELFCLASS32 = 1,
  ELFCLASS64 = 2,
};

enum Elf_DataEncoding {
  ELFDATA2LSB = 1,
  ELFDATA2MSB = 2,
};

enum Elf_OsAbi {
  ELFOSABI_SYSV = 0,
};

enum Elf_ObjectType {
  ET_NONE = 0,
  ET_REL = 1,
  ET_EXEC = 2,
  ET_DYN = 3,
  ET_CORE = 4,
};

enum Elf_Machine {
  EM_386 = 3,
  EM_X86_64 = 62,
};

enum ElfVersion {
  EV_CURRENT = 1,
};

enum ElfSectionInd {
  SHN_UNDEF = 0,
  SHN_ABS = 0xfff1,
  SHN_COMMON = 0xfff2,
};

struct Elf64_Shdr {
  Elf64_Word sh_name;
  Elf64_Word sh_type;
  Elf64_Xword sh_flags;
  Elf64_Addr sh_addr;
  Elf64_Off sh_offset;
  Elf64_Xword sh_size;
  Elf64_Word sh_link;
  Elf64_Word sh_info;
  Elf64_Xword sh_addralign;
  Elf64_Xword sh_entsize;
};

enum ElfSectionType {
  SHT_NULL = 0,
  SHT_PROGBITS = 1,
  SHT_SYMTAB = 2,
  SHT_STRTAB = 3,
  SHT_RELA = 4,
  SHT_HASH = 5,
  SHT_DYNAMIC = 6,
  SHT_NOTE = 7,
  SHT_NOBITS = 8,
  SHT_REL = 9,
  SHT_SHLIB = 10,
  SHT_DYNSYM = 11,
};

enum ElfSectionAttrib {
  SHF_WRITE = 0x1,
  SHF_ALLOC = 0x2,
  SHF_EXECINSTR = 0x4,
};

struct Elf64_Sym {
  Elf64_Word st_name;
  unsigned char st_info;
  unsigned char st_other;
  Elf64_Half st_shndx;
  Elf64_Addr st_value;
  Elf64_Xword st_size;
};

enum ElfSymbolBinding {
  STB_LOCAL = 0,
  STB_GLOBAL = 1,
  STB_WEAK = 2,
};

enum ElfSymbolType {
  STT_NOTYPE = 0,
  STT_OBJECT = 1,
  STT_FUNC = 2,
  STT_SECTION = 3,
  STT_FILE = 4,
};

struct Elf64_Rel {
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
};
struct Elf64_Rela {
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
  Elf64_Sxword r_addend;
};

struct Elf64_Phdr {
  Elf64_Word p_type;
  Elf64_Word p_flags;
  Elf64_Off p_offset;
  Elf64_Addr p_vaddr;
  Elf64_Addr p_paddr;
  Elf64_Xword p_filesz;
  Elf64_Xword p_memsz;
  Elf64_Xword p_align;
};

enum ElfSegmentType {
  PT_NULL = 0,
  PT_LOAD = 1,
  PT_DYNAMIC = 2,
  PT_INTERP = 3,
  PT_NOTE = 4,
  PT_SHLIB = 5,
  PT_PHDR = 6,
  PT_GNU_EH_FRAME = 0x6474e550,
  PT_GNU_STACK = 0x6474e551,
};
enum ElfSegmentAttrib {
  PF_X = 0x1,
  PF_W = 0x2,
  PF_R = 0x4,
};

ELFLoader::ELFLoader(const uint8_t* src) : src(src) {}
    // src(std::move(src)) {}

Status ELFLoader::parse_header() {
  elf_header = (const Elf64_Ehdr*)src;
  const Elf64_Ehdr* header = elf_header;
  if (header->e_ident[EI_MAG0] != '\x7f' ||
      header->e_ident[EI_MAG1] != 'E' ||
      header->e_ident[EI_MAG2] != 'L' ||
      header->e_ident[EI_MAG3] != 'F') {
    return Status::E_BAD_MAGIC;
  }
  if (header->e_ident[EI_CLASS] != ELFCLASS64) {
    return Status::E_UNSUPP_CLASS;
  }
  if (header->e_ident[EI_DATA] != ELFDATA2LSB) {
    return Status::E_UNSUPP_DATA;
  }
  if (header->e_ident[EI_VERSION] != EV_CURRENT ||
      header->e_version != EV_CURRENT) {
    return Status::E_BAD_VERSION;
  }
  if (header->e_ident[EI_OSABI] != ELFOSABI_SYSV ||
      header->e_ident[EI_ABIVERSION] != 0) {
    return Status::E_UNSUPP_ABI;
  }
  // TODO: Support regular executables?
  if (header->e_type != ET_DYN) {
    return Status::E_UNSUPP_TYPE;
  }
  // TODO: check e_machine
  // if (header->e_machine ...) {}
  entry = header->e_entry;
  prog_header_base = (const Elf64_Phdr*)(header->e_phoff + src);
  sec_header_base = (const Elf64_Shdr*)(header->e_shoff + src);
  return Status::SUCCESS;
}

Status ELFLoader::load_process_image(ProcAllocator& alloc) {
  // Let's do the thing!
  const unsigned max_sections = 16;
  [[maybe_unused]] // FORNOW
  const Elf64_Phdr* load_segments[max_sections] = {nullptr};
  unsigned num_load_segments = 0;
  Elf64_Addr min_vaddr = ULLONG_MAX;
  Elf64_Addr max_vaddr = 0;
  auto add_load_segment = [&](auto prog_header) {
    load_segments[num_load_segments++] = prog_header;
    if (prog_header->p_vaddr < min_vaddr) {
      min_vaddr = prog_header->p_vaddr;
    }
    if (prog_header->p_vaddr + prog_header->p_memsz > max_vaddr) {
      max_vaddr = prog_header->p_vaddr + prog_header->p_memsz;

    }
  };
  const char* interp = "/lib/ld64.so.1"; // Generic default

  debug::serial_printf("ELF: loading %d program segments\n", elf_header->e_phnum);
  for (unsigned i = 0; i < elf_header->e_phnum; ++i) {
    const Elf64_Phdr* prog_header = (const Elf64_Phdr*)
        (((uint8_t*)prog_header_base) + i*elf_header->e_phentsize);
    if (prog_header->p_type == PT_NULL) continue;
    else if (prog_header->p_type == PT_LOAD) {
      debug::serial_printf("ELF prog segment %d: PT_LOAD found\n", i);
      if (num_load_segments >= max_sections) {
        debug::serial_printf("Error: cannot load more than %u segments\n", max_sections);
        return Status::E_MAX_LOAD_SEGMENTS;
      }
      add_load_segment(prog_header);
    }
    else if (prog_header->p_type == PT_DYNAMIC) {
      debug::serial_printf("Warning: PT_DYNAMIC ignored\n");
    }
    else if (prog_header->p_type == PT_INTERP) {
      debug::serial_printf("ELF prog segment %d: PT_INTERP found\n", i);
      interp = (const char*)(prog_header->p_offset + src);
      debug::serial_printf("ELF wants to be run with interp %s\n", interp);
    }
    else if (prog_header->p_type == PT_NOTE) {
      debug::serial_printf("Warning: PT_NOTE ignored\n");
    }
    else if (prog_header->p_type == PT_SHLIB) {
      debug::serial_printf("Warning: PT_SHLIB ignored\n");
    }
    else if (prog_header->p_type == PT_PHDR) {
      debug::serial_printf("ELF prog segment %d: PT_PHDR found\n", i);
      if (num_load_segments > 0) {
        debug::serial_printf("Error: cannot have PT_PHDR after PT_LOAD segments\n");
        return Status::E_BAD_PTPHDR;
      }
      add_load_segment(prog_header);
    }
    else if (prog_header->p_type == PT_GNU_EH_FRAME ||
             prog_header->p_type == PT_GNU_STACK) {
      debug::serial_printf("Warning: PT_GNU_* extension ignored\n");
    }
    else {
      debug::serial_printf("Error: unsupported PHDR %u\n", prog_header->p_type);
      return Status::E_UNSUPP_PHDR;
    }
  }

  Elf64_Addr segment_start = round_down(min_vaddr, PAGE_SIZE);
  Elf64_Addr segment_end = round_up(max_vaddr, PAGE_SIZE);
  debug::serial_printf("Allocating (relocated) VM segments between (preferred) "
                       "%p and %p\n", (void*)min_vaddr, (void*)max_vaddr);
  [[maybe_unused]] // FORNOW
  void* proc_image = alloc.alloc_proc_segments(segment_end - segment_start);
  PANIC_NOT_IMPLEMENTED("ELF load");
}
