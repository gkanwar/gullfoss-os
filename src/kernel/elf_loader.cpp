#include <climits>
#include <cstring>
#include "assert.h"
#include "debug_serial.h"
#include "elf_loader.h"
#include "kernel.h"
#include "syscalls.h"
#include "util.h"
#include "virt_mem_allocator.h"

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
#define ELF64_R_SYM(info) ((info)>>32)
#define ELF64_R_TYPE(info) ((Elf64_Word)(info))

enum Elf_x86_64_RelocType {
  R_X86_64_NONE = 0,
  R_X86_64_64 = 1,
  R_X86_64_PC32 = 2,
  R_X86_64_GOT32 = 3,
  R_X86_64_PLT32 = 4,
  R_X86_64_COPY = 5,
  R_X86_64_GLOB_DAT = 6,
  R_X86_64_JUMP_SLOT = 7,
  // ...
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

struct Elf64_Dyn {
  Elf64_Xword d_tag;
  union {
    Elf64_Xword d_val;
    Elf64_Addr d_ptr;
  } d_un;
};
enum ElfDynTag {
  DT_NULL = 0,
  DT_NEEDED = 1,
  DT_PLTRELSZ = 2,
  DT_PLTGOT = 3,
  DT_HASH = 4,
  DT_STRTAB = 5,
  DT_SYMTAB = 6,
  // ...
  DT_JMPREL = 23,
};

ELFLoader::ELFLoader(const uint8_t* src) : src(src), symbol_map(new EmptySymbolMap) {}
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
    if (num_load_segments >= max_sections) {
      debug::serial_printf("Error: cannot load more than %u segments\n", max_sections);
      return Status::E_MAX_LOAD_SEGMENTS;
    }
    debug::serial_printf(
        "Adding loadable [%p, %p]\n", prog_header->p_vaddr,
        prog_header->p_vaddr + prog_header->p_memsz);
    load_segments[num_load_segments++] = prog_header;
    if (prog_header->p_vaddr < min_vaddr) {
      min_vaddr = prog_header->p_vaddr;
    }
    if (prog_header->p_vaddr + prog_header->p_memsz > max_vaddr) {
      max_vaddr = prog_header->p_vaddr + prog_header->p_memsz;
    }
    return Status::SUCCESS;
  };
  
  debug::serial_printf("ELF: loading %d program segments\n", elf_header->e_phnum);
  for (unsigned i = 0; i < elf_header->e_phnum; ++i) {
    Status status = Status::SUCCESS;
    const Elf64_Phdr* prog_header = (const Elf64_Phdr*)
        (((uint8_t*)prog_header_base) + i*elf_header->e_phentsize);
    
    if (prog_header->p_type == PT_PHDR) {
      debug::serial_printf("ELF prog segment %d: PT_PHDR found\n", i);
      if (num_load_segments > 0) {
        debug::serial_printf("Error: cannot have PT_PHDR after PT_LOAD segments\n");
        status = Status::E_BAD_PTPHDR;
      }
      else {
        status = add_load_segment(prog_header);
      }
    }
    else if (prog_header->p_type == PT_LOAD) {
      debug::serial_printf("ELF prog segment %d: PT_LOAD/PT_DYNAMIC found\n", i);
      status = add_load_segment(prog_header);
    }
    else if (prog_header->p_type == PT_NULL || prog_header->p_type == PT_INTERP ||
             prog_header->p_type == PT_NOTE || prog_header->p_type == PT_SHLIB ||
             prog_header->p_type == PT_DYNAMIC) {
      debug::serial_printf("ELF prog segment %d: non-loadable\n", i);
    }
    else if (prog_header->p_type == PT_GNU_EH_FRAME ||
             prog_header->p_type == PT_GNU_STACK) {
      debug::serial_printf("Warning: PT_GNU_* extension ignored\n");
    }
    else {
      debug::serial_printf("Error: unsupported PHDR %u\n", prog_header->p_type);
      status = Status::E_UNSUPP_PHDR;
    }
    
    if (status != Status::SUCCESS)
      return status;
  }

  // Find dynamic segment and info
  interp = "/lib/ld64.so.1"; // Generic default, if none specified
  for (unsigned i = 0; i < elf_header->e_phnum; ++i) {
    const Elf64_Phdr* prog_header = (const Elf64_Phdr*)
        (((uint8_t*)prog_header_base) + i*elf_header->e_phentsize);
    if (prog_header->p_type == PT_DYNAMIC) {
      dynamic = prog_header->p_vaddr;
      dynamic_len = prog_header->p_memsz;
    }
    else if (prog_header->p_type == PT_INTERP) {
      interp = (const char*)(prog_header->p_offset + src);
      debug::serial_printf("ELF wants to be run with interp %s\n", interp);
    }
  }

  Elf64_Addr segment_start = util::round_down(min_vaddr, PAGE_SIZE);
  Elf64_Addr segment_end = util::round_up(max_vaddr, PAGE_SIZE);
  debug::serial_printf("Allocating (relocated) VM segments between (preferred) "
                       "%p and %p\n", (void*)min_vaddr, (void*)max_vaddr);
  void* proc_image = alloc.reserve_proc_segments(segment_end - segment_start);
  // NOTE: base_addr should be considered to live in a (mod 2^64 space). We will
  // only do arithmetic with other addresses mod 2^64, so storing a negative
  // base addr using an unsigned `Elf64_Off` is okay.
  base_addr = (Elf64_Off)proc_image - segment_start;
  for (unsigned i = 0; i < num_load_segments; ++i) {
    const Elf64_Phdr* segment = load_segments[i];
    void* base = (void*)(base_addr + segment->p_vaddr);
    // Apparently PT_PHDR is canonically assumed to be "covered" by the first
    // PT_LOAD segment, so don't map it explicitly.
    if (segment->p_type == PT_PHDR) continue;
    uint8_t map_flags = 0;
    if (segment->p_flags & ElfSegmentAttrib::PF_X) {
      util::set_bit(map_flags, MapFlag::Executable);
    }
    if (segment->p_flags & ElfSegmentAttrib::PF_W) {
      util::set_bit(map_flags, MapFlag::Writeable);
    }
    if (segment->p_flags & ElfSegmentAttrib::PF_R) {
      util::set_bit(map_flags, MapFlag::UserReadable);
    }
    debug::serial_printf("Mapping segment %p [%llu]\n", base, segment->p_memsz);
    void* map_base = util::round_down(base, PAGE_SIZE);
    lsize_t map_size = util::round_up(
        segment->p_memsz + (Elf64_Xword)base - (Elf64_Xword)map_base, PAGE_SIZE);
    alloc.map_segment(map_base, map_size, map_flags);
  }
  debug::serial_printf("Map'd full proc image\n");
  for (unsigned i = 0; i < num_load_segments; ++i) {
    const Elf64_Phdr* segment = load_segments[i];
    void* base = (void*)(base_addr + segment->p_vaddr);
    const uint8_t* data = src + segment->p_offset;
    // NOTE: p_filesz may be shorter than p_memsz and base may be past map_base,
    // leaving uninitialized data at the start/end of the segment (intentionally)
    std::memcpy(base, data, segment->p_filesz);
  }
  debug::serial_printf("Copied full proc image\n");

  return Status::SUCCESS;
}

Status ELFLoader::load_dylib([[maybe_unused]] const char* name) {
  assert(std::strcmp(name, "libkernel_stubs.so") == 0,
         "we only support baked-in kernel dylib");
  // TODO: better syscall registration system
  symbol_map = unique_ptr<SymbolMap>(new CompositeSymbolMap(
      std::move(symbol_map), "get_framebuffer", (void*)get_framebuffer));
  symbol_map = unique_ptr<SymbolMap>(new CompositeSymbolMap(
      std::move(symbol_map), "spawn", (void*)spawn));
  symbol_map = unique_ptr<SymbolMap>(new CompositeSymbolMap(
      std::move(symbol_map), "yield", (void*)yield));
  symbol_map = unique_ptr<SymbolMap>(new CompositeSymbolMap(
      std::move(symbol_map), "exit", (void*)exit));
  return Status::SUCCESS;
}

Status ELFLoader::dynamic_link() {
  if (dynamic_len % sizeof(Elf64_Dyn) != 0) {
    debug::serial_printf("PT_DYNAMIC not a multiple of sizeof(Elf64_Dyn)");
    return Status::E_BAD_PTDYN;
  }
  uint dyn_count = dynamic_len / sizeof(Elf64_Dyn);
  const Elf64_Dyn* dyn_array = (const Elf64_Dyn*)(dynamic + base_addr);
  Elf64_Addr sym_tab_addr = (Elf64_Addr)nullptr;
  Elf64_Xword sym_tab_count = 0;
  const Elf64_Sym* sym_tab = nullptr;
  const char* str_tab = nullptr;
  Elf64_Addr rela_plt_addr = (Elf64_Addr)nullptr;
  Elf64_Xword rela_plt_count = 0;
  const Elf64_Rela* rela_plt = nullptr;
  constexpr uint max_loaded_libs = 8;
  uint num_loaded_libs = 0;
  uint needed_libs[max_loaded_libs];
  for (uint i = 0; i < dyn_count; ++i) {
    if (dyn_array[i].d_tag == DT_SYMTAB) {
      sym_tab_addr = dyn_array[i].d_un.d_ptr;
      sym_tab = (const Elf64_Sym*)(sym_tab_addr + base_addr);
    }
    else if (dyn_array[i].d_tag == DT_STRTAB) {
      str_tab = (const char*)(dyn_array[i].d_un.d_ptr + base_addr);
    }
    else if (dyn_array[i].d_tag == DT_NEEDED) {
      if (num_loaded_libs >= max_loaded_libs) {
        debug::serial_printf("ELF loads too many dylibs\n");
        return Status::E_DYNLINK_FAIL;
      }
      needed_libs[num_loaded_libs++] = dyn_array[i].d_un.d_val;
    }
    else if (dyn_array[i].d_tag == DT_JMPREL) {
      rela_plt_addr = dyn_array[i].d_un.d_ptr;
      rela_plt = (const Elf64_Rela*)(rela_plt_addr + base_addr);
    }
  }
  // TODO: is there a better way to match section info to section pointers?
  for (uint i = 0; i < elf_header->e_shnum; ++i) {
    const Elf64_Shdr* sec_header = sec_header_base + i;
    if (sec_header->sh_addr == sym_tab_addr) {
      sym_tab_count = sec_header->sh_size / sec_header->sh_entsize;
    }
    else if (sec_header->sh_addr == rela_plt_addr) {
      rela_plt_count = sec_header->sh_size / sec_header->sh_entsize;
    }
  }
  debug::serial_printf("Found .rela.plt with %d entries\n", rela_plt_count);

  // load dylibs, adding symbols to symbol_map
  for (uint i = 0; i < num_loaded_libs; ++i) {
    auto status = load_dylib(&str_tab[needed_libs[i]]);
    if (status != Status::SUCCESS) return status;
  }

  // resolve undefined symbols
  if (!sym_tab) {
    debug::serial_printf("Error: DT_SYMTAB required\n");
    return Status::E_DYNLINK_FAIL;
  }
  if (!rela_plt) {
    debug::serial_printf("Error: DT_JMPREL required\n");
    return Status::E_DYNLINK_FAIL;
  }
  for (uint i = 0; i < sym_tab_count; ++i) {
    if (i == 0) continue; // first symbol is null
    const Elf64_Sym& sym = sym_tab[i];
    if (sym.st_shndx != SHN_UNDEF) continue;
    const char* symbol_name = &str_tab[sym.st_name];
    void* ptr = symbol_map->resolve_symbol(symbol_name);
    if (!ptr) {
      debug::serial_printf("Unresolved sym %s during dynamic linking\n", symbol_name);
      return Status::E_DYNLINK_FAIL;
    }
  }
  for (uint i = 0; i < rela_plt_count; ++i) {
    const Elf64_Rela& rela = rela_plt[i];
    const char* symbol_name = &str_tab[sym_tab[ELF64_R_SYM(rela.r_info)].st_name];
    void* ptr = symbol_map->resolve_symbol(symbol_name);
    if (!ptr) {
      debug::serial_printf("Unresolved sym %s during dynamic linking relocs\n", symbol_name);
      return Status::E_DYNLINK_FAIL;
    }
    if (ELF64_R_TYPE(rela.r_info) != R_X86_64_JUMP_SLOT) {
      debug::serial_printf("Bad rela.plt reloc\n");
      return Status::E_DYNLINK_FAIL;
    }
    void** reloc = (void**)(rela.r_offset + base_addr);
    *reloc = ptr;
    debug::serial_printf("Placed reloc addr %p into %p\n", ptr, reloc);
  }

  return Status::SUCCESS;
}

void* ELFLoader::get_entry() const {
  return (void*)(entry + base_addr);
}


CompositeSymbolMap::CompositeSymbolMap(
    unique_ptr<SymbolMap> inner_map, const char* symbol, void* ptr)
    : inner_map(std::move(inner_map)), symbol(symbol), ptr(ptr) {}

void* CompositeSymbolMap::resolve_symbol(const char* name) const {
  if (std::strcmp(name, symbol) == 0)
    return ptr;
  else
    return inner_map->resolve_symbol(name);
}
