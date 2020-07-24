#include "debug_serial.h"
#include "tar.h"

#include <cstring>

using FileHeader = Tarball::FileHeader;

static const FileHeader* step_forward(const FileHeader* header, void* end_buffer) {
  lsize_t size = std::strtol((const char*)header->size, nullptr, 8);
  lsize_t next_ptr = (lsize_t)(header+1);
  next_ptr += size;
  if (next_ptr % 512 != 0) { // pad to 512 boundary
    next_ptr += 512 - (next_ptr % 512);
  }
  header = (const FileHeader*)next_ptr;
  if (header >= end_buffer) {
    return nullptr;
  }
  return header;
}

tar_file_t Tarball::find_file(const char* name) {
  debug::serial_printf("Finding file...\n");
  const FileHeader* header = (const FileHeader*)buffer;
  void* end_buffer = (void*)((uint8_t*)buffer + size);
  do {
    if (std::strcmp((const char*)header->name, name) == 0) {
      return {
        .buffer = (uint8_t*)(header + 1), // data is just past (padded) header
        .size = (lsize_t)std::strtol((const char*)header->size, nullptr, 8)
      };
    }
    header = step_forward(header, end_buffer);
  } while(header);
  return { .buffer = nullptr, .size = 0 };
}
