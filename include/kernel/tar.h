#ifndef TAR_H
#define TAR_H

#include <cstdlib>
#include <stddef.h>
#include <stdint.h>
#include <types.h>

struct tar_file_t {
  uint8_t* buffer; // nullptr indicates file not found
  lsize_t size;
};

/**
 * Small tar reader class, primarily used in parsing initrd.
 */
class Tarball {
 public:
  union FileHeader {
    struct {
      uint8_t name[100];
      uint64_t mode;
      uint64_t uid;
      uint64_t gid;
      uint8_t size[12]; // octal
      uint8_t mod_time[12]; // octal
      uint8_t checksum[8];
      uint8_t link_type;
      uint8_t link_name[100];
    } __attribute__((packed));
    uint8_t _pad[512];
  };
  Tarball(const void* buffer, lsize_t size) : buffer(buffer), size(size) {}
  // Return pointer to file matching name or nullptr
  tar_file_t find_file(const char* name);
 private:
  const void* buffer;
  lsize_t size;
};

#endif
