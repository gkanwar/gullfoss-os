#ifndef BLOCK_SOURCE_H
#define BLOCK_SOURCE_H

/**
 * Random-accessible source of data.
 */

#include <cstring>
#include <memory>
#include <stddef.h>
#include <stdint.h>
#include <types.h>

class BlockSource {
 public:
  virtual void read_block(lsize_t offset, lsize_t len, uint8_t* dest) = 0;
  virtual lsize_t size() const = 0;
};

class InMemorySource : public BlockSource {
 public:
  InMemorySource(const uint8_t* src, lsize_t size)
      : src(src), buffer_size(size) {}
  void read_block(lsize_t offset, lsize_t len, uint8_t* dest) {
    std::memcpy(dest, src + offset, len);
  }
  lsize_t size() const { return buffer_size; }
 private:
  const uint8_t* src;
  lsize_t buffer_size;
};


#endif
