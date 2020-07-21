#ifndef BLOCK_SOURCE_H
#define BLOCK_SOURCE_H

/**
 * Random-accessible source of data.
 */

#include <cstring>
#include <memory>
#include <stddef.h>
#include <stdint.h>

class BlockSource {
 public:
  virtual void read_block(size_t offset, size_t len, uint8_t* dest) = 0;
  virtual size_t size() const = 0;
};

class InMemorySource : public BlockSource {
 public:
  InMemorySource(const uint8_t* src, size_t size)
      : src(src), buffer_size(size) {}
  void read_block(size_t offset, size_t len, uint8_t* dest) {
    std::memcpy(dest, src + offset, len);
  }
  size_t size() const { return buffer_size; }
 private:
  const uint8_t* src;
  size_t buffer_size;
};


#endif
