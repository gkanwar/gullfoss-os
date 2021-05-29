#include <cstring>
#include "test.h"
#include "heap_allocator.h"

void test_kernel_early_main() {
  // TODO
}

void test_kernel_stage1() {
  test::pretty_print_test("malloc(128)", [&]()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(128);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xff, 128);
    for (int i = 0; i < 128; ++i) {
      if (mem_chunk[i] != 0xff) {
        return false;
      }
    }
    return true;
  });
  test::pretty_print_test("malloc(8)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(8);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xab, 8);
    for (int i = 0; i < 8; ++i) {
      if (mem_chunk[i] != 0xab) {
        return false;
      }
    }
    return true;
  });
  test::pretty_print_test("malloc(16)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(16);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xcd, 16);
    for (int i = 0; i < 16; ++i) {
      if (mem_chunk[i] != 0xcd) {
        return false;
      }
    }
    return true;
  });
  test::pretty_print_test("malloc(32)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(32);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0x88, 32);
    for (int i = 0; i < 32; ++i) {
      if (mem_chunk[i] != 0x88) {
        return false;
      }
    }
    return true;
  });
}
