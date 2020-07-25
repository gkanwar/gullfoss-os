#include <cstring>
#include "test.h"
#include "heap_allocator.h"

void test_kernel_early_main() {
  // TODO
}

void test_kernel_main() {
  test::pretty_print_test("malloc(128)", [&]()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(128);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xff, 128);
    uint8_t byte = mem_chunk[77];
    return byte == 0xff;
  });
  test::pretty_print_test("malloc(8)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(8);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xab, 8);
    uint8_t byte = mem_chunk[5];
    return byte == 0xab;
  });
  test::pretty_print_test("malloc(16)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(16);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0xcd, 16);
    uint8_t byte = mem_chunk[13];
    return byte == 0xcd;
  });
  test::pretty_print_test("malloc(32)", []()->bool {
    uint8_t* mem_chunk = (uint8_t*)HeapAllocator::get().malloc(32);
    if (!mem_chunk) return false;
    std::memset((void*)mem_chunk, 0x88, 32);
    uint8_t byte = mem_chunk[27];
    return byte == 0x88;
  });
}
