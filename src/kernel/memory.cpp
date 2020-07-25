#include <new>
#include "assert.h"
#include "heap_allocator.h"

void* operator new(size_t size) {
  return HeapAllocator::get().malloc(size);
}

void* operator new[](size_t size) {
  return HeapAllocator::get().malloc(size);
}

void operator delete(void*) {
  // TODO!
  // panic("delete not implemented yet");
}
void operator delete(void*, size_t) {
  // TODO!
  // panic("delete not implemented yet");
}

void operator delete[](void*) {
  // TODO!
  // panic("delete not implemented yet");
}
void operator delete[](void*, size_t) {
  // TODO!
  // panic("delete not implemented yet");
}
