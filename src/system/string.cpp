#include "cstring"

#include <stdint.h>

namespace std {

size_t strlen(const char* str) 
{
  size_t len = 0;
  while (str[len]) len++;
  return len;
}

int strcmp(const char* s1, const char* s2) {
  size_t l1 = strlen(s1);
  size_t l2 = strlen(s2);
  if (l1 < l2)
    return strncmp(s1, s2, l2);
  else
    return strncmp(s1, s2, l1);
}

int strncmp(const char* s1, const char* s2, size_t num) {
  unsigned i = 0;
  for (; s1[i] && s2[i]; ++i) {
    char c1 = s1[i];
    char c2 = s2[i];
    if (c1 < c2) return -1;
    else if (c1 > c2) return 1;
    else if (i == num-1) return 0;
  }
  // one of the strings terminated
  if (s1[i] < s2[i]) {
    return -1;
  }
  else if (s1[i] == s2[i]) {
    return 0;
  }
  else {
    return 1;
  }
}

void* memset(void* ptr, int value, size_t num) {
  uint8_t byte_value = (uint8_t) value;
  uint8_t* byte_ptr = (uint8_t*)ptr;
  for (unsigned i = 0; i < num; ++i) {
    byte_ptr[i] = byte_value;
  }
  return ptr;
}

void* memcpy(void* dest, const void* src, size_t num) {
  uint8_t* byte_ptr = (uint8_t*)dest;
  for (unsigned i = 0; i < num; ++i) {
    byte_ptr[i] = ((uint8_t*)src)[i];
  }
  return dest;
}

}

// Make sure we have the C linkages as well
extern "C" {
  void* memset(void* ptr, int value, size_t num) {
    return std::memset(ptr, value, num);
  }
  void* memcpy(void* dest, const void* src, size_t num) {
    return std::memcpy(dest, src, num);
  }
}
