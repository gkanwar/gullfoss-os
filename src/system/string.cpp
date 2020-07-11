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
  for (char c1 = *s1, c2 = *s2; c1 && c2;
       ++s1, ++s2, c1 = *s1, c2 = *s2) {
    if (c1 < c2) return -1;
    else if (c1 > c2) return 1;
  }
  // one of the strings terminated
  if (*s1 < *s2) {
    return -1;
  }
  else if (*s1 == *s2) {
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
