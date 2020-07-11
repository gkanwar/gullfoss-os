#ifndef IO_H
#define IO_H

/** Read and write from CPU ports. */

#include <stdint.h>

namespace io {

inline void out8(uint16_t port, uint8_t val) {
  asm volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}
inline void out16(uint16_t port, uint16_t val) {
  asm volatile ("outw %0, %1" : : "a"(val), "Nd"(port));
}
inline void out32(uint16_t port, uint32_t val) {
  asm volatile ("outl %0, %1" : : "a"(val), "Nd"(port));
}
inline uint8_t in8(uint16_t port) {
  uint8_t val;
  asm volatile ("inb %1, %0" : "=a"(val) : "Nd"(port));
  return val;
}
inline uint16_t in16(uint16_t port) {
  uint16_t val;
  asm volatile ("inw %1, %0" : "=a"(val) : "Nd"(port));
  return val;
}
inline uint32_t in32(uint16_t port) {
  uint32_t val;
  asm volatile ("inl %1, %0" : "=a"(val) : "Nd"(port));
  return val;
}

inline void set_bits8(uint16_t port, uint8_t mask) {
  uint8_t val = in8(port);
  val |= mask;
  out8(port, val);
}
inline void unset_bits8(uint16_t port, uint8_t mask) {
  uint8_t val = in8(port);
  val &= ~mask;
  out8(port, val);
}
inline void write_bits8(uint16_t port, uint8_t new_val, uint8_t mask) {
  uint8_t val = in8(port);
  val = (val & ~mask) | (new_val & mask);
  out8(port, val);
}

inline void io_wait() {
  // wait 1 IO cycle ~ 1usec, apparently regardless of proc speed
  out8(0x80, 0); 
}

}

#endif
