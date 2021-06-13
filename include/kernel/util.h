#ifndef UTIL_H
#define UTIL_H

#include <types.h>

#define UNUSED_PARAM(x) (sizeof(&x))

namespace util {

template<typename T, typename U>
inline T get_bit(T flags, U bit) {
  unsigned ubit = static_cast<unsigned>(bit);
  return (flags >> ubit) & 0x1;
}
template<typename T, typename U>
inline void set_bit(T& flags, U bit) {
  unsigned ubit = static_cast<unsigned>(bit);
  flags |= (1 << ubit);
}
template<typename T, typename U>
inline void unset_bit(T& flags, U bit) {
  unsigned ubit = static_cast<unsigned>(bit);
  flags &= ~(1 << ubit);
}

template <typename T, typename U> inline
T round_up(T addr, U size) {
  // WARNING: this cast is not portable
  if ((u64)addr % size != 0) {
    addr += size - (u64)addr % size;
  }
  return addr;
}
template <typename T, typename U> inline
T round_down(T addr, U size) {
  // WARNING: this cast is not portable
  return addr - (u64)addr % size;
}

}

#endif
