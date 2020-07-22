#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>

// GCC uses 32-bit sizeof/size_t on 64-bit systems. We want to work with large
// chunks of (virtual) memory, so need a bigger size_t.
typedef uint64_t lsize_t;

// Nicer aliases
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef unsigned int uint;

#endif