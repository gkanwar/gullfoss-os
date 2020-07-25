#ifndef PROG_INT_TIMER_H
#define PROG_INT_TIMER_H

#include <types.h>
#include "io.h"

#define PIT_CHAN0 0x40
#define PIT_CHAN1 0x41
#define PIT_CHAN2 0x42

namespace pit {

inline void set_channel0_divisor(u16 div) {
  io::out8(PIT_CHAN0, (u8)(div & 0xff));
  io::out8(PIT_CHAN0, (u8)((div >> 8) & 0xff));
};

}

#endif
