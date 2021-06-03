#include "clock.h"

#include "assert.h"
#include "kernel.h"

static PITClock* inst;

PITClock::PITClock() : ms_since_boot(0.0) {
  assert_make_inst(inst, this);
}

PITClock& PITClock::get() { return assert_get_inst(inst); }

double PITClock::get_ms() const {
  return ms_since_boot;
}

void PITClock::tick() {
  ms_since_boot += PIT_TICK_MS;
}
