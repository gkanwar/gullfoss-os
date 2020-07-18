#include <cassert>
#include "input_events.h"

static InputEvents* inst;
InputEvents& InputEvents::get() {
  assert(inst);
  return *inst;
}

InputEvents::InputEvents() {
  assert(!inst); inst = this;
}
