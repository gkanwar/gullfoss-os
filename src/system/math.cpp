#include "cmath"

#include "stdint.h"

namespace std {

/// FIXME: This is truly a hack, should be replaced with real IEEE754 manipulations!
float floor(float arg) {
  return (float)(int64_t)arg;
}
double floor(double arg) {
  return (double)(int64_t)arg;
}
float ceil(float arg) {
  float out = (float)(int64_t)arg;
  return (out == arg) ? out + 1.0 : out;
}
double ceil(double arg) {
  double out = (double)(int64_t)arg;
  return (out == arg) ? out + 1.0 : out;
}

float trunc(float arg) {
  return (arg > 0) ? floor(arg) : ceil(arg);
}
double trunc(double arg) {
  return (arg > 0) ? floor(arg) : ceil(arg);
}

}
