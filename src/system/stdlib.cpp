/**
 * Reproduce some C stdlib functionality, as needed.
 */

#include "cmath"
#include "cstring"
#include <stdarg.h>
#include <stdint.h>

namespace std {

// Not POSIX compliant
static bool isspace(char c) {
  return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}
// Convert to value in base, or -1 indicating invalid digit
static long int convert_digit(char c, int base) {
  if (c < '0') return -1;
  if (c <= '9') { // check 0-9 digits
    if (c < ('0' + base)) {
      return c - '0';
    }
    return -1;
  }
  else if (c < 'A') return -1;
  else if (c <= 'Z') { // check A-Z digits
    if (c < ('A' + base-10)) {
      return c - 'A' + 10;
    }
    return -1;
  }
  else if (c < 'a') return -1;
  else if (c <= 'z') { // check a-z digits
    if (c < ('a' + base-10)) {
      return c - 'a' + 10;
    }
    return -1;
  }
  else return -1;
}

long int strtol(const char* str, const char** endptr, int base) {
  long int sign = 1;
  while (isspace(*str)) { ++str; }
  if (*str == '+') { ++str; }
  if (*str == '-') { sign = -1; ++str; }
  // special case: strip optional 0x or 0X if base is 16
  if (base == 16 && *str == '0' && (*(str+1) == 'x' || *(str+1) == 'X')) {
    str += 2;
  }
  // special case: detect base if zero
  if (base == 0) {
    if (*str != '0') {
      base = 10;
    }
    else if (*(str+1) == 'x' || *(str+1) == 'X') {
      base = 16;
      str += 2;
    }
    else {
      base = 8;
      str += 1;
    }
  }
  long int value = 0;
  char next = *str;
  for (long int digit = convert_digit(next, base); digit >= 0;
       ++str, next = *str, digit = convert_digit(next, base)) {
    value *= base;
    value += digit;
  }
  if (endptr) {
    *endptr = str;
  }
  return sign * value;
}

struct format_spec {
  enum flag {
    LeftJustify = 1 << 0,
    ForceSign = 1 << 1,
    ForceSpace = 1 << 2,
    ForcePoint = 1 << 3,
    ZeroPad = 1 << 4,
  };
  uint8_t flags;
  int16_t min_width; 
  int16_t precision;
  enum length_mod {
    None,
    Long,
    LongLong,
  };
  length_mod length_modifier;
  char specifier;
  format_spec() : flags(0), min_width(-1), precision(-1),
                  length_modifier(length_mod::None), specifier('\0') {}
};

static format_spec parse_format_spec(const char** pfmt) {
  format_spec spec;
  const char* fmt = *pfmt;
  for (char next = *fmt; next != '\0'; ++fmt, next=*fmt) {
    switch(next) {
      case '-': {
        spec.flags |= format_spec::flag::LeftJustify;
        continue;
      }
      case '+': {
        spec.flags |= format_spec::flag::ForceSign;
        continue;
      }
      case ' ': {
        spec.flags |= format_spec::flag::ForceSpace;
        continue;
      }
      case '#': {
        spec.flags |= format_spec::flag::ForcePoint;
        continue;
      }
      case '0': {
        spec.flags |= format_spec::flag::ZeroPad;
        continue;
      }
    }
    break;
  }
  for (char next = *fmt; next != '\0'; ++fmt, next=*fmt) {
    if (next == '*') { // TODO: support this
      ++fmt;
      break;
    }
    if (next >= '0' && next <= '9') {
      if (spec.min_width < 0) {
        spec.min_width = next - '0';
      }
      else {
        spec.min_width = spec.min_width*10 + next - '0';
      }
    }
    else {
      break;
    }
  }
  // restrict fill width to sensible range
  if (spec.min_width > 1024) {
    spec.min_width = 1024;
  }
  if (*fmt == '.') {
    spec.precision = 0;
    ++fmt;
    for (char next = *fmt; next != '\0'; ++fmt, next=*fmt) {
      if (next == '*') { // TODO: support this
        ++fmt;
        break;
      }
      if (next >= '0' && next <= '9') {
        if (spec.precision < 0) {
          spec.precision = next - '0';
        }
        else {
          spec.precision = spec.precision*10 + next - '0';
        }
      }
      else {
        break;
      }
    }
  }
  // TODO: other length modifiers
  if (*fmt == 'l') {
    if (*(fmt+1) == 'l') {
      fmt += 2;
      spec.length_modifier = format_spec::length_mod::LongLong;
    }
    else {
      fmt++;
      spec.length_modifier = format_spec::length_mod::Long;
    }
  }
  switch (*fmt) {
    case 'd':
    case 'i':
    case 'u':
    case 'x':
    case 'c':
    case 's':
    case 'p':
    case 'f':
    case '%':
      spec.specifier = *fmt;
      ++fmt;
  }
  *pfmt = fmt;
  // set spec defaults
  if (spec.precision < 0) {
    spec.precision = 6;
  }
  return spec;
}

static size_t unsigned_to_buf(long long unsigned arg, char* buf) {
  size_t len = 0;
  while (arg > 0) {
    buf[len] = '0' + arg % 10;
    arg /= 10;
    len++;
  }
  return len;
}
static size_t hex_to_buf(long long unsigned arg, char* buf) {
  size_t len = 0;
  while (arg > 0) {
    if (arg % 16 < 10) {
      buf[len] = '0' + arg % 16;
    }
    else {
      buf[len] = 'a' + (arg % 16) - 10;
    }
    arg /= 16;
    len++;
  }
  return len;
}
// TODO: really want pair, but don't have it yet
struct WholeFracSizes {
  size_t whole_len;
  size_t frac_len;
};
static WholeFracSizes float_to_buf(
    double arg, char* whole_buf, char* frac_buf, size_t frac_max) {
  double whole = trunc(arg);
  double frac = (arg > 0) ? arg - whole : whole - arg;
  size_t whole_len = 0;
  size_t frac_len = 0;
  while (whole >= 1.0) {
    double whole_shift = whole / 10.0;
    // FIXME: what if inaccurate FP math?
    unsigned digit = (unsigned)(10.0*(whole_shift - trunc(whole_shift)));
    whole_buf[whole_len] = '0' + digit % 10;
    whole_len++;
    whole = whole_shift;
  }
  while (frac > 0 && frac_len < frac_max) {
    double frac_shift = frac * 10.0;
    unsigned digit = trunc(frac_shift);
    frac_buf[frac_len] = '0' + digit % 10;
    frac_len++;
    frac = frac_shift;
  }
  return { .whole_len = whole_len, .frac_len = frac_len };
}

template<typename Writer>
static void write_fill(Writer putc, const format_spec& spec, uint16_t print_len) {
  char fill = spec.flags & format_spec::flag::ZeroPad ? '0' : ' ';
  for (int i = 0; i < spec.min_width - print_len; ++i) {
    putc(fill);
  }
}


template<typename Writer>
static void format_unsigned(Writer putc, const format_spec& spec, uint32_t arg) {
  format_unsigned(putc, spec, (uint64_t)arg);
}
template<typename Writer>
static void format_unsigned(Writer putc, const format_spec& spec, uint64_t arg) {
  char buf[32];
  char extra = 0;
  if (arg == 0) {
    extra = '0';
  }
  size_t len = unsigned_to_buf(arg, buf);
  if (extra) {
    putc(extra);
  }
  write_fill(putc, spec, extra ? len+1 : len);
  for (size_t i = len; i > 0; --i) {
    putc(buf[i-1]);
  }
}

template<typename Writer>
static void format_int(Writer putc, const format_spec& spec, int32_t arg) {
  format_int(putc, spec, (int64_t)arg);
}
template<typename Writer>
static void format_int(Writer putc, const format_spec& spec, int64_t arg) {
  char buf[32];
  char extra = 0;
  if (arg == 0) {
    extra = '0';
  }
  else if (arg < 0) {
    extra = '-';
    arg *= -1;
  }
  unsigned uarg = (unsigned)arg;
  size_t len = unsigned_to_buf(uarg, buf);
  if (extra) {
    putc(extra);
  }
  write_fill(putc, spec, extra ? len+1 : len);
  for (size_t i = len; i > 0; --i) {
    putc(buf[i-1]);
  }
}

template<typename Writer>
static void format_hex(Writer putc, const format_spec& spec, uint32_t arg) {
  format_hex(putc, spec, (uint64_t)arg);
}
template<typename Writer>
static void format_hex(Writer putc, const format_spec& spec, uint64_t arg) {
  char buf[32];
  char extra = 0;
  if (arg == 0) {
    extra = '0';
  }
  size_t len = hex_to_buf(arg, buf);
  if (extra) {
    putc(extra);
  }
  write_fill(putc, spec, extra ? len+1 : len);
  for (size_t i = len; i > 0; --i) {
    putc(buf[i-1]);
  }
}

template<typename Writer>
static void format_float(Writer putc, const format_spec& spec, double arg) {
  char whole_buf[32];
  char frac_buf[32];
  WholeFracSizes len_parts = float_to_buf(arg, whole_buf, frac_buf, sizeof(frac_buf));
  size_t whole_len = len_parts.whole_len;
  size_t frac_len = len_parts.frac_len;
  bool write_decimal = (
      spec.precision > 0 ||
      (spec.flags & format_spec::flag::ForcePoint) != 0 );
  size_t write_len = spec.precision + whole_len;
  if (write_decimal) write_len++;
  write_fill(putc, spec, write_len);
  for (size_t i = whole_len; i > 0; --i) {
    putc(whole_buf[i-1]);
  }
  if (write_decimal) {
    putc('.');
  }
  for (size_t i = 0; i < (size_t)spec.precision; ++i) {
    if (i >= frac_len) {
      putc('0');
    }
    else {
      putc(frac_buf[i]);
    }
  }
}

template<typename Writer>
static void format_string(Writer putc, const format_spec&, const char* str) {
  for (char c = *str; c != '\0'; ++str, c = *str) {
    putc(c);
  }
}

template<typename Writer>
static void format_arg(Writer putc, const char** pfmt, va_list& args) {
  const char* fmt = *pfmt;
  const char* fmt_spec = fmt+1;
  format_spec spec = parse_format_spec(&fmt_spec);
  // if supported format spec, eat the spec and output formatted arg(s)
  switch (spec.specifier) {
    case 'd':
    case 'i':
      if (spec.length_modifier == format_spec::length_mod::None ||
          spec.length_modifier == format_spec::length_mod::Long) {
        format_int(putc, spec, va_arg(args, int32_t));
      }
      else if (spec.length_modifier == format_spec::length_mod::LongLong) {
        format_int(putc, spec, va_arg(args, int64_t));
      }
      else { // TODO assert not reached
      }        
      break;
    case 'u':
      if (spec.length_modifier == format_spec::length_mod::None ||
          spec.length_modifier == format_spec::length_mod::Long) {
        format_unsigned(putc, spec, va_arg(args, uint32_t));
      }
      else if (spec.length_modifier == format_spec::length_mod::LongLong) {
        format_unsigned(putc, spec, va_arg(args, uint64_t));
      }
      else { // TODO assert not reached
      }
      break;
    case 'x':
      if (spec.length_modifier == format_spec::length_mod::None ||
          spec.length_modifier == format_spec::length_mod::Long) {
        format_hex(putc, spec, va_arg(args, uint32_t));
      }
      else if (spec.length_modifier == format_spec::length_mod::LongLong) {
        format_hex(putc, spec, va_arg(args, uint64_t));
      }
      else { // TODO assert not reached
      }
      break;
    case 'p':
      spec.flags |= format_spec::flag::ZeroPad;
      spec.min_width = sizeof(void*)*2; // 2 hex chars per byte
      #ifdef __LP64__
      format_hex(putc, spec, (uint64_t)va_arg(args, long long unsigned));
      #else
      format_hex(putc, spec, (uint32_t)va_arg(args, uint32_t));
      #endif
      break;
    case 'f':
      format_float(putc, spec, va_arg(args, double));
      break;
    case 's':
      format_string(putc, spec, va_arg(args, const char*));
      break;
    case 'c':
      putc((char)va_arg(args, int)); // char promoted to int in va_arg
      break;
    default: // unknown format spec, just print '%' and move to next char
      putc(*fmt);
      *pfmt += 1;
      return;
  }
  *pfmt = fmt_spec;
}

size_t vfprintf(void putc(char), const char* fmt, va_list& args) {
  const char* s = fmt;
  size_t tot = 0;
  auto counting_putc = [&](char c) {
    putc(c);
    tot++;
  };
  for (char next = *s; next != '\0'; next = *s) {
    if (next != '%') {
      counting_putc(next);
      s++;
      continue;
    }
    format_arg(counting_putc, &s, args);
  }
  return tot;
}

}
