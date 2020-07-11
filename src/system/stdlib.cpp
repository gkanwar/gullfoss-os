/**
 * Reproduce some C stdlib functionality, as needed.
 */

#include "cstring"
#include <stdarg.h>
#include <stdint.h>

namespace std {

size_t strlen(const char* str) 
{
  size_t len = 0;
  while (str[len]) len++;
  return len;
}

void* memset(void* ptr, int value, size_t num) {
  uint8_t byte_value = (uint8_t) value;
  uint8_t* byte_ptr = (uint8_t*)ptr;
  for (; num > 0; num--, byte_ptr++) {
    *byte_ptr = byte_value;
  }
  return ptr;
}

struct format_spec {
  enum flag {
    LeftJustify = 1 << 0,
    ForceSign = 1 << 1,
    ForceSpace = 1 << 2,
    ForcePoint = 1 << 3,
    ZeroPad = 1 << 4
  };
  uint8_t flags;
  int16_t min_width; 
  int16_t precision;
  // TODO: length modifier
  char specifier;
  format_spec() : flags(0), min_width(-1), precision(-1), specifier('\0') {}
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
  // TODO: length modifier
  switch (*fmt) {
    case 'd':
    case 'i':
    case 'u':
    case 'x':
    case 'c':
    case 's':
    case 'p':
    case '%':
      spec.specifier = *fmt;
      ++fmt;
  }
  *pfmt = fmt;
  return spec;
}

static size_t unsigned_to_buf(unsigned arg, char* buf) {
  size_t len = 0;
  while (arg > 0) {
    buf[len] = '0' + arg % 10;
    arg /= 10;
    len++;
  }
  return len;
}
static size_t hex_to_buf(unsigned arg, char* buf) {
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

template<typename Writer>
static void write_fill(Writer putc, const format_spec& spec, uint16_t print_len) {
  char fill = spec.flags & format_spec::flag::ZeroPad ? '0' : ' ';
  for (int i = 0; i < spec.min_width - print_len; ++i) {
    putc(fill);
  }
}

template<typename Writer>
static void format_unsigned(Writer putc, const format_spec& spec, unsigned arg) {
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
static void format_int(Writer putc, const format_spec& spec, int arg) {
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
static void format_hex(Writer putc, const format_spec& spec, unsigned arg) {
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
static void format_arg(Writer putc, const char** pfmt, va_list& args) {
  const char* fmt = *pfmt;
  const char* fmt_spec = fmt+1;
  format_spec spec = parse_format_spec(&fmt_spec);
  // if supported format spec, eat the spec and output formatted arg(s)
  switch (spec.specifier) {
    case 'd':
    case 'i':
      format_int(putc, spec, va_arg(args, int));
      break;
    case 'u':
      format_unsigned(putc, spec, va_arg(args, unsigned));
      break;
    case 'x':
      format_hex(putc, spec, va_arg(args, unsigned));
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
