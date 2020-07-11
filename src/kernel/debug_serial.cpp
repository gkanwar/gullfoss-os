#include "debug_serial.h"

#define COM1_PORT 0x3f8
#define BAUD_38400_DIV 3
#define DLAB_BIT 1 << 7
#define PROTOCOL_BITS 0b00111111
#define LINE_STATUS_THRE 1 << 5

#include <stdarg.h>
#include <stdint.h>
#include <cstdio>
#include "io.h"

namespace debug {

void init_serial() {
  io::out8(COM1_PORT + 1, 0x00); // clear interrupts
  
  io::set_bits8(COM1_PORT + 3, DLAB_BIT);
  io::out8(COM1_PORT + 0, BAUD_38400_DIV & 0xff); // baud
  io::out8(COM1_PORT + 1, (BAUD_38400_DIV >> 8) & 0xff);
  io::write_bits8(COM1_PORT + 3, 0x03, PROTOCOL_BITS); // 8N1
  io::unset_bits8(COM1_PORT + 3, DLAB_BIT);

  io::out8(COM1_PORT + 2, 0xC7); // enable fifos with max queue size
  io::out8(COM1_PORT + 4, 0x0B); // ready, request to send, enable CPU interrupt
}

static uint8_t get_line_status() {
  return io::in8(COM1_PORT + 5);
}

void serial_putc(char c) {
  while (!(get_line_status() & LINE_STATUS_THRE));
  io::out8(COM1_PORT, c);
}

void serial_printf(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  std::vfprintf(serial_putc, fmt, args);
  va_end(args);
}

}
