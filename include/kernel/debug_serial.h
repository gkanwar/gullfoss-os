#ifndef DEBUG_SERIAL_H
#define DEBUG_SERIAL_H

/**
 * We need serial debugging as early as possible, even before the full kernel
 * driver system is booted. So here we provide a very raw interface to COM1
 * serial for this purpose.
 */

namespace debug {

void init_serial();
void serial_putc(char);
void serial_printf(const char* fmt, ...);

}

#endif
