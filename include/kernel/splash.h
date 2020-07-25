#ifndef SPLASH_H
#define SPLASH_H

#include "bootboot.h"
#include "tar.h"

void show_splash(const BOOTBOOT& info, pixel_t* framebuffer, Tarball& initrd);

#endif
