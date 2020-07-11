#!/bin/bash

# NOTE: Use alt-2 to get to QEMU monitor and quit
TARGET=$1
if [[ "x${TARGET}" == "x" ]]; then
    echo "Usage: $0 <target>"
    exit 1
fi
qemu-system-i386 -no-reboot -curses -kernel bin/${TARGET}/kernel.bin
