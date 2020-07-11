#!/bin/bash

TARGET=$1
if [[ "x${TARGET}" == "x" ]]; then
    echo "Usage: $0 <target>"
    exit 1
fi
qemu-system-i386 -no-reboot -serial stdio -kernel bin/${TARGET}/kernel.bin
