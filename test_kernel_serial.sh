#!/bin/bash

TARGET=$1
if [[ "x${TARGET}" == "x" ]]; then
    echo "Usage: $0 <target> [extra args]"
    exit 1
fi
qemu-system-x86_64 -no-reboot -serial stdio -cdrom bin/${TARGET}/gullfoss.iso ${@:2}
