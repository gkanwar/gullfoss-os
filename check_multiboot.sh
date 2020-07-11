#!/bin/bash
if grub-file --is-x86-multiboot $1; then
    echo "multiboot confirmed!"
else
    echo "file is not multiboot compatible"
fi
