#!/bin/bash

otf2bdf -r 72 -p 13 texgyrecursor-regular.otf |
    sed -e 's/AVERAGE_WIDTH.*/AVERAGE_WIDTH 80/' > texgyrecursor-regular.bdf
bdf2psf texgyrecursor-regular.bdf \
        /usr/share/bdf2psf/standard.equivalents \
        /usr/share/bdf2psf/ascii.set+/usr/share/bdf2psf/useful.set \
        512 texgyrecursor-regular.psf
