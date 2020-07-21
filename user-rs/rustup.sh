#!/bin/bash

# Need the nightly rustc to use some language features
rustup toolchain install nightly
rustup update nightly --force # get the very latest for now (7/17 build relevant)
rustup default nightly

# Need rust source to cross-compiler the core library
rustup component add rust-src

# Need a gcc cross compiler available at ../../opt/cross64
if [[ -d "../../opt/cross64" ]]; then
    echo "Found 64-bit cross compiler dir, we are good to go!"
else
    echo "Need 64-bit cross compiler (actually cross-linker)," \
         "please install in ../../opt/cross64"
    exit 1
fi
