#!/bin/bash
rustup toolchain install nightly
rustup update nightly --force # get the very latest for now (7/17 build relevant)
rustup default nightly
rustup component add rust-src

