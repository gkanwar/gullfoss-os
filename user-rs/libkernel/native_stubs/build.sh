#!/bin/bash

### FORNOW: Just build it manually
### TODO: why does this fail when using the cross compiler?
gcc stubs.c -shared -fPIC -o libkernel_stubs.so
