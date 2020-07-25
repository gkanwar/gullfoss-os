Need to set up a cross compiler
===============================
See the osdev page for more details: `https://wiki.osdev.org/GCC_Cross-Compiler`.

0. Install prereqs.
   `sudo apt install build-essential bison flex libgmp3-dev libmpc-dev libmpfr-dev texinfo`
1. Get GCC and binutils source via `https://ftp.gnu.org/gnu/binutils/` and `https://ftp.gnu.org/gnu/gcc/`.
2. Built into $HOME/opt/cross so you don't much up normal gcc.
   ```
   mkdir -p ${HOME}/opt/cross
   export PREFIX=${HOME}/opt/cross
   export TARGET=i686-elf
   export PATH=${PREFIX}/bin:${PATH}
   # these variables can get in the way of building gcc
   unset LD_LIBRARY_PATH LIBRARY_PATH CPATH C_INCLUDE_PATH \
         PKG_CONFIG_PATH CPLUS_INCLUDE_PATH INCLUDE
   ```
3. Build binutils
   ```
   cd ${HOME} && mkdir -p build-binutils
   cd build-binutils
   ../binutils-*.*/configure --target=${TARGET} --prefix=${PREFIX} --with-sysroot --disable-nls --disable-werror
   make && make install
   ```
4. Build gcc
   ```
   cd ${HOME} && mkdir -p build-gcc
   cd build-gcc
   ../gcc-*.*.*/configure --target=${TARGET} --prefix=${PREFIX} --disable-nls --enable-languages=c,c++ --without-headers
   make all-gcc
   make all-target-libgcc
   make install-gcc
   make install-target-libgcc
   ```

x64 cross compiler
==================
Everything pretty much works the same except the target triple is `x86_64-elf` instead. However, if global ctors are going to need linking to libgcc's `crtbegin.o` and `crtend.o` we should make sure libgcc is built to support long-mode relocation. We just modify the libgcc build as follows:
```
make all-target-libgcc CFLAGS_FOR_TARGET='-mcmodel=large -mno-red-zone'
```
