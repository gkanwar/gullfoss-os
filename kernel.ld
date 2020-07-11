/* Custom linker script to combine bootstrap and kernel */
_bootboot_mmio = 0xfffffffff8000000;
_bootboot_fb = 0xfffffffffc000000;
BOOTBOOT_START = 0xffffffffffe00000;
PHDRS {
      boot PT_LOAD;
}
ENTRY(_start)
SECTIONS {
         /* conventional memory load location for a kernel */
         . = BOOTBOOT_START;
         _bootboot = .; . += 0x1000;
         _bootboot_environment = .; . += 0x1000;
         _kernel_start = .;
         /* BOOTBOOT wants everything loadable in a single section */
         .text : {
               KEEP(*(.text.boot))
               *(.text .text.*)
               *(.rodata .rodata.*)
               *(.data .data.*)
         } :boot
         .bss (NOLOAD) : ALIGN(16) {
              *(COMMON)
              *(.bss)
         } :boot
         _kernel_end = .;
}
