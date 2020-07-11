/* Custom linker script to combine bootstrap and kernel */
KERNEL_BASE = 0xc0000000;
ENTRY(_start)
SECTIONS {
         /* conventional memory load location for a kernel */
         . = 1M;
         _kernel_start = . + KERNEL_BASE;
         /* pre-paging sections use physical addressing*/
         .pretext ALIGN(4K) : {
                  *(.multiboot)
                  *(.pretext)
         }
         .prerodata : {
                    *(.prerodata)
         }
         /* post-paging sections use virtual addressing */
         . = . + KERNEL_BASE;
         .text : AT(ADDR(.text) - KERNEL_BASE) {
               *(.multiboot)
               *(.text)
         }
         .rodata : AT(ADDR(.rodata) - KERNEL_BASE) {
                 *(.rodata)
         }
         .data : AT(ADDR(.data) - KERNEL_BASE) {
               *(.data)
         }
         .bss : AT(ADDR(.bss) - KERNEL_BASE) {
              *(COMMON) /* TODO: need to split between pre/post? */
              *(.bss)
         }
         _kernel_end = .;
}
