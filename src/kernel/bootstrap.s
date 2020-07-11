#define ASM_FILE 1
#include "multiboot.h"
#include "kernel.h"
#define MULTIBOOT_FLAGS MULTIBOOT_PAGE_ALIGN | MULTIBOOT_MEMORY_INFO
#define MULTIBOOT_CHECKSUM -(MULTIBOOT_HEADER_MAGIC + MULTIBOOT_FLAGS)

        .intel_syntax noprefix

        /* multiboot header */
        .section .multiboot
        .align 4
        .long MULTIBOOT_HEADER_MAGIC
        .long MULTIBOOT_FLAGS
        .long MULTIBOOT_CHECKSUM

        
        .section .bss
        .global init_page_dir, init_page_tables
        /* kernel stack */
        .align 16
stack_bottom:
        .skip 16384
stack_top:

        .align 4096
init_page_dir:
        .skip 4096
init_page_tables:
        .skip 4096*1024                  # allocate all page tables (wasteful!)

      
        .section .prerodata, "a", @progbits
        .set LIMIT_fffff, 0xf << 48 | 0xffff
        .set ACCESS_KERN_CODE, 0b10011010 << 40
        .set ACCESS_KERN_DATA, 0b10010010 << 40
        .set FLAGS_SIZE_32, 1 << 54
        .set FLAGS_PAGE_GRAN, 1 << 55
        .set KERN_CODE_DESC, LIMIT_fffff | ACCESS_KERN_CODE | FLAGS_SIZE_32 | FLAGS_PAGE_GRAN
        .set KERN_DATA_DESC, LIMIT_fffff | ACCESS_KERN_DATA | FLAGS_SIZE_32 | FLAGS_PAGE_GRAN
init_gdt:
        .align 16
        .skip 8 # null descriptor
        .8byte KERN_CODE_DESC
        .8byte KERN_DATA_DESC
        .skip 40 # some room for good measure
init_gdt_end:
init_gdtr:
        .2byte init_gdt_end - init_gdt - 1
        .4byte init_gdt


        .section .pretext, "ax", @progbits
        .set CR0_PG, 1 << 31
        .global _start
        .type _start, @function
_start:
        mov esp, KERNEL_VIRT_TO_PHYS(offset stack_top)       # let there be stack!

        /* set up initial GDT */
        call load_init_gdt

        /* set up minimal paging */
        call init_paging

        /* early main is responsible for establishing the kernel memory system */
        mov ecx, ebx
        add ecx, KERNEL_BASE            # fix multiboot struct addr
        call kernel_early_main
        test eax,eax
        jnz end                         # quit on error code

        /* invoke global ctors */
        call _init

        call kernel_main
        /* shouldn't ret from kernel_main, but who knows what garbage code I
         * will be writing, so catch it in case */
end:
        cli
        hlt
        jmp end
        .size _start, . - _start        # for debugging

        /* reload segments with code selector 0x8 and data selector 0x10 */
load_init_gdt:
        lgdt [init_gdtr]
        jmp 0x8:reload_data_segs
reload_data_segs:
        mov ax, 0x10
        mov ds, ax
        mov es, ax
        mov fs, ax
        mov gs, ax
        mov ss, ax
        ret

        /* load page dir and initial table with identity mapping and higher
         * half mapping so we can call into higher half C++ code */
init_paging:
        mov ecx, ebx                    # pass multiboot info addr
        call setup_paging
        /* enable paging */
        mov eax, KERNEL_VIRT_TO_PHYS(offset init_page_dir)
        mov cr3, eax
        mov eax, cr0
        or eax, CR0_PG
        mov cr0, eax
        /* fix the stack */
        add esp, KERNEL_BASE
        ret
