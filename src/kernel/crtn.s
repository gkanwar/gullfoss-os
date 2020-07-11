        .intel_syntax noprefix
        .section .init
        /* gcc puts .init from crtend.o here */
        pop rbp
        ret

        .section .fini
        /* gcc puts .fini from crtend.o here */
        pop rbp
        ret
