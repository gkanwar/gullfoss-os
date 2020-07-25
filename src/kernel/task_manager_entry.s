        .intel_syntax noprefix
        .globl task_entry
        /* expects rsp to point to entry fn, rsp+0x8 to point to single entry arg */
task_entry:
        sti                     // "unlock" scheduler
        mov rdi, [rsp + 0x8]    // set upfirst arg
        call [rsp]                // call into entry function
        /* TODO: if task exits back here, call scheduler exit() */
