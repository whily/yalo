;; NASM code for cross check the assembly output from test-cc.lisp.
;; The test is performed as follows:
;; 1. Run NASM with: /usr/local/bin/nasm -o misc.img misc.asm
;; 2. In CL REPL, compare the output with (equal (read-image "misc.img") (asm *misc-asm*))
;; Note that above nasm command assumes the usage of nasm installed from brew.

        org     7C00h
        bits    16

start:
        bt      edx, 29
        btc     eax, 28
        btr     ebx, 27
        bts     ecx, 26
        call    msg
        clc
        cld
        cli

        hlt
        in      al, 3
        in      ax, 4
        in      al, dx
        in      ax, dx
        int3
        int     10h
        .loop:
        jcxz    .loop
        je      .loop
        jmp     short .loop
        jmp     near meta_msg
        lgdt    [msg]
        lidt    [msg]
        lldt    dx
        lldt    [msg]
        lodsb
        lodsw
        loop    .loop
        mov     ah, 9
        mov     bx, endmsg - msg
        mov     ax, cx
        mov     [msg], bx
        mov     cx, [1c7bh]
        mov     byte [msg], 42
        mov     word [msg], 123
        mov     es, bx
        mov     ax, cs
        mov     cr0, eax
        mov     eax, cr0
        movzx   ax, byte [msg]
        movzx   edx, cx
        movzx   eax, byte [msg]
        movzx   eax, word [msg]

        nop
        out     3, al
        out     4, ax
        out     dx, al
        out     dx, ax
        push    cx
        push    cs
        push    ss
        push    ds
        push    es
        pushfd
        pop     dx
        pop     ss
        pop     ds
        pop     es
        popfd
        rdmsr
        rep     movsb
        rep     movsw
        rep     movsd
        ret
        stc
        std
        sti
        stosb
        stosw
        stosd
        wrmsr

        bits 64
        bswap   ebx
        bswap   rax
        bswap   r10
        bt      edx, 29
        bt      edx, ecx
        bt      rdx, 30
        bt      rax, rbx
        btc     rax, 29
        btr     rbx, 28
        bts     rcx, 27
        cmova   ax, bx
        cmovc   eax, edx
        cmove   rdx, r10
        cmpxchg cl, dl
        cmpxchg cx, dx
        cmpxchg edi, edx
        cmpxchg rcx, r10
        cmpxchg8b [rbx]
        cmpxchg16b [rbx]
        jb      near msg
        jecxz   msg
        jrcxz   msg
        movzx   r10, al
        ;;movzx   rdx, word [msg]
        syscall
        sysret
        xadd    cl, dl
        xadd    cx, dx
        xadd    edi, edx
        xadd    rcx, r10
        xchg    ax, bx
        xchg    cx, ax
        xchg    eax, ebx
        xchg    ecx, eax
        xchg    rax, rbx
        xchg    rcx, rax
        xchg    cl, al
        xchg    bx, cx
        xchg    ebx, edx
        xchg    r15, r10

        hi equ 4
meta_msg:
        dw      msg
msg:
        db      "Hello World! "
endmsg:

        times   3 db 0
        dw      0AA55h
        dd      123456,7891011
        dq      3372036854775808
