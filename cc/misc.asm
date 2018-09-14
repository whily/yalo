;; NASM code for cross check the assembly output from test-cc.lisp.
;; The test is performed as follows:
;; 1. Run NASM with: /usr/local/bin/nasm -o misc.img misc.asm
;; 2. In CL REPL, compare the output with (equal (read-image "misc.img") (asm *misc-asm*))
;; Note that above nasm command assumes the usage of nasm installed from brew.

        org     7C00h
        bits    16

start:
        align   4
        bt      edx, 29
        align   4
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
        lodsd
        loop    .loop
        mov     bl, byte [es:di]
        mov     byte [ds:si], 0ffh
        mov     ah, 9
        mov     bl, [msg]
        mov     [msg], bh
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
        push    edx
        push    cs
        push    ss
        push    ds
        push    es
        pushf
        pushfd
        pop     dx
        pop     ecx
        pop     ss
        pop     ds
        pop     es
        popf
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
        default rel
        bsf     ax, bx
        bsf     ecx, edx
        bsf     r10, [msg]
        bsr     ax, r8w
        bsr     r10d, r9d
        bsr     rax, r12
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
        call    msg
        cmova   ax, bx
        cmovc   eax, edx
        cmove   rdx, r10
        cmpxchg cl, dl
        cmpxchg cx, dx
        cmpxchg edi, edx
        cmpxchg rcx, r10
        cmpxchg8b [rbx]
        cmpxchg16b [rbx]
        invlpg  [abs 0]
        iretq
        jb      near msg
        jmp     rbx
        jmp     [msg]
        leave
        lgdt    [msg]
        lidt    [msg]
        lodsq
        mov     eax, 1234h
        mov     rsp, 90000h
        mov     rax, 1122334455667788h
        mov     rcx, [msg]
        mov     rcx, [abs msg]
        mov     rdi, [abs 0b8000h]
        mov     rbx, rcx
        mov     rax, -1
        mov     qword [msg], 1019
        mov     ax, r8w
        mov     r9w, word [fs: rel msg]
        mov     dil, 19
        movsq
        movzx   r10, al
        movzx   eax, byte [msg]
        movzx   rdx, word [msg]
        push    r10
        pushfq
        pop     rax
        popcnt  r11, [msg]
        popfq
        sal     ebx, 1
        sar     edx, cl
        seta    ah
        setz    byte [msg]
        shl     r10, 6
        shr     qword [msg], 8
        rep     stosq
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

        jecxz   msg
        jrcxz   msg

        hi equ 4
meta_msg:
        dw      msg
        resb    4
msg:
        db      "Hello World! "
endmsg:
        times   3 db 0
        dw      0AA55h
        resw    3
        dd      123456,7891011
        resd    2
        dq      3372036854775808
        resq    1
