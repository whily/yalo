;; NASM code for cross check the assembly output from test-cc.lisp.
;; The test is performed as follows:
;; 1. Run NASM with: /usr/local/bin/nasm -o arith.img arith.asm
;; 2. In CL REPL, compare the output with (equal (read-image "arith.img") (asm *arith-asm*))
;; Note that above nasm command assumes the usage of nasm installed from brew.

        org     7C00h
        bits    16

        add     al, 8
        add     ax, 1000
        add     bl, 3
        add     byte [msg], 4
        add     bx, 1234
        add     word [msg], 5678
        add     cx, byte 9
        add     word [msg], byte 12
        add     al, bl
        add     [msg], ch
        add     cx, bx
        add     word [msg], dx
        add     ch, byte [msg]
        add     dx, word [msg]
        and     al, 8
        and     ax, 1000
        and     bl, 3
        and     byte [msg], 4
        and     bx, 1234
        and     word [msg], 5678
        and     cx, byte 9
        and     word [msg], byte 12
        and     al, bl
        and     [msg], ch
        and     cx, bx
        and     word [msg], dx
        and     ch, byte [msg]
        and     dx, word [msg]
        cmp     al, 8
        cmp     ax, 1000
        cmp     bl, 3
        cmp     byte [msg], 4
        cmp     bx, 1234
        cmp     word [msg], 5678
        cmp     cx, byte 9
        cmp     word [msg], byte 12
        cmp     al, bl
        cmp     [msg], ch
        cmp     cx, bx
        cmp     word [msg], dx
        cmp     ch, byte [msg]
        cmp     dx, word [msg]
        dec     bx
        dec     eax
        dec     dl
        dec     byte [msg]
        dec     word [bp+si+3]
        dec     dword [msg]
        div     ch
        div     byte [msg]
        div     di
        div     word [bp+si+3]
        inc     bx
        inc     eax
        inc     dl
        inc     byte [msg]
        inc     word [bp+si+3]
        inc     dword [msg]
        mul     ch
        mul     byte [msg]
        mul     di
        mul     word [bp+si+3]
        neg     ch
        neg     byte [msg]
        neg     di
        neg     word [bp+si+3]
        not     ch
        not     byte [msg]
        not     di
        not     word [bp+si+3]
        or      al, 8
        or      ax, 1000
        or      bl, 3
        or      byte [msg], 4
        or      bx, 1234
        or      word [msg], 5678
        or      cx, byte 9
        or      word [msg], byte 12
        or      al, bl
        or      [msg], ch
        or      cx, bx
        or      word [msg], dx
        or      ch, byte [msg]
        or      dx, word [msg]
        shl     dh, 1
        shl     byte [msg], 1
        shl     dh, cl
        shl     byte [msg], cl
        shl     dh, 5
        shl     byte [msg], 5
        shl     dx, 1
        shl     word [msg], 1
        shl     dx, cl
        shl     word [msg], cl
        shl     dx, 5
        shl     word [msg], 5
        shr     dh, 1
        shr     byte [msg], 1
        shr     dh, cl
        shr     byte [msg], cl
        shr     dh, 5
        shr     byte [msg], 5
        shr     dx, 1
        shr     word [msg], 1
        shr     dx, cl
        shr     word [msg], cl
        shr     dx, 5
        shr     word [msg], 5
        sub     al, 8
        sub     ax, 1000
        sub     bl, 3
        sub     byte [msg], 4
        sub     bx, 1234
        sub     word [msg], 5678
        sub     cx, byte 9
        sub     word [msg], byte 12
        sub     al, bl
        sub     [msg], ch
        sub     cx, bx
        sub     word [msg], dx
        sub     ch, byte [msg]
        sub     dx, word [msg]
        test    al, 8
        test    ax, 1000
        test    bl, 3
        test    byte [msg], 4
        test    bx, 1234
        test    word [msg], 5678
        test    al, bl
        test    [msg], ch
        test    cx, bx
        test    word [msg], dx
        xor     al, 8
        xor     ax, 1000
        xor     bl, 3
        xor     byte [msg], 4
        xor     bx, 1234
        xor     word [msg], 5678
        xor     cx, byte 9
        xor     word [msg], byte 12
        xor     al, bl
        xor     [msg], ch
        xor     cx, bx
        xor     word [msg], dx
        xor     ch, byte [msg]
        xor     dx, word [msg]

        [bits 32]

        add     ax, 1000
        dec     dx
        dec     ebp
        inc     dx
        inc     ebp

        [bits 64]

        adc     rax, 12345h
        add     ebx, 1000
        add     rax, 10010203h
        ; add     rax, 10
        add     rbx, 267
        add     r15, 123456h
        add     r10d, byte 3
        add     sil, 6
        add     r9b, 8
        add     r10, rbx
        add     rsi, [rbx]
        dec     di
        dec     ecx
        dec     r10
        div     ebx
        inc     di
        inc     ecx
        inc     r10
        mul     dword [rbx]
        neg     rcx
        not     qword [rbx]
        sbb     rbx, rdx
        test    rax, 123456789
        test    r12, 11223344
        test    qword [rdi], 9966
        test    rdx, rdi
        test    qword [rdx], r9

msg:
        db      "Hello World! "
endmsg:
