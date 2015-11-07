;; NASM code for cross check the assembly output from test-cc.lisp.
;; The test is performed as follows:
;; 1. Run NASM with: /usr/local/bin/nasm -o address.img address.asm
;; 2. In CL REPL, compare the output with (equal (read-image "address.img") (asm *address-asm*))
;; Note that above nasm command assumes the usage of nasm installed from brew.

        org     7C00h

        bits     16
        mov     [bp], es
        mov     [bx+si], ds
        mov     [bx+di], es
        mov     [bp+si], ds
        mov     [bp+di], ss
        mov     [si], ds
        mov     [di], cs
        mov     [32330], ds
        mov     [msg], ds
        mov     [bx], cs
        mov     [bx+si+1], ds
        mov     [bx+di+2], es
        mov     [bp+si+3], ds
        mov     [bp+di+4], ss
        mov     [si+5], ds
        mov     [di+6], cs
        mov     [bp+7], ds
        mov     [bx+8], cs
        mov     ds, [di]
        mov     ds, [bx+si+1001]
        mov     es, [bx+di+1002]
        mov     ds, [bp+si+1003]
        mov     ss, [bp+di+1004]
        mov     ds, [si+1005]
        mov     es, [di+1006]
        mov     ds, [bp+1007]
        mov     es, [bx+1008]

        bits    32
        mov     [ebp], ebx
        mov     [esp], ecx
        mov     [123456], edx
        mov     [eax], edx
        mov     [ebp+36], ecx
        mov     [edi+1234], edx
        mov     [esi+123456], ecx
        mov     [esp+23h], ebx
        mov     [esp+12345678h], ecx
        mov     [eax+ebx], ecx
        mov     [esi+edi], edx
        mov     [esi+ebp], edx
        mov     [eax*2+esi], edx
        mov     [esi*2+ebp+123], edx
        mov     [edi*2+ebx+8], ecx
        mov     [ecx*4+ebp+123], edx
        mov     [esi*4+edx+8], ecx
        mov     [edx*8+ebp+123456], ebx
        mov     [edi*8+ecx+8], edx

        bits    64
        mov     [rbp], rbx
        mov     [rsp], rcx
        mov     [123456], rdx
        mov     [rax], rdx
        mov     [rbp+36], rcx
        mov     [rdi+1234], rdx
        mov     [rsi+123456], rcx
        mov     [rsp+23h], rbx
        mov     [rsp+12345678h], rcx
        mov     [rax+rbx], rcx
        mov     [rsi+rdi], rdx
        mov     [rsi+rbp], rdx
        mov     [rax*2+rsi], rdx
        mov     [rsi*2+rbp+123], rdx
        mov     [rdi*2+rbx+8], rcx
        mov     [rcx*4+rbp+123], rdx
        mov     [rsi*4+rdx+8], rcx
        mov     [rdx*8+rbp+123456], rbx
        mov     [rdi*8+rcx+8], rdx
msg:
        db      "Hello World! "
endmsg:
