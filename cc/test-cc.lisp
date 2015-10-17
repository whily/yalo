;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Regression tests. Cross checked with NASM.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2009-2012 Yujian Zhang

(in-package :cc)

(defun arith-test-1 (mnemonic)
  "Return test codes for arithmetic operations: add/and/cmp/or/sub/xor."
  `((,mnemonic al 8)
    (,mnemonic ax 1000)
    (,mnemonic bl 3)
    (,mnemonic byte (msg) 4)
    (,mnemonic bx 1234)
    (,mnemonic word (msg) 5678)
    (,mnemonic cx 9)
    (,mnemonic word (msg) 12)
    (,mnemonic al bl)
    (,mnemonic (msg) ch)
    (,mnemonic cx bx)
    (,mnemonic (msg) dx)
    (,mnemonic ch (msg))
    (,mnemonic dx (msg))))

(defun arith-test-2 (mnemonic)
  "Return test codes for arithmetic operations: div/mul/neg/not"
  `((,mnemonic ch)
    (,mnemonic byte (msg))
    (,mnemonic di)
    (,mnemonic word (bp si 3))))

(defun shift-test (mnemonic)
  "Return test codes for shift operations: shl/shr."
  `((,mnemonic dh 1)
    (,mnemonic byte (msg) 1)
    (,mnemonic dh cl)
    (,mnemonic byte (msg) cl)
    (,mnemonic dh 5)
    (,mnemonic byte (msg) 5)
    (,mnemonic dx 1)
    (,mnemonic word (msg) 1)
    (,mnemonic dx cl)
    (,mnemonic word (msg) cl)
    (,mnemonic dx 5)
    (,mnemonic word (msg) 5)))

(defparameter *arith-asm*
  `((bits    16)
    (org     #x7c00)

    ,@(arith-test-1 'add)
    ,@(arith-test-1 'and)
    ,@(arith-test-1 'cmp)
    (dec     bx)
    (dec     eax)
    (dec     dl)
    (dec     byte (msg))
    (dec     word (bp si 3))
    (dec     dword (msg))
    ,@(arith-test-2 'div)
    (inc     bx)
    (inc     eax)
    (inc     dl)
    (inc     byte (msg))
    (inc     word (bp si 3))
    (inc     dword (msg))
    ,@(arith-test-2 'mul)
    ,@(arith-test-2 'neg)
    ,@(arith-test-2 'not)
    ,@(arith-test-1 'or)
    ,@(shift-test 'shl)
    ,@(shift-test 'shr)
    ,@(arith-test-1 'sub)
    (test    al 8)
    (test    ax 1000)
    (test    bl 3)
    (test    byte (msg) 4)
    (test    bx 1234)
    (test    (msg) 5678)
    (test    al bl)
    (test    (msg) ch)
    (test    cx bx)
    (test    (msg) dx)
    ,@(arith-test-1 'xor)

    (bits 32)
    (add      ax 1000)
    (dec      dx)
    (dec      ebp)
    (inc      dx)
    (inc      ebp)

    (bits 64)
    (adc      rax #x12345)
    (add      ebx 1000)
    (add      rax #x10010203)
    (add      rax 10)
    (add      rbx 267)
    (add      r15 #x123456)
    (add      r10d 3)
    (add      sil 6)
    (add      r9l 8)
    (add      r10 rbx)
    (add      rsi (rbx))
    (dec      di)
    (dec      ecx)
    (dec      r10)
    (div      ebx)
    (inc      di)
    (inc      ecx)
    (inc      r10)
    (mul      dword (rbx))
    (neg      rcx)
    (not      qword (rbx))
    (sbb      rbx rdx)

    (db      msg "Hello World! ")
    endmsg)
  "Arithmetic instructions are tested separately.")

(defparameter *arith-code*
  '(4 8 5 232 3 128 195 3 128 6 82 126 4 129 195 210 4 129 6 82 126 46
    22 131 193 9 131 6 82 126 12 0 216 0 46 82 126 1 217 1 22 82 126 2
    46 82 126 3 22 82 126 36 8 37 232 3 128 227 3 128 38 82 126 4 129
    227 210 4 129 38 82 126 46 22 131 225 9 131 38 82 126 12 32 216 32
    46 82 126 33 217 33 22 82 126 34 46 82 126 35 22 82 126 60 8 61
    232 3 128 251 3 128 62 82 126 4 129 251 210 4 129 62 82 126 46 22
    131 249 9 131 62 82 126 12 56 216 56 46 82 126 57 217 57 22 82 126
    58 46 82 126 59 22 82 126 75 102 72 254 202 254 14 82 126 255 74 3
    102 255 14 82 126 246 245 246 54 82 126 247 247 247 114 3 67 102
    64 254 194 254 6 82 126 255 66 3 102 255 6 82 126 246 229 246 38
    82 126 247 231 247 98 3 246 221 246 30 82 126 247 223 247 90 3 246
    213 246 22 82 126 247 215 247 82 3 12 8 13 232 3 128 203 3 128 14
    82 126 4 129 203 210 4 129 14 82 126 46 22 131 201 9 131 14 82 126
    12 8 216 8 46 82 126 9 217 9 22 82 126 10 46 82 126 11 22 82 126
    208 230 208 38 82 126 210 230 210 38 82 126 192 230 5 192 38 82
    126 5 209 226 209 38 82 126 211 226 211 38 82 126 193 226 5 193 38
    82 126 5 208 238 208 46 82 126 210 238 210 46 82 126 192 238 5 192
    46 82 126 5 209 234 209 46 82 126 211 234 211 46 82 126 193 234 5
    193 46 82 126 5 44 8 45 232 3 128 235 3 128 46 82 126 4 129 235
    210 4 129 46 82 126 46 22 131 233 9 131 46 82 126 12 40 216 40 46
    82 126 41 217 41 22 82 126 42 46 82 126 43 22 82 126 168 8 169 232
    3 246 195 3 246 6 82 126 4 247 195 210 4 247 6 82 126 46 22 132
    216 132 46 82 126 133 217 133 22 82 126 52 8 53 232 3 128 243 3
    128 54 82 126 4 129 243 210 4 129 54 82 126 46 22 131 241 9 131 54
    82 126 12 48 216 48 46 82 126 49 217 49 22 82 126 50 46 82 126 51
    22 82 126 102 5 232 3 102 74 77 102 66 69 72 21 69 35 1 0 129 195
    232 3 0 0 72 5 3 2 1 16 72 5 10 0 0 0 72 129 195 11 1 0 0 73 129
    199 86 52 18 0 65 131 194 3 64 128 198 6 65 128 193 8 73 1 218 72
    3 51 102 255 207 255 201 73 255 202 247 243 102 255 199 255 193 73
    255 194 247 35 72 247 217 72 247 19 72 25 211 72 101 108 108 111
    32 87 111 114 108 100 33 32))

(defparameter *misc-asm*
  '((bits    16)
    (org     #x7c00)

    start

    (bt      edx 29)
    (btc     eax 28)
    (btr     ebx 27)
    (bts     ecx 26)
    (call    msg)
    (clc)
    (cld)
    (cli)

    (hlt)
    (in      al 3)
    (in      ax 4)
    (in      al dx)
    (in      ax dx)
    (int     3)
    (int     #x10)
    .loop
    (jcxz    .loop)
    (je      .loop)
    (jmp     short .loop)
    (jmp     near meta-msg)
    (lgdt    (msg))
    (lidt    (msg))
    (lldt    dx)
    (lldt    (msg))
    (lodsb)
    (lodsw)
    (loop    .loop)
    (mov     ah 9)
    (mov     bx (- endmsg msg))
    (mov     ax cx)
    (mov     (msg) bx)
    (mov     cx  (#x1c7b))
    (mov     byte (msg) 42)
    (mov     word (msg) 123)
    (mov     es bx)
    (mov     ax cs)
    (mov     cr0 eax)
    (mov     eax cr0)
    (movzx   ax (msg))
    (movzx   edx cx)
    (movzx   eax byte (msg))
    (movzx   eax word (msg))

    (nop)
    (out     3 al)
    (out     4 ax)
    (out     dx al)
    (out     dx ax)
    (push    cx)
    (push    cs)
    (push    ss)
    (push    ds)
    (push    es)
    (pushfd)
    (pop     dx)
    (pop     ss)
    (pop     ds)
    (pop     es)
    (popfd)
    (rdmsr)
    (rep     movsb)
    (rep     movsw)
    (rep     movsd)
    (ret)
    (stc)
    (std)
    (sti)
    (stosb)
    (stosw)
    (stosd)
    (wdmsr)

    (bits 64)
    (bswap   ebx)
    (bswap   rax)
    (bswap   r10)
    (bt      edx 29)
    (bt      edx ecx)
    (bt      rdx 30)
    (bt      rax rbx)
    (btc     rax 29)
    (btr     rbx 28)
    (bts     rcx 27)
    (cmova   ax bx)
    (cmovc   eax edx)
    (cmove   rdx r10)
    (cmpxchg cl dl)
    (cmpxchg cx dx)
    (cmpxchg edi edx)
    (cmpxchg rcx r10)
    (cmpxchg8b (rbx))
    (cmpxchg16b (rbx))
    (jb      near msg)
    (jecxz   msg)
    (jrcxz   msg)
    (movzx   r10 al)
    ;; TODO: check why following test fails (when cross check with NASM.
    ;;(movzx   rdx word (msg))
    (syscall)
    (sysret)
    (xadd    cl dl)
    (xadd    cx dx)
    (xadd    edi edx)
    (xadd    rcx r10)
    (xchg    ax bx)
    (xchg    cx ax)
    (xchg    eax ebx)
    (xchg    ecx eax)
    (xchg    rax rbx)
    (xchg    rcx rax)
    ;; (xchg a b) is equivalent to NASM's xchg b, a.
    ;; They are semantically equivalent anyway.
    (xchg    al cl)
    (xchg    cx bx)
    (xchg    edx ebx)
    (xchg    r10 r15)

    (equ     hi 4)
    (dw      meta-msg msg)
    (db      msg "Hello World! ")
    endmsg
    (times   3 db 0)
    (dw      #xaa55)
    (dd      (123456 7891011))
    (dq      3372036854775808))
  "Miscellaneous instructions.")

(defparameter *misc-code*
  '(102 15 186 226 29 102 15 186 248 28 102 15 186 243 27 102 15 186
    233 26 232 10 1 248 252 250 244 228 3 229 4 236 237 204 205 16 227
    254 116 252 235 250 233 242 0 15 1 22 33 125 15 1 30 33 125 15 0
    210 15 0 22 33 125 172 173 226 225 180 9 187 13 0 137 200 137 30
    33 125 139 14 123 28 198 6 33 125 42 199 6 33 125 123 0 142 195
    140 200 15 34 192 15 32 192 15 182 6 33 125 102 15 183 209 102 15
    182 6 33 125 102 15 183 6 33 125 144 230 3 231 4 238 239 81 14 22
    30 6 102 156 90 23 31 7 102 157 15 50 243 164 243 165 243 102 165
    195 249 253 251 170 171 102 171 15 48 15 203 72 15 200 73 15 202
    15 186 226 29 15 163 202 72 15 186 226 30 72 15 163 216 72 15 186
    248 29 72 15 186 243 28 72 15 186 233 27 102 15 71 195 15 66 194
    73 15 68 210 15 176 209 102 15 177 209 15 177 215 76 15 177 209 15
    199 11 72 15 199 11 15 130 49 0 0 0 103 227 46 227 44 76 15 182
    208 15 5 15 7 15 192 209 102 15 193 209 15 193 215 76 15 193 209
    102 147 102 145 147 145 72 147 72 145 134 200 102 135 217 135 218
    77 135 250 33 125 72 101 108 108 111 32 87 111 114 108 100 33 32 0
    0 0 85 170 64 226 1 0 67 104 120 0 0 0 230 130 217 250 11 0))

(defparameter *address-asm*
  '((org     #x7c00)

    (bits    16)
    (mov     (bp) es)
    (mov     (bx si) ds)
    (mov     (bx di) es)
    (mov     (bp si) ds)
    (mov     (bp di) ss)
    (mov     (si) ds)
    (mov     (di) cs)
    (mov     (32330) ds)
    (mov     (msg) ds)
    (mov     (bx) cs)
    (mov     (bx si 1) ds)
    (mov     (bx di 2) es)
    (mov     (bp si 3) ds)
    (mov     (bp di 4) ss)
    (mov     (si 5) ds)
    (mov     (di 6) cs)
    (mov     (bp 7) ds)
    (mov     (bx 8) cs)
    (mov     ds (di))
    (mov     ds (bx si 1001))
    (mov     es (bx di 1002))
    (mov     ds (bp si 1003))
    (mov     ss (bp di 1004))
    (mov     ds (si 1005))
    (mov     es (di 1006))
    (mov     ds (bp 1007))
    (mov     es (bx 1008))

    (bits    32)
    (mov     (ebp) ebx)
    (mov     (esp) ecx)
    (mov     (123456) edx)
    (mov     (eax) edx)
    (mov     (ebp 36) ecx)
    (mov     (edi 1234) edx)
    (mov     (esi 123456) ecx)
    (mov     (esp #x23) ebx)
    (mov     (esp #x12345678) ecx)
    (mov     (eax ebx) ecx)
    (mov     (esi edi) edx)
    (mov     (esi ebp) edx)
    ;; (mov     (eax*2 3456) edx) ; NASM encodes as (eax eax 3456)
    (mov     (eax*2 esi) edx)
    (mov     (esi*2 ebp 123) edx)
    (mov     (edi*2 ebx 8) ecx)
    (mov     (ecx*4 ebp 123) edx)
    (mov     (esi*4 edx 8) ecx)
    (mov     (edx*8 ebp 123456) ebx)
    (mov     (edi*8 ecx 8) edx)

    (db      msg "Hello World! ")
    endmsg)
  "Test addressing modes.")

(defparameter *address-code*
  '(140 70 0 140 24 140 1 140 26 140 19 140 28 140 13 140 30 74 126
    140 30 162 124 140 15 140 88 1 140 65 2 140 90 3 140 83 4 140 92 5
    140 77 6 140 94 7 140 79 8 142 29 142 152 233 3 142 129 234 3 142
    154 235 3 142 147 236 3 142 156 237 3 142 133 238 3 142 158 239 3
    142 135 240 3 137 93 0 137 12 36 137 21 64 226 1 0 137 16 137 77
    36 137 151 210 4 0 0 137 142 64 226 1 0 137 92 36 35 137 140 36
    120 86 52 18 137 12 24 137 20 62 137 20 46 137 20 70 137 84 117
    123 137 76 123 8 137 84 141 123 137 76 178 8 137 156 213 64 226 1
    0 137 84 249 8 72 101 108 108 111 32 87 111 114 108 100 33 32))

(defparameter *bootloader-code*
  '(180 3 205 16 184 1 19 187 15 0 185 15 0 189 20 124 205 16 235 254
    72 101 108 108 111 32 87 111 114 108 100 33 32 13 10 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 85 170))

(deftest test-cc ()
  (check
    (equal (asm *address-asm*) *address-code*)
    (equal (asm *arith-asm*) *arith-code*)
    (equal (asm *misc-asm*)  *misc-code*)))
