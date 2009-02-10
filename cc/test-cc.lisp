;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Regression tests. Cross checked with NASM.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html

(in-package :cc)

(defun arith-test-1 (mnemonic)
  "Return test codes for arithmetic operations: add/and/cmp/or/sub/xor."
  `((,mnemonic al 8)
    (,mnemonic ax 1000)
    (,mnemonic bl 3)
    (,mnemonic byte (msg) 4)
    (,mnemonic bx 1234)
    (,mnemonic (msg) 5678)
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
    ,@(arith-test-2 'div)
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

    (bits 64)
    (add      rax #x10010203)
    
    (db      msg "Hello World! ")
    endmsg)
  "Arithmetic instructions are tested separately.")

(defparameter *arith-code*
  '(4 8 5 232 3 128 195 3 128 6 219 125 4 129 195 210 4 129 6 219 125
    46 22 131 193 9 131 6 219 125 12 0 216 0 46 219 125 1 217 1 22 219
    125 2 46 219 125 3 22 219 125 36 8 37 232 3 128 227 3 128 38 219
    125 4 129 227 210 4 129 38 219 125 46 22 131 225 9 131 38 219 125
    12 32 216 32 46 219 125 33 217 33 22 219 125 34 46 219 125 35 22
    219 125 60 8 61 232 3 128 251 3 128 62 219 125 4 129 251 210 4 129
    62 219 125 46 22 131 249 9 131 62 219 125 12 56 216 56 46 219 125
    57 217 57 22 219 125 58 46 219 125 59 22 219 125 246 245 246 54
    219 125 247 247 247 114 3 246 229 246 38 219 125 247 231 247 98 3
    246 221 246 30 219 125 247 223 247 90 3 246 213 246 22 219 125 247
    215 247 82 3 12 8 13 232 3 128 203 3 128 14 219 125 4 129 203 210
    4 129 14 219 125 46 22 131 201 9 131 14 219 125 12 8 216 8 46 219
    125 9 217 9 22 219 125 10 46 219 125 11 22 219 125 208 230 208 38
    219 125 210 230 210 38 219 125 192 230 5 192 38 219 125 5 209 226
    209 38 219 125 211 226 211 38 219 125 193 226 5 193 38 219 125 5
    208 238 208 46 219 125 210 238 210 46 219 125 192 238 5 192 46 219
    125 5 209 234 209 46 219 125 211 234 211 46 219 125 193 234 5 193
    46 219 125 5 44 8 45 232 3 128 235 3 128 46 219 125 4 129 235 210
    4 129 46 219 125 46 22 131 233 9 131 46 219 125 12 40 216 40 46
    219 125 41 217 41 22 219 125 42 46 219 125 43 22 219 125 168 8 169
    232 3 246 195 3 246 6 219 125 4 247 195 210 4 247 6 219 125 46 22
    132 216 132 46 219 125 133 217 133 22 219 125 52 8 53 232 3 128
    243 3 128 54 219 125 4 129 243 210 4 129 54 219 125 46 22 131 241
    9 131 54 219 125 12 48 216 48 46 219 125 49 217 49 22 219 125 50
    46 219 125 51 22 219 125 102 5 232 3 72 5 3 2 1 16 72 101 108 108
    111 32 87 111 114 108 100 33 32))

(defparameter *misc-asm*
  '((bits    16)
    (org     #x7c00)

    start

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
    (jmp     short .loop)
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
    (mov     word (msg) 123)
    (mov     es bx)
    (mov     ax cs)

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
    (pop     dx)
    (pop     ss)
    (pop     ds)
    (pop     es)
    (rep     movsb)
    (rep     movsw)
    (rep     movsd)
    (ret)
    (stc)
    (std)
    (sti)
    (stosb)
    (stosw)

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
  '(232 93 0 248 252 250 244 228 3 229 4 236 237 204 205 16 235 254 15
    1 22 96 124 15 1 30 96 124 15 0 210 15 0 22 96 124 172 173 226 232
    180 9 187 13 0 137 200 137 30 96 124 139 14 123 28 199 6 96 124
    123 0 142 195 140 200 144 230 3 231 4 238 239 81 14 22 30 6 90 23
    31 7 243 164 243 165 243 102 165 195 249 253 251 170 171 96 124 72
    101 108 108 111 32 87 111 114 108 100 33 32 0 0 0 85 170 64 226 1
    0 67 104 120 0 0 0 230 130 217 250 11 0))

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
    ; (mov     (esi ebp) edx) 
    ;(mov     (eax*2 3456) edx)
    (mov     (eax*2 esi) edx)
    ;(mov     (esi*2 ebp 123) 
    ;(mov     (ecx*4 ebp 123) 
    ;(mov     (edx*8 ebp 123456) ebx)

    (db      msg "Hello World! ")
    endmsg)
  "Test addressing modes.")

(defparameter *address-code* 
  '(140 70 0 140 24 140 1 140 26 140 19 140 28 140 13 140 30 74 126
    140 30 132 124 140 15 140 88 1 140 65 2 140 90 3 140 83 4 140 92 5
    140 77 6 140 94 7 140 79 8 142 29 142 152 233 3 142 129 234 3 142
    154 235 3 142 147 236 3 142 156 237 3 142 133 238 3 142 158 239 3
    142 135 240 3 137 93 0 137 12 36 137 21 64 226 1 0 137 16 137 77
    36 137 151 210 4 0 0 137 142 64 226 1 0 137 92 36 35 137 140 36
    120 86 52 18 137 12 24 137 20 62 137 20 70 72 101 108 108 111 32
    87 111 114 108 100 33 32))

(deftest test-cc ()
  (check 
    (equal 
     (asm *bootloader*)
     '(180 3 205 16 184 1 19 187 15 0 185 15 0 189 20 124 205 16 235
       254 72 101 108 108 111 32 87 111 114 108 100 33 32 13 10 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 85 170))
    (equal (asm *address-asm*) *address-code*)
    (equal (asm *arith-asm*) *arith-code*)
    (equal (asm *misc-asm*)  *misc-code*)))




