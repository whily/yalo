;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Regression tests.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html

(in-package :cc)

(defparameter *test-asm*
  '((bits    16)
    (org     #x7c00)

    start
    (add     al 8)
    (add     ax 1000)
    (add     bl 3)
    (add     byte (msg) 4)
    (add     bx 1234)
    (add     (msg) 5678)
    (add     cx 9)
    (add     word (msg) 12)
    (add     al bl)
    (add     (msg) ch)
    (add     cx bx)
    (add     (msg) dx)
    (add     ch (msg))
    (add     dx (msg))
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
    (lodsb)
    (lodsw)
    (mov     ah 9)
    (mov     bx (- endmsg msg))
    (mov     ax cs)
    ;; Test r/m operand
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
    (mov     es bx)
    (mov     ds (di))
    (mov     ds (bx si 1001))
    (mov     es (bx di 1002))
    (mov     ds (bp si 1003))
    (mov     ss (bp di 1004))
    (mov     ds (si 1005))
    (mov     es (di 1006))
    (mov     ds (bp 1007))
    (mov     es (bx 1008))
    (mov     ax cx)
    (mov     (msg) bx)
    (mov     cx  (#x1c7b))
    (mov     word (msg) 123)
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
    (ret)
    (stc)
    (std)
    (sti)
    (stosb)
    (stosw)
    (sub     al 8)
    (sub     ax 1000)
    (sub     bl 3)
    (sub     byte (msg) 4)
    (sub     bx 1234)
    (sub     (msg) 5678)
    (sub     cx 9)
    (sub     word (msg) 12)
    (sub     al bl)
    (sub     (msg) ch)
    (sub     cx bx)
    (sub     (msg) dx)
    (sub     ch (msg))
    (sub     dx (msg))
    (equ     hi 4)
    (db      msg "Hello World! ")
    endmsg
    (times   3 db 0)
    (dw      #xaa55) 
    (dd      (123456 7891011))
    (dq      3372036854775808)
    ))

(deftest test-cc ()
  (check 
    (equal 
     (asm *bootloader*)
     '(180 3 205 16 184 1 19 187 15 0 185 15 0 189 20 124 205 16 235 254 72 101 
       108 108 111 32 87 111 114 108 100 33 32 13 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 85 170))
    (equal
     (asm *test-asm*) 
     '(4 8 5 232 3 128 195 3 128 6 253 124 4 129 195 210 4 129 6 253 124 46 22 131
       193 9 131 6 253 124 12 0 216 0 46 253 124 1 217 1 22 253 124 2 46 253 124 3 22
       253 124 232 199 0 248 252 250 244 228 3 229 4 236 237 204 205 16 235 254 172
       173 180 9 187 13 0 140 200 140 24 140 1 140 26 140 19 140 28 140 13 140 30 74
       126 140 30 253 124 140 15 140 88 1 140 65 2 140 90 3 140 83 4 140 92 5 140 77
       6 140 94 7 140 79 8 142 195 142 29 142 152 233 3 142 129 234 3 142 154 235 3
       142 147 236 3 142 156 237 3 142 133 238 3 142 158 239 3 142 135 240 3 137 200
       137 30 253 124 139 14 123 28 199 6 253 124 123 0 144 230 3 231 4 238 239 81 14
       22 30 6 90 23 31 7 243 164 243 165 195 249 253 251 170 171 44 8 45 232 3 128
       235 3 128 46 253 124 4 129 235 210 4 129 46 253 124 46 22 131 233 9 131 46 253
       124 12 40 216 40 46 253 124 41 217 41 22 253 124 42 46 253 124 43 22 253 124
       72 101 108 108 111 32 87 111 114 108 100 33 32 0 0 0 85 170 64 226 1 0 67 104
       120 0 0 0 230 130 217 250 11 0))))
