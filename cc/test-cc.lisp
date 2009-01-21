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
  '((org     #x7c00)

    start
    (call    msg)
    (clc)
    (cld)
    (cli)
    (in      al 3)
    (in      ax 4)
    (in      al dx)
    (in      ax dx)
    (int     3)
    (int     #x10)
    .loop
    (jmp     short .loop)
    (mov     ah 9)
    (mov     bx (- endmsg msg))
    (mov     ax cs)
    (mov     es bx)
    (out     3 al)
    (out     4 ax)
    (out     dx al)
    (out     dx ax)
    (rep     movsb)
    (rep     movsw)
    (ret)
    (stc)
    (std)
    (sti)
    (equ     hi 4)
    (db      msg "Hello World! ")
    endmsg
    (times   3 db 0)
    (dw      #xaa55) 
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
     '(232 37 0 248 252 250 228 3 229 4 236 237 204 205 16 235 254 180 9 187 13
       0 140 200 142 195 230 3 231 4 238 239 243 164 243 165 195 249 253 251 72 
       101 108 108 111 32 87 111 114 108 100 33 32 0 0 0 85 170))))
          

