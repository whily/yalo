;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Bootloader.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html

(in-package :cc)

(defparameter *bootloader* 
  `((org     #x7c00)
    (jmp     short start)

    msg
    (db      "Hello World! ")
    endmsg

    start
    (mov     cx 1)             ; Write 1 character
    (mov     bx #xf)           ; White color
    (mov     ah 3)
    (int     #x10)             ; Get current cursor position
    (mov     si msg)
    show-loop
    (mov     ah 2)
    (int     #x10)             ; Set cursor position
    (lodsb)                    ; Load a byte of the message into al
    (mov     ah 9)
    (int     #x10)             ; Write character
    (inc     dl)               ; Advance cursor
    (cmp     si  endmsg)
    (jne     show-loop)

    ;; Infinite loop.
    (jmp     $)

    ;; Fill up to 510 bytes.
    (times   (- #x200 2 (- $ $$)) db 0)

    (dw      #xaa55)           ; Boot sector signature
    ))
