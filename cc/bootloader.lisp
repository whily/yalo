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
    (jmp short start)

    msg
    (db      "Hello World! ")
    endmsg

    start

    ;; Get current cursor position.
    (mov     ah 3)
    (int     #x10)             

    ;; Write string.
    (mov     ah #x13)
    (mov     al 1)
    (mov     bx #xf)
    (mov     cx 13)   ; (- endmsg msg))
    (mov     bp msg)
    (int     #x10)  

    ;; Infinite loop.
    loop
    (jmp     short loop)

    ;; Fill up to 510 bytes.
    (times   (- #x200 2 (- $ $$)) db 0)

    (dw      #xaa55)           ; Boot sector signature
    ))
