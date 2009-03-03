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
  `((bits    16)
    (org     #x7c00)

    ;; Just in case any segment registers are touched.
    (xor     ax ax)
    (mov     ds ax)
    (mov     es ax)
    (mov     fs ax)
    (mov     gs ax)
    (mov     ss ax)

    ;; Load other sectors from floppy disk.
    ;; AL: # of sectors
    (mov     ax (+ #x200 (ceiling (- kernel-end real-start) 512)))
    (mov     bx real-start)   ; ES:BX is destination
    (mov     cx 2)            ; CH: cylinder; CL: sector
    (xor     dx dx)           ; DH: head; DL: drive
    (int     #x13)
    
    (times   480 nop)         ; To be removed once near jmp is available
    (jmp     short real-start)

    ;; Fill up to 510 bytes.
    (times   (- 510 (- $ $$)) db 0)
    
    (dw      #xaa55)           ; Boot sector signature

    ;; Real start up code.
    real-start

    ;; Get current cursor position.
    (mov     ah 3)
    (xor     bh bh)
    (int     #x10)             

    ;; Write string.
    (mov     ax #x1301)
    (mov     bx #xf)
    (mov     cx (- endmsg msg))
    (mov     bp msg)
    (int     #x10)  

    ;; Infinite loop.
    (jmp     short $)

    (db       msg ("Hello World! " 13 10))
    endmsg    

    kernel-end))
