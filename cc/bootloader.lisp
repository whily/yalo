;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Bootloader.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2009-2015 Yujian Zhang

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

    ;; Check whether BGA is available
    ;; The following two lines are disabled for now as BGA mode is not used currently.
    ;; If not disabled, error will be thrown on VirtualBox (however osdev.org actually says that
    ;; VirtualBox supports BGA: http://wiki.osdev.org/BGA)
    ;;(call    bga-available)
    ;;(jc      no-bga-error)
    ;; Set target screen mode.
    ;(call    set-bga-mode)
    ;; Show all red.
    ;(mov     ecx ,(/ 65536 4))
    ;(mov     eax #xa000)
    ;(mov     es ax)
    ;(xor     edi edi)
    ;(mov     eax #xff0000) ; Red
    ;(cld)
    ;(rep     stosd)

    ;; Check whether CPU supports Long Mode or not.
    (call    check-cpu)
    (jnc     no-long-mode-error)
    (clc)

    (mov     cx (- end-banner banner))
    (mov     bp banner)
    (call    println)
    (jmp     short read-start)
    (db      banner "Start your journey on yalo v0.0.0!")
    end-banner

    ;;; REPL: read
    read-start
    (mov     cx (- read repl))
    (mov     bp repl)
    (call    print)
    (jmp     short read)
    (db      repl ("REPL>"))
    read
    (call    getchar)
    (cmp     al 13)
    (je      eval-start)
    (call    putchar)
    (call    forward-cursor)
    (jmp     short read)

    ;;; REPL: eval
    eval-start

    ;;; REPL: print
    (call    printcrlf)
    (call    printcrlf)

    ;;; REPL: loop
    (jmp     short read-start)

    no-bga-error
    (mov     cx (- end-no-bga-message no-bga-message))
    (mov     bp no-bga-message)
    (call    println)
    (hlt)
    (db      no-bga-message "ERROR: BGA not available.")
    end-no-bga-message

    no-long-mode-error
    (mov     cx (- end-no-long-mode-message no-long-mode-message))
    (mov     bp no-long-mode-message)
    (call    println)
    (hlt)
    (db      no-long-mode-message "ERROR: CPU does not support long mode.")
    end-no-long-mode-message

    ;; Function check-cpu. Use CPUID to check if the process supports long mode.
    ;; From section 14.8 of AMD64 Architecture Programmer's Manual
    ;;     Volume 2: System Programming.
    ;;     Publication No. 24593; Revision: 3.25
    ;; If long mode is supported, CF is set; otherwise CF is cleared.

    check-cpu
    (mov     eax #x80000000)
    (cpuid)
    (cmp     eax #x80000000)  ; Whether any extended function > 0x800000000 is available?
    (jbe     no-long-mode)
    (mov     eax #x80000001)
    (cpuid)
    (bt      edx 29)          ; Test if long mode is supported.
    (jnc     no-long-mode)
    (ret)
    no-long-mode
    (clc)
    (ret)

    ;; Include content from console.lisp.
    ,@*console*

    ;; Include content from bga.lisp.
    ,@*bga*

    ;; Fill up to multiple of sectors, otherwise VirtualBox complains.
    (times   (- 8192 (- $ $$)) db 0)

    kernel-end))
