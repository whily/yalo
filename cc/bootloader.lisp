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

    ;; Setup stack, allocating 8k bytes (relative to 7c00)
    ;(mov     sp #xfc00)

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

    ;; Initialize text mode.
    (call init-text-mode)

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

    (mov     si banner)
    (call    println)
    (jmp     short read-start)
    (db      banner ("Start your journey on yalo v0.0.0!" 0))

    ;;; REPL: read
    read-start
    (mov     si repl)
    (call    print)
    (jmp     short read)
    (db      repl ("REPL>" 0))
    read
    (call    getchar)
    (cmp     al 13)
    (je      eval-start)
    (call    putchar)
    (jmp     short read)

    ;;; REPL: eval
    eval-start

    ;;; REPL: print
    (call    printcrlf)
    (call    printcrlf)

    ;;; REPL: loop
    (jmp     short read-start)

    no-bga-error
    (mov     si no-bga-message)
    (call    println)
    (hlt)
    (db      no-bga-message ("ERROR: BGA not available." 0))

    no-long-mode-error
    (mov     si no-long-mode-message)
    (call    println)
    (hlt)
    (db      no-long-mode-message ("ERROR: CPU does not support long mode." 0))

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

    ;; Include content from vga-text.lisp.
    ,@*vga-text*

    ;; Include content from keyboard.lisp
    ,@*keyboard*

    ;; Include content from bga.lisp.
    ,@*bga*

    ;; Fill up to multiple of sectors, otherwise VirtualBox complains.
    (times   (- 8192 (- $ $$)) db 0)

    kernel-end))
