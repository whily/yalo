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

    ;; Setup segments and stack.
    (cli)
    (xor     ax ax)
    (mov     ds ax)
    (mov     es ax)
    (mov     fs ax)
    (mov     gs ax)
    ;; Skip setting fs and gs since we will jump from protected mode
    ;; directly to long mode. Skip setting es as it will be handled in
    ;; vga-text.lisp.
    (mov     ax #x9000)
    (mov     ss ax)
    (mov     sp #xff00)
    (sti)

    ;; Load other sectors from floppy disk.
    ;; AL: # of sectors
    (mov     ax (+ #x200 (ceiling (- kernel-end stage-2) 512)))
    (mov     bx stage-2)   ; ES:BX is destination
    (mov     cx 2)            ; CH: cylinder; CL: sector
    (xor     dx dx)           ; DH: head; DL: drive
    (int     #x13)

    (jmp     near stage-2)

    ;; Fill up to 510 bytes.
    (times   (- 510 (- $ $$)) db 0)

    (dw      #xaa55)           ; Boot sector signature

    ;; Stage 2.
    stage-2

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

    (jmp     short switch-to-protected-mode)

    ;;; 32 bit Global Descriptor Table (GDT), according to
    ;;;   http://www.brokenthorn.com/Resources/OSDev8.html
    gdt32
    ;; Null descriptor
    (dd 0)
    (dd 0)
    ;; Code descriptor
    (dw #xffff)              ; Limit low
    (dw 0)                   ; Base low
    (db 0)                   ; Base middle
    (db #b10011010)          ; Access
    (db #b11001111)          ; Granularity
    (db 0)                   ; Base high
    ;; Data descriptor
    (dw #xffff)              ; Limit low
    (dw 0)                   ; Base low
    (db 0)                   ; Base middle
    (db #b10010010)          ; Access
    (db #b11001111)          ; Granularity
    (db 0)                   ; Base high
    end-gdt32
    pgdt32
    (dw (- end-gdt32 gdt32 1)) ; Limit (size of GDT)
    (dd gdt32)               ; Base of GDT

    switch-to-protected-mode
    (cli)
    (lgdt (pgdt32))          ; Load 32 bit GDT
    ;; Enter protected mode by setting CR0.PE = 1.
    (mov     eax #b11)
    (mov     cr0 eax)
    ;; Far jump to turn on protected mode. The following code is equivalent to
    ;; (jmp far #x8:protected-mode)
    (db      #xea)           ; Far jump
    (dw      protected-mode)
    (dw      8)              ; Code selector (8 is the offset relative to the beginning of gdt32)

    protected-mode
    (bits    32)
    ;; Setup registers.
    (mov     ax #x10)       ; Data selector (#x10 is the offset relative to the begining of gdt32)
    (mov     ss ax)
    (mov     esp #x90000)
    ;; TODO: skip ds/es setting after jumping to long mode
    (mov     ds ax)
    (mov     es ax)

    ;; Clear screen. This is just to test whether protected mode works or not. Will be removed later.
    (movzx   ax (text-rows))
    (movzx   dx (text-cols))
    (mul     dx)
    (movzx   ecx ax)           ; All screen to be cleared.
    (mov     al #x30)          ; Space char
    (mov     ah #xf)           ; Attribute: white on black
    (mov     edi #xb8000)
    (rep     stosw)

    (hlt)
    ;; Ignore the following code, as we will write 64 bit version of vga-text.lisp.

    (mov     si banner)
    (call    println)
    (jmp     short read-start)
    (db      banner ("Start your journey on yalo v0.0.0!" 0))

    (call    init-keyboard)

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
    (cmp     al 0)
    (jz      read)
    (call    putchar)
    (jmp     short read)

    ;;; REPL: eval
    eval-start

    ;;; REPL: print
    (call    printcrlf)
    (call    printcrlf)

    ;;; REPL: loop
    (jmp     short read-start)

    (bits    16)

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
