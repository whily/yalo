;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Bootloader.
;;;;
;;;;     Switch to 32 bit protected mode and 64 bit long mode is mainly based on
;;;;     Section 14.8 of
;;;;     [1] AMD64 Architecture Programmer's Manual Volume 2: System Programming.
;;;;         Publication No. 24593; Revision: 3.25
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2009-2015 Yujian Zhang

(in-package :cc)

(defparameter *bootloader*
  `(;;;==================== 16 bit real mode ====================
    (bits    16)
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
    (call init-text-mode-16)

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
    (jc      no-long-mode-error)

    (jmp     near switch-to-protected-mode)

    ;; Function check-cpu. Use CPUID to check if the process supports long mode.
    ;; From section 14.8 of [1].
    ;; If long mode is supported, CF is cleared; otherwise CF is set.

    check-cpu
    (mov     eax #x80000000)
    (cpuid)
    (cmp     eax #x80000000)  ; Whether any extended function > 0x800000000 is available?
    (jbe     no-long-mode)
    (mov     eax #x80000001)
    (cpuid)
    (bt      edx 29)          ; Test if long mode is supported.
    (jnc     no-long-mode)
    (clc)
    (ret)
    no-long-mode
    (stc)
    (ret)

    no-long-mode-error
    (mov     si no-long-mode-message)
    (call    println-16)
    .panic
    (hlt)
    (jmp     short .panic)
    (db      no-long-mode-message ("ERROR: CPU does not support long mode." 0))

    ;; Include content from vga-text.lisp.
    ,@*vga-text-code-16*

    ;;; Global Descriptor Table (GDT).
    ;;; 32 bit GDT entries are according to
    ;;;   http://www.brokenthorn.com/Resources/OSDev8.html
    ;;; 64 bit GDT entries are according to section 4.8 of [1].
    gdt
    ;; Null descriptor
    (dd 0)
    (dd 0)
    ;; 32 bit code descriptor.
    (equ code-selector-32 (- $ gdt))
    (dw #xffff)              ; Limit low
    (dw 0)                   ; Base low
    (db 0)                   ; Base middle
    (db #b10011010)          ; Access
    (db #b11001111)          ; Granularity
    (db 0)                   ; Base high
    ;; 32 bit data descriptor.
    (equ data-selector-32 (- $ gdt))
    (dw #xffff)              ; Limit low
    (dw 0)                   ; Base low
    (db 0)                   ; Base middle
    (db #b10010010)          ; Access
    (db #b11001111)          ; Granularity
    (db 0)                   ; Base high
    ;; 64 bit code descriptor.
    (equ code-selector-64 (- $ gdt))
    (dw 0)                   ; Limit low (ingored)
    (dw 0)                   ; Base low (ingored)
    (db 0)                   ; Base middle (ingored)
    (db #b10011000)          ; Access
    (db #b00100000)          ; Granularity
    (db 0)                   ; Base high (ingored)
    ;; 64 bit data descriptor (read/write).
    (equ data-selector-64 (- $ gdt))
    (dw 0)                   ; Limit low (ingored)
    (dw 0)                   ; Base low (ingored)
    (db 0)                   ; Base middle (ingored)
    (db #b10010000)          ; Access
    (db #b00000000)          ; Granularity
    (db 0)                   ; Base high (ingored)
    end-gdt
    pgdt
    (dw (- end-gdt gdt 1)) ; Limit (size of GDT)
    (dd gdt)               ; Base of GDT

    (align  4)
    idt
    .length (dw 0)
    .base   (dd 0)

    switch-to-protected-mode
    (cli)
    (lgdt (pgdt))          ; Load GDT
    ;; Enter protected mode by setting CR0.PE = 1.
    (mov     eax #b11)
    (mov     cr0 eax)
    ;; Far jump to turn on protected mode. The following code is equivalent to
    ;; (jmp far code-selector-32:protected-mode)
    (db      #xea)           ; Far jump
    (dw      protected-mode)
    (dw      code-selector-32)

    ;;;==================== 32 bit protected mode ====================

    protected-mode
    (bits    32)
    ;; Setup registers.
    (mov     ax data-selector-32)
    (mov     ss ax)
    (mov     esp #x90000)
    (mov     ds ax)
    (mov     es ax)

    switch-to-long-mode
    (call    setup-paging)

    ;; Enable 64 bit page-translation-table entries by setting
    ;; CR4.PAE=1. Paging is not enabled until after long mode is
    ;; enabled.
    (mov     eax cr4)
    (bts     eax 5)
    (mov     cr4 eax)

    ;; Initialize 64-bit CR3 to point to the base of PML4 page table.
    ;; Note that PML4 table must be below 4 GB.
    (mov     eax pml4-base)
    (mov     cr3 eax)

    ;; Enable long mode (set EFER.LME = 1).
    (mov     ecx #xc0000080)      ; EFER MSR number.
    (rdmsr)                       ; Read EFER.
    (bts     eax 8)               ; Set LME = 1.
    (wrmsr)                       ; Write EFER.

    ;; Enable paging to activate long mode (set CR0.PG = 1).
    (mov     eax cr0)             ; Read CR0.
    (bts     eax 31)              ; Set PE = 1.
    (mov     cr0 eax)             ; Write CR0.

    ;; Jump from 16 bit compatibility mode to 64 bit code segment.
    ;; Far jump to turn on long mode. The following code is equivalent to
    ;; (jmp far code-selector-64:protected-mode)
    (db      #x66)
    (db      #xea)           ; Far jump
    (dw      long-mode)      ; In [1], dd is used instead of dw.
    (dw      code-selector-64)

    ,@*paging*

    ;;;==================== 64 bit long mode ====================

    long-mode
    (bits    64)
    ;; Setup stack's linear address.
    ;(mov     rsp #x90000)    ; TODO: select appropriate stack address. Should be aligned on 16 byte boundary.

    ;; Reuse previous 32 bit GDT (as we don't consume more than 4 GB memory. So skip loading 64 bit GDT.
    ;; (lgdt (pgdt))

    ;; Load 64 bit IDT. So far IDT has zero length, therefore any NMI causes a triple fault.
    ;;(lidt    (idt))

    (call    clear)

    (mov     esi banner)
    (call    println)
    (jmp     short read-start)
    (db      banner ("Start your journey on yalo v0.0.1!" 0))

    (call    init-keyboard)

    ;;; REPL: read
    read-start
    (mov     esi repl)
    (call    print)
    (jmp     short read)
    (db      repl ("REPL>" 0))
    read
    (call    getchar)
    (cmp     al 10)
    (je      eval-start)
    (cmp     al 0)
    (jz      read)
    (call    putchar)
    (jmp     short read)

    ;;; REPL: eval
    eval-start

    ;;; REPL: print
    (call    printlf)
    (call    printlf)

    ;;; REPL: loop
    (jmp     short read-start)

    (bits 16)

    no-bga-error
    (mov     si no-bga-message)
    (call    println-16)
    .panic
    (hlt)
    (jmp     short .panic)
    (db      no-bga-message ("ERROR: BGA not available." 0))

    (bits 64)

    ;; Include content from bga.lisp.
    ,@*bga*

    ;; Include content from vga-text.lisp.
    ,@*vga-text-code*
    ,@*vga-text-data*

    ;; Include content from keyboard.lisp
    ,@*keyboard*

    ;; Fill up to multiple of sectors, otherwise VirtualBox complains.
    (times   (- 8192 (- $ $$)) db 0)

    kernel-end))
