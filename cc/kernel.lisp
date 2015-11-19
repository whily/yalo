;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     64 bit long mode kernel code.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015 Yujian Zhang

(in-package :cc)

(defparameter *kernel*
  `(
    ;;;==================== 64 bit long mode ====================

    long-mode
    (bits    64)

    ;; Load constant definitions
    ,@*vga-text-constants*

    ;; Setup stack's linear address. See the related 16 bit code for reason.
    (mov     rsp #x7ff00)

    ;; Reuse previous 32 bit GDT (as we don't consume more than 4 GB memory. So skip loading 64 bit GDT.
    ;; (lgdt (pgdt))

    ;; Load 64 bit IDT. So far IDT has zero length, therefore any NMI causes a triple fault.
    ;;(lidt    (idt))

    ;; Move kernel to physical address #x100000, which is virtual address
    ;; #xffffffff80100000.
    (mov     rsi kernel-before-relocation)
    (mov     rdi kernel-physical-base)
    (mov     rcx (- kernel-virtual-end kernel-virtual-start))
    (rep     movsb)

    ;; Jump to higher half kernel in 64 bit trampoline.
    (mov     rdi kernel-virtual-start)
    (push    rdi)
    (ret)                      ; Use push/ret together to implement a jmp.

    ;; Fill up to multiple of sectors, otherwise VirtualBox complains.
    (align   512)

    kernel-before-relocation

    (equ kernel-physical-base #x100000)
    (org (+ kernel-virtual-base kernel-physical-base))

    kernel-virtual-start

    ;; Setup stack's linear address again.
    (mov     rsp (+ kernel-virtual-start #x100000))

    ,@(call-function 'unmap-lower-memory)

    ,@(call-function 'clear)

    (mov     rdi banner)
    ,@(call-function 'println)
    (jmp     short read-start)
    banner   (db "Start your journey on yalo v0.0.1!" 0)

    ,@(call-function 'init-keyboard)

    ;;; REPL: read
    read-start
    (mov     rdi repl)
    ,@(call-function 'print)
    (jmp     short read)
    repl     (db "REPL> " 0)
    (equ     prompt-length (- $ repl 1))
    read
    ,@(call-function 'getchar)
    (cmp     al 10)
    (je      eval-start)
    (cmp     al 0)         ; Non-printable character.
    (jz      read)
    (cmp     al ascii-backspace)
    (jnz     .show)
    ,@(call-function 'backspace-char)
    (jmp     short read)
    .show
    (mov     dil al)
    ,@(call-function 'putchar)
    (jmp     short read)

    ;;; REPL: eval
    eval-start

    ;;; REPL: print
    ,@(call-function 'printlf)
    ,@(call-function 'printlf)

    ;;; REPL: loop
    (jmp     short read-start)

    ;; (bits 16)

    ;; no-bga-error
    ;; (mov     si no-bga-message)
    ;; (call    println-16)
    ;; .panic
    ;; (hlt)
    ;; (jmp     short .panic)
    ;; no-bga-message (db "ERROR: BGA not available." 0)

    (bits 64)

    ;; Include 64 bit paging related functiosn from paging.lisp
    ,@*paging-64*

    ;; Include content from bga.lisp.
    ;,@*bga*

    ;; Include content from vga-text.lisp.
    ,@*vga-text*

    ;; Include content from keyboard.lisp.
    ,@*keyboard*

    ;; Include content from bitmap.lisp.
    ,@*bitmap*

    ;; Function panic. Display error message and halt the computer.
    ,@(def-fun 'panic nil `(
    ,@(call-function 'println)
    .panic
    (hlt)
    (jmp     short .panic)))

    ;; Fill up to multiple of sectors, otherwise VirtualBox complains.
    (align 512)

    kernel-virtual-end))
