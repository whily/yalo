;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Paging functions.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015 Yujian Zhang

(in-package :cc)

(defparameter *paging*
  `(
    ;; Paging setup is based on http://wiki.osdev.org/Entering_Long_Mode_Directly
    ;; TODO: use 2 MB page instead of 4 kB. Use additional memory.

    setup-paging
    ;; Zero out the 16 kB buffer.
    (equ     pml4-base #x9000)
    (equ     page-present-writable #x3)   ; Flags indicate the page is present and writable.
    (mov     edi pml4-base)
    (mov     ecx #x1000)
    (xor     eax eax)
    (cld)
    (rep     stosd)
    (mov     edi pml4-base)

    ;; Build the Page Map Level 4.
    (mov     eax edi)
    (add     eax #x1000)              ; Address of the Page Directory Pointer Table.
    (or      eax page-present-writable)
    (mov     (edi) eax)

    ;; Build the Page Directory Pointer Table.
    (mov     eax edi)
    (add     eax #x2000)              ; Address of the Page Directory.
    (or      eax page-present-writable)
    (mov     (edi #x1000) eax)

    ;; Build the Page Directory.
    (mov     eax edi)
    (add     eax #x3000)              ; Address of the Page Table.
    (or      eax page-present-writable)
    (mov     (edi #x2000) eax)

    (add     edi #x3000)
    (mov     eax page-present-writable) ; Effectively point EAX to address #x0.

    ;; Build the Page Table.
    .loop-page-table
    (mov     (edi) eax)
    (add     eax #x1000)
    (add     edi 8)
    (cmp     eax #x200000)        ; Is 2 MB done? (TODO: based on actual memory size)
    (jb      .loop-page-table)

    (ret)))
