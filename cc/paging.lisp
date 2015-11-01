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
    ;; TODO: based on actual memory detection.
    setup-paging
    (equ     pml4-base #x9000)
    (equ     page-present-writable (+ 2 1))   ; Flags indicate the page is present and writable.
    (equ     page-present-writable-pde.ps (+ 128 2 1)) ; In addition to above flags, set PDE.PS for 2 MB page.

    (call32  get-memory-size)
    ;; TODO. So far we only handle < 4GB memory. As memory size is in
    ;; EDX:EAX, we ignore the value in EDX for now.
    (mov     edx eax)

    ;; Zero out the 12 kB buffer.
    (mov     edi pml4-base)
    (mov     ecx #xc00)
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

    ;; Build the Page Directory Table.
    (add     edi #x2000)
    (mov     eax page-present-writable-pde.ps) ; Effectively point EAX to address #x0.
    .loop-page-directory-table
    (mov     (edi) eax)
    (add     eax #x200000)
    (add     edi 8)
    (cmp     eax edx)                   ; Has all memory been mapped?
    (jb      .loop-page-directory-table)

    (ret)))
