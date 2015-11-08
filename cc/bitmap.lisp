;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Bitmap functions.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015 Yujian Zhang

(in-package :cc)

(defparameter *bitmap*
  `(
    ;;; Bitmap consists of a stream of bytes, representing a bit array.
    ;;; Bound check is NOT performed.
    ;;; Typically functions have two input parameters:
    ;;;   start-address: the starting address of the bitmap.
    ;;;   bit-position: the bit position in the bit array, starting from 0.

    ;;; Function bitmap-set. Set the bit (to 1).
    ;;; Input:
    ;;;     RDI: start-address.
    ;;;     RSI: bit-position.
    ,@(def-fun 'bitmap-set nil `(
    (mov     rdx rsi)       ; RDX: qword offset
    (shr     rdx 6)         ; Divide by 64 to get qword offset.
    (and     esi #b111111)  ; RSI: bit index within the qword.
    (bts     (rdx*8 rdi) rsi)))

    ;;; Function bitmap-test. Returns corresponding bit in RDX.
    ;;; Input:
    ;;;     RDI: start-address.
    ;;;     RSI: bit-position.
    ;;; Output:
    ;;;     RAX: bit value
    ,@(def-fun 'bitmap-test nil `(
    (xor     eax eax)       ; Prepare
    (mov     ecx 1)         ;   returning results.
    (mov     rdx rsi)       ; RDX: qword offset
    (shr     rdx 6)         ; Divide by 64 to get qword offset.
    (and     esi #b111111)  ; RSI: bit index within the qword.
    (bt      (rdx*8 rdi) rsi)
    (cmovc   rax rcx)))
    ))
