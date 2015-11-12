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

(defparameter *bitmap-prologue*
  `(;;; Bounds check.
    (cmp     rsi (rdi))
    (jb      .continue)
    (mov     rsi bitmap-overflow-message)
    ,@(call-function 'panic nil)
    .continue))

(defparameter *bitmap*
  `(
    ;;; Bitmap consists of a stream of bytes, representing a bit array.
    ;;; The first qword stores the size (in bits) of the bitmap.
    ;;; We assume the multiples of qwords are reserved to storet the bitmap.
    ;;;
    ;;; Typically functions have two input parameters:
    ;;;   start-address: the starting address of the bitmap.
    ;;;   bit-position: the bit position in the bit array, starting from 0.

    ;; The offset starting from which where actual bitmap is stored.
    (equ     bitmap-offset 8)
    ;; Divide by 64 to get qword offset
    (equ     bitmap-shift 6)
    ;; Mask to get the bit index within the qword.
    (equ     bitmap-mask (1- (expt 2 bitmap-shift)))

    ;;; Function bitmap-init. Initialize the bitmap.
    ;;; Perform the following:
    ;;;     1) Set the size field.
    ;;;     2) Initialize all bits to 0.
    ;;;        We assume multiples of qwords are reserved to store the bitmap.
    ;;;        This has the benefit that we have sentinel element for bitmap-scan
    ;;;        if the bitmap size is not the multiple of 64 (qword).
    ;;; Input:
    ;;;     RDI: start-address
    ;;;     RSI: size in bits
    ,@(def-fun 'bitmap-init nil `(
    (mov     (rdi) rsi)
    (add      rdi bitmap-offset)
    (mov      rcx rsi)
    (add      rcx bitmap-mask)
    (shr      rcx bitmap-shift)  ; RCX now is: ceil(rsi / 64)
    (xor      eax eax)
    (cld)
    (rep      stosq)))

    ;;; Function bitmap-set. Set the bit (i.e. to 1).
    ;;; Input:
    ;;;     RDI: start-address.
    ;;;     RSI: bit-position.
    ,@(def-fun 'bitmap-set nil `(
    ,@*bitmap-prologue*
    (mov     rdx rsi)       ; RDX: qword offset
    (shr     rdx bitmap-shift)
    (and     esi bitmap-mask)
    (bts     (rdx*8 rdi bitmap-offset) rsi)))

   ;;; Function bitmap-unset. Unset the bit (i.e. to 0).
    ;;; Input:
    ;;;     RDI: start-address.
    ;;;     RSI: bit-position.
    ,@(def-fun 'bitmap-unset nil `(
    ,@*bitmap-prologue*
    (mov     rdx rsi)       ; RDX: qword offset
    (shr     rdx bitmap-shift)
    (and     esi bitmap-mask)
    (btr     (rdx*8 rdi bitmap-offset) rsi)))

    ;;; Function bitmap-test. Returns corresponding bit in RDX.
    ;;; Input:
    ;;;     RDI: start-address.
    ;;;     RSI: bit-position.
    ;;; Output:
    ;;;     RAX: bit value
    ,@(def-fun 'bitmap-test nil `(
    ,@*bitmap-prologue*
    (xor     eax eax)       ; Prepare
    (mov     ecx 1)         ;   returning results.
    (mov     rdx rsi)       ; RDX: qword offset
    (shr     rdx bitmap-shift)
    (and     esi bitmap-mask)
    (bt      (rdx*8 rdi bitmap-offset) rsi)
    (cmovc   rax rcx)))

    ;;; Function bitmap-scan. Returns the index of the 1st bit which is 0.
    ;;; If every bit is 1, return -1.
    ;;; Input:
    ;;;     RDI: start-address
    ;;; Output:
    ;;;     RAX: corresponding index
    ,@(def-fun 'bitmap-scan nil `(
    (mov     rcx (rdi))
    (add     rcx bitmap-mask)      ; Scan ceil((rdi) / 64)
    (shr     rcx bitmap-shift)
    (xor     edx edx)
    .start
    (mov     r8 (rdx*8 rdi bitmap-offset))
    (not     r8)
    (bsf     r9 r8)
    (jnz     .scan-complete)
    ;; All zero means that the original qword contains all 1 (we use "NOT" instruction).
    (inc     rdx)
    (loop    .start)
    (jmp     short .not-found)
    .scan-complete
    (mov     rax rdx)
    (shl     rax bitmap-shift)
    (add     rax r9)             ; RAX is now the index.
    (cmp     rax (rdi))
    (jb      .done)
    .not-found
    (mov     rax -1)
    .done
    ))

    ;;; Bitmap regression test.
    ,@(def-fun 'bitmap-regression nil `(
    (equ bitmap-regression-addr #xffffffff80400000)
    (mov     rdi bitmap-regression-addr)
    (mov     rsi 100)
    ,@(call-function 'bitmap-init nil)
    (mov     rdi bitmap-regression-addr)
    (mov     rsi 0)
    ,@(call-function 'bitmap-set nil)
    (mov     rdi bitmap-regression-addr)
    (mov     rsi 1)
    ,@(call-function 'bitmap-set nil)
    (mov     rdi bitmap-regression-addr)
    ,@(call-function 'bitmap-scan nil)
    (cmp     rax 2)
    (jne     .error)
    (mov     rax #xffffffffffffffff)
    (mov     (rdi 8) rax)
    ,@(call-function 'bitmap-scan nil)
    (cmp     rax 64)
    (jne     .error)
    (mov     rsi 64)
    ,@(call-function 'bitmap-set nil)
    ,@(call-function 'bitmap-scan nil)
    (cmp     rax 65)
    (jne     .error)
    (mov     rsi 65)
    ,@(call-function 'bitmap-set nil)
    ,@(call-function 'bitmap-scan nil)
    (cmp     rax 66)
    (jne     .error)
    (mov     rsi 67)
    ,@(call-function 'bitmap-set nil)
    ,@(call-function 'bitmap-scan nil)
    (cmp     rax 66)
    (jne     .error)
    (mov     rsi 64)
    ,@(call-function 'bitmap-unset nil)
    ,@(call-function 'bitmap-scan nil)
    (cmp     rax 64)
    (jne     .error)
    (jmp     short .done)
    .error
    (mov     rdi bitmap-regression-error-message)
    ,@(call-function 'println nil)
    .done
    ))

    bitmap-overflow-message (db "ERROR: bitmap index exceeds bounds." 0)
    bitmap-regression-error-message (db "Regression test fails for bitmap." 0)
    ))
