;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     NASM related functions.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2012 Yujian Zhang

(in-package :cc)

(defun ->nasm(listing)
  "Return a string of NASM code translated from LISTING (as the input to (ASM))."
  (let ((fstr (make-array '(0) :element-type 'base-char
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (dolist (e listing (string-downcase fstr))
        (cond ((atom e) (format s "~A:~%" e))
              ((member (first e) '(db dw dd dq))
               (format s "~A:~%    ~A \"~A\"~%" (second e) (first e) (third e)))
              ((eql (first e) 'mov)
               (format s "    mov ")
               (format-addr s (second e))
               (format s ", ")
               (format-addr s (third e))
               (format s "~%"))
              (t (format s "    ~A ~{~A~^,~}~%" (car e) (cdr e))))))))

(defun format-addr (stream expr)
  "Format address related expressions e.g. in MOV."
  (if (atom expr)
      (format stream "~A" expr)
      (format stream "[~{~A~^+~}]" expr)))  

(deftest test-nasm ()
  (check
    (string= (->nasm
              '((org     #x7c00)
                (bits    16)
                (mov     (bp) es)
                (mov     (bx si) ds)
                (mov     (32330) ds)
                (mov     (msg) ds)
                (mov     (bx) cs)
                (mov     (bx si 1) ds)
                (mov     ds (bx si 1001))
                (bits    32)
                (mov     (ebp) ebx)
                (mov     (123456) edx)
                (mov     (eax) edx)
                (mov     (ebp 36) ecx)
                (mov     (esp #x23) ebx)
                (mov     (eax*2 esi) edx)
                (mov     (esi*2 ebp 123) edx)
                (db      msg "Hello World!")
                endmsg))
             (concatenate 
              'string
              "    org 31744"
              "    bits 16"
              "    mov [bp], es"
              "    mov [bx+si], ds"
              "    mov [32330], ds"
              "    mov [msg], ds"
              "    mov [bx], cs"
              "    mov [bx+si+1], ds"
              "    mov ds, [bx+si+1001]"
              "    bits 32"
              "    mov [ebp], ebx"
              "    mov [123456], edx"
              "    mov [eax], edx"
              "    mov [ebp+36], ecx"
              "    mov [esp+35], ebx"
              "    mov [eax*2+esi], edx"
              "    mov [esi*2+ebp+123], edx"
              "msg:"
              "    db \"hello world! \""
              "endmsg:"))))
