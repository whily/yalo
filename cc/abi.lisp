;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     x86-64 calling convention based on System V AMD64 ABI.
;;;; References:
;;;;     [1] http://www.x86-64.org/documentation/abi.pdf
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015 Yujian Zhang

(in-package :cc)

(defun call-function (function-name saved-registers)
  "Used by the caller. The function adds push and pops around function
calling automatically. It is assumed that RSP is aligned on 16 byte
boundary before calling. if the number of `saved-registers` is odd,
one more push/pop is added to ensure that RSP is aligned on 16 byte
boundaring before calling the function."
  (let ((saved-registers* (if (oddp (length saved-registers))
                              (push (car saved-registers) saved-registers)
                              saved-registers)))
    `(,@(mapcar #'(lambda (reg) (list 'push reg)) saved-registers*)
      (call ,function-name)
      ,@(mapcar #'(lambda (reg) (list 'pop reg)) (reverse saved-registers*)))))

(defun def-fun (function-name saved-registers body)
  "Define a function following System V ABI.
   * saved-registers: a set of registers to be preserved, a subset
     of (rbx rsp rbp r12 r13 r14 r15). TODO: automatically detect
     which registers to save.

   This function automatically detec whether the function is a leaf
   function (which does not call other functions) or not. For a
   non-leaf function, stack frames are saved via rbp; while such
   additional operation is not done for leaf functions.

  Note that 128 byte red zone is not used."
  (when (and saved-registers
             (notevery #'(lambda (x) (member x '(rbx rsp rbp r12 r13 r14 r15))) saved-registers))
    (error "def-fun: trying to save registers not in set (rbx rsp rbp r12 r13 r14 r15)."))
  (let* ((leaf? (notany #'(lambda (x) (and (listp x) (eq (car x) 'call))) body))
         (prologue (unless leaf?
                     '((push    rbp)
                       (mov     rbp rsp))))
         (epilogue (unless leaf?
                     '((leave))))
         )
    `(,function-name
      ,@prologue
      ,@(mapcar #'(lambda (reg) (list 'push reg)) saved-registers)
      ,@body
      ,@(mapcar #'(lambda (reg) (list 'pop reg)) (reverse saved-registers))
      ,@epilogue
      (ret)
      )))

(deftest test-abi ()
  (check
    (equal (call-function 'read '(r10))
           '((push r10)
             (push r10)
             (call read)
             (pop r10)
             (pop r10)))
    (equal (call-function 'write '(r10 r11))
           '((push r10)
             (push r11)
             (call write)
             (pop r11)
             (pop r10)))
    (equal (def-fun 'read '(r12) '((xor eax eax)))
           '(read
             (push r12)
             (xor eax eax)
             (pop r12)
             (ret)))
    (equal (def-fun 'write '(r12 r13) '((move rdi 42)
                                        (call universe)))
           '(write
             (push rbp)
             (mov rbp rsp)
             (push r12)
             (push r13)
             (move rdi 42)
             (call universe)
             (pop r13)
             (pop r12)
             (leave)
             (ret)))))
