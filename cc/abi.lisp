;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     x86-64 calling convention based on System V AMD64 ABI.
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
      ,@(mapcar #'(lambda (reg) (list 'pop reg)) (reverse saved-registers)))))

(deftest test-abi ()
  (check
    (equal (call-function 'read '(r10))
           '((push r10) (push r10) (call read) (pop r10) (pop r10)))
    (equal (call-function 'write '(r10 r11))
           '((push r10) (push r11) (call write) (pop r11) (pop r10)))))
