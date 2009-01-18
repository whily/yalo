;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Utilities.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html

(in-package :cc)

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric variant of if."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun repeat-element (n element)
  (loop for i from 0 below n collect element))

(defun repeat-list (n list)
  (case n
    (1 list)
    (t (if (<= n 0)
           (error "repeat-list: invalid n=~A" n)
           (append list (repeat-list (1- n) list))))))

