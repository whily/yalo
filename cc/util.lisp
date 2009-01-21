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
  "Anaphoric variant of if. From ON LISP."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun str (&rest args)
  "Take any number of arguments and concatenates their printed
representations into a string. From ON LISP."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Takes one or more arguments and returns the symbol whose printname
is their concatenation. From ON LISP."
  (values (intern (apply #'str args))))

(defun mklist (obj)
  "Returns obj if it is already a list; otherwise lispy it. From ON LISP."
  (if (listp obj)
      obj
      (list obj)))

(defun repeat-element (n element)
  (loop for i from 0 below n collect element))

(defun repeat-list (n list)
  (case n
    (1 list)
    (t (if (<= n 0)
           (error "repeat-list: invalid n=~A" n)
           (append list (repeat-list (1- n) list))))))

(defun assoc* (&rest args)
  "Return the cdr of the result of assoc."
  (cdr (apply #'assoc args)))

