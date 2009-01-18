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

(defmacro awhen (test-form &body body)
  "Anaphoric variant of when."
  `(aif ,test-form
        (progn ,@body)))

(defmacro acond (&rest clauses)
  "Anaphoric variant of cond."
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))
    
(defun mklist (obj)
  "Returns obj if it is already a list; otherwise lispy it."
  (if (listp obj)
      obj
      (list obj)))

(defun repeat-element (n element)
  (loop for i from 0 below n collect element))

(defun repeat-list (n list)
  (case n
    (1 list)
    (t (append list (repeat-list (1- n) list)))))

(defun replacer (list old new)
  "Recursively search list, replace old with new."
  (cond
    ((null list) nil)
    ((atom (car list)) (cons (if (eq (car list) old)
                                 new
                                 (car list))
                             (replacer (cdr list) old new)))
    (t (cons (replacer (car list) old new)
             (replacer (cdr list) old new)))))

