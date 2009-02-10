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

(defmacro acond (&rest clauses)		
  "Anaphoric variant of cond. From ON LISP."
  (if (null clauses)		
      nil		
      (let ((cl1 (car clauses))		
            (sym (gensym)))		
        `(let ((,sym ,(car cl1)))		
           (if ,sym		
               (let ((it ,sym)) ,@(cdr cl1))		
               (acond ,@(cdr clauses)))))))

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

(defun segment (list n)
  "Segment list with each sublist containing n elements."
  (cond
    ((null list) nil)
    ((<= (length list) n) (list list))
    (t (append (list (subseq list 0 n)) (segment (nthcdr n list) n)))))

(defun printable? (c)
  "Returns T if char (as integer) is printable."
  (<= 32 c 126)) ; Seems that graphic-char-p allows more chars as printable.

(defun pp-char (c)
  "Returns the character form of c if printable; otherwise ."
  (if (printable? c) (code-char c) #\.))

(defun pp-hex (s)
  "Pretty print S (stream of bytes) in hexadecimal manner. Output is
same as Emacs hexl-mode."
  (format 
   t "87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789abcdef~%")
  (let ((ss (segment s 16)))
    (loop for i from 0 below (length ss)
       do (format t "~8,'0X: ~{~{~2,'0X~} ~}~51T~{~c~}~%" 
                  (* i 16) (segment (nth i ss) 2) 
                  (mapcar #'pp-char (nth i ss))))))

  

