;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     ASDF definition.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html

(defpackage #:cc-system
  (:use #:cl #:asdf))
(in-package #:cc-system)

(defsystem cc
  :description "Cross compiler for yalo."
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "bga")
   (:file "bootloader")
   (:file "x86-64-syntax")
   (:file "lap")
   (:file "unit-test")
   (:file "nasm")
   (:file "test-cc")))
