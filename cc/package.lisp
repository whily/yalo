;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Package definition.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2009-2011 Yujian Zhang

(defpackage #:cc
  (:use #:common-lisp)
  (:export
   #:asm
   #:*bootloader*
   #:test-cc
   #:write-kernel))
