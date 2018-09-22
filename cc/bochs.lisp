;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     BOCHS specific functions.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2018 Yujian Zhang

(in-package :cc)

(defparameter *bochs*
  `(
    (equ bochs-shutdown-port #x8900)
    ;;; Shutdown computer.
    ;;; Note that although computer is already shutdown after sending the special string,
    ;;; a function is still defined for simplicity.
    ;;; Input: None
    ;;; Output: Noneq
    ,@(def-fun 'bochs-shutdown nil
        `(
          .start
          (mov     dx bochs-shutdown-port)
          (mov     rsi shutdown-str)
          .loop
          (lodsb)
          (out     dx al)
          (test    al al)
          (jz      .done)
          (jmp     short .loop)
          .done))

    shutdown-str (db "Shutdown" 0)
    ))
