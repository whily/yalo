;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     A20 address line related functions.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015 Yujian Zhang

(in-package :cc)

(defparameter *a20*
  `(
    ;;; Enable A20 address line using keyboard controller method.
    ;;;   http://www.independent-software.com/writing-your-own-toy-operating-system-enabling-the-a20-line/
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: AL, BL, CL
    enable-a20
    (mov     al kbd-ctrl-cmd-disable-keyboard)
    (call    kbd-ctrl-send-cmd-16)
    (mov     al kbd-ctrl-cmd-read-output-port)
    (call    kbd-ctrl-send-cmd-16)
    (call    wait-kbd-out-buf-16)
    (in      al kbd-encoder-buf)
    (mov     cl al)          ; Save AL
    (mov     al kbd-ctrl-cmd-write-output-port)
    (call    kbd-ctrl-send-cmd-16)
    (mov     al cl)          ; Restore AL
    (or      al 2)           ; Enable A20 by set bit 1 to 1.
    (call    kbd-encoder-send-cmd-16)
    (mov     al kbd-ctrl-cmd-enable-keyboard)
    (call    kbd-ctrl-send-cmd-16)
    (call    wait-kbd-in-buf-16)
    (ret)
    ))
