;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Keyboard handling mode functions.
;;;;     BIOS interruptions are not used.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015 Yujian Zhang

(in-package :cc)

(defparameter *keyboard-constants*
  `(
    ;;; Keyboard related constants. Note that input and output are
    ;;; from keyboard's perspective, not from user/cpu's persepctive.
    (equ kbd-encoder-buf      #x60)
    (equ kbd-encoder-cmd-reg  #x60)
    (equ kbd-ctrl-status-reg  #x64)
    (equ kbd-ctrl-cmd-reg     #x64)
    (equ kbd-ctrl-status-mask-out-buf  #x01)
    (equ kbd-ctrl-status-mask-in-buf   #x02)
    (equ kbd-ctrl-status-mask-system   #x04)
    (equ kbd-ctrl-status-mask-cmd-data #x08)
    (equ kbd-ctrl-status-mask-locked   #x10)
    (equ kbd-ctrl-status-mask-aux-buf  #x20)
    (equ kbd-ctrl-status-mask-timeout  #x40)
    (equ kbd-ctrl-status-mask-parity   #x80)
    (equ kbd-ctrl-cmd-disable-keyboard #xad)
    (equ kbd-ctrl-cmd-enable-keyboard  #xae)
    (equ kbd-ctrl-cmd-read-output-port #xd0)
    (equ kbd-ctrl-cmd-write-output-port #xd1)
    (equ kbd-ctrl-cmd-enable-a20       #xdd)
    (equ kbd-ctrl-cmd-system-reset     #xfe)
    (equ kbd-encoder-cmd-set-scan-code #xf0)
    (equ kbd-encoder-cmd-set-scan-code-1 #x1)
    (equ kbd-encoder-cmd-enable-scanning #xf4)
    (equ kbd-encoder-cmd-disable-scanning #xf5)
    (equ kbd-encoder-cmd-reset         #xff)))

(defparameter *keyboard-16*
  ;;; 16-bit keyboard code related to A20 enabling.
  `(;;; The following 4 functions are exactly same (except the label
    ;;; names) as those functions without -16 suffix, defined in
    ;;; *keyboard*. Function descriptions are not shown here.

    kbd-ctrl-send-cmd-16
    (mov     bl al)                  ; Save AL
    (call    wait-kbd-in-buf-16)
    (mov     al bl)
    (out     kbd-ctrl-cmd-reg al)
    (ret)

    kbd-encoder-send-cmd-16
    (mov     bl al)                  ; Save AL
    (call    wait-kbd-in-buf-16)
    (mov     al bl)
    (out     kbd-encoder-cmd-reg al)
    (ret)

    wait-kbd-in-buf-16
    (in      al kbd-ctrl-status-reg) ; Get status
    (test    al kbd-ctrl-status-mask-in-buf)
    (jnz     wait-kbd-in-buf-16)
    (ret)

    wait-kbd-out-buf-16
    (in      al kbd-ctrl-status-reg) ; Get status
    (test    al kbd-ctrl-status-mask-out-buf)
    (jz      wait-kbd-out-buf-16)
    (ret)
    ))

(defparameter *keyboard*
  `(
    ;;; Initialize keyboard by set scan code set 1.
    ;;; Input: None
    ;;; Output: None
    init-keyboard
    (mov     al kbd-encoder-cmd-reset)
    (call    kbd-encoder-send-cmd)
    (mov     al kbd-encoder-cmd-disable-scanning)
    (call    kbd-encoder-send-cmd)
    (mov     al kbd-encoder-cmd-set-scan-code)
    (call    kbd-encoder-send-cmd)
    (mov     al kbd-encoder-cmd-set-scan-code-1)
    (call    kbd-encoder-send-cmd)
    (mov     al kbd-encoder-cmd-enable-scanning)
    (call    kbd-encoder-send-cmd)
    (ret)

    ;;; Send command byte to keyboard controller.
    ;;; Input:
    ;;;   AL: command byte
    ;;; Output: None
    ;;; Modified registers: BL
    kbd-ctrl-send-cmd
    (mov     bl al)                  ; Save AL
    (call    wait-kbd-in-buf)
    (mov     al bl)
    (out     kbd-ctrl-cmd-reg al)
    (ret)

    ;;; Send command byte to keyboard encoder.
    ;;; Input:
    ;;;   AL: command byte
    ;;; Output: None
    ;;; Modified registers: BL
    kbd-encoder-send-cmd
    (mov     bl al)                  ; Save AL
    (call    wait-kbd-in-buf)
    (mov     al bl)
    (out     kbd-encoder-cmd-reg al)
    (ret)

    ;;; Wait until the keyboard controller input buffer empty,
    ;;; therefore command can be written
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: AL
    wait-kbd-in-buf
    (in      al kbd-ctrl-status-reg) ; Get status
    (test    al kbd-ctrl-status-mask-in-buf)
    (jnz     wait-kbd-in-buf)
    (ret)

    ;;; Wait until the keyboard controller output buffer ready for
    ;;; reading.
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: AL
    wait-kbd-out-buf
    (in      al kbd-ctrl-status-reg) ; Get status
    (test    al kbd-ctrl-status-mask-out-buf)
    (jz      wait-kbd-out-buf)
    (ret)

    ;;; Function getchar. Get keystroke from keyboard without echo. If
    ;;; keystroke is available, it is removed from keyboard buffer.
    ;;; Use polling method. TODO: use interrupt.
    ;;; So far only a few keys are scanned. TODO: support full set of scan code.
    ;;; Input: None
    ;;; Output:
    ;;;   AL: ASCII character (0 indicats a key is released or not handled)
    getchar
    (call    wait-kbd-out-buf)
    (in      al kbd-encoder-buf)     ; Get key data
    (mov     si scan-code-set-1)
    (xor     ah ah)
    (add     si ax)
    (lodsb)
    (ret)

    ;;; Table for Scan code set 1: http://wiki.osdev.org/Keyboard#Scan_Code_Set_1
    scan-code-set-1
    (db 0 27 49 50 51 52 53 54 55 56 57 48 45 61 8 9
        113 119 101 114 116 121 117 105 111 112 91 93 10 0 97 115
        100 102 103 104 106 107 108 59 39 96 0 92 122 120 99 118
        98 110 109 44 46 47 0 0 0 32 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    ))
