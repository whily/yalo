;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Keyboard handling mode functions.
;;;;     BIOS interruptions are not used.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015-2018 Yujian Zhang

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
    (equ kbd-encoder-cmd-reset         #xff)
    ;; Codes for key states and toggle stages. Defined by internal usage only.
    (equ kbd-left-shift   #x80)
    (equ kbd-right-shift  #x81)
    (equ kbd-left-ctrl    #x82)
    (equ kbd-right-ctrl   #x83)
    (equ kbd-left-alt     #x84)
    (equ kbd-right-alt    #x85)
    (equ kbd-capslock     #x86)
    (equ kbd-numlock      #x87)
    ;; Test whether key is released or not. If highest bit is 1, then key
    ;; is released; otherwise pressed.
    (equ kbd-key-released-mask #x80)
    ;; Reset the highest bit (indicating key is released or not) to 0.
    ;; kbd-scancode-mask is 1's complement of kbd-key-released-mask
    (equ kbd-scancode-mask #x7f)
    ))

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
    ,@(def-fun 'init-keyboard nil
        `(
          (mov     dil kbd-encoder-cmd-reset)
          ,@(call-function 'kbd-encoder-send-cmd)
          (mov     dil kbd-encoder-cmd-disable-scanning)
          ,@(call-function 'kbd-encoder-send-cmd)
          (mov     dil kbd-encoder-cmd-set-scan-code)
          ,@(call-function 'kbd-encoder-send-cmd)
          (mov     dil kbd-encoder-cmd-set-scan-code-1)
          ,@(call-function 'kbd-encoder-send-cmd)
          (mov     dil kbd-encoder-cmd-enable-scanning)
          ,@(call-function 'kbd-encoder-send-cmd)))

    ;;; Send command byte to keyboard controller.
    ;;; Input:
    ;;;   AL: command byte
    ;;; Output: None
    ;;; Modified registers: BL
    ,@(def-fun 'kbd-ctrl-send-cmd nil
        `(
          ,@(call-function 'wait-kbd-in-buf)
          (mov     al dil)
          (out     kbd-ctrl-cmd-reg al)))

    ;;; Send command byte to keyboard encoder.
    ;;; Input:
    ;;;   DIL: command byte
    ;;; Output: None
    ;;; Modified registers: AL
    ,@(def-fun 'kbd-encoder-send-cmd nil
        `(
          ,@(call-function 'wait-kbd-in-buf)
          (mov     al dil)
          (out     kbd-encoder-cmd-reg al)))

    ;;; Wait until the keyboard controller input buffer empty,
    ;;; therefore command can be written
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: AL
    ,@(def-fun 'wait-kbd-in-buf nil
        `(
          (in      al kbd-ctrl-status-reg) ; Get status
          (test    al kbd-ctrl-status-mask-in-buf)
          (jnz     wait-kbd-in-buf)))

    ;;; Wait until the keyboard controller output buffer ready for
    ;;; reading.
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: AL
    ,@(def-fun 'wait-kbd-out-buf nil
        `(
          (in      al kbd-ctrl-status-reg) ; Get status
          (test    al kbd-ctrl-status-mask-out-buf)
          (jz      wait-kbd-out-buf)))

    ;;; Function getchar. Get keystroke from keyboard without echo. If
    ;;; keystroke is available, it is removed from keyboard buffer.
    ;;; Use polling method. TODO: use interrupt.
    ;;; So far only a few keys are scanned. TODO: support full set of scan code.
    ;;; Input: None
    ;;; Output:
    ;;;   AL: ASCII character (0 indicats a key is released or not handled)
    ;;; Modified registers: RSI, RDX
    ,@(def-fun 'getchar nil
        `(
          ,@(call-function 'wait-kbd-out-buf)
          (xor     eax eax)
          ;; Use DL to store whether key is released or pressed.
          ;;   Zero: key is pressed; otherwise released.
          (xor     edx edx)
          (in      al kbd-encoder-buf)     ; Get key data
          (mov     dl al)
          ;; After following instruction, DL stores whether key is released (#x80)
          ;; or pressed (#x0).
          (and     dl kbd-key-released-mask)
          (and     al kbd-scancode-mask)
          ;; Now highest bit of AL is 0, and we can index into scan code set table.
          (mov     rsi scan-code-set-1)
          (add     rsi rax)
          (lodsb)
          (cmp     al kbd-left-shift)
          ;; kbd-left-shift is the first code for key and toggle states.
          (jb      .translate)
          (jnz     .check-right-shift)
          (test    dl dl)
          (setz    (kbd-left-shift-status))
          (jmp     short .set-shift-status)
          .check-right-shift
          (cmp     al kbd-right-shift)
          (jnz     .clear-key)
          (test    dl dl)
          (setz    (kbd-right-shift-status))
          .set-shift-status
          ;; Use DH to store the overall Shift status.
          ;; TODO reorganize the shift status checking code carefully.
          (mov     dh (kbd-left-shift-status))
          (or      dh (kbd-right-shift-status))
          (mov     (kbd-shift-status) dh)
          (jmp     short .clear-key)
          .translate
          (mov     dh (kbd-shift-status))
          (test    dh dh)
          (jz      .test-key-release)
          ;; Shift has been pressed
          (mov     rsi lower-to-upper-table)
          (add     rsi rax)
          (lodsb)
          .test-key-release
          (test    dl dl)
          (jnz     .done)
          .clear-key
          ;; Set AL to 0 if highest bit of DL is 1 as we don't handle key release for now.
          ;; Also set AL to 0 if keys for states and toggles are pressed/released.
          (xor     eax eax)
          .done
          ))

    ;; Left Shift status: 0: released; 1: pressed
    kbd-left-shift-status (db 0)
    ;; Right Shift status: 0: released; 1: pressed
    kbd-right-shift-status (db 0)
    ;; Shift status: 0: released (if all shift key are released);
    ;;               1: pressed (if any of the Shift key is pressed)
    kbd-shift-status (db 0)
    ;;; Table for Scan code set 1: http://wiki.osdev.org/Keyboard#Scan_Code_Set_1
    ;;; Release code is not stored as it is simply the sum of pressed code and #x80
    ;;; (as in http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html#ss1.1)
    scan-code-set-1
    (db 0 27 49 50 51 52 53 54 55 56 57 48 45 61 8 9
        113 119 101 114 116 121 117 105 111 112 91 93 10 kbd-left-ctrl 97 115
        100 102 103 104 106 107 108 59 39 96 kbd-left-shift 92 122 120 99 118
        98 110 109 44 46 47 kbd-right-shift 0 kbd-left-alt 32 kbd-capslock 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        )

    ;;; Convert from lower case ASCII (index) to upper case ASCII (value)
    ;;; from https://en.wikipedia.org/wiki/ASCII#Character_set
    ;;; An example: index 0x30 is the ASCII code for '0', and the corresponding
    ;;;             value is 0x29 (41 in decimal), which is the ASCII code for
    ;;;             ')'. This corresponds to the relationship that ')' is the
    ;;;             character when both Shift and '0' is pressed.
    lower-to-upper-table
    (db 0 0 0 0 0 0 0 0 8 9 10 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        32 0 0 0 0 0 0 34 0 0 0 0 60 95 62 63
        41 33 64 35 36 37 94 38 42 40 0 58 0 43 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 123 124 125 0 0
        126 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
        80 81 82 83 84 85 86 87 88 89 90 0 0 0 0 0)
    ))
