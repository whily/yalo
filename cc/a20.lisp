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
    ;;; Enable A20 address line based on:
    ;;;   http://www.independent-software.com/writing-your-own-toy-operating-system-enabling-the-a20-line/
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: AL, BL, CL
    enable-a20
    (call    check-a20)
    (cmp     ax 0)
    (jne     .done)

    ;; Enable A20 based on BIOS
    (mov     ax #x2401)
    (int     #x15)

    (call    check-a20)
    (cmp     ax 0)
    (jne     .done)

    ;; Enable A20 based on keyboard controller.
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

    (call    check-a20)
    (cmp     ax 0)
    (jne     .done)

    ;; Enable A20 based on fast gate method.
    (in      al #x92)
    (or      al 2)
    (out     #x92 al)

    (call    check-a20)
    (cmp     ax 0)
    (jne     .done)

    .fail
    (mov     si .a20-error-message)
    (call    println-16)
    .panic
    (hlt)
    (jmp     short .panic)
    .a20-error-message (db "ERROR: A20 address line cannot be enabled." 0)

    .done
    (ret)

    ;;; Check whether A20 is enabled or not.
    ;;; Input: None
    ;;; Output: AX=1 if A20 is enabled; AX=0 otherwise.
    ;;; The function writes different values to two addresses:
    ;;; 0000:0500 and ffff:0510. If A20 is not enabled, the two
    ;;; addresses are equivalent due to wrap around.
    ;;; This functions restores registers and states modified in the call.
    check-a20
    ;; Save flags and registers.
    (pushf)
    (push    ds)
    (push    es)
    (push    di)
    (push    si)
    ;; Set es:di = 0000:0500
    (xor     ax ax)
    (mov     es ax)
    (mov     di #x500)
    ;; Set ds:si = ffff:0510
    (mov     ax #xffff)
    (mov     ds ax)
    (mov     si #x510)
    ;; Save byte at es:di on stack.
    (es mov  al (di))
    (push    ax)
    ;; Save byte at ds:si on stack.
    (ds mov  al (si))
    (push    ax)
    (es mov  byte (di), #x00)    ; [es:di] = #x00
    (ds mov  byte (si), #xff)    ; [ds:si] = #xff
    (es cmp  byte (di), #xff)    ; Check memory wrap around.
    ;; Restore byte at ds:si
    (pop     ax)
    (ds mov  (si) al)
    ;; Restore byte at es:di
    (pop     ax)
    (es mov  (di) al)
    (mov     ax 0)
    (je      .exit)              ; If A20 is disabled, return 0.
    (mov     ax 1)               ; Else, return 1
    .exit
    (pop     si)
    (pop     di)
    (pop     es)
    (pop     ds)
    (popf)
    (ret)
    ))
