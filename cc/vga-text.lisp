;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     VGA text mode functions.
;;;;     BIOS interruptions are not used.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2009-2015 Yujian Zhang

(in-package :cc)

(defparameter *vga-text*
  `(
    ;;; VGA text output code is based on http://wiki.osdev.org/Babystep4

    ;;; Initialize text mode. Call it before calling any text mode functions.
    init-text-mode
    (xor     ax ax)
    (mov     ds ax)
    (mov     ax #xb800)            ; Text video memory
    (mov     es ax)
    (call    clear)
    (ret)

    ;;; Clear screen.
    clear
    (movzx   ax (text-rows))
    (movzx   dx (text-cols))
    (mul     dx)
    (mov     cx ax)            ; All screen to be cleared.
    (mov     al #x20)          ; Space char
    (mov     ah #xf)           ; Attribute: white on black
    (mov     di 0)
    (rep     stosw)
    (ret)

    ;;; Function println. Write a string and start a new line.
    ;;; It is equivalent to calling print twice: first to print the string,
    ;;; and then print CR/LF.
    ;;; Input:
    ;;;   DS:SI: points to the starting address of the 0 terminated string.
    ;;; Output: None
    ;;; Modified registers: same as putchar
    ;;; Global variables: same as putchar
    println
    (call    print)
    (call    printcrlf)
    (ret)

    ;;; Function printcrlf. Print crlf only.
    ;;; Input: None
    ;;; Output: None
    ;;; Modified reisters: None
    ;;; Global variables: text-x, text-y
    printcrlf
    (add     byte (text-y) 1)   ; Down one row
    (mov     byte (text-x) 0)   ; Back to left
    (ret)

    do-char
    (call    putchar)
    ;;; Function print. Write string to screen.
    ;;; Input:
    ;;;   DS:SI: points to the starting address of the 0 terminated string.
    ;;; Output: None
    ;;; Modified registers: same as putchar
    ;;; Global variables: same as putchar
    print
    (lodsb)         ; Load string char to AL
    (cmp     al 0)      ; 0 terminated string like C.
    (jne     do-char)
    (ret)

    ;;; Function getchar. Get keystroke from keyboard without echo. If
    ;;; keystroke is available, it is removed from keyboard buffer.

    ;;; Input: None
    ;;; Output:
    ;;;   AH: BIOS scan code
    ;;;   AL: ASCII character
    getchar
    (xor     ah ah)
    (int     #x16)
    (ret)

    ;;; Function putchar. Writer character at cursor position.
    ;;; Input:
    ;;;   AL: character to display
    ;;; Output: None
    ;;; Modified registers: AX, BX, CX, DX, DI
    ;;; Global variables: text-x, text-y, text-cols
    putchar
    (mov     ah #xf)               ; Attribute: white on black
    (mov     cx ax)                ; Save char/attribute
    (movzx   ax (text-y))
    (movzx   dx (text-cols))
    (shl     dx 1)                 ; 2 bytes for one character
    (mul     dx)
    (movzx   bx (text-x))
    (shl     bx 1)
    (mov     di 0)                 ; Start of video memory
    (add     di ax)                ; Add y offset
    (add     di bx)                ; Add x offset
    (mov     ax cx)                ; Restore char/attribute
    (stosw)                        ; Write char/atribute
    (add     byte (text-x) 1)      ; Advance to right
    (ret)

    (db text-rows 25)   ;; Number of rows in text mode.
    (db text-cols 80)   ;; Number of columns in text mode.
    (db text-x 0)       ;; Position x in text mode [0, text-cols)
    (db text-y 0)       ;; Position y in text mode [0, text-rows)
    (db hex-str "0123456789ABCDEF") ;; Hexadecimal output string
    ))
