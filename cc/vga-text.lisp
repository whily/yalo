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

(defparameter *vga-text-code-16*
  ;;; VGA text output code in 16 bit mode, based on http://wiki.osdev.org/Babystep4
  `(
    ;;; Initialize text mode. Call it before calling any text mode functions.
    init-text-mode-16
    (xor     ax ax)
    (mov     ds ax)
    (mov     ax #xb800)            ; Text video memory
    (mov     es ax)
    (call    clear-16)
    (ret)

    ;;; Clear screen.
    clear-16
    (movzx   ax (text-rows))
    (movzx   dx (text-cols))
    (mul     dx)
    (movzx   ecx ax)          ; All screen to be cleared.
    (mov     ax #x0f20)       ; Black background, white foreground, space char.
    (mov     di 0)
    (rep     stosw)
    (ret)

    ;;; Function println-16. Write a string and start a new line.
    ;;; It is equivalent to calling print twice: first to print the string,
    ;;; and then print CR/LF.
    ;;; Input:
    ;;;   DS:SI: points to the starting address of the 0 terminated string.
    ;;; Output: None
    ;;; Modified registers: same as putchar
    ;;; Global variables: same as putchar
    println-16
    (call    print-16)
    (call    printcrlf-16)
    (ret)

    ;;; Function printcrlf-16. Print crlf only.
    ;;; Input: None
    ;;; Output: None
    ;;; Modified reisters: None
    ;;; Global variables: text-x, text-y
    printcrlf-16
    (add     byte (text-y) 1)   ; Down one row
    (mov     byte (text-x) 0)   ; Back to left
    (ret)

    do-char-16
    (call    putchar-16)
    ;;; Function print-16. Write string to screen.
    ;;; Input:
    ;;;   DS:SI: points to the starting address of the 0 terminated string.
    ;;; Output: None
    ;;; Modified registers: same as putchar
    ;;; Global variables: same as putchar
    print-16
    (lodsb)         ; Load string char to AL
    (cmp     al 0)      ; 0 terminated string like C.
    (jne     do-char-16)
    (ret)

    ;;; Function putchar-16. Writer character at cursor position.
    ;;; Input:
    ;;;   AL: character to display
    ;;; Output: None
    ;;; Modified registers: AX, BX, CX, DX, DI
    ;;; Global variables: text-x, text-y, text-cols
    putchar-16
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
    ))

(defparameter *vga-text-code*
    ;;; VGA text output in 64 bit mode.
  `(
    (equ     vga-video-memory  #xb8000)

    ;;; Clear screen.
    clear
    (movzx   ax (text-rows))
    (movzx   dx (text-cols))
    (mul     dx)
    (shr     ax 1)
    (movzx   ecx ax)           ; All screen to be cleared.
    (mov     eax #x0f200f20)   ; Black background, white foreground, space char.
    (mov     edi #xb8000)
    (rep     stosd)
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
    ;;(jne     do-char)
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
    ))

(defparameter *vga-text-data*
  `(
    (db text-rows 25)   ;; Number of rows in text mode.
    (db text-cols 80)   ;; Number of columns in text mode.
    (db text-x 0)       ;; Position x in text mode [0, text-cols)
    (db text-y 0)       ;; Position y in text mode [0, text-rows)
    ))
