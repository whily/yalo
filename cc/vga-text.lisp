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

(defparameter *vga-text-constants*
  `(
    ;;; VGA text mode related constants.

    ;;; ASCII characters.
    (equ     ascii-backspace #x08)
    (equ     ascii-linefeed  #x0a)
    (equ     ascii-space     #x20)
    ))

(defparameter *vga-text-16*
  ;;; VGA text output code in 16 bit mode, based on http://wiki.osdev.org/Babystep4
  ;;; We only implement minimal set of features in 16 bit mode as the main intention
  ;;; is only to print error messages.
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
    (movzx   ax (text-rows-16))
    (movzx   dx (text-cols-16))
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
    (call    printlf-16)
    (ret)

    ;;; Function printlf-16. Print crlf only.
    ;;; Input: None
    ;;; Output: None
    ;;; Modified reisters: None
    ;;; Global variables: text-x-16, text-y-16
    printlf-16
    (add     byte (text-y-16) 1)   ; Down one row
    (mov     byte (text-x-16) 0)   ; Back to left
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
    ;;; Global variables: text-x-16, text-y-16, text-cols-16
    putchar-16
    (mov     ah #xf)               ; Attribute: white on black
    (mov     cx ax)                ; Save char/attribute
    (movzx   ax (text-y-16))
    (movzx   dx (text-cols-16))
    (shl     dx 1)                 ; 2 bytes for one character
    (mul     dx)
    (movzx   bx (text-x-16))
    (shl     bx 1)
    (mov     di 0)                 ; Start of video memory
    (add     di ax)                ; Add y offset
    (add     di bx)                ; Add x offset
    (mov     ax cx)                ; Restore char/attribute
    (stosw)                        ; Write char/atribute
    (add     byte (text-x-16) 1)   ; Advance to right
    (ret)

    text-rows-16 (db 25)   ;; Number of rows in text mode.
    text-cols-16 (db 80)   ;; Number of columns in text mode.
    text-x-16 (db 0)       ;; Position x in text mode [0, text-cols-16)
    text-y-16 (db 0)       ;; Position y in text mode [0, text-rows-16)
    ))

(defparameter *vga-text*
    ;;; VGA text output in 64 bit mode.
  `(
    (equ     vga-video-memory  (+ kernel-virtual-base #xb8000))
    ;; Blue background, white foreground, space char.
    (equ     background-filling #x1f201f20)

    ;;; Clear screen.
    ,@(def-fun 'clear nil `(
    (movzx   eax byte (text-rows))
    (movzx   edx byte (text-cols))
    (mul     edx)
    (shr     eax 1)
    (mov     ecx eax)           ; All screen to be cleared.
    (mov     eax background-filling)
    (mov     rdi vga-video-memory)
    (rep     stosd)
    (mov     byte (text-x) 0)
    (mov     byte (text-y) 0)
    ,@(call-function 'set-cursor)))

    ;;; Function println. Write a string and start a new line.
    ;;; It is equivalent to calling print twice: first to print the string,
    ;;; and then print CR/LF.
    ;;; Input:
    ;;;   RSI: points to the starting address of the 0 terminated string.
    ;;; Output: None
    ;;; Modified registers: same as putchar
    ;;; Global variables: same as putchar
    ,@(def-fun 'println nil `(
    ,@(call-function 'print)
    ,@(call-function 'printlf)))

    ;;; Function printlf. Print crlf only.
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: EAX
    ;;; Global variables: text-x, text-y
    ,@(def-fun 'printlf nil `(
    (inc     byte (text-y))     ; Down one row
    (mov     byte (text-x) 0)   ; Back to left
    (mov     al (text-y))
    (cmp     al (text-rows))
    (jb      .done)
    ,@(call-function 'scroll-up)
    .done
    ,@(call-function 'set-cursor)))

    ;;; Function print. Write string to screen.
    ;;; Input:
    ;;;   RDI: points to the starting address of the 0 terminated string.
    ;;; Output: None
    ;;; Modified registers: same as putchar
    ;;; Global variables: same as putchar
    ,@(def-fun 'print nil `(
    (mov     rsi rdi)
    .start
    (lodsb)         ; Load string char to AL
    (cmp     al 0)  ; 0 terminated string like C.
    (je      .done)
    (mov     dil al)
    ,@(call-function 'putchar)
    (jmp     short .start)
    .done
    ))

    ;;; Function putchar. Writer character at cursor position.
    ;;; Input:
    ;;;   DIL: character to display
    ;;; Output: None
    ;;; Modified registers: RAX, RCX, RDX, RDI, R8, R9
    ;;; Global variables: text-x, text-y, text-cols
    ,@(def-fun 'putchar nil `(
    (cmp     dil ascii-linefeed)
    (je      .next-line)
    (mov     r8d #x1f00)           ; Attribute: white on blue
    (mov     r8l dil)              ; Save char/attribute
    (movzx   eax byte (text-y))
    (movzx   edx byte (text-cols))
    (shl     edx 1)                ; 2 bytes for one character
    (mul     edx)
    (movzx   r9d byte (text-x))
    (shl     r9d 1)
    (mov     rdi vga-video-memory) ; Start of video memory
    (add     rdi rax)              ; Add y offset
    (add     rdi r9)              ; Add x offset
    (mov     ax r8w)               ; Restore char/attribute
    (stosw)                        ; Write char/atribute
    (inc     byte (text-x) 1)      ; Advance to right
    (mov     al (text-x))
    (cmp     al (text-cols))
    (je      .next-line)
    (jmp     short .done)
    .next-line
    ,@(call-function 'printlf)
    .done
    ,@(call-function 'set-cursor)))

    ;;; Function scroll-up. Scroll the screen up one line.
    ;;; Input: None
    ;;; Output: None
    ;;; Modified registers: RAX, RCX, RDX, RDI, RSI
    ;;; Global variables: text-x, text-y, text-rows, text-cols
    ,@(def-fun 'scroll-up '(rbx) '(
    ;; Copy rows from (1 to text-rows - 1) to (0 to text-fows - 2)
    (movzx   ebx byte (text-cols))
    (movzx   eax byte (text-rows))
    (dec     eax)           ; Only copy text-rows - 1 lines.
    (mul     ebx)
    (shr     eax 1)         ; 2 bytes per character, write 4 bytes with movsd
    (shl     ebx 1)         ; Number of bytes per line.
    (mov     rdi vga-video-memory)
    (mov     rsi rdi)
    (add     rsi rbx)
    (mov     ecx eax)
    (cld)
    ;; TODO: change to movsq
    (rep     movsd)
    ;; Clear the last line.
    (mov     eax background-filling)
    (shr     ebx 2)         ; As we're using stosd, divide by 4.
    (mov     ecx ebx)
    (rep     stosd)
    ;; Reset the y position to the last line.
    (dec     byte (text-y))))

    ;;; Function backspace-char. Remove one character before cursor,
    ;;; and move cursor one character back.
    ,@(def-fun 'backspace-char nil `(
    ;; If the cursor just follows prompt (e.g. REPL> ), then backspace
    ;; does nothing.
    (cmp     byte (text-x) prompt-length)
    (jbe     .done)
    ;; First go back one character and write a space.
    (dec     byte (text-x))
    (mov     dil ascii-space)
    ,@(call-function 'putchar)
    ;; Go back once more and set cursor.
    (dec     byte (text-x))
    ,@(call-function 'set-cursor)
    .done
    ))

    ;;; Function set-cursor. Set VGA hardware cursor.
    ;;; Input: based on text-xy, text-y
    ;;; Output: None
    ;;; Modified registers: RAX, RCX, RDX, RDI
    ;;; Global variables: text-x, text-y, text-cols
    ;;; Based on http://www.brokenthorn.com/Resources/OSDev10.html
    (equ     crt-index-reg #x3d4)
    (equ     crt-data-reg  #x3d5)
    (equ     cursor-location-high #xe)
    (equ     cursor-location-low  #xf)
    ,@(def-fun 'set-cursor nil `(
    ;; Get current cursor position. Note that we only care about the
    ;; location, not the memory (as in putchar). So following equation
    ;; is used: location = text-x + text-y * text-cols
    (movzx   eax byte (text-y))
    (movzx   edx byte (text-cols))
    (mul     edx)
    (movzx   ecx byte (text-x))
    (add     ecx eax)
    ;; Set low byte index to vga register.
    (mov     al cursor-location-low)
    (mov     dx crt-index-reg)
    (out     dx al)
    (mov     al cl)
    (mov     dx crt-data-reg)
    (out     dx al)
    ;; Set high byte index to vga register.
    (mov     al cursor-location-high)
    (mov     dx crt-index-reg)
    (out     dx al)
    (mov     al ch)
    (mov     dx crt-data-reg)
    (out     dx al)))

    text-rows (db 25)   ;; Number of rows in text mode.
    text-cols (db 80)   ;; Number of columns in text mode.
    text-x (db 0)       ;; Position x in text mode [0, text-cols)
    text-y (db 0)       ;; Position y in text mode [0, text-rows)
    ))
