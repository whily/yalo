;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Bootloader.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2009-2011 Yujian Zhang

(in-package :cc)

(defparameter *bootloader* 
  `((bits    16)
    (org     #x7c00)

    ;; Just in case any segment registers are touched.
    (xor     ax ax)
    (mov     ds ax)
    (mov     es ax)
    (mov     fs ax)
    (mov     gs ax)
    (mov     ss ax) 

    ;; Load other sectors from floppy disk.
    ;; AL: # of sectors
    (mov     ax (+ #x200 (ceiling (- kernel-end real-start) 512)))
    (mov     bx real-start)   ; ES:BX is destination
    (mov     cx 2)            ; CH: cylinder; CL: sector
    (xor     dx dx)           ; DH: head; DL: drive
    (int     #x13)
    
    (times   480 nop)         ; To be removed once near jmp is available
    (jmp     short real-start)

    ;; Fill up to 510 bytes.
    (times   (- 510 (- $ $$)) db 0)
    
    (dw      #xaa55)           ; Boot sector signature

    ;; Real start up code.
    real-start

    (mov     cx (- end-banner banner))
    (mov     bp banner)
    (call    println)
    (jmp     short read-start)
    (db      banner "Start your journey on yalo v0.0.0!")
    end-banner  

    ;;; REPL: read
    read-start
    (mov     cx (- read repl))
    (mov     bp repl)
    (call    print)
    (jmp     short read) 
    (db      repl ("REPL>")) 
    read 
    (call    getchar)
    (cmp     al 13) 
    (je      eval-start) 
    (call    putchar)
    (call    forward-cursor)
    (jmp     short read)
    
    ;;; REPL: eval
    eval-start

    ;;; REPL: print
    (call    printcrlf)
    (call    printcrlf)
    
    ;;; REPL: loop
    (jmp     short read-start) 

    ;;; Function println. Write a string and start a new line.
    ;;; It is equivalent to calling print twice: first to print the string,
    ;;; and then print CR/LF.
    println
    (call    print)
    (call    printcrlf)
  
    ;;; Function printcrlf. Print crlf only. 
    ;;; Input: None
    ;;; Output: None
    ;;; Modified reisters: AX, BX, CX, and DX.
    printcrlf
    (mov     cx 2)
    (mov     bp crlf)
    (call    print)
    (ret)
    (db      crlf (13 10)) 

    ;;; Function print. Write string to screen.
    ;;; Input:
    ;;;   CX: string length
    ;;;   BP: points to starting address.
    ;;; Output: None
    ;;; Modified registers: AX, BX, and DX.
    print
    (push    cx)
    (call    get-cursor)
    (pop     cx)
    ;; Write the string.
    (mov     ax #x1301)
    (mov     bx #xf)
    (int     #x10) 
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
    ;;;   BH: page number
    ;;;   CX: number of times to writer character.
    ;;; Output: None
    ;;; Modified registers: AX, BH, CX.
    putchar
    (mov     ah #x9)
    (xor     bh bh)                ; Page number (0)
    (mov     cx 1)                 ; Number of times to writer character.
    (int     #x10)
    (ret)

    ;;; Function get-cursor. Get current cursor position, stored in CX and DX.
    ;;; Input: None
    ;;; Output: 
    ;;;   DH: row
    ;;;   DL: column
    ;;;   CH: cursor start line
    ;;;   CL: cursor bottom line
    get-cursor
    (mov     ah 3)
    (xor     bh bh)                ; Page number (0)
    (int     #x10)           
    (ret)

    ;;; Function set-curor. Set cursor position.
    ;;; Input: 
    ;;;   DH: row
    ;;;   DL: column
    ;;; Output: None
    ;;; Modified registers AH, BH.
    set-cursor
    (mov     ah 2)
    (xor     bh bh)                ; Page number (0)
    (int     #x10)
    (ret)

    ;;; Function forward-cursor. Forward cursor position to right by 1 char.
    ;;; Note that wrap and scrolling not considered.
    ;;; Input: None
    ;;; Output: None
    ;;; Modifie registers: AH, BH, CX, DX
    forward-cursor
    (call    get-cursor)
    (inc     dl)
    (call    set-cursor)
    (ret)

    kernel-end))
