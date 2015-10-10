;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Bochs VBE Extensions, see http://wiki.osdev.org/BGA
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2012 Yujian Zhang

(in-package :cc)

(defparameter *bga*
  `(
    ;;; BGA constants
    (equ vbe-dispi-ioport-index #x1ce)
    (equ vbe-dispi-ioport-data #x1cf)
    (equ vbe-dispi-index-id 0)
    (equ vbe-dispi-index-xres 1)
    (equ vbe-dispi-index-yres 2)
    (equ vbe-dispi-index-bpp 3)
    (equ vbe-dispi-index-enable 4)
    (equ vbe-dispi-index-bank 5)
    (equ vbe-dispi-index-virt-width 6)
    (equ vbe-dispi-index-virt-height 7)
    (equ vbe-dispi-index-x-offset 8)
    (equ vbe-dispi-index-y-offset 9)
    ;; It seems that VirtualBox only supports BGA up to #xb0c4. See
    ;;   http://www.virtualbox.org/svn/vbox/trunk/src/VBox/Devices/Graphics/BIOS/vbe_display_api.txt
    (equ vbe-dispi-id2 #xb0c2)
    (equ vbe-dispi-id4 #xb0c4)
    (equ vbe-dispi-id5 #xb0c5)
    (equ vbe-dispi-bpp-32 #x20)
    (equ vbe-dispi-enabled 1)
    (equ vbe-dispi-disabled 0)

    ;;; Write index/data to one BGA register.
    ;;; Input:
    ;;;   AL: index value
    ;;;   BX: data value
    ;;; Output: None
    ;;; Modified registers: DX
    bga-write-register
    (mov     dx vbe-dispi-ioport-index)
    (out     dx al)
    (mov     dx vbe-dispi-ioport-data)
    (mov     ax bx)
    (out     dx ax)
    (ret)

    ;;; Read BGA register.
    ;;; Input:
    ;;;   AL: index value
    ;;; Output: in AX
    ;;; Modified registers: DX
    bga-read-register
    (mov     dx vbe-dispi-ioport-index)
    (out     dx al)
    (mov     dx vbe-dispi-ioport-data)
    (in      ax dx)
    (ret)

    ;;; Check whether BGA is available.
    ;;; Input: None
    ;;; Output: CF is set if BGA is unavailable.
    bga-available
    (clc)
    (mov     al vbe-dispi-index-id)
    (call    bga-read-register)
    (cmp     ax vbe-dispi-id2)
    (jb      no-bga)
    (ret)
    no-bga
    (stc)
    (ret)

    ;;; Set BGA mode.
    set-bga-mode
    (mov     al vbe-dispi-index-enable)
    (mov     bx vbe-dispi-disabled)
    (call    bga-write-register)
    (mov     al vbe-dispi-index-xres)
    (mov     bx 800)
    (call    bga-write-register)
    (mov     al vbe-dispi-index-yres)
    (mov     bx 600)
    (call    bga-write-register)
    (mov     al vbe-dispi-index-bpp)
    (mov     bx vbe-dispi-bpp-32)
    (call    bga-write-register)
    (mov     al vbe-dispi-index-enable)
    (mov     bx vbe-dispi-enabled)
    (call    bga-write-register)
    (ret)))
