;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Memory management.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2015 Yujian Zhang

(in-package :cc)

(defparameter *memory*
  `(
    ;;; http://wiki.osdev.org/Detecting_Memory_(x86)
    ;;; http://www.brokenthorn.com/Resources/OSDev17.html

    ;;; Use INT 0x15, EAX=#xe820 BIOS function to get memory map.
    ;;; Modified registers: all registers are trashed except ESI.
    get-memory-map
    ;; String "SMAP".
    (equ     mm-smap #x0534D4150)
    ;; Number of memory map entries.
    (equ     mm-count-addr #xd000)
    ;; Starting address of the memory map entries.
    (equ     mm-entries-addr (+ mm-count-addr 2))
    (push    es)
    (xor     ebx ebx)           ; Set EBX to 0 to start
    (mov     es bx)
    (mov     di mm-entries-addr)
    (xor     bp bp)
    (mov     edx mm-smap)	; Place "SMAP" into edx
    (mov     eax #xe820)
    (es mov  dword (di 20) 1)   ; Force a valid ACPI 3.x entry
    (mov     ecx 24)            ; Ask for 24 bytes
    (int     #x15)
    (jc      .fail)             ; CF is set if the BIO function is not supported.
    (mov     edx mm-smap)	; Some BIOSes may trash this register.
    (cmp     eax edx)           ; On success, EAX must have been rest to "SMAP"
    (jne     .fail)
    (test    ebx ebx)           ; EBX = 0 implies that the list contains only 1 entry.
    (je      .fail)
    (jmp     short .jump-in)
    .loop
    (mov     eax #xe820)        ; EAX, ECX get trashed on every int #x15 call.
    (es mov  dword (di 20) 1)   ; Force a valid ACPI 3.x entry
    (mov     ecx 24)            ; Ask for 24 bytes again.
    (int     #x15)
    (jc      .done)             ; CF means the end of list.
    (mov     edx mm-smap)	; Repair potentially trashed register.
    .jump-in
    (jcxz    .skip-entry)       ; Skip and 0 length entry.
    (cmp     cl, 20)            ; Got a 24 byte ACPI 3.x response?
    (jbe     .no-text)
    (es test byte (di 20) 1)    ; Test whether the "ignore this data" bit is clear or not.
    (je      .skip-entry)
    .no-text
    (es mov  ecx (di 8))        ; Get lower 32 bit of memory region length.
    (es or   ecx (di 12))       ; "Or" it with upper 32 bit to test for zero.
    (jz      .skip-entry)
    (inc     bp)                ; Got a good entry: increase entry count
    (add     di 24)             ;                   and move to next entry.
    .skip-entry
    (test    ebx ebx)           ; If EBX is reset to 0, the list is complete.
    (jne     .loop)
    .done
    (mov     (mm-count-addr) bp)
    (clc)                       ; Clear CF (since .done lable is entered after "jc" instruction).
    (pop     es)
    (ret)
    .fail
    (mov     si .mm-error-message)
    (call    println-16)
    .panic
    (hlt)
    (jmp     short .panic)
    .mm-error-message (db "ERROR: memory map cannot be detected." 0)
   ))
