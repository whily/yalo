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

(defparameter *memory-16*
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
    ;; Got a good entry. Modify 64 bit length field into end address.
    ;; Subtract 1 from length.
    (es dec  dword (di 8))
    (es sbb  dword (di 12) 0)
    ;; Then add base.
    (es mov  eax (di))
    (es add  (di 8) eax)
    (es mov  eax (di 4))
    (es adc  (di 12) eax)
    (inc     bp)                ; Increase entry count
    (add     di 24)             ; Move to next entry.
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

(defparameter *memory-32*
  `(
    ;;; Memory type constants.
    (equ     memory-usable 1)
    (equ     memory-reserved 2)
    (equ     memory-acpi-reclaimable 3)
    (equ     memory-acpi-nvs 4)
    (equ     memory-bad 5)

    ;;; Get the maximum physical memory size, in bytes. Return the result in (EDX, EAX),
    ;;; where EDX stores the upper 32 bits.
    ;;; This is based on memory map detected with get-memory-map.
    get-memory-size
    (push    esi)
    (push    ecx)
    (push    ebx)
    (push    ebp)
    (mov     ecx (mm-count-addr))     ; Number of entries to process
    (mov     esi mm-entries-addr)
    (xor     edx edx)                 ; EDX:EAX store the maximum physical address.
    (xor     eax eax)
    .start
    (mov     ebp (esi 16))            ; Memory type
    (cmp     ebp memory-usable)       ; TODO: consider ACPI reclaimable?
    (jnz     .skip-entry)
    (mov     ebp (esi 8))             ; Lower 32 bits.
    (mov     ebx (esi 12))            ; Higher 32 bits. Now EBX:EBP store the current physical address.
    (cmp     ebx edx)
    (jb      .skip-entry)
    (ja      .update)
    (cmp     ebp eax)
    (jbe     .skip-entry)
    .update
    (mov     edx ebx)
    (mov     eax ebp)
    .skip-entry
    (add     esi 24)
    (loop    .start)
    .done
    (pop     ebp)
    (pop     ebx)
    (pop     ecx)
    (pop     esi)
    (ret)
   ))
