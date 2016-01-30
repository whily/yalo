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
    (equ     mm-entry-size 24)
    (equ     mm-start-addr 0)
    ;; Offset for the end address. Note that it is the length field after
    ;; the BIOS call, but the value is replaced with end address by this function.
    (equ     mm-end-addr 8)
    (equ     mm-memory-type 16)
    (equ     mm-entries-offset 2)
    ;; Stores Number of memory map entries. As page tables start at #x10000, following
    ;; address allow we to store maximum 20 entries.
    (equ     mm-count-physical-addr #xfe1e)
    ;; Should be located in paging.lisp. Put it here to allow reference by mm-count-max.
    (equ     pml4-base #x10000)
    (equ     mm-count-max (/ (- pml4-base mm-count-physical-addr mm-entries-offset) mm-entry-size))
    ;; Starting address of the memory map entries. The offset relative to mm-physical-addr is
    ;;   mm-entries-offset.
    (equ     mm-entries-physical-addr (+ mm-count-physical-addr mm-entries-offset))
    (push    es)
    (xor     ebx ebx)           ; Set EBX to 0 to start
    (mov     es bx)
    (mov     di mm-entries-physical-addr)
    (xor     bp bp)
    (mov     edx mm-smap)	; Place "SMAP" into edx
    (mov     eax #xe820)
    (es mov  dword (di 20) 1)   ; Force a valid ACPI 3.x entry
    (mov     ecx mm-entry-size)
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
    (mov     ecx mm-entry-size)
    (int     #x15)
    (jc      .done)             ; CF means the end of list.
    (mov     edx mm-smap)	; Repair potentially trashed register.
    .jump-in
    (jcxz    .skip-entry)       ; Skip and 0 length entry.
    (cmp     cl 20)            ; Got a 24 byte ACPI 3.x response?
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
    (add     di mm-entry-size)  ; Move to next entry.
    .skip-entry
    (test    ebx ebx)           ; If EBX is reset to 0, the list is complete.
    (jne     .loop)
    (cmp     bp mm-count-max)   ; If the actual memory count exceeds the limit,
                                ; Memory data will be trashed by page tables.
    (jae     .fail)
    .done
    (mov     (mm-count-physical-addr) bp)
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
    ;;; TODO: for 32 bit operation, we don't need to get memory size as virtual
    ;;; memory management will be done in 64 bit mode. For page setup, we only map the 1st 2 MB for kernel
    ;;; will be sufficient. Code this function in 64 bit mode will be much easier
    ;;; as we can easily handle 64 bit arithmetic in 64 bit mode.
    get-memory-size
    (push    esi)
    (push    ecx)
    (push    ebx)
    (push    ebp)
    (movzx   ecx word (mm-count-physical-addr))     ; Number of entries to process
    (mov     esi mm-entries-physical-addr)
    (xor     edx edx)                 ; EDX:EAX store the maximum physical address.
    (xor     eax eax)
    .start
    (mov     ebp (esi mm-memory-type))
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
    (add     esi mm-entry-size)
    (loop    .start)
    .done
    (pop     ebp)
    (pop     ebx)
    (pop     ecx)
    (pop     esi)
    (ret)
    ))

(defparameter *memory*
  `(;;; ==================== Start of Physical Memory Manager (PMM) ====================
    (equ     page-size-shift 21)      ; Shift for 2 MB
    (equ     mm-count-virtual-addr (+ kernel-virtual-base mm-count-physical-addr))
    (equ     mm-entries-virtual-addr (+ kernel-virtual-base mm-entries-physical-addr))

    ;;; Function pmm-init: initilize the pmm.
    ;;; Input: None
    ;;; Output: None
    ;;;
    ;;; Algorithm:
    ;;; First we mark all page frames as free (set corresponding bit to 0).
    ;;; Then we goes through the memory map.
    ;;;   For every memory entry unusable:
    ;;;     For every page frame whose starting address <= end address of the memory entry:
    ;;;       If the end address of the page frame >= the starting address of the memory entry:
    ;;;         Mark the page frame as used (set the bit to 1).
    ,@(def-fun 'pmm-init nil `(
    (mov     rsi (memory-size-virtual-addr))
    (mov     eax 1)
    (shl     eax page-size-shift)
    (dec     eax)
    (add     rsi rax)
    (shr     rsi page-size-shift)       ; Get ceil(memory-size / (2 ^ page-size-shift))
    (mov     rdi page-frame-bitmap-virtual-addr)
    (push    rdi)
    ,@(call-function 'bitmap-init)
    (pop     rdi)
    ;; Now loop throught every unusable memory entry.
    (movzx   ecx word (mm-count-virtual-addr))     ; Number of entries to process
    (mov     rax mm-entries-virtual-addr) ; Use RAX to loop throught memory map entry.
    .memory-entry
    (mov     edx (rax mm-memory-type))
    (cmp     edx memory-usable)       ; TODO: consider ACPI reclaimable?
    (je      .skip-entry)
    (mov     r8 (rax mm-start-addr))
    (mov     r9 (rax mm-end-addr))
    (xor     r10d r10d)               ; The index to the page frames.
    .page-frame
    (mov     r11 r10)
    (shl     r11 page-size-shift)     ; Starting address of the page frame.
    (cmp     r11 r9)
    ;; Skip the current and remaining page frames, as the memory entry cannot
    ;; overlap with them.
    (ja      .skip-entry)
    (mov     r11 r10)
    (inc     r11)
    (shl     r11 page-size-shift)     ; End address of the page frame.
    (cmp     r11 r8)
    (jb      .next-page-frame)
    ;; Now the unusable memory entry overlaps with the page frame, so
    ;; we set it to used (bit is 1).
    (push    rsi)
    (push    rdx)
    (mov     rsi r10)
    ,@(call-function 'bitmap-set)
    (pop     rdx)
    (pop     rsi)
    .next-page-frame
    (inc     r10)
    (cmp     r10 rsi)
    (jb      .page-frame)
    .skip-entry
    (add     rax mm-entry-size)
    (loop    .memory-entry)
    ))

    ;;; Function pmm-alloc-page-frame: allocate one page frame.
    ;;; Input: None.
    ;;; Output:
    ;;;     RAX: starting physical address of the page frame.
    ;;;          0 if out of memory.
    ,@(def-fun 'pmm-alloc-page-frame nil `(
    (mov     rdi page-frame-bitmap-virtual-addr)
    ,@(call-function 'bitmap-scan)
    (mov     rsi -1)
    (cmp     rax rsi)
    (je      .out-of-memory)
    (mov     rsi rax)
    (push    rax)
    ,@(call-function 'bitmap-set)
    (pop     rax)
    (shl     rax page-size-shift)
    (jmp     short .done)
    .out-of-memory
    (xor     eax eax)
    .done
    ))

    ;;; Function pmm-free-page-frame: free one page frame.
    ;;; Input:
    ;;;     RDI: starting physical address of the page frame.
    ;;; Output: None.
    ,@(def-fun 'pmm-free-page-frame nil `(
    (mov     rsi rdi)
    (shr     rsi page-size-shift)
    (mov     rdi page-frame-bitmap-virtual-addr)
    ,@(call-function 'bitmap-unset)))

    ;;; ===================== End of Physical Memory Manager (PMM) =====================
    ))
