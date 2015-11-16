;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Lisp Assembly Program.
;;;; References:
;;;;     [1] AMD64 Architecture Programmer's Manual Volume 3:
;;;;         General-Purpose and System Instructions
;;;;         Publication No. 24594; Revision: 3.22
;;;;     [2] Intel 64 and IA-32 Architectures Software Developer's Manual
;;;;         Volume 2, Instruction Set Reference, A-Z. June 2015
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2009-2015 Yujian Zhang

(in-package :cc)

;;; CL code in this file will be reused by Ink bootstrapping,
;;; therefore only list data structure is used even though hash table
;;; might be better.

(defun asm (listing)
  "One pass assembler. listing is in the form of LAP as described in
       https://github.com/whily/yalo/blob/master/doc/AssemblyX64.md
   Returns opcodes as a list of bytes.

   To implement the assembler in one pass, for each instruction,
   expressions are tried to be evaluated. If fails (e.g. due to
   unresolvable labels), ((length expr) ? ...) with ? filling up to
   length serve as placeholders. At the end of the pass, those
   placeholders are evaluated and replaced with actual values, with ?
   removed (? is inserted just to make the cursor moving correct)."
  (let (symtab
        code
        (origin 0)
        (length 0)
        (cursor 0)
        (bits 64)
        (addressing 'rel)
        label)
    (dolist (e listing)
      (if (listp e)
          (let* ((e* (cons (car e)
                          (try-eval-values
                           (normalize-local-labels (cdr e) label)
                           cursor origin symtab)))
                 (snippet (case (car e*)
                            (align (let ((n (second e*)))
                                     (unless (member n '(4 8 16 32 64 128 256 512))
                                       (error "asm: unsupported alignment ~A." n))
                                     (repeat-list (mod (- n cursor) n) (list #x90)))) ; #x90 is NOP.
                            (addressing (setf addressing (second e*))
                                        nil)
                            (bits (setf bits (second e*))
                                  nil)
                            (equ (push (cons (second e*) (eval (third e*)))
                                       symtab)
                                 nil)
                            (org (setf origin (second e*)
                                       cursor origin)
                                 nil)
                            (times
                             (repeat-list (eval (second e*))
                                          (encode (nthcdr 2 e*) cursor bits addressing)))
                            (t (encode e* cursor bits addressing)))))
            (setf code (nconc code snippet))
            (incf length (length snippet))
            (incf cursor (length snippet)))
          ;; Labels
          (progn
            (unless (local-label? e)
              (setf label e))
            (let ((nl (normalize-label e label)))
              (when (assoc nl symtab)
                (error "asm: duplicated label ~A." nl))
              (push (cons nl cursor) symtab)))))
    (mapcan
     #'(lambda (c)
         (cond
           ((numberp c) (list c))
           ((eq c '?)   nil)
           ((listp c)   (encode-bytes (eval-final c symtab) (first c)))
           (t           (error "asm: wrong byte for final processing: ~A" c))))
     code)))

(defun pp-asm (listing)
  "Processing the list with asm, and pretty print the bytes with pp-hex."
  (pp-hex (asm listing)))

(defun write-kernel (filename)
  "Output kernel (including bootloader) as an image file with filename."
  (write-image (asm *bootloader*) filename))

(defun write-image (bytes filename)
  "Write a list of bytes to the file with filename."
  (with-open-file (s filename :direction :output :element-type 'unsigned-byte
                     :if-exists :supersede)
    (when s
      (dolist (b bytes)
        (write-byte b s)))))

(defun read-image (filename)
  "Return a list of bytes contained in the file with filename."
  (with-open-file (s filename :element-type 'unsigned-byte)
    (when s
      (let (output)
        (loop for byte = (read-byte s nil)
             while byte do (push byte output))
        (nreverse output)))))

(defun pp-image (filename)
  "Pretty print the image file."
  (pp-hex (read-image filename)))

(defparameter *prefix-mapping*
  ;; As in section 2.1.1 (Instruction Prefixes) of [2].
  `((lock  . #xf0)
    (repne . #xf2) (repnz .#xf2)
    (rep   . #xf3) (repe  . #xf3) (repz . #xf3)
    ;; Handle segment override prefix here for simplicity.
    (cs . #x2e) (ss . #x36) (ds . #x3e) (es . #x26) (fs . #x64) (gs . #x65)
    )
  "Prefix mapping table.")

(defun encode (e* cursor bits addressing)
  "Opcode encoding, including pseudo instructions like db/dw..., resb/resw..."
  (let ((e (if (and (eq (car e*) 'xchg)
                    (member (second e*) '(ax eax rax)))
               (list (car e*) (third e*) (second e*))
               e*)))
    (acond
     ((assoc* (car e) *prefix-mapping*)
      (cons it (encode (cdr e) (1+ cursor) bits addressing)))
     ((assoc* e (x86-64-syntax bits) :test #'equal)
      ;; Instructions with exact match, e.g. instructions without
      ;; operands (like nop, hlt), or special instructions like int 3.
      ;; copy-list is necessary since syntax table is LITERAL.
      (cond
        ((member (car it) '(o16 o32 a16 a32))
         (append (size-prefix (car it) bits) (copy-list (cdr it))))
        ((eq (car it) 'rex.w)
         (append (list (encode-rex 1 0 0 0)) (copy-list (cdr it))))
        (t (copy-list it))))
     ((assoc e (x86-64-syntax bits)
             :test #'(lambda (x y)
                       (and (> (length y) 1)
                            (eq (car x) (car y))
                            (or (and (member (elt x 1) '(al ax eax rax))
                                     (numberp (elt x 2))
                                     (eq (second x) (second y)))
                                (and (eq (car x) 'xchg)
                                     (member (third x) '(ax eax rax))
                                     (eq (third x) (third y)))))))
      ;; Some registers are explicitly given as destination operand,
      ;; e.g. (add al imm8).
      (encode-complex e (canonical-type (car it)) (cdr it) cursor bits addressing))
     ((assoc* e (x86-64-syntax bits)
              :test #'(lambda (x y)
                        (and (member (car x) '(sal sar shl shr))
                             (> (length y) 2)
                             (= (length x) (length y))
                             (eq (car x) (car y))
                             (eq (car (last x)) (car (last y)))
                             (eq (operand-type (car (last x 2))) (car (last y 2)))
                             (if (= (length x) 3)
                                 t
                                 (eq (second x) (second y)))
                             (member (car (last x)) '(1 cl)))))
      ;; Special case for (shl/shr r/m8/16 1/cl).
      (encode-complex (butlast e) (butlast (instruction-type e)) it cursor bits addressing))
     ((cc-instruction? e 'cmov) ; CMOVcc.
      (declare (ignore it))
      (cc-encode e 'cmov cursor bits addressing))
     ((cc-instruction? e 'j)    ; Jcc.
      (declare (ignore it))
      (cc-encode e 'j cursor bits addressing))
     (t
      (declare (ignore it))
      (case (car e)
        ;; Pseudo instructions.
        ((db dw dd dq)
         (mapcan #'(lambda (v)
                     (ecase (car e)
                       (db (cond
                             ((stringp v) (string->bytes v))
                             (t (try-encode-bytes v 1))))
                       (dw (try-encode-bytes v 2))
                       (dd (try-encode-bytes v 4))
                       (dq (try-encode-bytes v 8))))
                 (cdr e)))
        ((resb resw resd resq)
         (let ((n-bytes (ecase (car e)
                          (resb 1)
                          (resw 2)
                          (resd 4)
                          (resq 8))))
           ;; Use 0 as unitialized data, as in NASM.
           (repeat-list (* (second  e) n-bytes) (list 0))))
        ;; Normal instructions.
        (t (match-n-encode e cursor bits addressing)))))))

(defun match-n-encode (e cursor bits addressing &optional (cc-code 0))
  "Match instruction and encode it."
  (multiple-value-bind (type opcode)
      (match-instruction e (instruction-type e) bits)
    (if (and (>= (length opcode) 2) (eq (second opcode) 'rex.w))
        ;; Handle instructions like popcnt where rex.w is the 2nd element in opcode.
        (append (list (first opcode))
                (encode-complex e type (cdr opcode) (1+ cursor) bits addressing cc-code))
        (encode-complex e type opcode cursor bits addressing cc-code))))

(defun encode-complex (instruction type opcode cursor bits addressing
                       &optional (cc-code 0))
  "Encode instruction (with optional rex prefix). Other prefixes like
lock are directly handled in encode()."
  (let* (rex-set ; Possibly containing a subset of {w r x b}.
         (dummy (when (eq (car opcode) 'rex.w)
                  (push 'w rex-set)))
         (prefix (when (member (car opcode) '(o16 o32 a16 a32))
                   (size-prefix (car opcode) bits)))
         (opcode* (if (member (car opcode) '(rex.w o16 o32 a16 a32))
                      (cdr opcode)
                      opcode))
         (encoded-len (length prefix)) ; Tracking for (R)IP relative encoding.
         disp32 ; Pointer for RIP Relative Addressing.
         (remaining
          (mapcan
           #'(lambda (on)
               (let ((x
                      (cond
                        ((numberp on) (list on))
                        ((listp on)
                         (ecase (car on)
                           (+ (ecase (caddr on)
                                (r (multiple-value-bind (regi rex)
                                       (reg->int (second instruction))
                                     (when rex
                                       (if (eq rex 'p)
                                           (push 'p rex-set)
                                           (push 'b rex-set)))
                                     (list (+ (cadr on)
                                              regi))))
                                (cc (list (+ (cadr on) cc-code)))))))
                        (t
                         (ecase on
                           ((ib iw id io)
                            (try-encode-bytes
                             (if (equal type '(mov r64 -imm32))
                                 (third instruction)
                                 (instruction-value instruction type
                                                    (on->in on)))
                             (on-length on)))
                           ((cb cw cd co)
                            (try-encode-bytes
                             `(- ,(instruction-value instruction type (on->in on))
                                 ,(+ cursor encoded-len (on-length on)))
                             (on-length on)))
                           ((/0 /1 /2 /3 /4 /5 /6 /7 /r)
                            (let* ((mem (instruction-value instruction type
                                                           (find-r/m instruction type)))
                                   (mem* mem)
                                   (addressing*
                                    (if (and (listp mem) (> (length mem) 1)
                                             (member (car mem) '(abs rel)))
                                        ;; Explicit abs/rel override.
                                        (progn
                                          (setf mem* (cdr mem))
                                          (car mem))
                                        addressing)))
                              (multiple-value-bind (mod-sib-disp rex-set*)
                                  (encode-r/m-sib-disp
                                   mem*
                                   (if (eq on '/r)
                                       (instruction-value instruction type
                                                          (find-reg instruction type))
                                       on)
                                   bits addressing*)
                                (setf rex-set (append rex-set rex-set*))
                                (setf disp32 (mark-rip-relative mod-sib-disp bits))
                                mod-sib-disp))))))))
                 (incf encoded-len (length x))
                 x))
           opcode*)))
    (declare (ignore dummy))
    (when (and rex-set (/= bits 64))
      (error "Instruction ~A only supported in 64-bit mode." instruction))
    (when disp32
      (process-rip-relative disp32 cursor encoded-len rex-set))
    ;; REX prefix should precede immdiately the opcode, i.e. other
    ;; prefix should precede REX prefix (see section 2.2.1 (REX Prefixes) of [2]).
    (append prefix
            (if (null rex-set)
                nil
                (list (encode-rex (if (member 'w rex-set) 1 0)
                                  (if (member 'r rex-set) 1 0)
                                  (if (member 'x rex-set) 1 0)
                                  (if (member 'b rex-set) 1 0))))
            remaining)))

(defun mark-rip-relative (mod-sib-disp bits)
  "For RIP Relative Addressing, return the list pointing to the field disp32.
Actual modification is done by process-rip-relative."
  (when (and (= bits 64) (rip-relative? (car mod-sib-disp)))
    (cdr mod-sib-disp)))

(defun process-rip-relative (disp32 cursor encoded-len rex-set)
  "For RIP Relative Addressing, handle the encoding of disp32 (modify
mod-sib-disp if needed)."
  (let* ((next-rip (+ cursor encoded-len
                      (if rex-set 1 0)))
         (mem (if (numberp (first disp32)) ; TODO: handle in r/m-values-64 to avoid this case.
                  (error "process-rip-relative(): RIP relative addressing cannot be used on absolute address.")
                  (second (first disp32))))
         (new-disp (try-encode-bytes `(- ,mem ,next-rip) 4)))
    ;; In-place replacement.
    (setf (first disp32)  (first new-disp))
    (setf (second disp32) (second new-disp))
    (setf (third disp32)  (third new-disp))
    (setf (fourth disp32) (fourth new-disp))))

(defun size-prefix (op bits)
  "Handles operand/address-size override prefix o16/o32 and
  a16/a32. Returns nil if no prefix is needed, otherwise corresponding
  prefix #x66 or #x67."
  (let* ((s (str op))
         (st (symb (subseq s 0 1)))
         (sbit (read-from-string (subseq s 1 3))))
    (if (or (= sbit bits) (and (eq op 'o32) (member bits '(32 64))))
         nil
        (list (ecase st
                (o #x66)
                (a #x67))))))

(defun find-r/m (instruction type)
  "Return the r/m contained in type."
  (aif (member* '(m r/m8 r/m16 r/m32 r/m64 r8 r16 r32 r64) type)
       it
       (error "No r/m operand in ~A~%!" instruction)))

(defun find-reg (instruction type)
  "Return the reg contained in type."
  (aif (member* '(sreg cr0-cr7 r8 r16 r32 r64) type)
       it
       (error "No (s)reg operand in ~A~%" instruction)))

(defun encode-r/m-sib-disp (r/m reg/opcode bits addressing)
  "Return 2 values:
     - Encode ModR/M, SIB byte (if any) and displacement (if any).
     - A list of (w r x b) if present."
  (let* (rex-set
         (r/o (case reg/opcode
                ((/0 /1 /2 /3 /4 /5 /6 /7)
                 (- (char-code (elt (symbol-name reg/opcode) 1)) 48))
                (t (case (operand-type reg/opcode)
                     (sreg (sreg->int reg/opcode))
                     (cr0-cr7 (cr0-cr7->int reg/opcode))
                     (t    (multiple-value-bind (regi rex)
                               (reg->int reg/opcode)
                             (when rex (push (ecase rex
                                               (p 'p)
                                               (e 'r))
                                             rex-set))
                             regi)))))))
    (multiple-value-bind (mod rm sib disp disp-length rex-set*)
        (r/m-values r/m bits addressing)
      (values (append (list (encode-modr/m mod rm r/o))
                      (when sib (list sib))
                      (when disp (try-encode-bytes disp disp-length)))
              (append rex-set rex-set*)))))

(defun r/m-values (r/m bits addressing)
  "Return values: mod, r/m for encoding, sib, disp, length of disp
in bytes, and rex-set.

   Note
     1. If sib is not needed, return nil.
     2. If disp is not needed, return nil as disp and disp-length
        could be arbitrary."
  (ecase (operand-type r/m)
    ((r8 r16 r32 r64)
     (multiple-value-bind (regi rex)
         (reg->int r/m)
       (values #b11 regi nil nil 0 (if rex
                                       (list (ecase rex
                                               (p 'p)
                                               (e 'b)))
                                       nil))))
    (m (ecase bits ;; FIXME: should be directly related to address mode.
         (16 (r/m-values-16 r/m))
         (32 (r/m-values-32 r/m))
         (64 (r/m-values-64 r/m addressing))))))

(defun r/m-values-16 (r/m)
  (if (equal r/m '(bp))  ; Special handling of (bp)
      (values 1 #b110 nil 0 1 nil)
      (let ((type (mapcar #'operand-type r/m)))
        (if (and (= (length r/m) 1) ; Special handling of (disp16)
                 (member* '(imm8 imm16 label) type))
            (values 0 #b110 nil (car r/m) 2 nil)
            (let* ((mod (cond
                          ((member 'imm8 type) 1)
                          ((member 'imm16 type) 2)
                          (t 0)))
                   (disp (ecase mod
                           (1 (instruction-value r/m type 'imm8))
                           (2 (instruction-value r/m type 'imm16))
                           (0 nil)))
                   (rm (cond
                         ((and (member 'bx r/m) (member 'si r/m)) #b000)
                         ((and (member 'bx r/m) (member 'di r/m)) #b001)
                         ((and (member 'bp r/m) (member 'si r/m)) #b010)
                         ((and (member 'bp r/m) (member 'di r/m)) #b011)
                         ((member 'si r/m) #b100)
                         ((member 'di r/m) #b101)
                         ((member 'bp r/m) #b110)
                         ((member 'bx r/m) #b111)
                         (t (error "Incorrect memory addressing: ~A~%"
                                   r/m)))))
              (values mod rm nil disp mod nil))))))

(defun r/m-values-32 (r/m)
  (cond
    ((equal r/m '(ebp))  ; Special handling of (ebp)
     (values 1 #b101 nil 0 1 nil))
    ((equal r/m '(esp))  ; Special handling of (esp)
     (values 0 #b100  (encode-sib 0 #b100 4) nil 0 nil))
    (t
     (let ((type (mapcar #'operand-type r/m)))
       (cond
         ((and (= (length r/m) 1) ; Special handling of (disp32)
               (member* '(imm8 imm16 imm32 label) type))
          (values 0 #b101 nil (car r/m) 4 nil))
         ((and (= (length r/m) 2) (member 'esp r/m) (member 'imm8 type))
          ;; Special handling of (esp + disp8)
          (values 1 #b100  (encode-sib 0 #b100 4)
                  (instruction-value r/m type 'imm8) 1 nil))
         ((and (= (length r/m) 2) (member 'esp r/m)
               (member* '(imm16 imm32) type))
          ;; Special handling of (esp + disp32)
          (values 2 #b100  (encode-sib 0 #b100 4)
                  (instruction-value r/m type (member* '(imm16 imm32) type)) 4
                  nil))
         (t (let* ((mod (cond
                          ((member 'imm8 type) 1)
                          ((member* '(imm16 imm32) type) 2)
                          (t 0)))
                   (disp (ecase mod
                           (1 (instruction-value r/m type 'imm8))
                           (2 (instruction-value r/m type
                                                 (member* '(imm16 imm32) type)))
                           (0 nil)))
                   (disp-length
                    (if (= mod 2)
                        4
                        mod))
                   (sib (cond
                          ((some #'scaled-index? r/m)
                           (let* ((si (find-if #'scaled-index? r/m))
                                  (sis (str si))
                                  (scale (floor (log (read-from-string
                                                      (subseq sis 4 5))
                                                     2)))
                                  (index (reg->int (symb (subseq sis 0 3))))
                                  (base-reg (find-if #'r32? r/m))
                                  (base (if base-reg (reg->int base-reg) 5)))
                             (unless base-reg
                               ;; Special case of (scaled-index + disp32)
                               (setf mod 0
                                     disp (instruction-value
                                           r/m type
                                           (member* '(imm8 imm16 imm32) type))
                                     disp-length 4))
                             (encode-sib scale index base)))
                          ((= (count-if #'r32? r/m) 2)
                           (let* ((scale 0)
                                  (base (reg->int (find-if #'r32? r/m)))
                                  (index (reg->int (find-if #'r32? r/m
                                                            :from-end t))))
                             (encode-sib scale index base)))
                          (t nil)))
                   (rm (if sib
                           #b100
                           (reg->int (member* '(eax ecx edx ebx ebp esi edi esp
                                                ax cx dx bp bp si di sp)
                                              r/m)))))
              (values mod rm sib disp disp-length nil))))))))

(defun r/m-values-64 (r/m addressing)
  (cond
    ((equal r/m '(rbp))  ; Special handling of (rbp)
     (values 1 #b101 nil 0 1 nil))
    ((equal r/m '(rsp))  ; Special handling of (rsp)
     (values 0 #b100  (encode-sib 0 #b100 4) nil 0 nil))
    (t
     (let ((type (mapcar #'operand-type r/m)))
       (cond
         ((and (= (length r/m) 1)
               (member* '(imm8 imm16 imm32 label) type))
          ;; Special handling of (disp32).  In 64 bit mode,
          ;; RIP-Relative Addressing is used when mod=00 and r/m=101
          ;; (Table 1-16 of [1]). For abslute encoding, use
          ;; (mod=00, r/m=100, with SIB byte).
          (ecase addressing
            (abs (values 0 #b100 (encode-sib 0 #b100 #b101) (car r/m) 4 nil))
            ;; Not encoded yet. As we need to know the cursor and instruction length,
            ;; we will let higher level function (`encode-complex`) to actually encode
            ;; as it has more context e.g. the prefix to determine instruction length.
            (rel (values 0 #b101 nil (car r/m) 4 nil))))
         ((and (= (length r/m) 2) (member 'rsp r/m) (member 'imm8 type))
          ;; Special handling of (rsp + disp8)
          (values 1 #b100 (encode-sib 0 #b100 4)
                  (instruction-value r/m type 'imm8) 1 nil))
         ((and (= (length r/m) 2) (member 'rsp r/m)
               (member* '(imm16 imm32) type))
          ;; Special handling of (rsp + disp32)
          (values 2 #b100 (encode-sib 0 #b100 4)
                  (instruction-value r/m type (member* '(imm16 imm32) type)) 4
                  nil))
         (t (let* ((mod (cond
                          ((member 'imm8 type) 1)
                          ((member* '(imm16 imm32) type) 2)
                          (t 0)))
                   (disp (ecase mod
                           (1 (instruction-value r/m type 'imm8))
                           (2 (instruction-value r/m type
                                                 (member* '(imm16 imm32) type)))
                           (0 nil)))
                   (disp-length
                    (if (= mod 2)
                        4
                        mod))
                   (sib (cond
                          ((some #'scaled-index? r/m)
                           (let* ((si (find-if #'scaled-index? r/m))
                                  (sis (str si))
                                  (scale (floor (log (read-from-string
                                                      (subseq sis 4 5))
                                                     2)))
                                  (index (reg->int (symb (subseq sis 0 3))))
                                  (base-reg (find-if #'r64? r/m))
                                  (base (if base-reg (reg->int base-reg) 5)))
                             (unless base-reg
                               ;; Special case of (scaled-index + disp32)
                               (setf mod 0
                                     disp (instruction-value
                                           r/m type
                                           (member* '(imm8 imm16 imm32) type))
                                     disp-length 4))
                             (encode-sib scale index base)))
                          ((= (count-if #'r64? r/m) 2)
                           (let* ((scale 0)
                                  (base (reg->int (find-if #'r64? r/m)))
                                  (index (reg->int (find-if #'r64? r/m
                                                            :from-end t))))
                             (encode-sib scale index base)))
                          (t nil)))
                   (rm (if sib
                           #b100
                           (reg->int (member* '(rax rcx rdx rbx rbp rsi rdi rsp r8 r9 r10 r11 r12 r13 r14 r15
                                                eax ecx edx ebx ebp esi edi esp r8d r9d r10d r11d r12d r13d r14d r15d
                                                ax cx dx bx bp si di sp r8w r9w r10w r11w r12w r13w r14w r15w)
                                              r/m)))))
              (values mod rm sib disp disp-length nil))))))))

(defun try-encode-bytes (x length)
  "If x is evaluable, run encode-bytes.
   Otherwise return the placeholder list."
  (if (evaluable? x)
      (encode-bytes (eval x) length)
      (cons `(,length ,x) (repeat-element (1- length) '?))))

(defun encode-bytes (x length)
  "Encode byte, word, doubleword, quadword into bytes in
little-endian. Length is the number of bytes to convert to. X is first
converted from signed to unsigned."
  (ecase length
    ((1 2 4 8)
     (do* ((e (1- length) (1- e))
           (y (signed->unsigned x length))
           (r (floor y (expt 256 e)) (floor y (expt 256 e)))
           z)
          ((zerop e) (push (mod y 256) z) z)
       (decf y (* r (expt 256 e)))
       (push r z)))))

(defun try-eval-values (ops cursor origin symtab)
  "Run lookup-value. For each element, evaluate it if possible."
  (mapcar #'(lambda (v)
              (let ((v* (lookup-value v cursor origin symtab)))
                (if (evaluable? v*) (eval v*) v*)))
          ops))

(defun evaluable? (e)
  "Returns T is expression e is evaluable."
  (cond
    ((atom e) (numberp e))
    ((null e) t)
    ((atom (car e)) (and (member (car e) '(+ - ceiling))
                         (every #'evaluable? (cdr e))))
    (t nil)))

(defun eval-final (revisit symtab)
  "Final evaluation of the revisit."
  (car (try-eval-values (cdr revisit) -1 -1 symtab)))

(defun on->in (on)
  "Maps opcode notation (e.g. ib, iw) to instruction notation (e.g. imm8)."
  (ecase on
    ((ib cb) 'imm8)
    ((iw cw) 'imm16)
    ((id cd) 'imm32)
    ((io co) 'imm64)))

(defun on-length (on)
  "Return lengths (in terms of bytes) for opcode notation (e.g. ib, iw)."
  (ecase on
    ((ib cb) 1)
    ((iw cw) 2)
    ((id cd) 4)
    ((io co) 8)))

(defun lookup-value (ops cursor origin symtab)
  "Replace special variables and labels with values if possible."
  (cond
    ((atom ops) (operand->value ops cursor origin symtab))
    ((null ops) nil)
    ((atom (car ops))
     (cons (car ops)
           (mapcar #'(lambda (co)
                       (lookup-value co cursor origin symtab))
                   (cdr ops))))
    (t (cons (lookup-value (car ops) cursor origin symtab)
             (lookup-value (cdr ops) cursor origin symtab)))))

(defun operand->value (operand cursor origin symtab)
  "For labels, returns its value if possible.
   For special variables, returns its value.
   For all other stuff, just return it."
  (cond
    ((eq operand '$) cursor)
    ((eq operand '$$) origin)
    ((eq (operand-type operand) 'label)
     (aif (assoc* operand symtab)
          it
          operand))
    (t operand)))

(defun instruction-value (instruction type name)
  "Get the value (in instruction) corresponding to the name (in type)."
  (assoc* name (mapcar #'cons type instruction)))

(defun match-instruction (instruction type bits)
  "Returns values of (type opcode).
   In the first run, when the type does not appear in syntax table,
     try to match immediate data with register length."
  ;; Special handling for (mov r64 imm32) if imm32 < 0.
  (if (and (eq (car instruction) 'mov)
           (= (length instruction) 3)
           (eq (second type) 'r64)
           (numberp (third instruction))
           (<= (- (expt 2 31)) (third instruction) -1))
      (values (list 'mov 'r64 'imm32) (list 'rex.w #xc7 '/0 'id))
      (aif (assoc-x86-64-opcode type bits)
           (values (canonical-type (car it)) (copy-list (cdr it)))
           (error "match-instruction: unsupported instruction ~A" instruction))))

(defun canonical-type (type)
  "Return the canonical form of the type."
  (mapcar #'(lambda (x) (if (listp x) (car x) x)) type))

(defun assoc-x86-64-opcode (type bits)
  "Returns a associated opcode based on x86-64 syntax."
  (assoc type (x86-64-syntax bits)
         :test #'(lambda (x y)
                   (every #'(lambda (a b)
                              (if (listp b)
                                  (sub-type-list? a b)
                                  (sub-type? a b)))
                          x y))))

(defun sub-type? (a b)
  "Returns T if `a` is a sub-type of `b`."
  (or (eq a b)
      (case a
        (imm8  (member b '(imm16 imm32 imm64)))
        (imm16 (member b '(imm32 imm64)))
        (imm32 (eq     b 'imm64))
        (r8    (eq     b 'r/m8))
        (r16   (eq     b 'r/m16))
        (r32   (eq     b 'r/m32))
        (r64   (eq     b 'r/m64))
        (m     (member b '(r/m8 r/m16 r/m32 r/m64))))))

(defun sub-type-list? (a list)
  "Returns T if `a` is a sub-type of at least one element of `list`."
  (some #'(lambda (x) (sub-type? a x)) list))

(defun signed->unsigned (value length)
  "Change value from signed to unsigned."
  (if (>= value 0)
      value
      (ecase length
        ((1 2 4 8) (+ (expt 256 length) value)))))

(defun encode-modr/m (mod r/m reg/opcode)
  "Encode ModR/M byte."
  (+ (* mod #b1000000) (* reg/opcode #b1000) r/m))

(defun rip-relative? (modr/m)
  "Returns T if ModR/M byte indicates RIP relative addressing."
  (= (logand modr/m #b11000111) #b00000101))

(defun encode-sib (scale index base)
  "Encode SIB byte."
  (+ (* scale #b1000000) (* index #b1000) base))

(defun encode-rex (w r x b)
  "Encode rex prefix."
  (+ #b01000000 (* w #b1000) (* r #b100) (* x #b10) b))

(defun instruction-type (instruction)
  "Returns the instruction type for encoding."
  (cons (car instruction)
        (mapcar #'operand-type (cdr instruction))))

(defun operand-type (operand)
  "Returns operand type."
  (cond
    ((numberp operand)
     (cond
       ((<= (- (expt 2 7))  operand (1- (expt 2 8)))  'imm8)
       ((<= (- (expt 2 15)) operand (1- (expt 2 16))) 'imm16)
       ((<= (- (expt 2 31)) operand (1- (expt 2 32))) 'imm32)
       ((<= (- (expt 2 63)) operand (1- (expt 2 64))) 'imm64)
       (t (error "Invalid operand: ~A" operand))))
    ((listp operand)
     (case (car operand)
       ((+ -) 'imm)
       (t     'm)))
    (t
     (case operand
       ((al cl dl bl ah ch dh bh bpl spl dil sil
            r8l r9l r10l r11l r12l r13l r14l r15l)  'r8)
       ((ax cx dx bx sp bp si di
            r8w r9w r10w r11w r12w r13w r14w r15w)  'r16)
       ((eax ecx edx ebx esp ebp esi edi
             r8d r9d r10d r11d r12d r13d r14d r15d) 'r32)
       ((rax rcx rdx rbx rsp rbp rsi rdi
             r8 r9 r10 r11 r12 r13 r14 r15)         'r64)
       ((cs ds es ss fs gs)                         'sreg)
       ((cr0 cr2 cr3 cr4)                           'cr0-cr7)
       ((near short byte word dword qword)           operand)
       (t                                           'label)))))

(defun r32? (op)
  "Returns T if operand op is a 32-bit general purpose register."
  (eq (operand-type op) 'r32))

(defun r64? (op)
  "Returns T if operand op is a 64-bit general purpose register."
  (eq (operand-type op) 'r64))

(defun reg->int (reg)
  "Returns values of:
     - the integer representation for register when encoding
       ModR/M byte.
     - whether extension (e.g. rex.b) is needed:
       * nil: no REX extension
       * p: REX extension is present but no field (e.g. rex.b or rex.r) is used.
            Can be used to encode SIL, DIL, SPL, BPL.
       * e: REX extension is used and one field will be set."
  (ecase reg
    ((al ax eax rax mm0 xmm0) (values 0 nil))
    ((cl cx ecx rcx mm1 xmm1) (values 1 nil))
    ((dl dx edx rdx mm2 xmm2) (values 2 nil))
    ((bl bx ebx rbx mm3 xmm3) (values 3 nil))
    ((ah sp esp rsp mm4 xmm4) (values 4 nil))
    ((ch bp ebp rbp mm5 xmm5) (values 5 nil))
    ((dh si esi rsi mm6 xmm6) (values 6 nil))
    ((bh di edi rdi mm7 xmm7) (values 7 nil))
    (spl                      (values 4 'p))
    (bpl                      (values 5 'p))
    (sil                      (values 6 'p))
    (dil                      (values 7 'p))
    ((r8l  r8w  r8d  r8)      (values 0 'e))
    ((r9l  r9w  r9d  r9)      (values 1 'e))
    ((r10l r10w r10d r10)     (values 2 'e))
    ((r11l r11w r11d r11)     (values 3 'e))
    ((r12l r12w r12d r12)     (values 4 'e))
    ((r13l r13w r13d r13)     (values 5 'e))
    ((r14l r14w r14d r14)     (values 6 'e))
    ((r15l r15w r15d r15)     (values 7 'e))))

(defun sreg->int (sreg)
  "Returns the integer representation for segment register when
encoding ModR/M byte."
  (ecase sreg
    (es 0)
    (cs 1)
    (ss 2)
    (ds 3)
    (fs 4)
    (gs 5)))

(defun cr0-cr7->int (cr0-cr7)
  "Returns the integer representation for control registers when
encoding ModR/M byte."
  (ecase cr0-cr7
    (cr0 0)
    (cr2 2)
    (cr3 3)
    (cr4 4)))

(defun cc->int (cc)
  "Returns the integer representing conditional codes (cc) used by
cmovcc and jcc. Returns -1 if cc is not a valid value."
  (case cc
    (o          0)  (no         1)  ((b c nae)  2)  ((ae nb nc) 3)
    ((e z)      4)  ((ne nz)    5)  ((be na)    6)  ((a nbe)    7)
    (s          8)  (ns         9)  ((p pe)     10) ((np po)    11)
    ((l nge)    12) ((ge nl)    13) ((le ng)    14) ((g nle)    15)
    (t -1)))

(defun cc-instruction? (e prefix)
  "Returns T if instruction e contains instruction code with prefix."
  (let* ((mnemonic (str (car e)))
         (prefix-s (str prefix))
         (prefix-len (length prefix-s)))
    (and (>= (length mnemonic) (1+ prefix-len))
         (string= (subseq mnemonic 0 prefix-len) prefix-s)
         (>= (cc->int (symb (subseq mnemonic prefix-len))) 0))))

(defun cc-encode (e prefix cursor bits addressing)
  "Encode instructions with conditional codes."
  (let* ((cc (symb (subseq (str (car e)) (length (str prefix)))))
         (cc-code (cc->int cc))
         (e* (cons (symb prefix 'cc) (cdr e))))
    (match-n-encode e* cursor bits addressing cc-code)))

(defun string->bytes (s)
  (map 'list #'char-code s))

(defun local-label? (l)
  "Returns T is L is a local label (starting with period)."
  (and (symbolp l) (eq (elt (symbol-name l) 0) #\.)))

(defun normalize-label (e label)
  "If e is a local label, prefix it with label. Otherwise, return as
is."
  (if (local-label? e)
      (symb label e)
      e))

(defun normalize-local-labels (e label)
  "For all local labels in E, normalize it by prefixing it with
current label."
  (cond
    ((null e) e)
    ((atom (car e)) (cons (normalize-label (car e) label)
                          (normalize-local-labels (cdr e) label)))
    (t (cons (normalize-local-labels (car e) label)
             (normalize-local-labels (cdr e) label)))))

(defun member* (options list)
  "Returns the first element of OPTIONS in list. NIL if none found."
  (dolist (o options)
    (when (member o list)
      (return o))))

(defun scaled-index? (v)
  "Returns T if v is a scaled index (e.g. eax*2)."
  ;; TODO: using regular expression when available.
  (let ((s (str v)))
    (and (= (length s) 5)
         (member (read-from-string (subseq s 4 5)) '(2 4 8))
         (char= (elt s 3) #\*)
         (member (symb (subseq s 0 3)) '(eax ecx edx ebx ebp esi edi rax rcx rdx rbx rbp rsi rdi)))))
