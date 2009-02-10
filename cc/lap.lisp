;;;; -*- Mode: Lisp -*-
;;;; Author: 
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Lisp Assembly Program.
;;;; License: 
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html

(in-package :cc)

;;; CL code in this file will be reused by Ink bootstrapping,
;;; therefore only list data structure is used even though hash table
;;; might be better.

(defun asm (listing)
  "One pass assembler. listing is in the form of LAP as described in
       http://code.google.com/p/yalo/wiki/AssemblySyntax
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
        label)
    (dolist (e listing)
      (if (listp e) 
          (let* ((e* (cons (car e) 
                          (try-eval-values 
                           (normalize-local-labels (cdr e) label)
                           cursor origin symtab)))
                 (snippet (case (car e*)
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
                                          (encode (nthcdr 2 e*) cursor bits)))
                            (t (encode e* cursor bits)))))
            (when (and (= (length e) 3) (member (car e) '(db dw dd dq)))
              (push (cons (second e) cursor) symtab))
            (setf code (nconc code snippet))
            (incf length (length snippet))
            (setf cursor (+ origin length)))
          ;; Labels
          (progn
            (unless (local-label? e)
              (setf label e))
            (push (cons (normalize-label e label) cursor) symtab))))
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

(defun arith-syntax-1 (mnemonic)
  "Return syntax table for arithmetic operations: add/and/cmp/or/sub/xor."
  (let ((base   ; Base opcode for operation on r/m8 r8.
         (ecase mnemonic
           (add #x00) (and #x20) (cmp #x38)
           (or  #x08) (sub #x28) (xor #x30)))
        (opcode ; Opcode used when one operand is immediate.
         (ecase mnemonic
           (add '/0) (and '/4) (cmp '/7)
           (or  '/1) (sub '/5) (xor '/6))))
    `(((,mnemonic al imm8)                   . (,(+ base #x04) ib))
      ((,mnemonic ax (imm16 imm8))           . (o16 ,(+ base #x05) iw))
      ((,mnemonic eax (imm32 imm16 imm8))    . (o32 ,(+ base #x05) id))
      ((,mnemonic (r/m8 r8) imm8)            . (#x80 ,opcode ib))
      ((,mnemonic byte m imm8)               . (#x80 ,opcode ib))
      ((,mnemonic (r/m16 r16 m) imm16)       . (o16 #x81 ,opcode iw))
      ((,mnemonic (r/m32 r32 m) imm32)       . (o32 #x81 ,opcode id))
      ((,mnemonic (r/m16 r16) imm8)          . (o16 #x83 ,opcode ib))
      ((,mnemonic (r/m32 r32) imm8)          . (o32 #x83 ,opcode ib))
      ((,mnemonic word m imm8)               . (o16 #x83 ,opcode ib))
      ((,mnemonic dword m imm8)              . (o32 #x83 ,opcode ib))
      ((,mnemonic (r/m8 r8 m) r8)            . (,base /r))
      ((,mnemonic (r/m16 r16 m) r16)         . (o16 ,(+ base #x01) /r))
      ((,mnemonic (r/m32 r32 m) r32)         . (o32 ,(+ base #x01) /r))
      ((,mnemonic r8 (r/m8 r8 m))            . (,(+ base #x02) /r))
      ((,mnemonic r16 (r/m16 r16 m))         . (o16 ,(+ base #x03) /r))
      ((,mnemonic r32 (r/m32 r32 m))         . (o32 ,(+ base #x03) /r)))))

(defun arith-syntax-2 (mnemonic)
  "Return syntax table for arithmetic operations: div/mul/neg/not."
  (let ((opcode (ecase mnemonic
                  (div '/6) (mul '/4) (neg '/3) (not '/2))))
    `(((,mnemonic (r/m8 r8))                 . (#xf6 ,opcode))
      ((,mnemonic byte m)                    . (#xf6 ,opcode))
      ((,mnemonic (r/m16 r16))               . (#xf7 ,opcode))
      ((,mnemonic word m)                    . (#xf7 ,opcode)))))

(defun shift-syntax (mnemonic)
  "Return syntax table for shift operations: shl/shr."
  (let ((opcode (ecase mnemonic
                  (shl '/4) (shr '/5))))
    `(((,mnemonic r8 1)                      . (#xd0 ,opcode))
      ((,mnemonic byte m 1)                  . (#xd0 ,opcode))
      ((,mnemonic r8 cl)                     . (#xd2 ,opcode))
      ((,mnemonic byte m cl)                 . (#xd2 ,opcode))
      ((,mnemonic r8 imm8)                   . (#xc0 ,opcode ib))
      ((,mnemonic byte m imm8)               . (#xc0 ,opcode ib))
      ((,mnemonic r16 1)                     . (#xd1 ,opcode))
      ((,mnemonic word m 1)                  . (#xd1 ,opcode))
      ((,mnemonic r16 cl)                    . (#xd3 ,opcode))
      ((,mnemonic word m cl)                 . (#xd3 ,opcode))
      ((,mnemonic r16 imm8)                  . (#xc1 ,opcode ib))
      ((,mnemonic word m imm8)               . (#xc1 ,opcode ib)))))

;;; Following are syntax tables for x86-64. For each entry, 1st part
;;; is the instruction type, 2nd part is the corresponding opcode.
;;; Note that for the 1st part, list may be used for the operand to
;;; match the type (e.g. imm8 converted to imm16). Note that the
;;; canonical form should be placed first (e.g. if the operand type
;;; should be imm16, place it as the car of the list).
;;;
;;;  For details,
;;;    refer to http://code.google.com/p/yalo/wiki/AssemblyX64Overview")

(defparameter *x86-64-syntax-common*
  `(,@(arith-syntax-1 'add)
    ,@(arith-syntax-1 'and)  
    ((clc)                                   . (#xf8))
    ((cld)                                   . (#xfc))
    ((cli)                                   . (#xfa))
    ,@(arith-syntax-1 'cmp)
    ,@(arith-syntax-2 'div)
    ((hlt)                                   . (#xf4))
    ((in     al imm8)                        . (#xe4 ib)) 
    ((in     ax imm8)                        . (#xe5 ib))
    ((in     al dx)                          . (#xec))
    ((in     ax dx)                          . (#xed))
    ((int    3)                              . (#xcc))
    ((int    imm8)                           . (#xcd ib))
    ((jmp    short (imm8 label imm16))       . (#xeb rb))
    ((lldt   (r/m16 r16 m))                  . (#x0f #x00 /2))
    ((lodsb)                                 . (#xac))
    ((lodsw)                                 . (#xad))
    ((loop   (imm8 label imm16))             . (#xe2 rb))
    ((mov    r8 imm8)                        . ((+ #xb0 r) ib))
    ((mov    r16 (imm16 imm8 imm label))     . (o16 (+ #xb8 r) iw))
    ((mov    r32 (imm32 imm16 imm8 imm label)) . (o32 (+ #xb8 r) id))
    ((mov    (r/m16 r16 m) r16)              . (o16 #x89 /r))
    ((mov    (r/m32 r32 m) r32)              . (o32 #x89 /r))
    ((mov    r16 (r/m16 r16 m))              . (o16 #x8b /r))
    ((mov    r32 (r/m32 r32 m))              . (o32 #x8b /r))
    ((mov    word m (imm16 imm8 imm label))  . (o16 #xc7 /0 iw))
    ((mov    dword m (imm32 imm16 imm8 imm label)) . (o32 #xc7 /0 id))
    ((mov    sreg (r/m16 r16 m))             . (#x8e /r)) 
    ((mov    (r/m16 r16 m) sreg)             . (#x8c /r)) 
    ((movsb)                                 . (#xa4))
    ((movsw)                                 . (o16 #xa5))
    ((movsd)                                 . (o32 #xa5))
    ,@(arith-syntax-2 'mul)
    ,@(arith-syntax-2 'neg)   
    ((nop)                                   . (#x90))
    ,@(arith-syntax-2 'not)
    ,@(arith-syntax-1 'or)
    ((out    imm8 r8)                        . (#xe6 ib))   ; (out imm8 al)
    ((out    imm8 r16)                       . (#xe7 ib))   ; (out imm8 ax)
    ((out    dx al)                          . (#xee))
    ((out    dx ax)                          . (#xef))
    ((pop    r16)                            . ((+ #x58 r)))
    ((push   r16)                            . ((+ #x50 r)))
    ((ret)                                   . (#xc3))
    ,@(shift-syntax 'shl)
    ,@(shift-syntax 'shr)
    ((stc)                                   . (#xf9))
    ((std)                                   . (#xfd))
    ((sti)                                   . (#xfb))
    ((stosb)                                 . (#xaa))
    ((stosw)                                 . (#xab))
    ,@(arith-syntax-1 'sub)
    ((test    al imm8)                       . (#xa8 ib))
    ((test    ax (imm16 imm8))               . (#xa9 iw))
    ((test    (r/m8 r8) imm8)                . (#xf6 /0 ib))
    ((test    byte m imm8)                   . (#xf6 /0 ib))
    ((test    (r/m16 r16 m) (imm16 imm8))    . (#xf7 /0 iw))
    ((test    word m (imm16 imm8))           . (#xf7 /0 iw))
    ((test    (r/m8 r8 m) r8)                . (#x84 /r))
    ((test    (r/m16 r16 m) r16)             . (#x85 /r))
    ,@(arith-syntax-1 'xor))
  "Valid for both 16-bit and 64-bit modes.")

(defparameter *x86-64-syntax-16/32-bit-only*
  `(((call   (imm16 imm8 label))             . (#xe8 rw))
    ((lgdt   m)                              . (#x0f #x01 /2))
    ((lidt   m)                              . (#x0f #x01 /3))
    ((pop    ss)                             . (#x17))
    ((pop    ds)                             . (#x1f))
    ((pop    es)                             . (#x07))
    ((push   cs)                             . (#x0e))
    ((push   ss)                             . (#x16))
    ((push   ds)                             . (#x1e))
    ((push   es)                             . (#x06)))
  "Valid for 16-bit mode only.")

(defparameter *x86-64-syntax-64-bit-only*
  `(((add    rax (imm32 imm16 imm8))         . (rex.w #x05 id))))

(defparameter *x86-64-syntax-16/32-bit*
  (append *x86-64-syntax-common* *x86-64-syntax-16/32-bit-only*)
  "Syntax table for 16-bit mode.")

(defparameter *x86-64-syntax-64-bit*
  (append *x86-64-syntax-common* *x86-64-syntax-64-bit-only*)
  "Syntax table for 64-bit mode.")

(defun x86-64-syntax (bits)
  "Returns syntax table according to bit mode (16, 32 or 64)."
  (ecase bits
    ((16 32) *x86-64-syntax-16/32-bit*)
    (64 *x86-64-syntax-64-bit*)))

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
  `((lock . #xf0) 
    (repne . #xf2) (repnz .#xf2) 
    (rep   . #xf3) (repe  . #xf3) (repz . #xf3))
  "Prefix mapping table.")

(defun encode (e cursor bits)
  "Opcode encoding, including pseudo instructions like db/dw."
  (acond
   ((assoc* (car e) *prefix-mapping*)
    (cons it (encode (cdr e) (1+ cursor) bits)))
   ((assoc* e (x86-64-syntax bits) :test #'equal) 
    ;; Instructions with exact match, e.g. instructions without
    ;; operands (like nop, hlt), or special instructions like int 3.
    ;; copy-list is necessary since syntax table is LITERAL.
    (if (member (car it) '(o16 o32 a16 a32))
        (append (size-prefix (car it) bits) (copy-list (cdr it)))
        (copy-list it)))
   ((assoc* e (x86-64-syntax bits)
            :test #'(lambda (x y)
                      (and (> (length y) 1)
                           (equal (subseq x 0 2) (subseq y 0 2))
                           (member (elt x 1) '(al ax eax rax))
                           (numberp (elt x 2)))))
    ;; Some registers are explicitly given as destination operand,
    ;; e.g. (add al imm8).
    (encode-complex e (instruction-type e) it cursor bits))
   ((assoc* e (x86-64-syntax bits)
            :test #'(lambda (x y)
                      (and (member (car x) '(shl shr))
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
    (encode-complex (butlast e) (butlast (instruction-type e)) it cursor bits))
   (t
    (declare (ignore it))
    (case (car e)
      ;; Pseudo instructions.
      ((db dw dd dq)
       (let ((val (mklist (nth (1- (length e)) e))))
         (mapcan #'(lambda (v)
                     (ecase (car e)
                       (db (cond
                             ((stringp v) (string->bytes v))
                             (t (try-encode-bytes v 1))))
                       (dw (try-encode-bytes v 2))
                       (dd (try-encode-bytes v 4))
                       (dq (try-encode-bytes v 8))))
                 val)))
      ;; Normal instructions.
      (t (match-n-encode e cursor bits))))))

(defun match-n-encode (e cursor bits)
  "Match instruction and encode it."
  (multiple-value-bind (type opcode)
      (match-instruction e (instruction-type e) bits)
    (encode-complex e type opcode cursor bits)))

(defun encode-complex (instruction type opcode cursor bits)
  "Return opcode for the given instruction."
  (mapcan 
   #'(lambda (on) 
       (cond
         ((numberp on) (list on))
         ((listp on)  
          (ecase (car on)
            (+ (ecase (caddr on)
                 (r (list (+ (cadr on) (reg->int (second instruction)))))))))
         (t 
          (ecase on
            (rex.w (list (encode-rex 1 0 0 0)))
            ((o16 o32 a16 a32) (size-prefix on bits))
            ((ib iw id io) 
             (try-encode-bytes (instruction-value instruction type (on->in on))
                               (on-length on)))
            ((rb rw rd ro)
             (try-encode-bytes 
              `(- ,(instruction-value instruction type (on->in on))
                  ,(+ cursor 1 (on-length on)))
              (on-length on)))
            ((/0 /1 /2 /3 /4 /5 /6 /7) 
             (encode-r/m-sib-disp
              (instruction-value instruction type (find-r/m instruction type))
              on bits))
            (/r (encode-r/m-sib-disp 
                 (instruction-value instruction type (find-r/m instruction type))
                 (instruction-value instruction type (find-reg instruction type))
                 bits))))))
   opcode))

(defun size-prefix (op bits)
  "Handles operand/address-size override prefix o16 & o32. Returns nil
  if no prefix is needed, otherwise corresponding prefix #x66 or #x67."
  (let* ((s (str op))
         (st (symb (subseq s 0 1)))
         (sbit (read-from-string (subseq s 1 3))))
    (if (= sbit bits) nil (list (ecase st
                                  (o #x66)
                                  (a #x67))))))

(defun find-r/m (instruction type)
  "Return the r/m contained in type."
  (aif (member* '(m r/m8 r/m16 r/m32 r/m64 r8 r16 r32 r64) type)
       it
       (error "No r/m operand in ~A~%!" instruction)))

(defun find-reg (instruction type)
  "Return the reg contained in type."
  (aif (member* '(sreg r8 r16 r32 r64) type)
       it
       (error "No (s)reg operand in ~A~%" instruction)))

(defun encode-r/m-sib-disp (r/m reg/opcode bits)
  "Encode ModR/M, SIB byte (if any) and displacement (if any)."
  (let ((r/o (case reg/opcode
               ((/0 /1 /2 /3 /4 /5 /6 /7) 
                (- (char-code (elt (symbol-name reg/opcode) 1)) 48))
               (t (case (operand-type reg/opcode)
                    (sreg (sreg->int reg/opcode))
                    (t    (reg->int reg/opcode)))))))
    (multiple-value-bind (mod rm sib disp disp-length) 
        (r/m-values r/m bits)
      (append (list (encode-modr/m mod rm r/o))
              (when sib
                (list sib))
              (when disp
                (try-encode-bytes disp disp-length))))))

(defun r/m-values (r/m bits)
  "Return values: mod, r/m for encoding, sib, disp, and length of disp
in bytes. 

   Note 
     1. If sib is not needed, return nil.
     2. If disp is not needed, return nil as disp and disp-length
     could be arbitrary."
  (ecase (operand-type r/m)
    ((r8 r16 r32 r64) (values #b11 (reg->int r/m) nil nil 0))
    (m (ecase bits ;; FIXME: should be directly related to address mode.
         (16 (r/m-values-16 r/m))
         (32 (r/m-values-32 r/m))))))

(defun r/m-values-16 (r/m)
  (if (equal r/m '(bp))  ; Special handling of (bp)
      (values 1 #b110 nil 0 1)
      (let ((type (mapcar #'operand-type r/m)))
        (if (and (= (length r/m) 1) ; Special handling of (disp16)
                 (member* '(imm8 imm16 label) type))
            (values 0 #b110 nil (car r/m) 2)
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
              (values mod rm nil disp mod))))))

(defun r/m-values-32 (r/m)
  (cond
    ((equal r/m '(ebp))  ; Special handling of (ebp)
     (values 1 #b101 nil 0 1))
    ((equal r/m '(esp))  ; Special handling of (esp)
     (values 0 #b100  (encode-sib 0 #b100 4) nil 0))
    (t 
     (let ((type (mapcar #'operand-type r/m)))
       (cond 
         ((and (= (length r/m) 1) ; Special handling of (disp32)
               (member* '(imm8 imm16 imm32 label) type))
          (values 0 #b101 nil (car r/m) 4))
         ((and (= (length r/m) 2) (member 'esp r/m) (member 'imm8 type))
          ;; Special handling of (esp + disp8)
          (values 1 #b100  (encode-sib 0 #b100 4) 
                  (instruction-value r/m type 'imm8) 1))
         ((and (= (length r/m) 2) (member 'esp r/m) 
               (member* '(imm16 imm32) type))
          ;; Special handling of (esp + disp32)
          (values 2 #b100  (encode-sib 0 #b100 4) 
                  (instruction-value r/m type (member* '(imm16 imm32) type)) 4))
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
                           (reg->int (member* '(eax ecx edx ebx ebp esi edi) 
                                              r/m)))))
              (values mod rm sib disp disp-length))))))))  
                
(defun try-encode-bytes (x length)
  "If x is evaluable, run encode-bytes.
   Otherwise return the placeholder list."
  (if (evaluable? x)
      (encode-bytes (eval x) length)
      (cons `(,length ,x) (repeat-element (1- length) '?))))

(defun encode-bytes (x length)
  "Encode byte, word, doubleword, quadword into bytes in
little-ending. Length is the number of bytes to convert to. X is first
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
    ((atom (car e)) (and (member (car e) '(+ -)) 
                         (every #'evaluable? (cdr e))))
    (t nil)))

(defun eval-final (revisit symtab)
  "Final evaluation of the revisit."
  (car (try-eval-values (cdr revisit) -1 -1 symtab)))

(defun on->in (on)
  "Maps opcode notation (e.g. ib, iw) to instruction notation (e.g. imm8)."
  (ecase on
    ((ib rb) 'imm8)
    ((iw rw) 'imm16)
    ((id rd) 'imm32)
    ((io ro) 'imm64)))

(defun on-length (on)
  "Return lengths (in terms of bytes) for opcode notation (e.g. ib, iw)."
  (ecase on
    ((ib rb) 1)
    ((iw rw) 2)
    ((id rd) 4)
    ((io ro) 8)))

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
  (aif (assoc-x86-64-opcode type bits)
       (values (canonical-type (car it)) (copy-list (cdr it)))
       (error "match-instruction: unsupported instruction ~A" instruction)))

(defun canonical-type (type)
  "Return the canonical form of the type."
  (mapcar #'(lambda (x) (if (listp x) (car x) x)) type))

(defun assoc-x86-64-opcode (type bits)
  "Returns a associated opcode based on x86-64 syntax."
  (assoc type (x86-64-syntax bits)
         :test #'(lambda (x y) 
                   (every #'(lambda (a b) 
                              (if (listp b)
                                  (member a b)
                                  (eq a b)))
                          x y))))

(defun signed->unsigned (value length)
  "Change value from signed to unsigned."
  (if (>= value 0)
      value
      (ecase length
        ((1 2 4 8) (+ (expt 256 length) value)))))

(defun encode-modr/m (mod r/m reg/opcode)
  "Encode ModR/M byte."
  (+ (* mod #b1000000) (* reg/opcode #b1000) r/m))

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
       ((and (<= (- (expt 2 7))  operand (1- (expt 2 8))))  'imm8)
       ((and (<= (- (expt 2 15)) operand (1- (expt 2 16)))) 'imm16)
       ((and (<= (- (expt 2 31)) operand (1- (expt 2 32)))) 'imm32)
       ((and (<= (- (expt 2 63)) operand (1- (expt 2 64)))) 'imm64)
       (t (error "Invalid operand: ~A" operand))))
    ((listp operand)
     (case (car operand)
       ((+ -) 'imm)
       (t     'm)))
    (t 
     (case operand
       ((al cl dl bl ah ch dh bh bpl spl dil sil) 'r8)
       ((ax cx dx bx sp bp si di)                 'r16)
       ((eax ecx edx ebx esp ebp esi edi)         'r32)
       ((rax rcx rdx rbx rsp rbp rsi rdi 
             r8 r9 r10 r11 r12 r13 r14 r15)       'r64)
       ((cs ds es ss fs gs)                       'sreg)
       ((short byte word dword qword)             operand)
       (t                                         'label)
       ))))

(defun r32? (op)
  "Returns T if operand op is a 32-bit general purpose register."
  (eq (operand-type op) 'r32))

(defun reg->int (reg)
  "Returns the integer representation for register when encoding
ModR/M byte."
  (ecase reg
    ((al ax eax mm0 xmm0) 0)
    ((cl cx ecx mm1 xmm1) 1)
    ((dl dx edx mm2 xmm2) 2)
    ((bl bx ebx mm3 xmm3) 3)
    ((ah sp esp mm4 xmm4) 4)
    ((ch bp ebp mm5 xmm5) 5)
    ((dh si esi mm6 xmm6) 6)
    ((bh di edi mm7 xmm7) 7)))

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
         (member (symb (subseq s 0 3)) '(eax ecx edx ebx ebp esi edi)))))
