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
                           cursor origin symtab t)))
                 ;; FIXME: the above may not handle times correctly if
                 ;; labels have same name as instructions.
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
          ; Labels
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
  `(((add    al imm8)                        . (#x04 ib))
    ((add    ax (imm16 imm8))                . (#x05 iw))
    ((add    (r/m8 r8) imm8)                 . (#x80 /0 ib))
    ((add    byte m imm8)                    . (#x80 /0 ib))
    ((add    (r/m16 r16 m) imm16)            . (#x81 /0 iw))
    ((add    (r/m16 r16) imm8)               . (#x83 /0 ib))
    ((add    word m imm8)                    . (#x83 /0 ib))
    ((add    (r/m8 r8 m) r8)                 . (#x00 /r))
    ((add    (r/m16 r16 m) r16)              . (#x01 /r))
    ((add    r8 (r/m8 r8 m))                 . (#x02 /r))
    ((add    r16 (r/m16 r16 m))              . (#x03 /r))
    ((and    al imm8)                        . (#x24 ib))
    ((and    ax (imm16 imm8))                . (#x25 iw))
    ((and    (r/m8 r8) imm8)                 . (#x80 /4 ib))
    ((and    byte m imm8)                    . (#x80 /4 ib))
    ((and    (r/m16 r16 m) imm16)            . (#x81 /4 iw))
    ((and    (r/m16 r16) imm8)               . (#x83 /4 ib))
    ((and    word m imm8)                    . (#x83 /4 ib))
    ((and    (r/m8 r8 m) r8)                 . (#x20 /r))
    ((and    (r/m16 r16 m) r16)              . (#x21 /r))
    ((and    r8 (r/m8 r8 m))                 . (#x22 /r))
    ((and    r16 (r/m16 r16 m))              . (#x23 /r))
    ((clc)                                   . (#xf8))
    ((cld)                                   . (#xfc))
    ((cli)                                   . (#xfa))
    ((cmp    al imm8)                        . (#x3c ib))
    ((cmp    ax (imm16 imm8))                . (#x3d iw))
    ((cmp    (r/m8 r8) imm8)                 . (#x80 /7 ib))
    ((cmp    byte m imm8)                    . (#x80 /7 ib))
    ((cmp    (r/m16 r16 m) imm16)            . (#x81 /7 iw))
    ((cmp    (r/m16 r16) imm8)               . (#x83 /7 ib))
    ((cmp    word m imm8)                    . (#x83 /7 ib))
    ((cmp    (r/m8 r8 m) r8)                 . (#x38 /r))
    ((cmp    (r/m16 r16 m) r16)              . (#x39 /r))
    ((cmp    r8 (r/m8 r8 m))                 . (#x3a /r))
    ((cmp    r16 (r/m16 r16 m))              . (#x3b /r))
    ((hlt)                                   . (#xf4))
    ((in     al imm8)                        . (#xe4 ib)) 
    ((in     ax imm8)                        . (#xe5 ib))
    ((in     al dx)                          . (#xec))
    ((in     ax dx)                          . (#xed))
    ((int    3)                              . (#xcc))
    ((int    imm8)                           . (#xcd ib))
    ((jmp    short (imm8 label imm16))       . (#xeb rb))
    ((lodsb)                                 . (#xac))
    ((lodsw)                                 . (#xad))
    ((mov    r8 imm8)                        . ((+ #xb0 r) ib))
    ((mov    r16 (imm16 imm8 imm label))     . ((+ #xb8 r) iw))
    ((mov    (r/m16 r16 m) r16)              . (#x89 /r))
    ((mov    r16 (r/m16 r16 m))              . (#x8b /r))
    ((mov    word m (imm16 imm8 imm label))  . (#xc7 /0 iw))
    ((mov    sreg (r/m16 r16 m))             . (#x8e /r))   ; (mov sreg r/m16)
    ((mov    (r/m16 r16 m) sreg)             . (#x8c /r))   ; (mov r/m16 sreg)
    ((neg    (r/m8 r8))                      . (#xf6 /3))
    ((neg    byte m)                         . (#xf6 /3))
    ((neg    (r/m16 r16))                    . (#xf7 /3))
    ((neg    word m)                         . (#xf7 /3))
    ((nop)                                   . (#x90))
    ((not    (r/m8 r8))                      . (#xf6 /2))
    ((not    byte m)                         . (#xf6 /2))
    ((not    (r/m16 r16))                    . (#xf7 /2))
    ((not    word m)                         . (#xf7 /2))
    ((or     al imm8)                        . (#x0c ib))
    ((or     ax (imm16 imm8))                . (#x0d iw))
    ((or     (r/m8 r8) imm8)                 . (#x80 /1 ib))
    ((or     byte m imm8)                    . (#x80 /1 ib))
    ((or     (r/m16 r16 m) imm16)            . (#x81 /1 iw))
    ((or     (r/m16 r16) imm8)               . (#x83 /1 ib))
    ((or     word m imm8)                    . (#x83 /1 ib))
    ((or     (r/m8 r8 m) r8)                 . (#x08 /r))
    ((or     (r/m16 r16 m) r16)              . (#x09 /r))
    ((or     r8 (r/m8 r8 m))                 . (#x0a /r))
    ((or     r16 (r/m16 r16 m))              . (#x0b /r))
    ((out    imm8 r8)                        . (#xe6 ib))   ; (out imm8 al)
    ((out    imm8 r16)                       . (#xe7 ib))   ; (out imm8 ax)
    ((out    dx al)                          . (#xee))
    ((out    dx ax)                          . (#xef))
    ((pop    r16)                            . ((+ #x58 r)))
    ((push   r16)                            . ((+ #x50 r)))
    ((rep    movsb)                          . (#xf3 #xa4))
    ((rep    movsw)                          . (#xf3 #xa5))
    ((ret)                                   . (#xc3))
    ((stc)                                   . (#xf9))
    ((std)                                   . (#xfd))
    ((sti)                                   . (#xfb))
    ((stosb)                                 . (#xaa))
    ((stosw)                                 . (#xab))
    ((sub    al imm8)                        . (#x2c ib))
    ((sub    ax (imm16 imm8))                . (#x2d iw))
    ((sub    (r/m8 r8) imm8)                 . (#x80 /5 ib))
    ((sub    byte m imm8)                    . (#x80 /5 ib))
    ((sub    (r/m16 r16 m) imm16)            . (#x81 /5 iw))
    ((sub    (r/m16 r16) imm8)               . (#x83 /5 ib))
    ((sub    word m imm8)                    . (#x83 /5 ib))
    ((sub    (r/m8 r8 m) r8)                 . (#x28 /r))
    ((sub    (r/m16 r16 m) r16)              . (#x29 /r))
    ((sub    r8 (r/m8 r8 m))                 . (#x2a /r))
    ((sub    r16 (r/m16 r16 m))              . (#x2b /r))
    ((test    al imm8)                       . (#xa8 ib))
    ((test    ax (imm16 imm8))               . (#xa9 iw))
    ((test    (r/m8 r8) imm8)                . (#xf6 /0 ib))
    ((test    byte m imm8)                   . (#xf6 /0 ib))
    ((test    (r/m16 r16 m) (imm16 imm8))    . (#xf7 /0 iw))
    ((test    word m (imm16 imm8))           . (#xf7 /0 iw))
    ((test    (r/m8 r8 m) r8)                . (#x84 /r))
    ((test    (r/m16 r16 m) r16)             . (#x85 /r))
    ((xor     al imm8)                       . (#x34 ib))
    ((xor     ax (imm16 imm8))               . (#x35 iw))
    ((xor     (r/m8 r8) imm8)                . (#x80 /6 ib))
    ((xor     byte m imm8)                   . (#x80 /6 ib))
    ((xor     (r/m16 r16 m) imm16)           . (#x81 /6 iw))
    ((xor     (r/m16 r16) imm8)              . (#x83 /6 ib))
    ((xor     word m imm8)                   . (#x83 /6 ib))
    ((xor     (r/m8 r8 m) r8)                . (#x30 /r))
    ((xor     (r/m16 r16 m) r16)             . (#x31 /r))
    ((xor     r8 (r/m8 r8 m))                . (#x32 /r))
    ((xor     r16 (r/m16 r16 m))             . (#x33 /r)))
  "Valid for both 16-bit and 64-bit modes.")

(defparameter *x86-64-syntax-16-bit-only*
  `(((call   (imm16 imm8 label))             . (#xe8 rw))
    ((pop    ss)                             . (#x17))
    ((pop    ds)                             . (#x1f))
    ((pop    es)                             . (#x07))
    ((push   cs)                             . (#x0e))
    ((push   ss)                             . (#x16))
    ((push   ds)                             . (#x1e))
    ((push   es)                             . (#x06)))
  "Valid for 16-bit mode only.")

(defparameter *x86-64-syntax-64-bit-only*
  nil
  "Valid for 64-bit mode only.")

(defparameter *x86-64-syntax-16-bit*
  (append *x86-64-syntax-common* *x86-64-syntax-16-bit-only*)
  "Syntax table for 16-bit mode.")

(defparameter *x86-64-syntax-64-bit*
  (append *x86-64-syntax-common* *x86-64-syntax-64-bit-only*)
  "Syntax table for 64-bit mode.")

(defun x86-64-syntax (bits)
  "Returns syntax table according to bit mode (16 or 64)."
  (ecase bits
    (16 *x86-64-syntax-16-bit*)
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

(defun encode (e cursor bits)
  "Opcode encoding, including pseudo instructions like db/dw."
  (aif (assoc* e (x86-64-syntax bits) :test #'equal) 
       ;; Instructions with exact match, e.g. instructions without
       ;; operands (like nop, hlt), or special instructions like int 3.
       (copy-list it) ; copy-list is necessary since syntax table is
                                        ; LITERAL.
       (aif (assoc* e (x86-64-syntax bits) 
                    :test #'(lambda (x y)
                              (and (> (length y) 1)
                                   (equal (subseq x 0 2) (subseq y 0 2))
                                   (member (elt x 1) '(al ax))
                                   (numberp (elt x 2)))))
            ;; The case that some registers are explicitly given as
            ;; destination operand.  e.g. (add al imm8).
            (encode-complex e (instruction-type e) it cursor bits)
            (case (car e)
              ;; Pseudo instructions.
              ((db dw dd dq)
               (let ((val (mklist (nth (1- (length e)) e))))
                 (mapcan #'(lambda (v)
                             (ecase (car e)
                               (db (etypecase v
                                     (string (string->bytes v))
                                     (number (list v))))
                               (dw (encode-bytes v 2))
                               (dd (encode-bytes v 4))
                               (dq (encode-bytes v 8))))
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
            ((ib iw id io) 
             (try-encode-bytes (instruction-value instruction type (on->in on))
                               (on-length on)))
            ((rb rw rd ro)
             (try-encode-bytes 
              `(- ,(instruction-value instruction type (on->in on))
                  ,(+ cursor 1 (on-length on)))
              (on-length on)))
            ((/0 /1 /2 /3 /4 /5 /6 /7) 
             (encode-r/m-disp
              (instruction-value instruction type (find-r/m instruction type))
              on bits))
            (/r (encode-r/m-disp 
                 (instruction-value instruction type (find-r/m instruction type))
                 (instruction-value instruction type (find-reg instruction type))
                 bits))))))
   opcode))

(defun find-r/m (instruction type)
  "Return the r/m contained in type."
  (cond
    ((member 'r/m8  type) 'r/m8)
    ((member 'r/m16 type) 'r/m16)
    ((member 'r/m32 type) 'r/m32)
    ((member 'r/m64 type) 'r/m64)
    ((member 'm type) 'm)
    (t (error "No r/m operand in ~A~%!" instruction))))  

(defun find-reg (instruction type)
  "Return the reg contained in type."
  (cond
    ((member 'sreg type) 'sreg)
    ((member 'r8 type)  'r8)
    ((member 'r16 type) 'r16)
    ((member 'r32 type) 'r32)
    ((member 'r64 type) 'r64)
    (t (error "No (s)reg operand in ~A~%" instruction))))

(defun encode-r/m-disp (r/m reg/opcode bits)
  "Encode ModR/M byte and displacement (if any)."
  (let ((r/o (case reg/opcode
               ((/0 /1 /2 /3 /4 /5 /6 /7) 
                (- (char-code (elt (symbol-name reg/opcode) 1)) 48))
               (t (case (operand-type reg/opcode)
                    (sreg (sreg->int reg/opcode))
                    (t    (reg->int reg/opcode)))))))
    (multiple-value-bind (mod rm disp disp-length) 
        (r/m-values r/m bits)
      (append (list (encode-modr/m mod rm r/o))
              (when disp
                (try-encode-bytes disp disp-length))))))

(defun r/m-values (r/m bits)
  "Return values: mod, r/m for encoding, disp, and length of disp in
bytes. Note that if disp is not needed, return nil as disp and
disp-length could be arbitrary."
  (ecase (operand-type r/m)
    ((r8 r16 r32 r64) (values #b11 (reg->int r/m)))
    (m (ecase bits
         (16
          (if (equal r/m '(bp))  ; Special handling of (bp)
              (values 1 #b110 nil 0)
              (let ((type (mapcar #'operand-type r/m)))
                (if (and (= (length r/m) 1) ; Special handling of (disp16)
                         (or (member 'imm8 type) (member 'imm16 type) 
                             (member 'label type)))
                    (values 0 #b110 (car r/m) 2)
                    (let* (disp
                           (mod (cond 
                                  ((member 'imm8 type) 
                                   (setf disp (instruction-value r/m type 'imm8))
                                   1)
                                  ((member 'imm16 type)
                                   (setf disp 
                                         (instruction-value r/m type 'imm16))
                                   2)
                                  (t 0)))
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
                      (values mod rm disp mod))))))))))
                
(defun try-encode-bytes (x length)
  "If x is evaluable, run encode-bytes.
   Otherwise return the placeholder list."
  (handler-case (encode-bytes x length)
    (error ()
        (cons `(,length ,x) (repeat-element (1- length) '?))))) 

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

(defun try-eval-values (ops cursor origin symtab has-real-car?)
  "Run lookup-value. For each element, evaluate it if possible."
  (let ((vs (lookup-value ops has-real-car? cursor origin symtab)))
    (mapcar #'(lambda (v) 
                (handler-case (eval v)
                  (error () v)))
            vs)))

(defun eval-final (revisit symtab)
  "Final evaluation of the revisit."
  (car (try-eval-values (cdr revisit) -1 -1 symtab nil)))

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

(defun lookup-value (ops has-real-car? cursor origin symtab)
  "Replace special variables and labels with values if possible.
   Note that we do NOT handle car which are lisp
   operators. has-real-car?  indicates the ops position in original
   list. T if its car is an operator (real car)."
  (cond
    ((atom ops) (operand->value ops cursor origin symtab))
    ((null ops) nil)
    ((atom (car ops)) 
     (cons (if has-real-car? 
               (car ops)
               (lookup-value (car ops) t cursor origin symtab))
           (lookup-value (cdr ops) nil cursor origin symtab)))
    (t (cons (lookup-value (car ops) t cursor origin symtab)
             (lookup-value (cdr ops) nil cursor origin symtab)))))

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

