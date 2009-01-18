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
   unresolvable labels), ((length expr) ? ...) with number of ?
   filling up to length serve as placeholders. At the end of the pass,
   those placeholders are evaluated and replaced with actual values."
  (let (symtab
        code
        (origin 0)
        (cursor 0))
    (dolist (e listing)
      (if (listp e) 
          (let ((e* (cons (car e) 
                          (try-eval-values (cdr e) cursor origin symtab t))))
            ;; FIXME: the above does not handle var definitions (db, dw
            ;; etc.) and times if labels have same name as instructions.
            (setf code 
                  (nconc code
                         (case (car e*)
                           (org (unless (null code)
                                  (error "asm: org should be placed earlier."))
                                (setf origin (second e*)
                                      cursor origin)
                                nil)
                           (times 
                            (repeat-list (eval (second e*)) 
                                         (encode (nthcdr 2 e*) cursor)))
                           (t (encode e* cursor))))))
          ;; TODO: add local label support.
          (aif (assoc e symtab)
               (if (eq (cdr it) '?)
                   (setf (cdr it) cursor)
                   (error "asm: duplicated symbol ~A." e))
               (push (cons e cursor) symtab)))
      (setf cursor (+ origin (length code))))
    (mapcan 
     #'(lambda (c)
         (cond
           ((numberp c) (list c))
           ((eq c '?)   nil)
           ((listp c)   (encode-bytes (eval-final c symtab)  (first c)))
           (t           (error "asm: wrong byte for final processing: ~A" c))))
     code)))

(defparameter *x86-64-syntax*
  `(((int    3)                 . (#xcc))
    ((int    imm8)              . (#xcd ib))
    ((jmp    short imm8)        . (#xeb rb))
    ((mov    r8 imm8)           . ((+ #xb0 r) ib))
    ((mov    r16 imm16)         . ((+ #xb8 r) iw)))
  "Syntax table for x86-64. For each entry, 1st part is the mnemonic
  code, 2nd part is the corresponding opcode. For details, refer to
  http://code.google.com/p/yalo/wiki/AssemblyX64Overview")

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

(defun encode (e cursor)
  "Opcode encoding, including pseudo instructions like db/dw."
  (aif (assoc e *x86-64-syntax* :test #'equal) 
       ;; Instructions with exact match, e.g. instructions without
       ;; operands (like nop, hlt), or special instructions like int 3.
       (second it) 
       (case (car e)
         ;; Pseudo instructions.
         (db (etypecase (second e)
               (string (string->bytes (second e)))
               (number (list (second e)))))
         (dw (encode-bytes (second e) 2))
         ;; Normal instructions.
         (t (multiple-value-bind (format opcode)
                (match-instruction (instruction-format e))
              (encode-complex e format opcode cursor))))))

(defun encode-complex (instruction format opcode cursor)
  "Return opcode for the given instruction."
  (cons
   (etypecase (car opcode)
     (number (car opcode))
     (list (ecase (caar opcode)
             (+ (ecase (caddar opcode)
                  (r (+ (cadar opcode) (reg->int (second instruction)))))))))
   (mapcan 
    #'(lambda (on) 
        (ecase on
          ((ib iw id io) 
           (try-encode-bytes (get-value instruction format (on->in on))
                             (on-length on)))
          (rb (try-encode-bytes `(- ,(get-value instruction format 'imm8)
                                    ,(+ cursor 2))
                                1))))
    (cdr opcode))))

(defun try-eval-values (ops cursor origin symtab has-real-car?)
  "Run lookup-value. For each element, evaluate it if possible."
  (let ((vs (lookup-value ops has-real-car? cursor origin symtab)))
    (mapcar #'try-eval-value vs)))

(defun try-eval-value (op)
  "Try to evaluate op if possible; other return op as is."
  (handler-case (eval op)
    (unbound-variable () op)))

(defun eval-final (revisit symtab)
  "Final evaluation of the revisit."
  (car (try-eval-values (cdr revisit) -1 -1 symtab nil)))

(defun on->in (on)
  "Maps opcode notation (e.g. ib, iw) to instruction notation (e.g. imm8)."
  (ecase on
    (ib 'imm8)
    (iw 'imm16)
    (id 'imm32)
    (io 'imm64)))

(defun on-length (on)
  "Return lengths (in terms of bytes) for opcode notation (e.g. ib, iw)."
  (ecase on
    (ib 1)
    (iw 2)
    (id 4)
    (io 8)))

(defun lookup-value (ops has-real-car? cursor origin symtab)
  "Replace special variables and labels with values if possible.
   Note that we do NOT handle car which are lisp
   operators. has-real-car?  indicates the ops position in original
   list. T if its car is an operator (real car)."
  (cond
    ((atom ops) (operand->value-if ops cursor origin symtab))
    ((null ops) nil)
    ((atom (car ops)) 
     (cons (if has-real-car? 
               (car ops)
               (lookup-value (car ops) t cursor origin symtab))
           (lookup-value (cdr ops) nil cursor origin symtab)))
    (t (cons (lookup-value (car ops) t cursor origin symtab)
             (lookup-value (cdr ops) nil cursor origin symtab)))))

(defun operand->value-if (operand cursor origin symtab)
  "For labels, returns its value if possible.
   For special variables, returns its value.
   For all other stuff, just return it."
  (cond
    ((eq operand '$) cursor)
    ((eq operand '$$) origin)
    ((eq (operand-type operand) 'label)
     (if (sym-found? operand symtab)
         (cdr (assoc operand symtab))
         operand))
    (t operand)))
         
(defun get-value (instruction format name)
  "Get the value (in instruction) corresponding to the name (in format)."
  (cdr (assoc name (mapcar #'cons format instruction))))

(defun match-instruction (format)
  "Returns values of (format opcode successful?). 
   In the first run, when the format does not appear in syntax table,
     try to match immediate data with register length."
  (aif (assoc-x86-64-opcode format)
       (values format (cdr it) t)
       (let ((matched-format (match-format format)))
         (aif (assoc-x86-64-opcode matched-format)
              (values matched-format (cdr it) t)
              (error "match-instruction: unsupported instruction!")))))

(defun match-format (format)
  "Returns new format if immediate data can be matched with register
length. Otherwise, just return format."
  (if (= (length format) 3)
      (cond
        ((and (eq (second format) 'r16) 
              (member (third format) '(imm8 imm label)))
         (list (car format) (second format) 'imm16))
        ((and (eq (second format) 'short) 
              (member (third format) '(label imm16)))
         (list (car format) (second format) 'imm8))
        (t format))
      format))

(defun assoc-x86-64-opcode (format)
  "Returns a associated opcode based on x86-64 syntax."
  (assoc format *x86-64-syntax* :test #'equal))

(defun sym-found? (sym symtab)
  "Returns T if sym is found in symtab."
  (and (assoc sym symtab) (not (eq (cdr (assoc sym symtab)) '?))))

(defun signed->unsigned (value length)
  "Change value from signed to unsigned."
  (if (>= value 0)
      value
      (ecase length
        ((1 2 4 8) (+ (expt 256 length) value)))))

(defun encode-modr/m (mod rm reg)
  "Encode ModR/M byte."
  (+ (* mod #b1000000) (* reg #b1000) rm))

(defun encode-1-operand (dest reg)
  (encode-modr/m #b11 (reg->int dest) reg))

(defun instruction-format (instruction)
  "Returns the instruction format for encoding."
  (cons (car instruction) 
        (mapcar #'operand-type (cdr instruction))))

(defun operand-type (operand)
  "Returns operand type with following values:
     imm8, imm16, imm32, imm64,
     r8, r16, r32, r64"
  (cond
    ((numberp operand)
     (cond 
       ((and (<= (- (expt 2 7))  operand (1- (expt 2 7))))  'imm8)
       ((and (<= (- (expt 2 15)) operand (1- (expt 2 15)))) 'imm16)
       ((and (<= (- (expt 2 31)) operand (1- (expt 2 31)))) 'imm32)
       ((and (<= (- (expt 2 63)) operand (1- (expt 2 31)))) 'imm64)
       (t (error "Invalid operand: ~A" operand))))
    ((listp operand)
     'imm)
    (t 
     (case operand
       ((al cl dl bl ah ch dh bh bpl spl dil sil) 'r8)
       ((ax cx dx bx sp bp si di)                 'r16)
       ((eax ecx edx ebx esp ebp esi edi)         'r32)
       ((rax rcx rdx rbx rsp rbp rsi rdi 
             r8 r9 r10 r11 r12 r13 r14 r15)       'r64)
       ((short)                                   operand)
       (t                                         'label)
       ))))

(defun reg->int (register)
  "Returns the integer representation for register when encode ModR/M byte.
   Returns -1 if not a register."
  (ecase register
    ((al ax eax mm0 xmm0) 0)
    ((cl cx ecx mm1 xmm1) 1)
    ((dl dx edx mm2 xmm2) 2)
    ((bl bx ebx mm3 xmm3) 3)
    ((ah sp esp mm4 xmm4) 4)
    ((ch bp ebp mm5 xmm5) 5)
    ((dh si esi mm6 xmm6) 6)
    ((bh di edi mm7 xmm7) 7)))

(defun string->bytes (s)
  (map 'list #'char-code s))

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
