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

(defparameter *symtab* nil)
(defparameter *revisits* nil)

(defun asm (listing)
  "One pass assembler. listing is in the form of LAP as described in
       http://code.google.com/p/yalo/wiki/AssemblySyntax
   Returns opcodes as a list of bytes."
  (setf *symtab* nil)
  (let (code
        (origin 0)
        (cursor 0))
    (dolist (e listing)
      (if (listp e) 
          (let ((e* (cons (car e) (try-eval-value (cdr e) cursor origin))))
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
                                         (encode (nthcdr 2 e*) origin cursor)))
                           (t (encode e* origin cursor))))))
          ;; TODO: add local label support.
          (aif (assoc e *symtab*)
               (if (eq (cdr it) '?)
                   (setf (cdr it) cursor)
                   (error "asm: duplicated symbol ~A." e))
               (push (cons e cursor) *symtab*)))
      (setf cursor (+ origin (length code))))
    (awhen (rassoc '? *symtab*)
      (error "asm: undefined symbol ~A" (car it)))
    (dolist (r *revisits*)
      (ecase (second r)
        (1 (setf (elt code (first r)) 
                 (signed->unsigned (- (cdr (assoc (fourth r) *symtab*)) 
                                      (third r)) 
                                   (second r))))))
    code))

(defparameter *x86-64-syntax*
  `(((int    3)                 . (#xcc))
    ((int    imm8)              . (#xcd ib))
    ((jmp    short imm8)        . (#xeb rb))
    ((mov    r8 imm8)           . ((+ #xb0 r) ib))
    ((mov    r16 imm16)         . ((+ #xb8 r) iw)))
  "Syntax table for x86-64. For each entry, 1st part is the mnemonic
  code, 2nd part is the corresponding opcode. For details, refer to
  http://code.google.com/p/yalo/wiki/AssemblyX64Overview")

(defun encode (e origin cursor)
  "Opcode encoding, including pseudo instructions like db/dw."
  (mklist 
   (aif (assoc e *x86-64-syntax* :test #'equal) 
        (second it)
        (case (car e)
          (db (etypecase (second e)
                (string (string->bytes (second e)))
                (number (second e))))
          (dw (encode-bytes (second e) 2))
          (jmp (ecase (second e)
                 (short (encode-jmp (third e) cursor 1 origin))))
          (t (multiple-value-bind (format opcode)
                 (match-instruction (instruction-format e))
               (translate e format opcode cursor)))))))

(defun translate (instruction format opcode cursor)
  "Return opcode for the given instruction."
  (cons
   (etypecase (car opcode)
     (number (car opcode))
     (list (ecase (caar opcode)
             (+ (ecase (caddar opcode)
                  (r (+ (cadar opcode) 
                        (register->int (second instruction)))))))))
   (mapcan 
    #'(lambda (on) 
        (mklist
         (ecase on
           ((ib iw id io) 
            (encode-bytes (get-value instruction format (on->in on))
                          (on-length on)))
           (rb (encode-bytes (get-value instruction format 'imm8) 1)))))
    (cdr opcode))))

(defun try-eval-value (ops cursor origin)
  "Run lookup-value. For each element, evaluate it if possible."
  (let ((vs (lookup-value ops t cursor origin)))
    (mapcar #'(lambda (v)
                (handler-case (eval v)
                  (unbound-variable () v)))
            vs)))

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

(defun lookup-value (ops has-real-car? cursor origin)
  "Replace special variables and labels with values if possible.
   Note that we do NOT handle car which are lisp
   operators. has-real-car?  indicates the ops position in original
   list. T if its car is an operator (real car)."
  (cond
    ((atom ops) (operand->value-if ops cursor origin))
    ((null ops) nil)
    ((atom (car ops)) 
     (cons (if has-real-car? 
               (car ops)
               (lookup-value (car ops) t cursor origin))
           (lookup-value (cdr ops) nil cursor origin)))
    (t (cons (lookup-value (car ops) t cursor origin)
             (lookup-value (cdr ops) nil cursor origin)))))

(defun operand->value-if (operand cursor origin)
  "For labels, returns its value if possible.
   For special variables, returns its value.
   For all other stuff, just return it."
  (cond
    ((eq operand '$) cursor)
    ((eq operand '$$) origin)
    ((eq (operand-type operand) 'label)
     (if (sym-found? operand)
         (cdr (assoc operand *symtab*))
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
              (eq (third format) 'imm8))
         (list (car format) 'r16 'imm16))
        (t format))
      format))

(defun assoc-x86-64-opcode (format)
  "Returns a associated opcode based on x86-64 syntax."
  (assoc format *x86-64-syntax* :test #'equal))

(defun lookup-sym (sym index length base origin)
  "If sym is a number of has a value other than ? in *symtab*, 
      return the value;
   Otherwise:
     - make a new entry in *symtab* with value ?
     - make a new entry in *revisits*
     - return a length number of ?"
  (cond
    ((numberp sym) (list (signed->unsigned (- sym base) length)))
    ((sym-found? sym)
     (list (signed->unsigned (- (cdr (assoc sym *symtab*)) base) length)))
    (t
     (push (cons sym '?) *symtab*)
     (push (list (- index origin) length base sym) *revisits*)
     (repeat-element length '?))))

(defun sym-found? (sym)
  "Returns T if sym is found in *symtab*."
  (and (assoc sym *symtab*) (not (eq (cdr (assoc sym *symtab*)) '?))))

(defun signed->unsigned (value length)
  "Change value from signed to unsigned."
  (if (>= value 0)
      value
      (ecase length
        ((1 2 4 8) (+ (expt 256 length) value)))))

(defun encode-jmp (sym cursor length origin)
  "Encode mnemonic jmp."
  (ecase length
    (1 (cons #xeb
             (lookup-sym sym (1+ cursor) length (+ cursor 1 length)
                         origin)))))

(defun encode-modr/m (mod rm reg)
  "Encode ModR/M byte."
  (+ (* mod #b1000000) (* reg #b1000) rm))

(defun encode-1-operand (dest reg)
  (encode-modr/m #b11 (register->int dest) reg))

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

(defun register->int (register)
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

(defun encode-bytes (x length)
  "Encode byte, word, doubleword, quadword into bytes in
little-ending. Length is the number of bytes to convert to."
  (ecase length
    ((1 2 4 8) 
     (do* ((e (1- length) (1- e))
           (y x)
           (r (floor y (expt 256 e)) (floor y (expt 256 e)))
           z)
          ((zerop e) (push (mod y 256) z) z)
       (decf y (* r (expt 256 e)))
       (push r z)))))

(defun read-image (filename)
  "Return a list of bytes contained in the file with filename."
  (with-open-file (s filename :element-type 'unsigned-byte)
    (when s
      (let (output)
        (loop for byte = (read-byte s nil)
             while byte do (push byte output))
        (nreverse output)))))

(defun write-image (bytes filename)
  "Write a list of bytes to the file with filename."
  (with-open-file (s filename :direction :output :element-type 'unsigned-byte
                     :if-exists :supersede)
    (when s
      (dolist (b bytes)
        (write-byte b s)))))

(defun write-kernel (filename)
  "Output kernel (including bootloader) as an image file with filename."
  (write-image (asm *bootloader*) filename))
    

