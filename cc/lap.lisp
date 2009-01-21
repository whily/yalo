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
                            (equ (push (cons (second e) (eval (third e))) 
                                       symtab)
                                 nil)
                            (org (setf origin (second e*)
                                       cursor origin)
                                 nil)
                            (times 
                             (repeat-list (eval (second e*)) 
                                          (encode (nthcdr 2 e*) cursor)))
                            (t (encode e* cursor)))))
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

(defparameter *x86-64-syntax*
  `(((call   (imm16 imm8 label))             . (#xe8 rw))
    ((clc)                                   . (#xf8))
    ((cld)                                   . (#xfc))
    ((cli)                                   . (#xfa))
    ((hlt)                                   . (#xf4))
    ((in     r8 imm8)                        . (#xe4 ib))   ; (in al imm8)
    ((in     r16 imm8)                       . (#xe5 ib))   ; (in ax imm8)
    ((in     al dx)                          . (#xec))
    ((in     ax dx)                          . (#xed))
    ((int    3)                              . (#xcc))
    ((int    imm8)                           . (#xcd ib))
    ((jmp    short (imm8 label imm16))       . (#xeb rb))
    ((lodsb)                                 . (#xac))
    ((lodsw)                                 . (#xad))
    ((mov    r8 imm8)                        . ((+ #xb0 r) ib))
    ((mov    r16 (imm16 imm8 imm label))     . ((+ #xb8 r) iw))
    ((mov    sreg r16)                       . (#x8e /r))   ; (mov sreg r/m16)
    ((mov    r16 sreg)                       . (#x8c /r))   ; (mov r/m16 sreg)
    ((nop)                                   . (#x90))
    ((out    imm8 r8)                        . (#xe6 ib))   ; (out imm8 al)
    ((out    imm8 r16)                       . (#xe7 ib))   ; (out imm8 ax)
    ((out    dx al)                          . (#xee))
    ((out    dx ax)                          . (#xef))
    ((pop    r16)                            . ((+ #x58 r)))
    ((pop    ss)                             . (#x17))
    ((pop    ds)                             . (#x1f))
    ((pop    es)                             . (#x07))
    ((push   r16)                            . ((+ #x50 r)))
    ((push   cs)                             . (#x0e))
    ((push   ss)                             . (#x16))
    ((push   ds)                             . (#x1e))
    ((push   es)                             . (#x06))
    ((rep    movsb)                          . (#xf3 #xa4))
    ((rep    movsw)                          . (#xf3 #xa5))
    ((ret)                                   . (#xc3))
    ((stc)                                   . (#xf9))
    ((std)                                   . (#xfd))
    ((sti)                                   . (#xfb))
    ((stosb)                                 . (#xaa))
    ((stosw)                                 . (#xab)))
  "Syntax table for x86-64. For each entry, 1st part is the
  instruction type, 2nd part is the corresponding opcode.  Note that
  for the 1st part, list may be used for the operand to match the
  type (e.g. imm8 converted to imm16). Note that the canonical form
  should be placed first (e.g. if the operand type should be imm16,
  place it as the car of the list)

  For details,
    refer to http://code.google.com/p/yalo/wiki/AssemblyX64Overview")

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
  (aif (assoc* e *x86-64-syntax* :test #'equal) 
       ;; Instructions with exact match, e.g. instructions without
       ;; operands (like nop, hlt), or special instructions like int 3.
       (copy-list it) ; copy-list is necessary since syntax table is
                      ; LITERAL.
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
         (t (multiple-value-bind (type opcode)
                (match-instruction (instruction-type e))
              (encode-complex e type opcode cursor))))))

(defun encode-complex (instruction type opcode cursor)
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
           (try-encode-bytes (instruction-value instruction type (on->in on))
                             (on-length on)))
          ((rb rw rd ro)
           (try-encode-bytes `(- ,(instruction-value instruction type (on->in on))
                                 ,(+ cursor 1 (on-length on)))
                             (on-length on)))
          (/r (list (encode-modr/m 
                     #b11 
                     (reg->int (instruction-value instruction type 'r16))
                     (sreg->int (instruction-value instruction type 'sreg)))))))
    (cdr opcode))))

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

(defun match-instruction (type)
  "Returns values of (type opcode).
   In the first run, when the type does not appear in syntax table,
     try to match immediate data with register length."
  (aif (assoc-x86-64-opcode type)
       (values (canonical-type (car it)) (copy-list (cdr it)))
       (error "match-instruction: unsupported instruction!")))

(defun canonical-type (type)
  "Return the canonical form of the type."
  (mapcar #'(lambda (x) (if (listp x) (car x) x)) type))

(defun assoc-x86-64-opcode (type)
  "Returns a associated opcode based on x86-64 syntax."
  (assoc type *x86-64-syntax* 
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

(defun encode-modr/m (mod rm reg)
  "Encode ModR/M byte."
  (+ (* mod #b1000000) (* reg #b1000) rm))

(defun encode-1-operand (dest reg)
  (encode-modr/m #b11 (reg->int dest) reg))

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
       ((short)                                   operand)
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

