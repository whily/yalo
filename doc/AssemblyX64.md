Assembly syntax used by CL cross compiler and Ink
=================================================

# Introduction

Lisp Assembly Program (LAP) uses list structure to contain assembly
code. Basically, atoms represent labels while lists represent (pseudo)
instructions. Local labels (starting with a period) are also supported.

Lisp expressions can be used for immediate values. In addition, since
LAP is an S-expression, macros could be used to mimic the macro
facility of *real* assemblers.

The syntax is similar to what NASM uses. Basically, Intel syntax is
used, i.e. destination operand precedes source operand.

To get a feeling about what LAP looks like, one may take a look at the
value of `*bootloader*` defined in https://github.com/whily/yalo/blob/master/cc/bootloader.lisp.

# Pseudo Instructions of Assembly.

Directives (e.g. bits, origin) are categorized as pseudo instructions
here too.

## bits: Bit Mode

| Instruction | Description           |
| ----------- | --------------------- |
| bits 16     | 16-bit mode           |
| bits 64     | 64-bit mode (default) |

## db: Declaring initialized byte value(s)

| Instruction                         | Description | 
| ----------------------------------- | ----------- |
| db [_var_] [_number_ | _string_]    | Declare byte values via atoms |
| db [_var_] ([_number_ | _string_]*) | Declare byte values via lists |

Note:

* _number_ should be in range -128..+255. Expression within the same
range can be used as well.
* _string_ is converted byte by bytes.

## dw: Declaring initialized word value(s)

| Instruction            | Description                    |
| ---------------------- | ------------------------------ |
| dw [_var_] _number_    | Declare word value via numbers |
| dw [_var_] (_number_)* | Declare word value via lists   |

Note:

* _number_ should be in range -32,768..+65,535. Expression within the same
range can be used as well.

## dd: Declaring initialized doubleword value(s) 

| Instruction            | Description                          |
| ---------------------- | ------------------------------------ |
| dd [_var_] _number_    | Declare doubleword value via numbers |
| dd [_var_] (_number_)* | Declare doubleword value via lists   |

Note:

* _number_ should be in range -2,147,483,648..+4,294,967,295.
Expression within the same range can be used as well.

## dq: Declaring initialized quadword value(s)

| Instruction            | Description                        |
| ---------------------- | ---------------------------------- |
| dq [_var_] _number_    | Declare quadword value via numbers |
| dq [_var_] (_number_)* | Declare quadword value via lists   |

Note:

* _number_ should be in range 
-9,223,372,036,854,775,808..+18,446,744,073,709,551,615.
Expression within the same range can be used as well.

## equ: Defint Constant

| Instruction        | Description                               |
| ------------------ | ----------------------------------------- |
| equ _const_ _expr_ | Define constant _const_ with value _expr_ |

Note:

* _expr_ should be evaulated when encoutering the instruction,
i.e. forward reference is not allowed.

## org: Define Origin 

| Instruction | Description             |
| ----------- | ----------------------- |
| org val     | Define origin to be val | 

Note:

* *org* should precede any (pseudo) instructions that actually generating code.

## times: Repeat instruction =

| Instruction                 | Description                             |
| --------------------------- | --------------------------------------- |
| times _count_ _instruction_ | Assemble _count_ times of _instruction_ |

Note:

* _count_ could be an expression which can be evaluated when *times*
instruction is encoutered, e.g. containing $, $$, or labels defined
before *times* instruction.
* _instruction_ can be any other (pseudo) instructions.

# Special Variables of Assembly.

Following special variables are supported:
* *$* Location of the start of current instruction.
* *$$* Origin (start address) of current assembly program.

# Overview of x86-64 Assembly

## Intruction

This wiki lists the machine instructions supported by x86-64 LAP, and
the short description of the functions for each instruction. One
should refer to Intel or AMD manuals for complete reference.

For each mnemonic, one table is provided to list the supported
instructions. There are 5 columns in the table:
* *Instruction*: documents mnemonic and operands.
* *Opcode*: documents the translated opcode.
* *64-bit Mode*: whether the instruction is supported in 64-bit mode. 
* *16/32-bit Mode*: whether the instruction is supported in 16/32-bit mode.
* *Description*: short description about the instruction.

*16-bit Mode* is used for bootloaders only.

*32-bit Mode* is provided for completeness only.

For columns *64-bit Mode* and *16/32-bit Mode*, following notations are used:
* *Valid*: supported.
* *Invalid*: not supported.
* *N.E.*: not encodable. The opcode may be part of a sequence of other valid instructions.
* *N.S.*: not supported. The instruction requires an address override prefix.

## Notations for Instructions
  
Following notations are used:
* *imm8*: immediate byte value in the range of -128..+255.
* *imm16*: immediate word value in the range of -32,768..+65,535.
* *imm32*: immediate doubleword value in the range of -2,147,483,648..+4,294,967,295.
* *imm64*: immediate quadword value in the range of -9,223,372,036,854,775,808..+18,446,744,073,709,551,615..
* *!r8*: one of the byte general-purpose registers: al, cl, dl, bl, ah, ch, dh, bh, bpl, spl, dil, and sil.
* *!r16*: one of the word general-purpose registers: ax, cx, dx, bx, sp, bp, si, di.
* *!r32*: one of the doubleword general-purpose registers: eax, ecx, edx, ebx, esp, ebp, esi, edi.
* *!r64*: one of the quadword general-purpose registers: rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, !r8-!r15.
* *m8, m16, m32, m64, m128*: memory references. Specifier (`byte`, `word`, `dword`, `qword`) is needed for ambiguous cases. For example in  `(mov word [12345] 16).
* *m16&32*, *m16&16*, *m32&32*, *m16&64*, a memory operand containing one pair whose sizes are indicated on the left and right size of the ampersand.
* *r/m8, r/m16, r/m32, r/m64*: register or memeory choices. For example, r/m8 means that either r8 or m8 can be used as operand.
* *sreg*: segment register.

## Range of Immediate Values

Note that the range for imm8/16/32/64 is defined considering the
capcity of 8/16/32/64 bits. The range might be too broad for jmp like
instructions which require *true* signed integers, however the impact
is minimal.

## Memory Address Operands

Memory address operands are supported, e.g. `(bp)`, `(var)`, `(bp si
5)`. Note that the order is flexible. For example, `(bp si 3)` is
equivalent to `(si 3 bp)`.

For scaled index of 32-bit addressing modes, one has to use the form
of register*scale, e.g. eax*8. 

## Notations for Opcodes

Following notations are used:
* A hex number, such as CC, indicates a fixed byte containing that number.
* A hex number followed by *+r*, like *B0+r*, indicates that one of the operands is a register, and correspondign register value should be added to the opcode.
* */n* (where n is 0 to 7): indictes that one of the operand is r/m, and the field Reg/Opcode should be encoded with n.
* */r*: ModR/M byte of the instruction contains a register operand (encoded in field Reg/Opcode) and an r/m operand (encoded in field R/M).
* *rb, rw, rd, ro*: one of the operands is an immediate value, and the _difference_ between this value and the end address of the instruction is to be encoded as byte (rb), little-endian word (rw), little-endian doubleword (rd), and little-endian quadword (ro) respectively.
* *ib, iw, id, io*: one of the operands is an immediate value, and it is to be encoded as byte (rb), little-endian word (rw), little-endian doubleword (rd), and little-endian quadword (ro) respectively.
* *o16, o32*: operand-size override prefix. o16 generates no code in 16-bit mode, but indicates a 66h prefix in 32/64-bit mode; similarly, o32 generates no code in 32/64-bit mode, but indicates a 66h prefix in 16-bit mode.
* *a16, a32*: address-size override prefix. a16 generates no code in 16-bit mode, but indicates a 67h prefix in 32/64-bit mode; similarly, a32 generates no code in 32-bit mode, but indicates a 67h prefix in 16/64-bit mode.

Note that REX prefix are not used in opcode notations. The prefix is
automatically generated by analyzing the operands.

### Conditional Codes

Conditional codes are used for Jcc and CMOVcc instructions, which are
encoded as (+ xx cc). Meaning of conditional codes are listed below.

| cc       | value | trigger flag |
| -------- | ----- | ------------ |
| o        | 0     | overflow flag set                                                            |
| no       | 1     | overflow flag not set                                                        |
| b c nae  | 2     | carry flag set                                                               |
| ae nb nc | 3     | carry flag not set                                                           |
| e z      | 4     | zero flag set                                                                |
| ne nz    | 5     | zero flag not set                                                            |
| be na    | 6     | either of carry or zero flag set                                             |
| a nbe    | 7     | neither carry nor zero flag set                                              |
| s        | 8     | sign flag set                                                                |
| ns       | 9     | sign flag not set                                                            |
| p pe     | 10    | parity flag set                                                              |
| np po    | 11    | parity flag not set                                                          |
| l nge    | 12    | exactly one of sign and overflow flag is set                                 |
| ge nl    | 13    | opposite case of above                                                       |
| le ng    | 14    | either the zero flag is set, or exactly one of sign and overflow flag is set |
| g nle    | 15    | opposite case of above                                                       |

 
