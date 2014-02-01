x86-64 Arithmetic Instruction
=============================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [H](AssemblyX64H.md) [I](AssemblyX64I.md)
[J](AssemblyX64J.md) [L](AssemblyX64L.md) [M](AssemblyX64M.md)
[N](AssemblyX64N.md) [O](AssemblyX64O.md) [P](AssemblyX64P.md)
[R](AssemblyX64R.md) [S](AssemblyX64S.md) [T](AssemblyX64T.md)
[X](AssemblyX64X.md)

There are 8 arithmetic instructions which have similar formats of
instructions. They are discussed together here.

The instructions are:
* adc: add with carry
* add: add
* and: logical and
* cmp: compare
* or:  logical or
* sbb: subtract with borrow
* sub: subtract
* xor: logical exclusive or

The format of the instructions are given below.

| Instruction     | Opcode             | 64-Bit Mode | 16/32-Bit Mode |
| --------------- | ------------------ | ----------- | -------------- | 
| ins al imm8     | (+ base 04) ib     | Valid       | Valid          |
| ins ax imm16    | o16 (+ base 05) iw | Valid       | Valid          |
| ins eax imm32   | o32 (+ base 05) id | Valid       | Valid          |
| ins rax imm32   | (+ base 05) id     | Valid       | ~~N.E.~~       |
| ins r/m8 imm8   | 80 opcode ib       | Valid       | Valid          |
| ins r/m16 imm16 | o16 81 opcode iw   | Valid       | Valid          |
| ins r/m32 imm32 | o32 81 opcode id   | Valid       | Valid          |
| ins r/m64 imm32 | 81 opcode id       | Valide      | ~~N.E.~~       |
| ins r/m16 imm8  | o16 83 opcode ib   | Valid       | Valid          |
| ins r/m32 imm8  | o32 83 opcode ib   | Valid       | Valid          |
| ins r/m64 imm8  | 83 opcode ib       | Valid       | ~~N.E.~~       |
| ins r/m8 r8     | base /r            | Valid       | Valid          |
| ins r/m16 r16   | o16 (+ base 01) /r | Valid       | Valid          |
| ins r/m32 r32   | o32 (+ base 01) /r | Valid       | Valid          |
| ins r/m64 r64   | (+ base 01) /r     | Valid       | ~~N.E.~~       |
| ins r8 r/m8     | (+ base 02) /r     | Valid       | Valid          |
| ins r16 r/m16   | o16 (+ base 03) /r | Valid       | Valid          |
| ins r32 r/m32   | o32 (+ base 03) /r | Valid       | Valid          |
| ins r64 r/m64   | (+ base 03) /r     | Valid       | ~~N.E.~~       |

Base and opcode in above table are given below for each instruction.

| Instruction | Base (hexadecimal) | Opcode |
| ----------- | ------------------ | ------ |
| adc         | 10                 | /2     |
| add         | 00                 | /0     |
| and         | 20                 | /4     |
| cmp         | 38                 | /7     |
| or          | 08                 | /1     |
| sbb         | 18                 | /3     |
| sub         | 28                 | /5     |
| xor         | 30                 | /6     |

