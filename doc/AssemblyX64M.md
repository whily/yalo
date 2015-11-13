x86-64 Instruction Set M
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) M [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### mov: Move

| Instruction     | Opcode         |
| --------------- | -------------- |
| mov r8 imm8     | B0+r ib        |
| mov r16 imm16   | o16 B8+r iw    |
| mov r32 imm32   | o32 B8+r id    |
| mov r64 imm64   | REX.W B8+r io  |
| mov r/m8 r8     | 88 /r          |
| mov r/m16 r16   | o16 89 /r      |
| mov r/m32 r32   | o32 89 /r      |
| mov r/m64 r64   | REX.W 89 /r    |
| mov r8 r/m8     | 8A /r          |
| mov r16 r/m16   | o16 8B /r      |
| mov r32 r/m32   | o32 8B /r      |
| mov r64 r/m64   | REX.W 8B /r    |
| mov r/m8 imm8   | C6 /0 ib       |
| mov r/m16 imm16 | o16 C7 /0 iw   |
| mov r/m32 imm32 | o32 C7 /0 id   |
| mov r/m64 imm32 | REX.W C7 /0 id |
| mov sreg r/m16  | 8E /r          |
| mov r/m16 sreg  | 8C /r          |

### mov: Move to/from Control Registers

| Instruction     | Opcode       |
| --------------- | ------------ |
| mov r32 cr0-cr7 | 0F 20 /r     |
| mov cr0-cr7 r32 | 0F 22 /r     |

### movsb/movsw: Move Data from String to String

| Instruction | Opcode   |
| ----------- | -------- |
| movsb       | A4       |
| movsw       | o16 A5   |
| movsd       | o32 A5   |
| movsq       | REX.W A5 |

### movzx: Move with Zero-Extend

| Instruction     | Opcode         |
| --------------- | -------------- |
| movzx r16 r/m8  | o16 0F B6 /r   |
| movzx r32 r/m8  | o32 0F B6 /r   |
| movzx r64 r/m8  | REX.W 0F B6 /r |
| movzx r32 r/m16 | o32 0F B7 /r   |
| movzx r64 r/m16 | REX.W 0F B7 /r |

### mul: Unsigned Multiply

| Instruction | Opcode      |
| ----------- | ----------- |
| mul r/m8    | F6 /4       |
| mul r/m16   | o16 F7 /4   |
| mul r/m32   | o32 F7 /4   |
| mul r/m64   | REX.W F7 /4 |
