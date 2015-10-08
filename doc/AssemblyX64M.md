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

| Instruction     | Opcode       | 64-Bit Mode | 16/32-Bit Mode | Description                    |
| --------------- | ------------ | ----------- | -------------- | ------------------------------ |
| mov r8 imm8     | B0+r ib      | Valid       | Valid          | Move imm8 to r8                |
| mov r16 imm16   | o16 B8+r iw  | Valid       | Valid          | Move imm16 to r16              |
| mov r32 imm32   | o32 B8+r id  | Valid       | Valid          | Move imm32 to r32              |
| mov r64 imm64   | B8+r io      | Valid       | ~~N.E.~~       | Move imm64 to r364             |
| mov r/m16 r16   | o16 89 /r    | Valid       | Valid          | Move r16 to r/m16              |
| mov r/m32 r32   | o32 89 /r    | Valid       | Valid          | Move r32 to r/m32              |
| mov r/m64 r64   | 89 /r        | Valid       | ~~N.E.~~       | Move r64 to r/m64              |
| mov r16 r/m16   | o16 8B /r    | Valid       | Valid          | Move r/m16 to r16              |
| mov r32 r/m32   | o32 8B /r    | Valid       | Valid          | Move r/m32 to r32              |
| mov r64 r/m64   | 8B /r        | Valid       | Valid          | Move r/m64 to r64              |
| mov r/m8 imm8   | C6 /0 ib     | Valid       | Valid          | Move imm8 to r/m8              |
| mov r/m16 imm16 | o16 C7 /0 iw | Valid       | Valid          | Move imm16 to r/m16            |
| mov r/m32 imm32 | o32 C7 /0 id | Valid       | Valid          | Move imm32 to r/m32            |
| mov r/m64 imm32 | C7 /0 io     | Valid       | ~~N.E.~~       | Move imm32 sign extended to 64 bits to r/m64 |
| mov sreg r/m16  | 8E /r        | Valid       | Valid          | Move r/m16 to segment register |
| mov r/m16 sreg  | 8C /r        | Valid       | Valid          | Move segment register to r/m16 |

### movsb/movsw: Move Data from String to String

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description                    |
| ----------- | ------ | ----------- | -------------- | ------------------------------ |
| movsb       | A4     | Valid       | Valid          | Move byte from DS:SI to ES:DI  |
| movsw       | o16 A5 | Valid       | Valid          | Move word from DS:SI to ES:DI  |
| movsd       | o32 A5 | Valid       | Valid          | Move dword from DS:SI to ES:DI |

### movzx: Move with Zero-Extend

| Instruction     | Opcode       | 64-Bit Mode | 16/32-Bit Mode | Description                             |
| --------------- | ------------ | ----------- | -------------- | --------------------------------------- |
| movzx r16 r/m8  | o16 0F B6 /r | Valid       | Valid          | Move byte to word, zero extension       |
| movzx r32 r/m8  | o32 0F B6 /r | Valid       | Valid          | Move byte to doubleword, zero extension |
| movzx r64 r/m8  | 0F B6 /r     | Valid       | ~~N.E.~~       | Move byte to quadword, zero extension   |
| movzx r32 r/m16 | o32 0F B7 /r | Valid       | Valid          | Move word to doubleword, zero extension |
| movzx r64 r/m16 | 0F B7 /r     | Valid       | ~~N.E.~~       | Move word to quadword, zero extension   |

### mul: Unsigned Multiply

| Instruction | Opcode     | 64-Bit Mode | 16/32-Bit Mode | Description                               |
| ----------- | ---------- | ----------- | -------------- |------------------------------------------ |
| mul r/m8    | F6 /4      | Valid       | Valid          | Unsigned multiply (ax <- al * r/m8)       |
| mul r/m16   | o16 F7 /4  | Valid       | Valid          | Unsigned multiply (dx:ax <- ax * r/m16    |
| mul r/m32   | o32 F7 /4  | Valid       | Valid          | Unsigned multiply (edx:eax <- ax * r/m32  |
| mul r/m64   | F7 /4      | Valid       | ~~N.E.~~       | Unsigned multiply (rdx:rax <- rax * r/m64 |
