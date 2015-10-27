x86-64 Instruction Set C
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) C
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### call: Call Procedure

| Instruction | Opcode     |
| ----------- | ---------- |
| call imm16  | o16 E8 cw  |
| call imm32  | o32 E8 cd  |

### clc: Clear Carry Flag

| Instruction | Opcode |
| ----------- | ------ |
| clc         | F8     |

### cld: Clear Direction Flag
| Instruction | Opcode |
| ----------- | ------ |
| cld         | FC     |

### cli: Clear Interrupt Flag

| Instruction | Opcode |
| ----------- | ------ |
| cli         | FA     |

### cmovcc: Conditional Move

| Instruction       | Opcode                |
| ----------------- | --------------------- |
| cmovcc r16 r/m16  | o16 0F (+ 40 cc) /r   |
| cmovcc r32 r/m32  | o32 0F (+ 40 cc) /r   |
| cmovcc r64 r/m64  | REX.W 0F (+ 40 cc) /r |

Please refer [x86-64 conditional codes](AssemblyX64.md#conditional-codes) for details.

### cmp: Compare

Please refer to [x86-64 arithmetic instructions](AssemblyX64Arith.md) for details.

### cmpxchg: Compare and Exchange

| Instruction       | Opcode             |
| ----------------- | ------------------ |
| cmpxchg r/m8 r8   | 0F B0 /r           |
| cmpxchg r/m16 r16 | o16 0F B1 /r       |
| cmpxchg r/m32 r32 | o32 0F B1 /r       |
| cmpxchg r/m64 r64 | REX.W 0F B1 /r     |

### cmpxchg8b/cmpxchg16b: Compare and Exchange Bytes

| Instruction     | Opcode         |
| --------------- | -------------- |
| cmpxchg8b m64   | 0F C7 /1       |
| cmpxchg16b m128 | REX.W 0F C7 /1 |
