x86-64 Instruction Set I
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) I [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### in: Input from Port

| Instruction | Opcode |
| ----------- | ------ |
| in al imm8  | E4 ib  |
| in ax imm8  | E5 ib  |
| in al dx    | EC     |
| in ax dx    | ED     |

### inc: Increment by 1

| Instruction | Opcode      |
| ----------- | ----------- |
| inc r/m8    | FE /0       |
| inc r/m16   | o16 FF /0   |
| inc r/m32   | o32 FF /0   |
| inc r/m64   | REX.W FF /0 |
| inc r16     | o16 40+r    |
| inc r32     | o32 40+r    |

### int: Call Interrupt Procedure

| Instruction | Opcode |
| ----------- | ------ |
| int 3       | CC     |
| int imm8    | CD ib  |

### invlpg: Invalidate TLB entries

| Instruction | Opcode   |
| ----------- | -------- |
| invlpg m    | 0F 01 /7 |
