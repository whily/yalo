x86-64 Instruction Set S
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
S [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### sbb: subtract with borrow

Please refer to [x86-64 arithmetic instructions](AssemblyX64Arith.md) for details.

### shl/shr: Shift

| Instruction    | Opcode   |
| -------------- | -------- |
| shl r/m8, 1    | D0 /4    |
| shl r/m8, cl   | D2 /4    |
| shl r/m8, imm8 | C0 /4 ib |
| shl r/m16 1    | D1 /4    |
| shl r/m16 cl   | D3 /4    |
| shl r/m16 imm8 | C1 /4 ib |
| shr r/m8, 1    | D0 /5    |
| shr r/m8, cl   | D2 /5    |
| shr r/m8, imm8 | C0 /5 ib |
| shr r/m16 1    | D1 /5    |
| shr r/m16 cl   | D3 /5    |
| shr r/m16 imm8 | C1 /5 ib |

### stc: Set Carry Flag

| Instruction | Opcode |
| ----------- | ------ |
| stc         | F9     |

### std: Set Direction Flag
| Instruction | Opcode |
| ----------- | ------ |
| std         | FD     |

### sti: Set Interrupt Flag

| Instruction | Opcode |
| ----------- | ------ |
| sti         | FB     |

### stosb/stosw/stosd: Store String

| Instruction | Opcode   |
| ----------- | -------- |
| stosb       | AA       |
| stosw       | o16 AB   |
| stosd       | o32 AB   |
| stosq       | REX.W AB |

### sub: Sub

Please refer to [x86-64 arithmetic instructions](AssemblyX64Arith.md) for details.

### syscall/sysret: (Return from) Fast System Call

| Instruction | Opcode |
| ----------- | ------ |
| syscall     | 0F 05  |
| sysret      | 0F 07  |
