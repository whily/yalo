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

### sal/sar/shl/shr: Shift

| Instruction    | Opcode             |
| -------------- | ------------------ |
| ins r/m8, 1    | D0 opcode          |
| ins r/m8, cl   | D2 opcode          |
| ins r/m8, imm8 | C0 opcode ib       |
| ins r/m16 1    | o16 D1 opcode      |
| ins r/m16 cl   | o16 D3 opcode      |
| ins r/m16 imm8 | o16 C1 opcode ib   |
| ins r/m32 1    | o32 D1 opcode      |
| ins r/m32 cl   | o32 D3 opcode      |
| ins r/m32 imm8 | o32 C1 opcode ib   |
| ins r/m64 1    | REX.W D1 opcode    |
| ins r/m64 cl   | REX.W D3 opcode    |
| ins r/m64 imm8 | REX.W C1 opcode ib |

The opcodes are /4, /7, /4, 5 for sal, sar, shl, shr, respectively.

Note that sal and shl perform the same operation.

### setcc: Set Byte on Condition

| Instruction       | Opcode                |
| ----------------- | --------------------- |
| setcc r/m8        | 0F (+ 90 cc) /0       |

Please refer [x86-64 conditional codes](AssemblyX64.md#conditional-codes) for details.

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
