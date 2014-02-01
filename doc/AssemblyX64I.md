x86-64 Instruction Set I
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [H](AssemblyX64H.md) I
[J](AssemblyX64J.md) [L](AssemblyX64L.md) [M](AssemblyX64M.md)
[N](AssemblyX64N.md) [O](AssemblyX64O.md) [P](AssemblyX64P.md)
[R](AssemblyX64R.md) [S](AssemblyX64S.md) [T](AssemblyX64T.md)
[X](AssemblyX64X.md)

### in: Input from Port

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description                                      |
| ----------- | ------ | ----------- | -------------- | ------------------------------------------------ |
| in al imm8  | E4 ib  | Valid       | Valid          | Input byte from I/O port address in imm8 into al |
| in ax imm8  | E5 ib  | Valid       | Valid          | Input word from I/O port address in imm8 into ax |
| in al dx    | EC     | Valid       | Valid          | Input byte from I/O port address in dx into al   |
| in ax dx    | ED     | Valid       | Valid          | Input word from I/O port address in dx into ax   |

### inc: Increment by 1

| Instruction | Opcode     | 64-Bit Mode | 16/32-Bit Mode | Description          |
| ----------- | ---------- | ----------- | -------------- | -------------------- |
| inc r/m8    | FE /0      | Valid       | Valid          | Increment r/m8 by 1  |
| inc r/m16   | o16 FF /0  | Valid       | Valid          | Increment r/m16 by 1 |
| inc r/m32   | o32 FF /0  | Valid       | Valid          | Increment r/m32 by 1 |
| inc r/m64   | FF /0      | Valid       | ~~N.E.~~       | Increment r/m64 by 1 |
| inc r16     | o16 40+r   | ~~N.E.~~    | Valid          | Increment r/16 by 1  |
| inc r32     | o32 40+r   | ~~N.E.~~    | Valid          | Increment r/32 by 1  |

### int: Call Interrupt Procedure

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description      |
| ----------- | ------ | ----------- | -------------- | ---------------- |
| int 3       | CC     | Valid       | Valid          | Trap to debugger |
| int imm8    | CD ib  | Valid       | Valid          | Soft interrupt   |
