x86-64 Instruction Set J
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) J
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### jcc: Conditional Branch

| Instruction    | Opcode          | 64-Bit Mode | 16/32-Bit Mode | Description            |
| -------------- | --------------- | ----------- | -------------- | ---------------------- |
| jcc imm        | (+ 70 cc) rb    | Valid       | Valid          | Short conditional jump |
| jcc near imm   | 0F (+ 80 cc) rd | Valid       | Valid          | Near conditional jump  |
| jcxz imm       | a16 E3 rb       | ~~N.E.~~    | Valid          | Jump short if cx=0     |
| jecxz imm      | a32 E3 rb       | Valid       | Valid          | Jump short if ecx=0    |
| jrcxz imm      | E3 rb           | Valid       | ~~N.E.~~       | Jump short if rcx=0    |

Please refer to [conditional codes](AssemblyX64.md) for details.

### jmp: Jump

| Instruction    | Opcode   | 64-Bit Mode | 16/32-Bit Mode | Description        |
| -------------- | -------- | ----------- | -------------- | ------------------ |
| jmp short imm  | EB rb    | Valid       | Valid          | Jump short         |
| jmp near imm   | E9 rw    | ~~N.S.~~    | Valid          | Jump near          |
