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

| Instruction    | Opcode          |
| -------------- | --------------- |
| jcc imm        | (+ 70 cc) cb    |
| jcc near imm   | 0F (+ 80 cc) cd |
| jcxz imm       | a16 E3 cb       |
| jecxz imm      | a32 E3 cb       |
| jrcxz imm      | E3 cb           |

Please refer to [conditional codes](AssemblyX64.md) for details.

### jmp: Jump

| Instruction    | Opcode                 |
| -------------- | --------               |
| jmp short imm  | EB cb                  |
| jmp near imm   | E9 cw (for 16/32 bits) |
| jmp near imm   | E9 cd (for 64 bit)     |
| jmp near r/m64 | FF /4                  |
