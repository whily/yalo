x86-64 Instruction Set R
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) R
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### rdmsr: Read From Model Specific Register

| Instruction | Opcode |
| ----------- | ------ |
| rdmsr       | 0F 32  |

### rep: Repeat String Operation Prefix

Following are examples.

| Instruction | Opcode |
| ----------- | ------ |
| rep movsb   | F3 A4  |
| rep movsw   | F3 A5  |

Note

*rep* is a kind of prefix.

### ret: Return From Procedure

| Instruction | Opcode |
| ----------- | ------ |
| ret         | C3     |
