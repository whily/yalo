x86-64 Instruction Set L
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
L [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### lgdt/lidt/lldt: Load Descriptor Tables

| Instruction | Opcode   |
| ----------- | -------- |
| lgdt m16&32 | 0F 01 /2 |
| lidt m16&32 | 0F 01 /3 |
| lldt r/m16  | 0F 00 /2 |

### lodsb/lodsw: Load String

| Instruction | Opcode   |
| ----------- | -------- |
| lodsb       | AC       |
| lodsw       | o16 AD   |
| lodsd       | o32 AD   |
| lodsq       | REX.W AD |

### loop: Loop According to (E)CX Counter

| Instruction | Opcode |
| ----------- | ------ |
| loop imm16  | E2 cb  |
