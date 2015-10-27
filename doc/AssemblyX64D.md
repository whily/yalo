x86-64 Instruction Set D
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
D [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### dec: Decrement by 1

| Instruction | Opcode      |
| ----------- | ----------- |
| dec r/m8    | FE /1       |
| dec r/m16   | o16 FF /1   |
| dec r/m32   | o32 FF /1   |
| dec r/m64   | REX.W FF /1 |
| dec r16     | o16 48+r    |
| dec r32     | o32 48+r    |

### div: Unsigned Divide

| Instruction | Opcode      |
| ----------- | ----------- |
| div r/m8    | F6 /6       |
| div r/m16   | o16 F7 /6   |
| div r/m32   | o32 F7 /6   |
| div r/m64   | REX.W F7 /6 |
