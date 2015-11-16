x86-64 Instruction Set P
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) P [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### pop: Pop a Value from the Stack

| Instruction | Opcode   |
| ----------- | -------- |
| pop r16     | o16 58+r |
| pop r32     | o32 58+r |
| pop r64     | 58+r     |
| pop ss      | 17       |
| pop ds      | 1F       |
| pop es      | 07       |
| popf        | o16 9D   |
| popfd       | o32 9D   |
| popfq       | 9D       |

### popcnt: Return the Count of the Number of Bits Set to 1

| Instruction      | Opcode            |
| ---------------- | ----------------- |
| popcnt r64 r/m64 | F3 REX.W 0F B8 /r |

### push: Push a Value Onto the Stack

| Instruction | Opcode   |
| ----------- | -------- |
| push r16    | o16 50+r |
| push r32    | o32 50+r |
| push r64    | 50+r     |
| push cs     | 0E       |
| push ss     | 16       |
| push ds     | 1E       |
| push es     | 06       |
| pushf       | o16 9C   |
| pushfd      | o32 9C   |
| pushfq      | 9C       |
