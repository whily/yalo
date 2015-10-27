x86-64 Instruction Set X
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) X

### xadd: Exchange and Add

| Instruction    | Opcode         |
| -------------- | -------------- |
| xadd r/m8 r8   | 0F C0 /r       |
| xadd r/m16 r16 | o16 0F C1 /r   |
| xadd r/m32 r32 | o32 0F C1 /r   |
| xadd r/m64 r64 | REX.W 0F C1 /r |

### xchg: Exchange

| Instruction    | Opcode      |
| -------------- | ----------- |
| xchg ax r16    | o16 90+r    |
| xchg r16 ax    | o16 90+r    |
| xchg eax r32   | o32 90+r    |
| xchg r32 eax   | o32 90+r    |
| xchg rax r64   | REX.W 90+r  |
| xchg r64 rax   | REX.W 90+r  |
| xchg r/m8 r8   | 86 /r       |
| xchg r8 r/m8   | 86 /r       |
| xchg r/m16 r16 | o16 87 /r   |
| xchg r16 r/m16 | o16 87 /r   |
| xchg r/m32 r32 | o32 87 /r   |
| xchg r32 r/m32 | o32 87 /r   |
| xchg r/m64 r64 | REX.W 87 /r |
| xchg r64 r/m64 | REX.W 87 /r |

Note that when a memory operand is referenced, lock protocal is
automatically implemented, regardless whether LOCK prefix is used or
not. This may has impact on performance.

### xor: Logical Exclusive OR

Please refer to [x86-64 arithmetic instructions](AssemblyX64Arith.md) for details.
