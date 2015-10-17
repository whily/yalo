x86-64 Instruction Set B
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) B [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### bswap: Byte Swap

| Instruction | Opcode   | 64-Bit Mode | 16/32-Bit Mode | Description               |
| ----------- | -------- | ----------- | -------------- | ------------------------- |
| bswap r32   | 0F C8+r  | Valid       | Valid          | Reverse byte order of r32 |
| bswap r64   | 0F C8+r  | Valid       | ~~N.E.~~       | Reverse byte order of r64 |

### bt: Bit Test

Please refer to [x86-64 bit instructions](AssemblyX64Bit.md) for details.

### btc: Bit Test and Complement

Please refer to [x86-64 bit instructions](AssemblyX64Bit.md) for details.

### btr: Bit Test and Reset

Please refer to [x86-64 bit instructions](AssemblyX64Bit.md) for details.

### bts: Bit Test and Set

Please refer to [x86-64 bit instructions](AssemblyX64Bit.md) for details.
