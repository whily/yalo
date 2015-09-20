x86-64 Instruction Set N
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) N
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### neg: Two's Complement Negation

| Instruction | Opcode    | 64-Bit Mode | 16/32-Bit Mode | Description                   |
| ----------- | --------- | ----------- | -------------- | ----------------------------- |
| neg r/m8    | F6 /3     | Valid       | Valid          | Two's complement negate r/m8  |
| neg r/m16   | o16 F7 /3 | Valid       | Valid          | Two's complement negate r/m16 |
| neg r/m32   | o32 F7 /3 | Valid       | Valid          | Two's complement negate r/m32 |
| neg r/m64   | F7 /3     | Valid       | ~~N.E.~~       | Two's complement negate r/m64 |

Replaces the value of destination operand with its two's complement (-dest).

### nop: No Operation

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description  |
| ----------- | ------ | ----------- | -------------- | ------------ |
| nop         | 90     | Valid       | Valid          | No operation |

### not: One's Complement Negation

| Instruction | Opcode    | 64-Bit Mode | 16/32-Bit Mode | Description               |
| ----------- | --------- | ----------- | -------------- | ------------------------- |
| not r/m8    | F6 /2     | Valid       | Valid          | Reverse each bit of r/m8  |
| not r/m16   | o16 F7 /2 | Valid       | Valid          | Reverse each bit of r/m16 |
| not r/m32   | o32 F7 /2 | Valid       | Valid          | Reverse each bit of r/m32 |
| not r/m64   | F7 /2     | Valid       | ~~N.E.~~       | Reverse each bit of r/m64 |

Replaces the value of destination operand with its one's complement (bitwise NOT).
