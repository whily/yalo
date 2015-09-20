x86-64 Instruction Set T
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) T [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### test: Logical Compare

| Instruction      | Opcode   | 64-Bit Mode | 16/32-Bit Mode | Description         |
| ---------------- | -------- | ----------- | -------------- | --------------------|
| test al imm8     | A8 ib    | Valid       | Valid          | AND imm8 with al                  |
| test ax imm16    | A9 iw    | Valid       | Valid          | AND imm16 with ax                 |
| test r/m8 imm8   | F6 /0 ib | Valid       | Valid          | AND imm8 with r/m8                |
| test r/m16 imm16 | F7 /0 iw | Valid       | Valid          | AND imm16 with r/m16              |
| test r/m8 r8     | 84 /r    | Valid       | Valid          | AND r8 with r/m8                  |
| test r/m16 r16   | 85 /r    | Valid       | Valid          | AND r16 with r/m16                |

After ANDing the two operands, flags SF, ZF, and PF are set according
to the result. The destination operand is *not* modified.
