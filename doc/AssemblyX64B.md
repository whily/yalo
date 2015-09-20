x86-64 Instruction Set B
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) B [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [H](AssemblyX64H.md) [I](AssemblyX64I.md)
[J](AssemblyX64J.md) [L](AssemblyX64L.md) [M](AssemblyX64M.md)
[N](AssemblyX64N.md) [O](AssemblyX64O.md) [P](AssemblyX64P.md)
[R](AssemblyX64R.md) [S](AssemblyX64S.md) [T](AssemblyX64T.md)
[X](AssemblyX64X.md)

### bswap: Byte Swap

| Instruction | Opcode   | 64-Bit Mode | 16/32-Bit Mode | Description               |
| ----------- | -------- | ----------- | -------------- | ------------------------- |
| bswap r32   | 0F C8+r  | Valid       | Valid          | Reverse byte order of r32 |
| bswap r64   | 0F C8+r  | Valid       | ~~N.E.~~       | Reverse byte order of r64 |

### bt: Bit Test

| Instruction   | Opcode      | 64-Bit Mode | 16/32-Bit Mode | Description                   |
| ------------- | ----------- | ----------- | -------------- | ----------------------------- |
| bt r/m16 r16  | 0F A3 /r    | Valid       | Valid          | Store selected bit in CF flag |
| bt r/m32 r32  | 0F A3 /r    | Valid       | Valid          | Store selected bit in CF flag |
| bt r/m32 r32  | 0F A3 /r    | Valid       | ~~N.E.~~       | Store selected bit in CF flag |
| bt r/m16 imm8 | 0F BA /4 ib | Valid       | Valid          | Store selected bit in CF flag |
| bt r/m32 imm8 | 0F BA /4 ib | Valid       | Valid          | Store selected bit in CF flag |
| bt r/m64 imm8 | 0F BA /4 ib | Valid       | ~~N.E.~~       | Store selected bit in CF flag |
