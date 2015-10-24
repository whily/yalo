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

| Instruction | Opcode      | 64-Bit Mode | 16/32-Bit Mode | Description          |
| ----------- | ----------- | ----------- | -------------- | -------------------- |
| dec r/m8    | FE /1       | Valid       | Valid          | Decrement r/m8 by 1  |
| dec r/m16   | o16 FF /1   | Valid       | Valid          | Decrement r/m16 by 1 |
| dec r/m32   | o32 FF /1   | Valid       | Valid          | Decrement r/m32 by 1 |
| dec r/m64   | REX.W FF /1 | Valid       | ~~N.E.~~       | Decrement r/m64 by 1 |
| dec r16     | o16 48+r    | ~~N.E.~~    | Valid          | Decrement r/16 by 1  |
| dec r32     | o32 48+r    | ~~N.E.~~    | Valid          | Decrement r/32 by 1  |

### div: Unsigned Divide

| Instruction | Opcode      | 64-Bit Mode | 16/32-Bit Mode | Description                                               |
| ----------- | ----------- | ----------- | -------------- | --------------------------------------------------------- |
| div r/m8    | F6 /6       | Valid       | Valid          | Unsigned divide ax by r/m8, al <- quot, ah <- rem         |
| div r/m16   | o16 F7 /6   | Valid       | Valid          | Unsigned divide dx:ax by r/m16, ax <- quot, dx <- rem     |
| div r/m32   | o32 F7 /6   | Valid       | Valid          | Unsigned divide edx:eax by r/m32, eax <- quot, edx <- rem |
| div r/m64   | REX.W F7 /6 | Valid       | ~~N.E.~~       | Unsigned divide rdx:rax by r/m64, rax <- quot, rdx <- rem |
