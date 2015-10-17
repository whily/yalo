x86-64 Bit Instruction
=============================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64B.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
[L](AssemblyX64L.md) [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

There are 4 bit instructions which have similar formats of
instructions. They are discussed together here.

The instructions are:
* bt:  bit test
* btc: bit test and complement
* btr: bit test and reset
* bts: bit test and set

The format of the instructions are given below.

| Instruction    | Opcode            | 64-Bit Mode | 16/32-Bit Mode |
| -------------- | ----------------- | ----------- | -------------- |
| ins r/m16 r16  | 0F (+ base A3) /r | Valid       | Valid          |
| ins r/m32 r32  | 0F (+ base A3) /r | Valid       | Valid          |
| ins r/m32 r32  | 0F (+ base A3) /r | Valid       | ~~N.E.~~       |
| ins r/m16 imm8 | 0F BA opcode ib   | Valid       | Valid          |
| ins r/m32 imm8 | 0F BA opcode ib   | Valid       | Valid          |
| ins r/m64 imm8 | 0F BA opcode ib   | Valid       | ~~N.E.~~       |

Base and opcode in above table are given below for each instruction.

| Instruction | Base (hexadecimal) | Opcode |
| ----------- | ------------------ | ------ |
| bt          | 0                  | /4     |
| btc         | 18                 | /7     |
| btr         | 10                 | /6     |
| bts         | 8                  | /5     |
