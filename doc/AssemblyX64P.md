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

| Instruction | Opcode   | 64-Bit Mode | 16/32-Bit Mode | Description                                         |
| ----------- | -------- | ----------- | -------------- | --------------------------------------------------- |
| pop r16     | o16 58+r | Valid       | Valid          | Pop top of stack into r16; increment stack pointer. |
| pop r32     | o32 58+r | ~~N.E.~~    | Valid          | Pop top of stack into r32; increment stack pointer. |
| pop r64     | 58+r     | Valid       | ~~N.E.~~       | Pop top of stack into r64; increment stack pointer. |
| pop ss      | 17       | ~~Invalid~~ | Valid          | Pop top of stack into SS; increment stack pointer.  |
| pop ds      | 1F       | ~~Invalid~~ | Valid          | Pop top of stack into DS; increment stack pointer.  |
| pop es      | 07       | ~~Invalid~~ | Valid          | Pop top of stack into ES; increment stack pointer.  |
| popf        | o16 9D   | Valid       | Valid          | Pop lower 16 bits of EFLAGS.                        |
| popfd       | o32 9D   | ~~N.E.~~    | Valid          | Pop EFLAGS.                                         |
| popfq       | 9D       | Valid       | ~~N.E.~~       | Pop RFLAGS.                                         |

### push: Push a Value Onto the Stack

| Instruction | Opcode   | 64-Bit Mode | 16/32-Bit Mode | Description                  |
| ----------- | -------- | ----------- | -------------- | ---------------------------- |
| push r16    | o16 50+r | Valid       | Valid          | Push r16                     |
| push r32    | o32 50+r | ~~N.E.~~    | Valid          | Push r32                     |
| push r64    | 50+r     | Valid       | ~~N.E.~~       | Push r64                     |
| push cs     | 0E       | ~~Invalid~~ | Valid          | Push CS                      |
| push ss     | 16       | ~~Invalid~~ | Valid          | Push SS                      |
| push ds     | 1E       | ~~Invalid~~ | Valid          | Push DS                      |
| push es     | 06       | ~~Invalid~~ | Valid          | Push ES                      |
| pushf       | o16 9C   | Valid       | Valid          | Push lower 16 bits of EFLAGS.|
| pushfd      | o32 9C   | ~~N.E.~~    | Valid          | Push EFLAGS.                 |
| pushfq      | 9C       | Valid       | ~~N.E.~~       | Push RFLAGS.                 |
