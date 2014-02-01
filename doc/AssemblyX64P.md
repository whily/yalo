x86-64 Instruction Set P
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [H](AssemblyX64H.md) [I](AssemblyX64I.md)
[J](AssemblyX64J.md) [L](AssemblyX64L.md) [M](AssemblyX64M.md)
[N](AssemblyX64N.md) [O](AssemblyX64O.md) P
[R](AssemblyX64R.md) [S](AssemblyX64S.md) [T](AssemblyX64T.md)
[X](AssemblyX64X.md)

### pop: Pop a Value from the Stack

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description                                         |
| ----------- | ------ | ----------- | -------------- | --------------------------------------------------- |
| pop r16     | 58+r   | Valid       | Valid          | Pop top of stack into r16; increment stack pointer. |
| pop ss      | 17     | ~~Invalid~~ | Valid          | Pop top of stack into SS; increment stack pointer.  |
| pop ds      | 1F     | ~~Invalid~~ | Valid          | Pop top of stack into DS; increment stack pointer.  |
| pop es      | 07     | ~~Invalid~~ | Valid          | Pop top of stack into ES; increment stack pointer.  |
| popfd       | 9D     | ~~N.E.~~    | Valid          | Pop EFLAGS.                                         |

### push: Push a Value Onto the Stack

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description |
| ----------- | ------ | ----------- | -------------- | ----------- |
| push r16    | 50+r   | Valid       | Valid          | Push r16    |
| push cs     | 0E     | ~~Invalid~~ | Valid          | Push CS     |
| push ss     | 16     | ~~Invalid~~ | Valid          | Push SS     |
| push ds     | 1E     | ~~Invalid~~ | Valid          | Push DS     |
| push es     | 06     | ~~Invalid~~ | Valid          | Push ES     |
| pushfd      | 9C     | ~~N.E.~~    | Valid          | Push EFLAGS.|
