x86-64 Instruction Set L
========================

[Assembly syntax](AssemblyX64.md)
[A](AssemblyX64A.md) [B](AssemblyX64B.md) [C](AssemblyX64C.md)
[D](AssemblyX64D.md) [E](AssemblyX64E.md) [F](AssemblyX64F.md)
[H](AssemblyX64H.md) [I](AssemblyX64I.md) [J](AssemblyX64J.md)
L [M](AssemblyX64M.md) [N](AssemblyX64N.md)
[O](AssemblyX64O.md) [P](AssemblyX64P.md) [R](AssemblyX64R.md)
[S](AssemblyX64S.md) [T](AssemblyX64T.md) [U](AssemblyX64U.md)
[V](AssemblyX64V.md) [W](AssemblyX64W.md) [X](AssemblyX64X.md)

### lgdt/lidt/lldt: Load Descriptor Tables

| Instruction | Opcode   | 64-Bit Mode | 16/32-Bit Mode | Description                           |
| ----------- | -------- | ----------- | -------------- | ------------------------------------- |
| lgdt m16&32 | 0F 01 /2 | ~~N.E.~~    | Valid          | Load m into GDTR                      |
| lidt m16&32 | 0F 01 /3 | ~~N.E.~~    | Valid          | Load m into IDTR                      |
| lldt r/m16  | 0F 00 /2 | Valid       | Valid          | Load segment selector r/m16 into LDTR |

### lodsb/lodsw: Load String

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description                               |
| ----------- | ------ | ----------- | -------------- | ----------------------------------------- |
| lodsb       | AC     | Valid       | Valid          | Load byte at address [ds:](e/r)si into al |
| lodsw       | AD     | Valid       | Valid          | Load word at address [ds:](e/r)si into ax |

### loop: Loop According to (E)CX Counter

| Instruction | Opcode | 64-Bit Mode | 16/32-Bit Mode | Description                               |
| ----------- | ------ | ----------- | -------------- | ----------------------------------------- |
| loop imm16  | E2 rb  | Valid       | Valid          | Decrement count; jump short if count <> 0 |
