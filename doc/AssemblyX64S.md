#summary x86-64 Instruction Set S
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />


= sbb: subtract with borrow = 

Please refer to [AssemblyX64Arith x86-64 arithmetic instructions] for details.

--------

= shl/shr: Shift =

|| Instruction    || Opcode   || 64-Bit Mode || 16/32-Bit Mode || Description                           ||
|| shl r/m8, 1    || D0 /4    || Valid       || Valid          || Multiply r/m8 by 2, once              ||
|| shl r/m8, cl   || D2 /4    || Valid       || Valid          || Multiply r/m8 by 2, cl times          ||
|| shl r/m8, imm8 || C0 /4 ib || Valid       || Valid          || Multiply r/m8 by 2, imm8 times        ||
|| shl r/m16 1    || D1 /4    || Valid       || Valid          || Multiply r/m16by 2, once              ||
|| shl r/m16 cl   || D3 /4    || Valid       || Valid          || Multiply r/m16by 2, cl times          ||
|| shl r/m16 imm8 || C1 /4 ib || Valid       || Valid          || Multiply r/m16by 2, imm8 times        ||
|| shr r/m8, 1    || D0 /5    || Valid       || Valid          || Unsigned divide r/m8 by 2, once       ||
|| shr r/m8, cl   || D2 /5    || Valid       || Valid          || Unsigned divide r/m8 by 2, cl times   ||
|| shr r/m8, imm8 || C0 /5 ib || Valid       || Valid          || Unsigned divide r/m8 by 2, imm8 times ||
|| shr r/m16 1    || D1 /5    || Valid       || Valid          || Unsigned divide r/m16by 2, once       ||
|| shr r/m16 cl   || D3 /5    || Valid       || Valid          || Unsigned divide r/m16by 2, cl times   ||
|| shr r/m16 imm8 || C1 /5 ib || Valid       || Valid          || Unsigned divide r/m16by 2, imm8 times ||

--------

= stc: Set Carry Flag =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description  ||
|| stc         || F9     || Valid       || Valid          || Set CF flag  ||

= std: Set Direction Flag =
|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                                 ||
|| std         || FD     || Valid       || Valid          || String operations decrement index registers ||

= sti: Set Interrupt Flag =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                          ||
|| sti         || FB     || Valid       || Valid          || Enable external, maskable interrupts ||

--------

= stosb/stosw: Store String = 

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                      ||
|| stosb       || AA     || Valid       || Valid          || Store al at address [ds:](e/r)si ||
|| stosw       || AB     || Valid       || Valid          || Store ax at address [ds:](e/r)si ||

--------

= sub: Sub = 

Please refer to [AssemblyX64Arith x86-64 arithmetic instructions] for details.

--------

= syscall/sysret: (Return from) Fast System Call = 

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                  ||
|| syscall     || 0F 05  || Valid       || ~~Invalid~~    || Fast system call             ||
|| sysret      || 0F 07  || Valid       || ~~Invalid~~    || Return from fast system call ||


