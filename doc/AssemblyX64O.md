#summary x86-64 Instruction Set O
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

--------

= or: Logical Inclusive OR = 

Please refer to [AssemblyX64Arith x86-64 arithmetic instructions] for details.

--------

= out: Output from Port =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                                   ||
|| out imm8 al || E6 ib  || Valid       || Valid          || Output byte in al to I/O port address in imm8 ||
|| out imm8 ax || E7 ib  || Valid       || Valid          || Output word in ax to I/O port address in imm8 ||
|| out dx al   || EE     || Valid       || Valid          || Output byte in al to I/O port address in dx   ||
|| out dx ax   || EF     || Valid       || Valid          || Output word in ax to I/O port address in dx   ||
