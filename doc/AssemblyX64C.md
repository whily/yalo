#summary x86-64 Instruction Set C
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= call: Call Procedure = 

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description      ||
|| call imm16  || E8 rw  || ~~N.S.~~    || Valid          || Call near        ||

--------

= clc: Clear Carry Flag =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description    ||
|| clc         || F8     || Valid       || Valid          || Clear CF flag  ||

= cld: Clear Direction Flag =
|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                                 ||
|| cld         || FC     || Valid       || Valid          || String operations increment index registers ||

= cli: Clear Interrupt Flag =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                           ||
|| cli         || FA     || Valid       || Valid          || Disable external, maskable interrupts ||

--------

= cmovcc: Conditional Move =

|| Instruction       || Opcode              || 64-Bit Mode || 16/32-Bit Mode || Description ||
|| cmovcc r16 r/m16  || o16 0F (+ 40 cc) /r || Valid       || Valid          ||             ||  
|| cmovcc r32 r/m32  || o32 0F (+ 40 cc) /r || Valid       || Valid          ||             ||
|| cmovcc r64 r/m64  || 0F (+ 40 cc) /r     || Valid       || ~~N.E.~~       ||             ||

Please refer [AssemblyX64Overview#Conditional_Codes conditional codes] for details.

--------

= cmp: Compare = 

Please refer to [AssemblyX64Arith x86-64 arithmetic instructions] for details.

--------

= cmpxchg: Compare and Exchange =

|| Instruction       || Opcode       || 64-Bit Mode || 16/32-Bit Mode || Description ||
|| cmpxchg r/m8 r8   || 0F B0 /r     || Valid       || Valid          ||             ||
|| cmpxchg r/m16 r16 || o16 0F B1 /r || Valid       || Valid          ||             ||
|| cmpxchg r/m32 r32 || o32 0F B1 /r || Valid       || Valid          ||             ||
|| cmpxchg r/m64 r64 || 0F B1 /r     || Valid       || ~~N.E.~~       ||             ||

--------

= cmpxchg8b/cmpxchg16b: Compare and Exchange Bytes =

|| Instruction     || Opcode   || 64-Bit Mode || 16/32-Bit Mode || Description ||
|| cmpxchg8b m64   || 0F C7 /1 || Valid       || Valid          ||             ||
|| cmpxchg16b m128 || 0F C7 /1 || Valid       || ~~N.E.~~       ||             ||
