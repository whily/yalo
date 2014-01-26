#summary x86-64 Instruction Set X
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= xadd: Exchange and Add =

|| Instruction    || Opcode       || 64-Bit Mode || 16/32-Bit Mode || Description                                 ||
|| xadd r/m8 r8   || 0F C0 /r     || Valid       || Valid          || Exchange r8 and r/m8, load sum into r/m8    ||
|| xadd r/m16 r16 || o16 0F C1 /r || Valid       || Valid          || Exchange r16 and r/m16, load sum into r/m16 ||
|| xadd r/m32 r32 || o32 0F C1 /r || Valid       || Valid          || Exchange r32 and r/m32, load sum into r/m32 ||
|| xadd r/m64 r64 || 0F C1 /r     || Valid       || ~~N.E.~~       || Exchange r64 and r/m64, load sum into r/m64 ||

--------

= xchg: Exchange =

|| Instruction    || Opcode    || 64-Bit Mode || 16/32-Bit Mode || Description     ||
|| xchg ax r16    || o16 90+r  || Valid       || Valid          || Exchange ax with r16 ||
|| xchg r16 ax    || o16 90+r  || Valid       || Valid          || Exchange ax with r16 ||
|| xchg eax r32   || o32 90+r  || Valid       || Valid          || Exchange eax with r32 ||
|| xchg r32 eax   || o32 90+r  || Valid       || Valid          || Exchange eax with r32 ||
|| xchg rax r64   || 90+r      || Valid       || ~~N.E.~~       || Exchange rax with r64 ||
|| xchg r64 rax   || 90+r      || Valid       || ~~N.E.~~       || Exchange rax with r64 ||
|| xchg r/m8 r8   || 86 /r     || Valid       || Valid          || Exchange r/m8 with r8 ||
|| xchg r8 r/m8   || 86 /r     || Valid       || Valid          || Exchange r/m8 with r8   ||
|| xchg r/m16 r16 || o16 87 /r || Valid       || Valid          || Exchange r/m16 with r16 ||
|| xchg r16 r/m16 || o16 87 /r || Valid       || Valid          || Exchange r/m16 with r16 ||
|| xchg r/m32 r32 || o32 87 /r || Valid       || Valid          || Exchange r/m32 with r32 ||
|| xchg r32 r/m32 || o32 87 /r || Valid       || Valid          || Exchange r/m32 with r32 ||
|| xchg r/m64 r64 || 87 /r     || Valid       || ~~N.E.~~       || Exchange r/m64 with r64 ||
|| xchg r64 r/m64 || 87 /r     || Valid       || ~~N.E.~~       || Exchange r/m64 with r64 ||

Note that when a memory operand is referenced, lock protocal is
automatically implemented, regardless whether LOCK prefix is used or
not. This may has impact on performance.

--------

= xor: Logical Exclusive OR = 

Please refer to [AssemblyX64Arith x86-64 arithmetic instructions] for details.

