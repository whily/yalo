#summary x86-64 Instruction Set L
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

== lgdt/lidt/lldt: Load Descriptor Tables

|| Instruction || Opcode   || 64-Bit Mode || 16/32-Bit Mode || Description                           ||
|| lgdt m16&32 || 0F 01 /2 || ~~N.E.~~    || Valid          || Load m into GDTR                      ||
|| lidt m16&32 || 0F 01 /3 || ~~N.E.~~    || Valid          || Load m into IDTR                      ||
|| lldt r/m16  || 0F 00 /2 || Valid       || Valid          || Load segment selector r/m16 into LDTR ||

--------

= lodsb/lodsw: Load String = 

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                               ||
|| lodsb       || AC     || Valid       || Valid          || Load byte at address [ds:](e/r)si into al ||
|| lodsw       || AD     || Valid       || Valid          || Load word at address [ds:](e/r)si into ax ||

--------

= loop: Loop According to (E)CX Counter = 

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                               ||
|| loop imm16  || E2 rb  || Valid       || Valid          || Decrement count; jump short if count <> 0 ||


