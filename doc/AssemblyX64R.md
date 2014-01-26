#summary x86-64 Instruction Set R
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= rep: Repeat String Operation Prefix =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                       ||
|| rep movsb   || F3 A4  || Valid       || Valid          || Move CX bytes from DS:SI to ES:DI ||
|| rep movsw   || F3 A5  || Valid       || Valid          || Move CX words from DS:SI to ES:DI ||

== Note ==

*rep* is a kind of prefix.

--------

= ret: Return From Procedure = 

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                      ||
|| ret         || C3     || Valid       || Valid          || Near return to calling procedure ||

