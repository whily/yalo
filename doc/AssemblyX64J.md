#summary x86-64 Instruction Set J
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= jcc: Conditional Branch =

|| Instruction    || Opcode          || 64-Bit Mode || 16/32-Bit Mode || Description            ||
|| jcc imm        || (+ 70 cc) rb    || Valid       || Valid          || Short conditional jump ||  
|| jcc near imm   || 0F (+ 80 cc) rd || Valid       || Valid          || Near conditional jump  ||
|| jcxz imm       || a16 E3 rb       || ~~N.E.~~    || Valid          || Jump short if cx=0     ||
|| jecxz imm      || a32 E3 rb       || Valid       || Valid          || Jump short if ecx=0    ||
|| jrcxz imm      || E3 rb           || Valid       || ~~N.E.~~       || Jump short if rcx=0    ||

Please refer [AssemblyX64Overview#Conditional_Codes conditional codes] for details.

= jmp: Jump = 

|| Instruction    || Opcode   || 64-Bit Mode || 16/32-Bit Mode || Description        ||
|| jmp short imm  || EB rb    || Valid       || Valid          || Jump short         ||

