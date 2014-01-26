#summary x86-64 Instruction Set D
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= dec: Decrement by 1 =

|| Instruction || Opcode     || 64-Bit Mode || 16/32-Bit Mode || Description          ||
|| dec r/m8    || FE /1      || Valid       || Valid          || Decrement r/m8 by 1  ||
|| dec r/m16   || o16 FF /1  || Valid       || Valid          || Decrement r/m16 by 1 ||
|| dec r/m32   || o32 FF /1  || Valid       || Valid          || Decrement r/m32 by 1 ||
|| dec r/m64   || FF /1      || Valid       || ~~N.E.~~       || Decrement r/m64 by 1 ||
|| dec r16     || o16 48+r   || ~~N.E.~~    || Valid          || Decrement r/16 by 1  ||
|| dec r32     || o32 48+r   || ~~N.E.~~    || Valid          || Decrement r/32 by 1  ||

--------

= div: Unsigned Divide = 

|| Instruction || Opcode     || 64-Bit Mode || 16/32-Bit Mode || Description                                               ||
|| div r/m8    || F6 /6      || Valid       || Valid          || Unsigned divide ax by r/m8, al <- quot, ah <- rem         ||
|| div r/m16   || o16 F7 /6  || Valid       || Valid          || Unsigned divide dx:ax by r/m16, ax <- quot, dx <- rem     ||
|| div r/m32   || o32 F7 /6  || Valid       || Valid          || Unsigned divide edx:eax by r/m32, eax <- quot, edx <- rem ||
|| div r/m64   || F7 /6      || Valid       || ~~N.E.~~       || Unsigned divide rdx:rax by r/m64, rax <- quot, rdx <- rem ||

