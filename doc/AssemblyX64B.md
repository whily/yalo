#summary x86-64 Instruction Set B
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= bswap: Byte Swap =

|| Instruction || Opcode   || 64-Bit Mode || 16/32-Bit Mode || Description               ||
|| bswap r32   || 0F C8+r  || Valid       || Valid          || Reverse byte order of r32 ||
|| bswap r64   || 0F C8+r  || Valid       || ~~N.E.~~       || Reverse byte order of r64 ||
