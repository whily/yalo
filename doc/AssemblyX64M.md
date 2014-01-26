#summary x86-64 Instruction Set M
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= mov: Move = 

|| Instruction     || Opcode       || 64-Bit Mode || 16/32-Bit Mode || Description                    ||
|| mov !r8 imm8    || B0+r ib      || Valid       || Valid          || Move imm8 to !r8               ||
|| mov !r16 imm16  || o16 B8+r iw  || Valid       || Valid          || Move imm16 to !r16             ||
|| mov !r32 imm32  || o32 B8+r id  || Valid       || Valid          || Move imm32 to !r32             ||
|| mov r/m16 !r16  || o16 89 /r    || Valid       || Valid          || Move !r16 to r/m16             ||
|| mov r/m32 !r32  || o32 89 /r    || Valid       || Valid          || Move !r32 to r/m32             ||
|| mov !r16 r/m16  || o16 8B /r    || Valid       || Valid          || Move r/m16 to !r16             ||
|| mov !r32 r/m32  || o32 8B /r    || Valid       || Valid          || Move r/m32 to !r32             ||
|| mov r/m16 imm16 || o16 C7 /0 iw || Valid       || Valid          || Move imm16 to r/m16            ||
|| mov r/m32 imm32 || o32 C7 /0 id || Valid       || Valid          || Move imm32 to r/m32            ||
|| mov sreg r/m16  || 8E /r        || Valid       || Valid          || Move r/m16 to segment register ||
|| mov r/m16 sreg  || 8C /r        || Valid       || Valid          || Move segment register to r/m16 ||

--------

= movsb/movsw: Move Data from String to String =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                    ||
|| movsb       || A4     || Valid       || Valid          || Move byte from DS:SI to ES:DI  ||
|| movsw       || o16 A5 || Valid       || Valid          || Move word from DS:SI to ES:DI  ||
|| movsd       || o32 A5 || Valid       || Valid          || Move dword from DS:SI to ES:DI ||

--------

= mul: Unsigned Multiply = 

|| Instruction || Opcode     || 64-Bit Mode || 16/32-Bit Mode || Description                               ||
|| mul r/m8    || F6 /4      || Valid       || Valid          || Unsigned multiply (ax <- al * r/m8)       ||
|| mul r/m16   || o16 F7 /4  || Valid       || Valid          || Unsigned multiply (dx:ax <- ax * r/m16    ||
|| mul r/m32   || o32 F7 /4  || Valid       || Valid          || Unsigned multiply (edx:eax <- ax * r/m32  ||
|| mul r/m64   || F7 /4      || Valid       || ~~N.E.~~       || Unsigned multiply (rdx:rax <- rax * r/m64 ||

