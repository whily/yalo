#summary x86-64 Instruction Set P
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

= pop: Pop a Value from the Stack =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description                                         ||
|| pop r16     || 58+r   || Valid       || Valid          || Pop top of stack into r16; increment stack pointer. ||
|| pop ss      || 17     || ~~Invalid~~ || Valid          || Pop top of stack into SS; increment stack pointer.  ||
|| pop ds      || 1F     || ~~Invalid~~ || Valid          || Pop top of stack into DS; increment stack pointer.  ||
|| pop es      || 07     || ~~Invalid~~ || Valid          || Pop top of stack into ES; increment stack pointer.  ||
|| popfd       || 9D     || ~~N.E.~~    || Valid          || Pop EFLAGS.                                         ||

--------

= push: Push a Value Onto the Stack =

|| Instruction || Opcode || 64-Bit Mode || 16/32-Bit Mode || Description ||
|| push r16    || 50+r   || Valid       || Valid          || Push r16    ||
|| push cs     || 0E     || ~~Invalid~~ || Valid          || Push CS     ||
|| push ss     || 16     || ~~Invalid~~ || Valid          || Push SS     ||
|| push ds     || 1E     || ~~Invalid~~ || Valid          || Push DS     ||
|| push es     || 06     || ~~Invalid~~ || Valid          || Push ES     ||
|| pushfd      || 9C     || ~~N.E.~~    || Valid          || Push EFLAGS.||

--------

