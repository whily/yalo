#summary Assembly syntax used by CL cross compiler and Ink.
#labels Phase-Implementation
#sidebar SBX64Assembly

= Introduction =

Lisp Assembly Program (LAP) uses list structure to contain assembly
code. Basically, atoms represent labels while lists represent (pseudo)
instructions. Local labels (starting with a period) are also supported.

Lisp expressions can be used for immediate values. In addition, since
LAP is an S-expression, macros could be used to mimic the macro
facility of _real_ assemblers.

The syntax is similar to what NASM uses. Basically, Intel syntax is
used, i.e. destination operand precedes source operand.

To get a feeling about what LAP looks like, one may take a look at the
value of `*bootloader*` defined in
[http://code.google.com/p/yalo/source/browse/trunk/cc/bootloader.lisp
bootloader.lisp].

= Reference =

 * [AssemblyPseudoInstruction Pseduo Instructions]: reference of pseudo 
instructions.
 * [AssemblySpecialVariable Special Variables]: reference of special variables.
 * [AssemblyX64Overview]: Overview of x86-64 instructions




