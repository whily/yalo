Cross Compilation
=================

# Plan

Below is the initial thinking on the plan to cross compile and
bootstrap Yalo.

## Cross compile the minimal bootloader and Ink interpreter

The milestone of this stage is to generate a minimal bootloader (OS is
far away) which finally runs an Ink interpreter.

* Input: a minimal version of Ink interpreter written in Ink
* Cross compiling platform: x86-64 assembler and a
  minimal version of Ink compiler, both written in Common Lisp. The
  compiler may use some intermediate language (e.g. LLVM IR).

## Develop Ink compiler with Ink interpreter

Once the minimal Ink interpreter is up, the main target is then to
implement basic file system (ext2) support. The interpreter then is
able to read/write Ink source files, which can then be put under
version control via the interface between VM and host machine. 

The next milestone is to implement x86-64 assembler and a full fledged
Ink compiler, both in Ink itself.
