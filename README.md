Yalo
====

# Target

Yalo is a Lisp OS running on bare metal x86-64 hardware. The system
programming language is **Ink**, a new Lisp dialect which combines the
elegance of Scheme and powerfulness of Common Lisp. The project webpage
is: <https://github.com/whily/yalo>

# Status

The project is only at very very initial stage, with an
assembler written in Common Lisp, and a 16 bit bootloader.

# Getting Started

## Getting Bootable Image

Currently, cross compilation is needed to build one floppy image
containing yalo.

### Cross compilation

Although yalo should compile on any Ansi Common Lisp implementation,
it is only tested on SBCL. Therefore the following discussion is
focused on how to build yalo from SBCL.

Mandatory Requirements:
* [http://sbcl.sourceforge.net SBCL]
* git

Optional Requirements:
* Emacs
* SLIME

#### Getting Source Code

Run following command to anonymously checkout the latest source code
of yalo.

```shell
$ git clone https://github.com/whily/yalo.git
```

#### Setup link for ASDF

Run following commands to make SBCL aware of the ASDF file for the
cross compiler.

```shell
$ cd yalo/cc  
$ ./lnasdf
```

#### Build Floppy Image

##### With SBCL alone

When using SBCL alone, type the following at REPL:

```lisp
* (require 'asdf)
* (asdf:oos 'asdf:load-op 'cc)
* (in-package :cc)
* (write-kernel "floppy.img")
```

One may type `Ctrl-d` to exit from SBCL.

##### With Emacs+SLIME

Inside Emacs,
1. First type `M-x slime` if SLIME is not started.
2. Type `M-x slime-load-system` then type `cc` when prompted for the
   system.
3. At REPL, type `(in-package :cc)` to switch to package *cc*
   (alternatively, one can user keyboard shortcut `C-x M-p` and then type `cc`).
4. Type `(write-kernel "floppy.img")` at SLIME REPL to generate the kernel.

## Run Image

There are various ways to run the image. 

### Bochs

First go to the directory where floppy image file is located. Then run
the command below and select *6* to proceed emulation.

```shell
bochs -n 'boot: a' 'floppya: 1_44=floppy.img, status=inserted'
```

One may also use the script `run-bochs` (which basically wraps up
above command) if `floppy.img` is placed in the same directory.

### VirtualBox

First, add the generated floopy.img to the Virtual Media Manager. Then
for the virtual machine settings, select the image file for floppy
drive. Make sure that Floppy is checked for Boot order.





   
