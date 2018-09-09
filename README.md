Yalo
====

# Target

Yalo is a Lisp OS running on bare metal x86-64 hardware. The system
programming language is **Ink**, a new Lisp dialect which combines the
elegance of Scheme and powerfulness of Common Lisp. The project webpage
is: <https://github.com/whily/yalo>

# Status

The project is only at very very initial stage, with an
[assembler](https://github.com/whily/yalo/blob/master/doc/AssemblyX64.md)
written in Common Lisp, and a 64 bit bootloader.

"Short term" plan:

- [x] VGA text mode without using BIOS.
- [x] Keyboard handling without using BIOS.
- [x] Switch to 32 bit protected mode.
- [x] Switch to 64 bit long mode.
- [ ] Physical/virtual memory management.
- [ ] Userland and system call.
- [ ] Implement Ink interpreter with assembly.
- [ ] Self hosting a more powerful Ink interpreter with Ink itself.

# Getting Started

## Getting Bootable Image

Currently, cross compilation is needed to build one floppy image
containing yalo.

### [Cross compilation](https://github.com/whily/yalo/blob/master/doc/CrossCompilation.md)

Although yalo should compile on any Ansi Common Lisp implementation,
it is only tested on SBCL and Clozure CL (aka CCL).

Mandatory Requirements:
* [SBCL](http://sbcl.sourceforge.net SBCL) or [CCL](https://ccl.clozure.com/)
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

Run following commands to make SBCL/CCL aware of the ASDF file for the
cross compiler. Note that one only needs to run the script once.

```shell
$ cd yalo/cc
$ ./lnasdf
```

#### Build Floppy Image

##### With SBCL or CCL alone

For SBCL, in the root directory of the project (e.g. directory
`yalo`), run script `write-kernel-sbcl`. Afterwards,`floppy.img` is
written directly to the same directory, where scripts to run the image
(e.g. `run-bochs`) are located. It should be noted that it is assumed
that `sbcl` executable is installed in directory `/usr/bin`. Otherwise
modify script `write-kernel-sbcl` accordingly.

For CCL, in the root directory of the project, type the following at
REPL to generate `floppy.img`:

```lisp
(require 'asdf)
(asdf:oos 'asdf:load-op 'cc)
(in-package :cc)
(write-kernel "../floppy.img")
```

One may type `(ccl:quit)` to exit from CCL.

##### With Emacs+SLIME

Inside Emacs,

1. First type `M-x slime` if SLIME is not started.
2. Type `M-x slime-load-system` then type `cc` when prompted for the
   system.
3. At REPL, type `(in-package :cc)` to switch to package *cc*
   (alternatively, one can user keyboard shortcut `C-x M-p` and then type `cc`).
4. Type `(write-kernel "../floppy.img")` at SLIME REPL to generate the kernel.
   Here we assume that current directory is `cc` of the source tree, therefore
   `floppy.img` is written directly to the source tree, where scripts to run the
   image (e.g. `run-bochs`) are located.

Above is applicable for both SBCL and CCL, assuming SLIME is
configured properly for the CL implementation(s).

## Run Image

There are various ways to run the image.

### Bochs

Go to the root directory of the source code, where script `run-bochs`
and `debug-bochs` are located. Run the scripts and select *6* to
proceed emulation. The difference between `run-bochs` and
`debug-bochs` is that after selecting *6*, emulation starts directly
for `run-bochs`; while for `debug-bochs`, emulator pauses before BIOS,
and one needs to type `c` in the debugger window to continue the
emulation.

Note that on Ubuntu (at least in 16.10), package `bochs-x` should be
installed in addition to package `bochs`.

### QEMU

Similar to Bochs, go to the root directory of the source code, where
script `run-qemu` and `debug-qemu` are located.

In both `run-qemu` and `debug-qemu`, QEMU monitor is redirectted to
stdio (via argument `-monitor stdio`). Script `run-qemu` will start
the emulator without GDB support; while `debug-qemu` enables GDB
support: `-s` argument makes QEMU listens to port 1234 for GDB, while
`-S` argument makes QEMU pauses at the beginning for GDB's `continue`
command. After running `debug-qemu` and starting GDB, first type
`target remote :1234` to connect to QEMU, then type command `continue`
to resume the emulation.

### VirtualBox

In the **Storage** page of the virtual machine settings, right click
and select "Add Floppy Controller". And the select the image file
`floppy.img` for floppy drive. In the **System** page of virtual
machine settings, make sure that Floppy is checked for Boot order.

Assuming the VM name is `yalo`, go to the root directory of the source
code, where script `run-virtualbox` and `debug-virtualbox` are
located. Run script `run-virtualbox` to start the emulator, or script
`debug-virtualbox` to start the emulator with debug window.

## Software version

So far, the development is done on Arch Linux. For above-mentioned
software, the corresponding version is listed below:

* SBCL: 1.4.10 or CCL 1.12
* Emacs: 26.1
* SLIME: 2.22
* Bochs: 2.6.9
* QEMU: 3.0.0
* VirtualBox: 5.2.18
