emul8
=====

emul8 is a CHIP-8 emulator.


CHIP-8
------

CHIP-8 is a virtual machine invented in the 80s. Originally, it was implemented
as an interpreter running on an RCA CDP1802 8-bit microcontroller. Because of
its simplicity, it was later widely emulated on a variety of platforms.


The Emulator
------------

The emulator is implemented in Haskell. It aims to model CHIP-8 almost
completely. In fact, the only feature that it is missing from the original
CHIP-8 implementation is the SYS opcode, the implementation of which actually
requires emulating another system altogether. (This opcode is so seldom used
that this is not a problem in practice, anyway).

The emulator should be compatible with pretty much any CHIP-8 program that
doesn't use the keyboard. It might work with programs that use the keyboard as
well, but I haven't gotten around to testing this ;) . To use the emulator,
simply run

    emul8 FILE

Where `FILE` is the CHIP-8 program file. To quite the emulator, just press
<ESC>.

A lot of CHIP-8 programs can be downloaded from <http://chip8.com>. (Download
the "Chip-8 Program Pack").


Building and Installing
-----------------------

Building emul8 requires GHC and cabal-install. With these installed, simply run

    cabal install

from the project directory. To build emul8 and install it locally (on a
single-user basis). To install it for all users on the system, become root and
run

    cabal install --global


License
-------

emul8 is licensed under the Apache License, Version 2.0. See LICENSE for the
full text of the license.
