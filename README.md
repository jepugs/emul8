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

Right now, the emulator is not complete because using Haskell for graphics is an
indescribable form of Hell. However, the internal model of the machine is fully
complete.


License
-------

emul8 is licensed under the Apache License, Version 2.0. See LICENSE for the
full text of the license.
