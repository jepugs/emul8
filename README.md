emul8
=====

emul8 is a CHIP-8 emulator.


CHIP-8
------

CHIP-8 is a virtual machine invented in the 80s. Originally, it was implemented
as an interpreter running on an RCA CDP1802 8-bit microcontroller. Because of
its simplicity, it was later widely emulated on a variety of platforms.

A lot of CHIP-8 programs can be downloaded from <http://chip8.com>. (Download
the "Chip-8 Program Pack").


The Emulator
------------

The emulator is implemented in Haskell. It aims to model the original CHIP-8
almost completely. In fact, the only feature that it is missing from the
original CHIP-8 implementation is the SYS opcode, the implementation of which
actually requires emulating another system altogether. (This opcode is seldom
used, and even then often does nothing of consequence).

To use the emulator, simply run

    emul8 FILE

Where `FILE` is the CHIP-8 program file. To quite the emulator, just press
<ESC>. More information can be found by running

    emul8 -h


Keyboard Input
--------------

The original CHIP-8 had a hexadecimal keypad which was laid out like this:

<pre>
1 2 3 C
4 5 6 D
7 8 9 E
A 0 B F
</pre>

emul8 attempts to preserve the general shape of the keypad with its
mappings. The chart below shows the keymap on a QWERTY keyboard, with the CHIP-8
key in parentheses next to the keyboard key to which it corresponds:

<pre>
1(1) 2(2) 3(3) 4(C)
  q(4) w(5) e(6) r(D)
   a(7) s(8) d(9) f(E)
    z(A) x(0) c(B) v(F)
</pre>

For instance, the 'w' key maps to the '5' key on the CHIP-8 keypad.


Building and Installing
-----------------------

Building emul8 requires GHC and cabal-install. With these installed, simply run

    cabal install

from the project directory. To build emul8 and install it locally (on a
single-user basis). To install it for all users on the system, become root and
run

    cabal install --global


Compatibility
-------------

The emulator should be compatible with pretty much any standard CHIP-8
program. Some examples of incompatible programs are

- Some seemingly-normal CHIP-8 programs actually start execution at address
  0x600 instead of the original 0x200. Support for these programs will probably
  be added in the future.

- SuperChip8 and MegaChip8 are later platforms that are backwards-compatible
  with the original CHIP-8. emul8 does not support these platforms at this time.

- The emulator does not support sound at the moment. This is because audio
  output in Haskell is a pain in the butt. This will probably be fixed
  eventually.

- CHIP-8 does not have a standard rate of instruction execution. emul8 executes
  instructions at a rate of 840 Hz. This speed seems to work well with most
  programs. Some CHIP-8 programs, however, work better at different clock
  speeds. At some point, a flag will be added to allow different speeds.


License
-------

emul8 is licensed under the Apache License, Version 2.0. See LICENSE for the
full text of the license.
