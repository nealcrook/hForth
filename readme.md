hForth was developed by Dr Wonyong Koh and placed in the public domain
by him. The major releases were v0.9.6 and v0.9.9.

The original/primary platform was the Intel 8086. Dr Koh also produced
a Z80 port.

Dr Koh wrote an article for Forth Dimensions (included here as 8086/HFORTH.HTM) describing the origins and evolution of his design, and describing some of his design decisions in detail.

Neal Crook produced a port for the StrongARM RISC processor, and wrote an article for Forth Dimensions (included here as arm/port2arm.html).

This Repository
===============

TODO where it came from what came from taygeta and what value I added


Tools
=====

The 8086 version builds and executes under MSDOS. If you do not have MSDOS any more, it will build and execute successfully under DOSbox.

Building the 8086 version from source requires Borland TASM/TLINK in order to bootstrap the core executables. Those tools are now freely available; use a search engine to locate a copy.

The Z80 version builds and executes under CP/M 80. If you do not have CP/M 80 any more, it will build and execute successfully under an emulator; the z80 tree contains a zipped up emulator that will run from MSDOS (which in turn can be run from within DOSbox..)

The ARM version was originally designed to build using the ARM SDK, and requires AWK for pre-processing of the assembler source file prior to build.

Other Sightings of hForth on the web
====================================

TODO


ARM/EPOC version

Super8 version?

http://www.hytherion.com/beattidp/comput/z80forth.htm has a version of z80 hForth revised for the Zilog Macro Assembler.

At one time there was an 8051 version of hForth here:

http://www.stacktech.co.kr/download/KFP/hForth/hf8051.zip

Originally this had a restrictive licence (could not be used for commercial purposes) but the authors announced on c.l.f that they had switched it to be freely usable. Unfortunately, I can find no trace of the .zip file any more.

http://www.gumbley.me.uk/epoc-forth.html has a modified version of ARM hForth to run on the EPOC PDA devices. In particular, the author replaced my AWK scripts with some PERL that generated source for the ARM gcc tool-chain.

