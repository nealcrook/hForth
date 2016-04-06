                                                Jun. 5, 1997

Z80 hForth v0.9.9 RAM model is newly ported from 8086 hForth v0.9.9
RAM model. Following the great Forth tradition hForth is free
software. You can use it for whatever purpose commercial or
non-commercial. Please spread it as widely as you can.

hForth is based on eForth model published by Mr. Bill Muench and Dr.
C. H. Ting in 1990. The key features of the original eForth model is
preserved. Following is quoted from the original 8086 eForth source.

    > small machine dependent kernel and portable high level code
    > source code in the MASM format
    > direct threaded code
    > separated code and name dictionaries
    > simple vectored terminal and file interface to host computer
    > aligned with the proposed ANS Forth Standard
    > easy upgrade path to optimize for specific CPU

These are also the characteristics of hForth. For better, hForth
is ANS Forth system which complies the Standard, not just aligns
with the Standard. Colon definitions for all high level words are
also given as comments in TASM source code. The source code should
be a working example for Forth learners.

The files on this package are:

HFZ80RAM.ASM  TASM source of hForth Z80 RAM model for CP/M.
HFZ80RAM.CPM  Executable image of HFZ80RAM.ASM .
HFZ80RAM.COM  Loader program for MS-DOS CP/M emulator 22NICE.COM.
              It will run CP/M image HFZ80RAM.CPM
OPTIONAL.F    Forth source code of Optional wordset words.
COREEXT.F     Additional definitions for complete CORE EXT words except
              obsolescent ones and [COMPILE].
MULTI.F       Forth source code of Bill Muench's multitasker.
MULDEMO.F     Simple example for hForth multitasker.
DOUBLE.F      Complete DOUBLE and DOUBLE EXT word definitions.
DISCP.F       Words for Dijkstra guarded command control structures
              by M. Edward Borasky
HFORTH.HTM    My article on hForth published on Forth Dimensions
README.ENG    This file.
README86.ENG  README.ENG for 8086 hForth v0.9.9.
22NCE142.ZIP  CP/M emulator for MS-DOS from Sydex.

Changes from 8086 hForth RAM model are listed below.

1. Boot code and all low level CODE definitions were redefined in
   Z80 machine code.

2. Input/output words (RX?, RX@, TX!) were redefined using CP/M BDOS
   function. BYE was defined using BDOS function also. Words only
   for MS-DOS such as 'stdin' and '<<' were removed.

3. '?call' and 'xt,' were redefined since Z80 uses absolute
   addressing while 8086 uses relative addressing for machine CALL
   instruction.

4. 'same?', '(search-wordlist)', and 'UM/MOD' were redefined using
   Z80 machine code for speedy interpretation.

Please refer README86.ENG and HFORTH.HTM for more information about hForth.

Wonyong Koh, Ph.D.
wykoh@pado.krict.re.kr

KRICT
100 Chang-dong, Yusong-gu
Taejon, 305-343
South Korea
82-42-861-4245 (FAX)
