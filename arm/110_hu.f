\ $Id$
\ $Log$
\ Revision 1.1  1997/03/30 21:20:18  crook
\ Initial revision
\
\
\ ebsa-110 hForth utility words
\ these utilities expect coreext and optional to be loaded; they require
\ definitions of HEX MARKER SAVE-SYSTEM ROM RAM
\
\ TODO: check SOFTSTAT refers to pass-2 jumpers and change the message to
\ report which image number is selected
\ TODO: fix Ethernet code to work with cache on; delay loops?
\ TODO: fix Flash programming with cache on; delay after byte programming?
\ TODO: what wordlist should these go into?
\ TODO: should Flash moves be DATA or CODE space? 
\ TODO: in FLASHFORTH make sure DRAM is working first, OR (better) don't
\ copy it; blow the checksum later, once it's in Flash.
\ TODO: revamp error codes for Flash words to make abort" more sensible
\ TODO: in FLASHFORTH I doen't need to check the bitmap -- already have the
\ address. BUT do want to set bitmap to only allocate 1 block, in case of
\ FMU bug. Note that you cannot program the Flash whilst running from DEMON
\ with my code.

HEX
MARKER STARTOF-UTILS

: todo
 CR ." This word is not yet implemented"
;

VARIABLE OLDBASE

: SAVBASE \ ( -- )
 \ save current value of BASE
 BASE @ OLDBASE !
;

: RESBASE \ ( -- )
 \ restore old value of BASE
 OLDBASE @ BASE !
;

: BIT \ ( n1 n2 -- n1 f )
 \ test to see if bit n2 is set in value n1.
 \ n1 is unchanged, and f is TRUE if bit is set
 1 SWAP LSHIFT OVER AND
 0= INVERT		\ yeuch
;

: CSUM \ ( n1 n2 -- n3)
 \ perform 16-bit checksum of 32-bit memory
 \ n1 = start address, n2 = end address
 \ n3 = checksum
 \ note that data at addresses n1, n2 are included in the calculation and that
 \ both are assumed to be longword aligned. Checksum is computed by taking
 \ each 16-bit value in turn and adding it to the checksum, which has a
 \ starting value of 0. Any carry beyond bit 15 is masked out
 OVER -			\ start_a #bytes
 2 RSHIFT		\ start-a #quadwords
 0 SWAP
 0 DO			\ start-a csum
   OVER I 2 LSHIFT + @	\ get value from location
   DUP FFFF AND
   SWAP 10 RSHIFT FFFF AND
   + + FFFF AND
 LOOP
 SWAP DROP		\ drop start_a
;

: SIO>ISAIO \ ( n -- n) "sio to sys"
 \ convert super I/O port address to system address
 \ use this for Super I/O port addresses
 2 LSHIFT 0F0000000 OR
;

: ISA>ISAMEM \ ( n -- n) "isa to isa mem"
 \ convert ISA mem address to ISAMEM system address
 \ use this for Ethernet, PCMCIA controller memory addresses
 DUP 1 AND SWAP FFFFFFFE AND 1 LSHIFT E0000000 OR OR
;

: ISA>ISAIO \ ( n -- n) "isa to isa io"
 \ convert ISA mem address to ISAIO system address
 \ use this for Ethernet, PCMCIA controller memory addresses
 DUP 1 AND SWAP FFFFFFFE AND 1 LSHIFT F0000000 OR OR
;

: SOFTSTAT \ ( -- )
 \ decode state of soft inputs
 F2400000 @
 CR ." Debug LED is "               7 BIT IF ." OFF" ELSE ." ON" THEN
 CR ." Speaker is "                 6 BIT IF ." ON" ELSE ." OFF" THEN
 CR ." DRAM configuration mode is " 5 BIT IF ." ON" ELSE ." OFF" THEN
 CR ." DRAM configured for "    4 BIT IF ." BURST EDO" ELSE ." EDO" THEN
 CR ." Link J17 13-14 is "      3 BIT IF ." NOT fitted" ELSE ." fitted" THEN
 CR ." Link J17 15-16 is "      2 BIT IF ." NOT fitted" ELSE ." fitted" THEN
 CR ." Link J4  13-14 is "      1 BIT IF ." NOT fitted" ELSE ." fitted" THEN
 CR ." Link J4  15-16 is "      0 BIT IF ." NOT fitted" ELSE ." fitted" THEN
 DROP SPACE
;

:  LED \ ( F -- )
 \ if TRUE, LED on, else LED off.
 F2400000 DUP @ 7F AND \ make hole in current value
 ROT INVERT 80 AND \ get flag and make new LS bit for SOFT
 OR SWAP !
;

: LEDON \ ( -- )
 -1 LED
;

: LEDOFF \ ( -- )
 0 LED
;

: CTBTYPE \ ( -- )
 \ report CTB type 
 FF F3000000 C!		\ TRICK4
 55 F2C00000 C!		\ TRICK3
 00 F2C00000 C!
 F2C00000 C@		\ 55 => OS, 00 => ARCH
 CR ." CTB interrupt configuration is "
 DUP 55 =
 IF
   ." OS (operating system) "
 ELSE
   0=
   IF
     ." ARCH (architectural verification) "
   ELSE
     ." UNKNOWN "
   THEN
 THEN
;

: TONE \ ( n -- )
\ set timer channel 2 to the frequency represented by
\ the low 16-bits of the value on the stack
\ A value of 200H gives a frequency of about 12kHz, about 
\ the highest frequency that the speaker (or my ear) can
\ manage. A value of FFFFH gives the lowest frequency that
\ the counter can generate; about 100Hz.
\ 
 B6 F200000D C!
 DUP
 F2000009 C! \ LS byte
 8 RSHIFT
 F2000009 C! \ MS byte
;

: QUIET \ ( -- )
\ Turn off the speaker output
 F2400000 @
 BF AND
 F2400000 !
;

: NOISE \ ( -- )
\ Turn on the speaker output
 F2400000 @
 40 OR
 F2400000 !
;

: BEEP \ ( -- )
\ Make a short beep
 750 TONE NOISE
 20000 0 DO
   1 1 * DROP \ waste some time
 LOOP
 QUIET
;

: RABORTR \ ( -- )
 \ generate read cycle to read-abort space
 C0000000 @ DROP
;

: RABORTW \ ( -- )
 \ generate write cycle to read-abort space
 0 C0000000 !
;

: RWABORTR \ ( -- )
 \ generate read cycle to read-write abort space
 D0000000 @ DROP
;

: RWABORTW \ ( -- )
 \ generate write cycle to read-write abort space
 0 D0000000 !
;

: MEMTEST \ (???)
 \ test an area of memory
 \ algorithm TDB
todo
;

\ ---------------------------------------------------------------
\ Words for using the Flash ROM
\ ---------------------------------------------------------------

VARIABLE FLBASE

: FLFIND \ ( -- f)
 \ find the Flash ROM and set FLBASE to its base address.
 \ return flag indicating success or failure
 80000000 
 FF OVER !		\ reset array
 90 OVER !		\ prepare to read intelligent identifier
 DUP @ FFFF AND		\ low two bytes come from addresses 1, 0
 A289 XOR 0=		\ check for device code, manuf code
 IF
   FF OVER !		\ reset array
   -1			\ found it LOW
 ELSE
   DROP A0000000	\ try the other possibility
   FF OVER !		\ reset array
   90 OVER !		\ prepare to read intelligent identifier
   DUP @ FFFF AND	\ low two bytes come from addresses 1, 0
   A289 XOR 0=	\ check for device code, manuf code
   IF
     FF OVER !		\ reset array
     -1			\ found it HIGH
   ELSE
     DROP 0 0		\ failed to find it
   THEN
 THEN
 SWAP FLBASE !		\ leave 0 in FLBASE if failed, else address  
;

: ROMMAP \ ( -- )
 \ report the Flash and ROM presence/location
 \ the EPROM is 512Kbyte, the Flash is 1MByte. They could be either way
 \ around.
 CR FLFIND DROP
 80000000 FLBASE @ =
 IF
   ." Flash (1Mx8) found in low (boot) position"
 ELSE
   A0000000 FLBASE @ =
   IF
     ." Flash (1Mx8) found in high position"
   ELSE
     ." No Flash found - missing/faulty/wrong ID?"
   THEN
 THEN
 CR
 \ now look for non-Flash ROMS
 FLBASE @ 80000000 = INVERT
 IF			\ Flash NOT found low so look for ROM
   80000000 8007FFFC CSUM
   80000000 8007FFFC CSUM
   - 0=			\ same both times?
   IF
     ." non-Flash ROM found in low (boot) position"
   THEN
 THEN
 FLBASE @ A0000000 = INVERT
 IF			\ Flash NOT found high so look
   A0000000 A007FFFC CSUM
   A0000000 A007FFFC CSUM
   - 0=			\ same both times?
   IF
     ." non-Flash ROM found in high position"
   THEN
 THEN CR
;

: FLSTAT \ ( -- n)
 \ return flash status byte
 FLBASE @
 DUP
 70 SWAP C! C@
;

: FLSTAT0 \ ( -- )
 \ clear flash status
 50 FLBASE @ C!
;

: FLBUSY \ ( -- )
 \ wait while flash is indicating that it is busy
 BEGIN
   FLSTAT
   80 AND 80 =
 UNTIL
;

: FLB2A \ ( n -- a)
 \ take block block number (0-F) and use the value
 \ in FLBASE to convert it to an absolute base address
 10 LSHIFT
 FLBASE @ +
;

: FLBERASE \ ( n -- f)
 \ erase block n, return status flag
 FLB2A
 DUP
 20 SWAP C!
 D0 SWAP C!	\ do the erase
 FLBUSY		\ wait for completion
 FLSTAT
 28 AND 0=	\ did it work OK
 FLSTAT0	\ clear any error ready for next time
;

: FLPROG \ ( a n -- f )
 \ program 64Kbyte block, n, with data from address a
 FLB2A		\ froma toa
 DUP 10000 +	\ froma toa endtoa
 >R
 BEGIN
   40 OVER !	\ prime for 1st byte
   OVER @	\ get 32-bits of data
   DUP		\ froma toa dat dat
   2 PICK !	\ program 1st byte
   FLBUSY
   8 RSHIFT	\ get 2nd data ready
   SWAP 400000 + \ 2nd address froma dat toa
   40 OVER !	\ prime for 2nd byte
   SWAP DUP	\ froma toa dat dat
   2 PICK !	\ program 2nd
   FLBUSY
   8 RSHIFT
   SWAP 400000 +
   40 OVER !
   SWAP DUP
   2 PICK !	\ program 3rd
   FLBUSY
   8 RSHIFT
   SWAP 400000 +
   40 OVER !	\ prime for final byte
   SWAP		\ froma toa dat
   OVER !	\ program 4th
   FLBUSY
   BFFFFC -	\ next location
   SWAP 4 + SWAP
   DUP R@ - 0=	\ reached end address?
 UNTIL
 R> DROP DROP DROP	\ clear up the mess
 FLSTAT 28 AND 0=	\ get error status
 FLSTAT0		\ clear any errors
;

: HFHDR \ ( -- n ) find hForth header in Flash 
\ return block number of block containing the
\ hForth header, or 0x10 if not found. Header is
\ identified by the first 4 characaters of its
\ name, which must be hFor
 0
 BEGIN
   DUP FLB2A 14 + \ point to name in this header
   @ 726F4668 = \ text string hFor
   IF
     -1 \ remember we found it
   ELSE
     1 + DUP 10 = \ next block number
   THEN
 UNTIL
;

: QCSUM \ ( n1 n2 -- n)
\ compute quadword 2s complement checksum starting at n1, of n2 quadwords
 0 SWAP \ start_addr sum #quadwords
 0 DO
   OVER I 2 LSHIFT + \ calculate address
   @ + \ add its value in
 LOOP
 SWAP DROP NEGATE
;

: FLASHFORTH \ put current hForth image back into FLASH
 \ note: there is a BUG in old versions of the FMU that result in it
 \ wrongly calculating that 2 blocks are required to hold an image of
 \ 65536 bytes. If the map and length allocate 2 blocks, the effect of
 \ FLASHFORTH is to re-use the lower of the two blocks and liberate the
 \ second, whilst fixing up the image length.
 CR SAVE-SYSTEM
 FLFIND INVERT ABORT" Fatal: Couldn't find the flash"
 HFHDR DUP 10 = ABORT" Fatal: couldn't find Flash image 'hForth'"
 FLB2A DUP ." Found image " 14 + 10 TYPE
 \ copy image to DRAM @ 10000-1FFFF - need this so we can calculate the C'sum
 40000000
 20000 10000 DO
   DUP @ I ! 4 +
 4 +LOOP DROP
 DUP 10000 C0 MOVE	\ copy header from Flash to DRAM image
 0 1000C !		\ clear out the checksum in the DRAM image
 10000 10010 !		\ set the length to a full block in the DRAM image
 10000 4000 QCSUM  1000C !	\ update checksum in the DRAM image
 10008 @ 		\ get bitmap of used blocks
 DUP 0= ABORT" Fatal: no blocks allocated in this image's block map"
 20 0 DO \ 32 bits to check; bits 31:0
   DUP 1 AND
   IF
     DROP I LEAVE	\ got the block number
   THEN
   1 RSHIFT \ look at next bit in bitmap
 LOOP			\ loop always LEAVEs without completion
 \ stack now holds block number. Erase and program it
 DUP
 ." erasing.." FLBERASE INVERT ABORT" Fatal: Flash erase failed"
 ." programming.." 10000 SWAP FLPROG INVERT ABORT" Fatal: Flash program failed"
 ." done. Flash updated successfully."
 CR
;

\ ---------------------------------------------------------------
\ Words for using the DRAM
\ ---------------------------------------------------------------

MARKER STARTOF-DRAM

: RFRSHOFF \ ( -- )
 \ turn off DRAM refresh
 \ TODO: may be able to turn off requests better by setting mode but no count
 3A 1F200000D C!	\ load 16-bit count for channel 0 in mode 5
 1 F2000001 C!		\ least-significant count is 1
 0 F2000001 C!		\ most-significant count is 0
;

: RFRSHON \ ( -- )
 \ turn on DRAM refresh
 36 F200000D C!		\ load 16-bit count for channel 0 in mode 3
 67 F2000001 C!		\ count is 14.54us
 0 F2000001 C!
;

: BEDOINIT
 \ follows procedure in EBSA-110 reference manual
 F2400000 DUP @ EF AND SWAP !	\ clear SOFT_BURST
 F2400000 DUP @ 20 OR SWAP !		\ set SOFT_DCBR
 \ fiddle with each possible block
 20 0 DO
   I 16 LSHIFT 40000080 AND	\ generate magic address
   DUP 0 SWAP !			\ write to it to get into setup mode, linear
   9 0 DO
     DUP @ DROP
   LOOP
   DROP				\ read from magic address 9 times and discard
 LOOP
 F2400000 DUP @ 20 XOR SWAP !	\ toggle SOFT_DCBR to clear it
;

: DRAMTYPE
 \ assumes DRAM is in EDO mode, as though BEDOINIT has just been run
 \ detect DRAM type and leave SOFT_BURST set correctly.
 AAAAAAAA 40000000 !
 55555555 40000004 !
 40000000 @
 AAAAAAAA XOR 0=
 IF
   ." EDO DRAM fitted." CR	\ 0 => EDO DRAM, BURST is in correct state
 ELSE
   \ try again in BURST EDO mode
   F2400000 DUP @ 10 OR SWAP !	\ set SOFT_BURST
   AAAAAAAA 40000000 !
   55555555 40000004 !
   40000000 @
   AAAAAAAA XOR 0=
   IF
     ." Burst EDO DRAM fitted." CR
   ELSE
     ." Error: DRAM type could not be identified" CR
   THEN
 THEN
;

: DRAMSIZE
 todo
;

: DRAMTEST
 todo \ use MEMTEST
;

: DRAMINIT \ ( -- )
 \ init the DRAM.
 CR ." Turn off refresh... " RFRSHOFF
 ." Init as BEDO DRAM (in case that's what we've got)... " BEDOINIT
 ." Turn on refresh... " RFRSHON CR
 DRAMTYPE
 DRAMSIZE
 DRAMTEST
;

\ ---------------------------------------------------------------
\ Words for using the Ethernet controller
\ ---------------------------------------------------------------

MARKER STARTOF-ETHER

: SAMCARRAY \ ( a +n -- )
 \ single-address, multiple characters array. The address is a, the size is n
 \ so the index will go from 0 to n-1
 \
 \ Create a setup array. Stack holds the number of characters to be allotted.
 \
 \ The run-time behaviour is ( +n -- c a )
 \ where c is the nth char in the array and a is the (fixed) store address.
 \ The maximum index is retrieved using LIMIT. Index goes from 0 to LIMIT
 CREATE DUP 1 - , SWAP , ALLOT	\ first entry is limit, second is address
 ALIGN				\ get to next cell boundary
 DOES> CELL+			\ point to store address
 DUP CELL+ ROT +		\ address_of_address address_of_data
 C@ SWAP @			\ data address
;

: SAMCSET \ ( $$ n1 n2 -- )
 \ set character in array $$
 \ n1 is the character, n2 is the (char) offset
 ' >BODY CELL+ CELL+		\ point past limit addr
 + C!				\ offset into char array and store
;

: XAMCDUMP \ ( $$ -- )
 \ dump contents of array $$
 \ works for SAM and MAM arrays
 ' DUP >BODY @			\ ca limit
 1+ 0 DO			\ ca
   I				\ ca offset
   OVER				\ ca offset ca
   EXECUTE			\ ca data address
   SWAP CR . .			\ ca
 LOOP DROP
;

: MAMCARRAY \ ( +n -- )
 \ multiple-address, multiple characters array. The size is n
 \ so the index will go from 0 to n-1
 \
 \ create a setup array. Stack holds the number of characters to be allotted.
 \
 \ The run-time behaviour is ( +n -- c a )
 \ where c is the nth char in the array and a is the (variable) store address.
 \ The maximum index is retrieved using LIMIT. Index goes from 0 to LIMIT
 CREATE DUP 1 - ,		\ first entry is limit
 DUP CELLS + ALLOT		\ leave 1 cell and 1 char for each entry
 ALIGN				\ get to next cell boundary
 DOES>				\ point to limit
 \ address of data is Alimit + cell + (limit + 1)*cell + offset
 \ address of address is Alimit + cell + offset*cell
 DUP @ 1 + CELLS 2 PICK + SWAP CELL+
 ROT CELLS OVER + @ >R		\ get data
 + C@ R>			\ get address data
;

: MAMCSET \ ( $$ n1 n2 n3-- )
 \ set character in array $$
 \ n1 is the character, n2 is the address, n3 is the (char) offset
 ' >BODY			\ point to limit
 \ address for data is Alimit + cell + (limit + 1)*cell + offset
 \ address for address is Alimit + cell + offset*cell
 DUP @ 1 + CELLS 2 PICK + SWAP CELL+
 ROT CELLS OVER +
 ROT ROT + >R ! R> C!		\ store address and data
;

: LIMIT \ ( $ -- +n )
 \ take the next word in the input stream and get its code address
 \ from this, extract the first data item, the upper bound of the array,
 \ and leave on the stack
 '				\ get code address
 >BODY				\ point to 1st data item; the limit
 @				\ get limit and leave on the stack
; IMMEDIATE


\ define array of characters to hold ethernet key. The ethernet key is
\ 32 bytes (20H) in size and is sent to ISAIO space
ROM \ put data into ROM so it gets Flashed
279 ISA>ISAIO 1 - 20 SAMCARRAY ETHERKEY

6B 0 SAMCSET ETHERKEY
35 1 SAMCSET ETHERKEY
9A 2 SAMCSET ETHERKEY
CD 3 SAMCSET ETHERKEY
E6 4 SAMCSET ETHERKEY
F3 5 SAMCSET ETHERKEY
79 6 SAMCSET ETHERKEY
BC 7 SAMCSET ETHERKEY
5E 8 SAMCSET ETHERKEY
AF 9 SAMCSET ETHERKEY
57 A SAMCSET ETHERKEY
2B B SAMCSET ETHERKEY
15 C SAMCSET ETHERKEY
8A D SAMCSET ETHERKEY
C5 E SAMCSET ETHERKEY
E2 F SAMCSET ETHERKEY
F1 10 SAMCSET ETHERKEY
F8 11 SAMCSET ETHERKEY
7C 12 SAMCSET ETHERKEY
3E 13 SAMCSET ETHERKEY
9F 14 SAMCSET ETHERKEY
4F 15 SAMCSET ETHERKEY
27 16 SAMCSET ETHERKEY
13 17 SAMCSET ETHERKEY
09 18 SAMCSET ETHERKEY
84 19 SAMCSET ETHERKEY
42 1A SAMCSET ETHERKEY
A1 1B SAMCSET ETHERKEY
D0 1C SAMCSET ETHERKEY
68 1D SAMCSET ETHERKEY
34 1E SAMCSET ETHERKEY
1A 1F SAMCSET ETHERKEY
RAM \ end of ethernet key

\ define array of bytes that are used to configure the
\ ethernet controller. (Really want 2D char array for this -
\ here I am wasting a whole cell on the address byte)
ROM \ put data into ROM so it gets Flashed
13 MAMCARRAY ETHERCONF

\ data address index
05 02  0 MAMCSET ETHERCONF
00 03  1 MAMCSET ETHERCONF
80 00  2 MAMCSET ETHERCONF
01 06  3 MAMCSET ETHERCONF
FE 43  4 MAMCSET ETHERCONF
0C 48  5 MAMCSET ETHERCONF
00 49  6 MAMCSET ETHERCONF
02 4A  7 MAMCSET ETHERCONF
FF 4B  8 MAMCSET ETHERCONF
00 4C  9 MAMCSET ETHERCONF
02 60  A MAMCSET ETHERCONF
20 61  B MAMCSET ETHERCONF
03 70  C MAMCSET ETHERCONF
02 71  D MAMCSET ETHERCONF
00 74  E MAMCSET ETHERCONF
04 F0  F MAMCSET ETHERCONF
00 31 10 MAMCSET ETHERCONF
01 30 11 MAMCSET ETHERCONF
05 F0 12 MAMCSET ETHERCONF
RAM \ end of ethernet config sequence

: ETHERINIT \ ( -- )
 \ initialise the Ethernet controller.
 ." Send the initiation key... "
 LIMIT ETHERKEY LITERAL		\ elements from 0 -> tos
 1+ 0 DO			\ loop for 0 to limit
   I ETHERKEY !			\ get address and data and send it
 LOOP
  ." Configure... "
  LIMIT ETHERCONF LITERAL
  1+ 0 DO
    I ETHERCONF F00004F0 ! F00014F0 !
  LOOP
  02 F00004F1 C!
  02 F00014F1 C!
  CR
;

: .UID \ ( -- )
 \ Print the Ethernet unique ID (aka Ethernet address)
 6 0 DO
  F0000440 I 2 LSHIFT + C@ S>D
  <# # # #> TYPE
  I 5 = IF
   CR
  ELSE
   ." -"
  THEN
 LOOP
;

\ ---------------------------------------------------------------
\ Words for using the PCMCIA controller
\ ---------------------------------------------------------------

MARKER STARTOF-PCMCIA

: IDEDIS \ ( -- )
 \ the IDE Controller in the Super I/O clashes with the
 \ PCMCIA registers, so it must be disabled
 0 F0000E60 ! \ SIO config register; select register 0
 F0000E64 DUP @
 BF AND \ mask off IDE enable
 \ to update the register, the SIO requires you to write
 \ the same configuration value TWICE
 DUP 2 PICK ! SWAP !
;

: PCMCIA! \ ( c1 c2 -- ) "pcmcia store"
 \ store c1 to PCMCIA register c2
 F00007C0 C! \ set address to be accessed
 F00007C0 ! \ write data (32-bit access to write odd byte address)
;

: PCMCIA@ \ ( c1 -- c2 ) "pcmcia fetch"
 \ fetch data (c2) from PCMCIA register c1
 F00007C0 C! \ set register to access
 F00007C0 @ FF AND \ get data
;

: ATTWIN \ set up attribute windows for slot a and b
\ A window 0, at E802.0000 - E803.FFFF (corresponds
\ to 0x0001.0000 - 0x0001.FFFF in ISA address space)
\ B window 0, at E804.0000 - E805.FFFF (corresponds
\ to 0x0002.0000 - 0x0002.FFFF in ISA address space)

\ note: A01 boards have a wiring bug which remaps these
\ windows (to E804.0000 and E808.0000?)

06 DUP PCMCIA@ FE AND SWAP PCMCIA! \ disable window A0
10 10 PCMCIA!
80 11 PCMCIA!
1F 12 PCMCIA!
00 13 PCMCIA!
F0 14 PCMCIA!
7F 15 PCMCIA!
06 DUP PCMCIA@ 1 OR SWAP PCMCIA! \ enable window A0

46 DUP PCMCIA@ FE AND SWAP PCMCIA! \ disable window B0
20 50 PCMCIA!
80 51 PCMCIA!
2F 52 PCMCIA!
00 53 PCMCIA!
E0 54 PCMCIA!
7F 55 PCMCIA!
46 DUP PCMCIA@ 1 OR SWAP PCMCIA! \ enable window B0
;

: MEMWIN \ set up a memory window for slot a and b
\ A window 1, at 0xE820.0000 - 0xE82F.FFFF, mapped to
\ address 0 in PCMCIA slot A memory space
\ B window 1, at 0xE840.0000 - 0xE84F.FFFF, mapped
\ address 0 in PCMCIA slot B memory space

06 DUP PCMCIA@ FD AND SWAP PCMCIA! \ disable window A1
00 18 PCMCIA!
81 19 PCMCIA!
FF 1A PCMCIA!
01 1B PCMCIA!
00 1C PCMCIA!
3F 1D PCMCIA!
06 DUP PCMCIA@ 2 OR SWAP PCMCIA! \ enable window

46 DUP PCMCIA@ FD AND SWAP PCMCIA! \ disable window B1
00 58 PCMCIA!
82 59 PCMCIA!
FF 5A PCMCIA!
02 5B PCMCIA!
00 5C PCMCIA!
3E 5D PCMCIA!
46 DUP PCMCIA@ 2 OR SWAP PCMCIA!
;

\ define some constants for the start of memory and attribute space
E8020000 CONSTANT A_AT0
E8040000 CONSTANT B_AT0
E8200000 CONSTANT A_MEM0
E8400000 CONSTANT B_MEM0

: DLINE \ ( n1 --)
\ dump line: 16 bytes from the PCMCIA registers
 10 0 DO
   DUP I + \ incrementing address
   PCMCIA@
   3 U.R
 LOOP DROP CR
;

: DUMP468
 \ Show contents of VG468 registers
 CR
 ."  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F" CR
 ." Socket A registers:" CR
 0 DLINE
 10 DLINE
 20 DLINE
 30 DLINE
 ." Socket B registers:" CR
 40 DLINE
 50 DLINE
 60 DLINE
 70 DLINE
;

: INIT468 \ ( -- )
 \ VG468 initialisation

 IDEDIS \ Disable the IDE port in the SIO

 \ **** unique registers
 0E F00007C0 C!
 37 F00007C0 C! \ unlock the unique registers
 12 38 PCMCIA!  \ Control
 12 78 PCMCIA!
  0 39 PCMCIA!  \ Timer
  0 79 PCMCIA!
                \ MISC register unused
  0 3B PCMCIA!  \ GPIO configuration
  0 7B PCMCIA!
  0 3D PCMCIA!  \ programmable chip select
  0 7D PCMCIA!
  0 3E PCMCIA!  \ programmable chip select config
  0 7E PCMCIA!
  0 3F PCMCIA!  \ ATA
  0 7F PCMCIA!

 \ **** General setup registers

 \ ID and revision is not interesting - no setup needed

 \ Interface Status - no setup needed

 \ Power and reset drive control
 \ - output enable disabled
 \ - disable resume x
 \ - don't auto power switch
 \ - disable power to socket
 \ - VPP will be 0V
 0 02 PCMCIA!
 0 42 PCMCIA!

 \ Card status change - no setup needed
 
 \ Address window enable - Use all address
 \ lines to decode MEMCS16 (the hardware assumes all accesses are
 \ 16-bit and ignores this signal)
 20 06 PCMCIA!
 20 46 PCMCIA!

 \ -- card detect and general control register
 0 16 PCMCIA!
 0 56 PCMCIA!

 \ global control register (for both slots)
 0 1E PCMCIA!

 \ **** Interrupt registers
 \ Socket A uses IRQ4, socket B uses IRQ3. The interrupts are enabled but
 \ the cards are left in RESET. Interrupt is used for PC card interrupt and
 \ for card status change interrupt.
 04 03 PCMCIA! \ Interrupt and general control 
 03 43 PCMCIA!
 48 05 PCMCIA! \ Card status change interrupt
 38 45 PCMCIA!

 \ **** I/O registers

 \ Socket A
 \ Address 0; start L, H, stop L, H - unused
 \ Address 1; start L, H, stop L, H - unused

 \ Socket B
 \ Address 0; start L, H, stop L, H - unused
 \ Address 1; start L, H, stop L, H - unused

 \ **** memory registers

 \ NOTE: some address lines are scrambled on A01 boards.

 \ Socket A
 \ Address 2; sysstart L, H sysstop L, H offset L, H - unused
 \ Address 3; sysstart L, H sysstop L, H offset L, H - unused
 \ Address 4; sysstart L, H sysstop L, H offset L, H - unused

 \ Socket B
 \ Address 2; sysstart L, H sysstop L, H offset L, H - unused
 \ Address 3; sysstart L, H sysstop L, H offset L, H - unused
 \ Address 4; sysstart L, H sysstop L, H offset L, H - unused

 ATTWIN \ set up attribute space for slot A,B in window 0
 MEMWIN \ set up memory space for slot A,B in window 1
;

: A-ON \ (-- c)
 \ turn on card A by turning on the power and taking it out of reset. Leave
 \ the card status on the stack
 02 DUP PCMCIA@ 10 OR 02 PCMCIA! \ power on
        PCMCIA@ 80 OR 02 PCMCIA! \ enable outputs
 200 0 DO I DROP LOOP \ wait
 03 PCMCIA@ 40 OR 03 PCMCIA! \ out of reset
 01 PCMCIA@
;

: A-OFF \ (--)
 \ turn off card A by putting it in reset and turning off the power
 03 PCMCIA@ BF AND 03 PCMCIA! \ into reset
 200 0 DO I DROP LOOP \ wait
 02 DUP PCMCIA@ 7F AND 02 PCMCIA! \ disable outputs
        PCMCIA@ EF AND 02 PCMCIA! \ power off
;

: A-VPP \ (n --)
 \ set programming voltage
 \ 0 => 0v
 \ 5 => 5V
 \ 12 => 12V
 \ anything else sets the VPP pins to Z
 02 PCMCIA@ F0 AND
 SWAP DUP 0=
 IF DROP 0 ELSE
   DUP 5 =
   IF DROP 8 ELSE
     0C =
     IF 2 ELSE A
     THEN
   THEN
 THEN
 OR 02 PCMCIA!
;

: B-ON \ (-- c)
 \ turn on card B by turning on the power and taking it out of reset. Leave
 \ the card status on the stack
 42 DUP PCMCIA@ 10 OR 42 PCMCIA! \ power on
        PCMCIA@ 80 OR 42 PCMCIA! \ enable outputs
 200 0 DO I DROP LOOP \ wait
 43 PCMCIA@ 40 OR 43 PCMCIA! \ out of reset
 41 PCMCIA@
;

: B-OFF \ (--)
 \ turn off card B by putting it in reset and turning off the power
 43 PCMCIA@ BF AND 43 PCMCIA! \ into reset
 200 0 DO I DROP LOOP \ wait
 42 DUP PCMCIA@ 7F AND 42 PCMCIA! \ disable outputs
        PCMCIA@ EF AND 42 PCMCIA! \ power off
;

: B-VPP \ (n --)
 \ set programming voltage
 \ 0 => 0v
 \ 5 => 5V
 \ 12 => 12V
 \ anything else sets the VPP pins to Z
 42 PCMCIA@ F0 AND
 SWAP DUP 0=
 IF DROP 0 ELSE
   DUP 5 =
   IF DROP 8 ELSE
     0C =
     IF 2 ELSE A
     THEN
   THEN
 THEN
 OR 42 PCMCIA!
;

: .TUPL \ (addr -- next_addr)
\ print a tuple given its start address.
\ attribute space is byte mapped
\ assume caller has verified that this is
\ a valid tuple
 DUP C@ .       \ tuple code
 4 + DUP C@ DUP \ length_address length length
 0 DO           \ length_address length    
   .
   4 + DUP C@
 LOOP
 DROP CR
;

: .ALLTUPL \ (addr --)
\ print all the tuples, starting at the given
\ address. End of tuples is marked by a
\ tuple code of FF or a tuple link of FF
\ in addition, recognise the tuple code
\ CISTPL_VER1 and print this tuple as text
 CR
 BEGIN
   DUP C@ DUP
   FF = 2 PICK 4 + C@ FF = OR
   IF                   \ end of tuple so we're done
     DROP -1            \ addr TRUE
   ELSE
     15 =
     IF                 \ CISTPL_VER1 so print as text
       DUP C@ . 4 +     \ print tuple code and move on
       DUP C@ SWAP OVER . 4 + \ print length and remember it
       DUP C@ . 4 +     \ print major revision
       DUP C@ . 4 +     \ print minor revision
       SWAP 3 -         \ address string-length<-TOS
       CR
       0 DO             \ length_address length 0
         DUP C@ DUP 0= OVER FF = OR
         IF DROP CR ELSE EMIT THEN
         4 +
       LOOP
     ELSE               \ addr
       .TUPL            \ next_tuple_addr
     THEN
     0                  \ again, blistering barnacles
   THEN
 UNTIL                  \ Loop until TRUE
 DROP
;

\ display information about the card in slot A
: .A_ID
 CR 01 PCMCIA@ C AND 0=
 IF
   ." No card fitted in PCMCIA slot A (lower slot)"
 ELSE
   A_AT0 .ALLTUPL
 THEN
;

\ display information about the card in slot B
: .B_ID
 CR 41 PCMCIA@ C AND 0=
 IF
   ." No card fitted in PCMCIA slot B (upper slot)"
 ELSE
   B_AT0 .ALLTUPL
 THEN
;


\ ---------------------------------------------------------------
\ Words for using memory-management and other CP15 stuff.
\ ---------------------------------------------------------------

MARKER STARTOF-MM

: BUILDFLATMAP ( n -- )
\ build a set of level-1 page tables for 1-1 mapping of the whole
\ 32-bit address space. The four quadrants have these properties:
\
\ 31:30
\  0  0   DRAM  - cacheable, bufferable
\  0  1   SSRAM - cacheable, bufferable
\  1  0   EPROM - non-cacheable, non-bufferable
\  1  1   I/O   - non-cacheable, bufferable
\
\ all the domains are set to 0. All the permissions are set to
\ the most permissive.
\
\ The page tables occupy 16Kbytes at address n
 0 \ base address that this section will map
 800 0 DO \ first two quadrants
   DUP >R
   C0E OR
   OVER !
   4 + R> 100000 + \ increment table address and base address
 LOOP
 400 0 DO \ EPROM quadrant
   DUP >R
   C02 OR
   OVER !
   4 + R> 100000 +
 LOOP

 400 0 DO \ I/O quadrant
   DUP >R
   C06 OR
   OVER !
   4 + R> 100000 +
 LOOP
 DROP DROP
;

: INITMMU ( n -- )
\ build page tables at address n and enable the MMU
 DUP
 TTB! BUILDFLATMAP
 TLBFLUSH \ in case they are still valid
 3 DAC! \ domain 0 accessible to all
 MMUON
;

: SPEEDUP ( -- )
\ start up mmu and clock switching
 MMUPTAB \ build-time constant tells us where to build the tables
 INITMMU CKSWON
;

\ end

HAND