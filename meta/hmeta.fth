\ $Id$
\ $Log$
\ Revision 1.1  1998/06/07 22:50:48  crook
\ Initial revision
\

\ requires: nothing
\ hForth meta-compiler project
\
\ meta-compiler for compiling an hForth image for a target ARM processor
\ on a host system running pfe under Linux.
\
\ Project files:
\ hfsarom.asm - original source file
\ hf.lst - original listing file
\ prebuild/blah.aif - binary produced at the same time as the listing file
\ hf.lst
\ asmarm.fth - arm assembler
\ hforth.fth - hfsarom.asm re-expressed wholly in Forth and meta-compiler
\              structures
\ compare.fth - words for loading a pre-build image, comparing against a new
\              image and saving the new image.
\ ?? - target-specific files to be included?
\
\ Tasks:
\ 1. build a regression facility in which the prebuilt image can be loaded
\    into a 64K block of memory and compared against the meta-compiled
\    version - DONE.
\ 2. write the meta-compiler
\ 3. attempt to meta-compile an image that will be identical to the prebuild
\    image
\ 4. verify the meta-compiled image
\ 5. restructure to allow multiple targets
\ 6. re-verify
\ 7. port to new platform
\ 8. debug 
\ 9. attempt to meta-compile from within hForth.
\
\  
\	ANS Forth Standard divide Forth dictionary into code, name, and
\	data space. When hForth ROM model starts, the code space resides
\	at bottom of ROM, name space at top of ROM, and data space in
\	RAM address space. Code and name parts of new definitions will
\	split into proper spaces if "ROM" is writable. If "ROM" is not
\	writable, code and data part of new definitions goes into bottom
\	of RAM and name part of new definitions goes into top of RAM.
\
\	You can use the words 'RAM' and 'ROM' to switch data space
\	between RAM and ROM address space.
\
\ code is emitted at xhere
\ data is emitted at HERE
\

MARKER *HMETA*

HEX

: LOAD-ASM
DEPTH ABORT" dstack depth is non-zero at start of hmeta"
DEPTH S" asmarm.fth" INCLUDED DEPTH 1- - ABORT" armasm affected depth"
CR ." ..asmarm" ;

: LOAD-COLON
DEPTH S" hmeta_colon.fth" INCLUDED DEPTH 1- - ABORT" hmeta_colon affected depth"
CR ." ..hmeta_colon" ;

: LOAD-LAST
DEPTH ABORT" dstack depth is non-zero after hmeta loaded"
DEPTH S" compare.fth" INCLUDED DEPTH 1- - ABORT" compare affected depth"
CR ." ..compare"
DEPTH S" hforth.fth" INCLUDED DEPTH 1- - ABORT" hforth affected depth"
CR ." ..hforth" CR ;

\ Load the assembler NOW!
LOAD-ASM

: hCONSTANT CONSTANT ;


\ TODO the pieces below determine the target machine, mem-map, comm port
\ and baud rate. This all needs to be rationalised somewhat.
TRUE  hCONSTANT TAR_EBSA110 \ Targets
FALSE hCONSTANT TAR_EBSA285
FALSE hCONSTANT TAR_COGENT
TRUE  hCONSTANT IO_COMDSWI \ DEFIO
FALSE hCONSTANT IO_COM0
FALSE hCONSTANT IO_COM1
FALSE hCONSTANT IO_COM2
4     hCONSTANT DEFBAUD 
TRUE  hCONSTANT MM_DEMON \ MEMMAP 
FALSE hCONSTANT MM_BOOT
FALSE hCONSTANT MM_PBLOADED


\ The memory map for the EBSA-285 and EBSA-110 ARM systems is designed to fit
\ into 2, 64Kbyte sections. The first section is the ROM section, which can
\ be loaded from Flash and stored back to Flash with new definitions added.
\ The second section is the RAM Section which is volatile: it never gets
\ saved away. The first 16Kbytes of the RAM section is reserved for the
\ memory-management page tables. The remainder is used for data storage
\ (for example, if you define a VARIABLE its value gets stored there)

TAR_EBSA110 [IF]
	\ load into SSRAM, 128Kbytes maximum (faster than DRAM)
40010000 hCONSTANT MMU0		\ start of page tables in RAM
40014000 hCONSTANT RAM0		\ first usable RAM location
40020000 hCONSTANT RAMEnd	\ first unusable RAM location => 48Kbytes RAM
40010000 hCONSTANT ROMEnd	\ first unusable ROM location => 64Kbytes ROM

MM_BOOT MM_DEMON OR [IF]
	\ code in EPROM or for DEMON is linked to run at the start of
	\ SSRAM. For EPROM, the initial piece of code will copy the
	\ image to SSRAM.
40000000 hCONSTANT ROM0		\ first usable ROM location
[ELSE] \ MM_PBLOADED
	\ code in Flash is linked to run at start of SSRAM plus c0. The
	\ PBL copies the image to SSRAM. The gap of c0 leaves room to
	\ form a formated AIF/Flash header for programming an updated
	\ image back to Flash.
400000C0 hCONSTANT ROM0		\ first usable ROM location
[THEN] \ MM
[THEN] \ TAR_EBSA110

TAR_COGENT [IF]
00008000 hCONSTANT ROM0		\ first usable ROM location
00018000 hCONSTANT ROMEnd       \ first unusable ROM location => 64Kbytes ROM
00018000 hCONSTANT MMU0		\ start of page tables in RAM
0001C000 hCONSTANT RAM0		\ first usable RAM location
0002C000 hCONSTANT RAMEnd       \ first unusable ROM location => 64Kbytes RAM
[THEN] \ TAR_COGENT

TAR_EBSA285 [IF]

MM_PBLOADED [IF]
	\ code in Flash is linked to run at start of SSRAM plus c0. The gap of
	\ c0 leaves room to form a formated AIF/Flash header for programming
	\ an updated image back to Flash. The PBL will switch the memory map,
	\ init the X-Bus and SDRAM and copy the image from Flash to SDRAM,
	\ then branch to it. Thereby, the image is magically invoked at the
	\ right place.
000100c0 hCONSTANT ROM0		\ first usable ROM location
00020000 hCONSTANT ROMEnd       \ first unusable ROM location => 64Kbytes ROM
	\ load into SDRAM. Ignore the first 64Kbytes of memory
	\ for now. Eventually want a version that will run
	\ at 0 but then will need to leave room for the vectors there.
00020000 hCONSTANT MMU0		\ start of page tables in RAM
00024000 hCONSTANT RAM0		\ first usable RAM location
00030000 hCONSTANT RAMEnd       \ first unusable ROM location => 48Kbytes RAM
[ELSE] \ must be MM_DEMON
00008000 hCONSTANT ROM0		\ first usable ROM location
00018000 hCONSTANT ROMEnd       \ first unusable ROM location => 64Kbytes ROM
00018000 hCONSTANT MMU0		\ start of page tables in RAM
0001C000 hCONSTANT RAM0		\ first usable RAM location
0002C000 hCONSTANT RAMEnd       \ first unusable ROM location => 64Kbytes RAM
[THEN] \ MM
[THEN] \ TAR_EBSA285



\ Create an area of memory in which to generate the target image. This memory
\ represents the ROM space, and will be dumped out as an image to load into
\ the target. The RAM space is not represented in the host system, although
\ there are obviously references to it.
\ In this case, the image is created within the data space of the host
\ system but this is not a requirement. The only requirement is that there
\ is a way of accessing locations within the image.
CREATE TARGET-IMAGE ROMEnd ROM0 - ALLOT
TARGET-IMAGE ROMEnd ROM0 - 00 FILL


\ take an address in target space and convert it to an address in host space
\ - check it for legality.
: t2h ( n -- n )
	DUP ROM0 ROMEnd WITHIN IF
		TARGET-IMAGE + ROM0 -
	ELSE
		CR SOURCE TYPE CR
		." Error: t2h target address " . ." is out of range" ABORT
	THEN ;

\ Access locations within the target space. The address that is supplied
\ is an absolute address within target space. It is expected to map to
\ an address within the host's image of the target memory. 
: t@ ( n -- n ) t2h @ ;
: tC@ ( n -- n ) t2h C@ ;
: t! ( n n -- ) t2h ! ;
: tC! ( n n -- ) t2h C! ;
: tMOVE ( addr1 taddr2 u -- ) \ move to taddr2 in target address space
  SWAP t2h SWAP MOVE ;

\ TODO - don't think I need this.. it is defined ultimately in target space.
\ TARGET-IMAGE VALUE			cpVar
\ TARGET-IMAGE ROMEnd ROM0 - + VALUE	npVar 
\ RAM0 VALUE				hereVar


\ in the normal hForth assembler source:
\ - cpVar is the address at which to emit new code
\ - npVar is the address at which to emit new dictionary data.
\ ** these names are exposed in the target Forth source, so they get
\    redefined eventually to be within the target address space. 
\ -- may not actually need them..

\ data is emitted at the current value of the VALUE hereVar
\ In Forth definitions, hereVar is accessed using HERE and cpVar is accessed
\ using xhere.

\ load compare function and the forth source itself
LOAD-LAST

ALSO ASSEMBLER
GLOBAL-RESOLVED \ check that global forward references are resolved.
SYM-TABLE-STATS DUMP-SYM-TABLE LTORG-STATS
PREVIOUS

   .( Code pointer is 0x) _CODE U.
CR .( Name pointer is 0x) _NAME U.
CR .( Space is 0x) _NAME _CODE - U.
ALSO its-words TUNRESOLVED @ PREVIOUS
CR .( Unresolved high-level forward references ) U.

