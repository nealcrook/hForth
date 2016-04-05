\ $Id: hforth.fth,v 1.1 1999/10/31 17:30:31 crook Exp $
\ $Log: hforth.fth,v $
\ Revision 1.1  1999/10/31 17:30:31  crook
\ Work in progresss towards an ANS cross-compiler wordset that can
\ cross-compile hForth for the ARM.
\
\ Revision 1.18  1998/11/25 21:48:57  crook
\ check-point before starting on STC modifications.
\
\ Revision 1.17  1998/10/08 20:33:10  crook
\ tidy ups and optimisations; first working asm version.
\
\ Revision 1.16  1998/10/06 00:27:14  crook
\ fixed many assembler TODOs.
\
\ Revision 1.15  1998/10/03 18:05:09  crook
\ Updates to head, pack" ACCEPT PARSE REFILL SPACES from Wonyong's
\ release of 05-Jan-1998
\
\ Revision 1.14  1998/10/03 15:26:45  crook
\ merged errors found in my high-level source back into hfsarom.asm
\
\ Revision 1.13  1998/10/01 00:17:24  crook
\ fixed bug in ACCEPT that stopped backspace from working.
\
\ Revision 1.12  1998/09/30 23:55:22  crook
\ First working binary.
\
\ Revision 1.11  1998/09/05 12:07:57  crook
\ many changes to allow immediates to be defined in a separate file
\
\ Revision 1.10  1998/09/02 20:40:53  crook
\ fix init code.
\
\ Revision 1.9  1998/09/01 22:26:19  crook
\ fix all assembler references to variables and the udebug routines
\
\ Revision 1.8  1998/09/01 21:41:35  crook
\ build initilaisation tables correctly.
\
\ Revision 1.11  1998/08/25 00:59:13  crook
\ minor tweak for naming of non-immediate target words
\
\ Revision 1.10  1998/08/24 22:15:42  crook
\ minor tweaks
\
\ Revision 1.9  1998/08/23 23:10:33  crook
\ add table-initialisation routines and many other small fixes
\
\ Revision 1.8  1998/08/22 17:23:05  crook
\ rearrange to achieve 0 forward references in non-immediate words
\
\ Revision 1.7  1998/08/20 23:49:43  crook
\ fixed environment strings - the whole table matches the assembler version
\
\ Revision 1.6  1998/08/20 22:32:41  crook
\ tidy-ups
\
\ Revision 1.5  1998/08/18 22:13:35  crook
\ all but a few tiny parts of the source now compile
\
\ Revision 1.4  1998/08/17 23:59:25  crook
\ much closer to finishing
\
\ Revision 1.3  1998/06/30 22:49:01  crook
\ Remove all DW definitions in favour of Forth definitions
\
\ Revision 1.2  1998/06/14 18:31:55  crook
\ meta compilation debug. Some colon definitions.
\
\ Revision 1.1  1998/06/07 22:50:31  crook
\ Initial revision
\

\ TODO --- could change all xts to be BL instructions - then need
\ to rearrange the return stack and FPC somewhat but can remove the
\ call-dolist interpeter - super speedup and the only cost is the
\ restraint that all forth code must be in a 23-bit window.. big deal!
\ The compiler can range-check to ensure that no out-of-range code was
\ ever generated.

\ TODO --- CELLS in [ ] constructs will use the HOST rather than
\ the target defn - non-portable at the moment (Actually, this applies
\ to *all* stuff within [ ] ). Should change the search list so that, by
\ default, no words are available

\ This is derived from 1.17 of hfsarom.asm
\ which in turn is derived from Wonyong's v0.9.9
\ $Id: hforth.fth,v 1.1 1999/10/31 17:30:31 crook Exp $

\ The following values of TARGET are supported:
\ EBSA110 - EBSA-110, SA-110 StrongARM Evaluation Board
\ EBSA285 - StrongARM/21285 PCI Evaluation Board
\ COGENT  - StrongARM PCI Evaluation Board.
\ note that the ARM PIE card is no longer supported.

\ At startup, the default I/O and baud rate are set as build-time options. At
\ run-time they can be changed to use any method available on the target. The
\ following values of DEFIO are supported:
\ COMDSWI    - serial I/O through the ARM debugger terminal using DEMON SWI
\		calls. This is rather crude since the SWI don't handle
\		backspace or other control characters and insist on echoing
\		everything before you get a chance to parse it. Useful for
\		initial debugging but no more than that.
\ COM0       - 21285 internal UART
\ COM1, COM2 - on-board UART on EBSA110, COGENT and (some) EBSA285
\
\ For all com ports except COMDSWI, the following values of
\ DEFBAUD are supported:
\ 4    - 9600 baud
\ 5    - 19K2
\ 6    - 38K4
\ 7    - 56K
\
\ The memory-map is a build-time option. In general, the image is set up so
\ that all its resources fit into a 128Kbyte block but it could be cut down
\ below that. For ROM-type targets, 64Kbytes of ROM is required and the image
\ gets copied to RAM where it occupies 64K for ROM and the next 64K for data
\ and for building memory-management page tables. The following values of
\ MEMMAP are supported:
\ DEMON      - image loaded at 0x8000 and executes in place
\                              ^^^^^^ not always true!!
\ BOOT       - designed to be blown into ROM and executed from
\              reset; sets up memory and copies itself to RAM
\ PBLOADED   - designed to be blown into Flash and loaded
\              by the PBL; the PBL will initialise RAM, copy the
\              image to RAM and jump to it.
\
\ Not all permutations of target/io/memory-map are supported and/or tested.
\ Some (but not all) illegal permutations will generate a fatal error at
\ build time.

\ Mods for mk2 meta-compilation.
\
\ The whole software suite can be divided into two parts:
\ - target independent
\ - target dependent
\
\ The former can be completely portable. The latter can be further
\ sub-divided as follows:
\ - target CPU dependent
\ - target memory-map and I/O dependent
\ - target low-level Forth dependent
\ - target high-level Forth dependent
\
\ Where possible, the various dependencies will be broken into as
\ large chunks as possible to facilitate porting.
\
\ The structure of the code looks like this:
\  1 EQUates
\  2 Define target sections
\  3 target-dependent startup code
\  5 Define primitives used by defining words and immediate words.
\  6 Set vectors for xts of primitives
\  7 Define data structures used for memory allocation
\  8 Define remaining data structures
\  9 Define remaining primitives
\ 10 I/O stuff
\ 11 THROW strings
\ 12 Environment strings
\ 13 Target-specific definitions (?optional)
\ 14 Remaining definitions
\ 15 Image generation
\ TODO: neat to divide this into 'kernel' and 'non-kernel' where the former is the
\ minimum required to allow turn-key applications to be built with an inner interpreter
\ on the target but no outer interpreter.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 1: EQUates
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CR .( *** Assembly constants)

HEX -1		CONSTANT TRUEE
0		CONSTANT FALSEE
1		CONSTANT CHARR \ byte size of a character
4		CONSTANT CELLL \ byte size of a cell
07F		CONSTANT MaxChar \ use ASCII only
0FF		CONSTANT MaxCountedString \ max length of a string
07FFFFFFF	CONSTANT MaxSigned \ max value of a signed integer
0FFFFFFFF	CONSTANT MaxUnsigned \ max value of an unsigned integer
080000000	CONSTANT MaxNegative \ max value of a negative integer
DECIMAL 134	CONSTANT PADSize \ PAD area size
64		CONSTANT RTCells \ return stack size
256		CONSTANT DTCells \ data stack size
10		CONSTANT BASEE \ default radix
10		CONSTANT OrderDepth \ depth of search order stack
20		CONSTANT MaxWLISTS \ maximum number of wordlists
				\ 2 are used by the system
				\ 18 are available to Forth programs
58		CONSTANT NumTHROWMsgs \ number of throw messages
\ COMPO IMMED MASKK are now =MASK =COMP =IMED to match the colon defns
HEX 1F		CONSTANT =MASK \ lexicon compile-only bit
20		CONSTANT =COMP \ lexicon immediate bit
40		CONSTANT =IMED \ lexicon bit mask
				  \ extended character set
				  \ maximum name length = 0x1F

\ NAC: added SPC, XON, XOFF
DECIMAL 32	CONSTANT SPC \ space - blank
17		CONSTANT XON \ CTRL-Q - flow control for FILE
19		CONSTANT XOFF \ CTRL-S - flow control for FILE
8		CONSTANT BKSPP \ backspace
9		CONSTANT TABB \ tab
10		CONSTANT LFF \ line feed
13		CONSTANT CRR \ carriage return
127		CONSTANT DEL \ delete
\ For the ARM, this is the branch-with-link op-code; the offset gets ORed in.
\ Used by ?call and xt, but only xt, actually emits opcodes.
HEX 0EB000000	CONSTANT CALLL


base @ HEX
\ The memory map for the EBSA-285 and EBSA-110 ARM systems is designed to fit
\ into 2, 64Kbyte sections. The first section is the ROM section, which can
\ be loaded from Flash and stored back to Flash with new definitions added.
\ The second section is the RAM Section which is volatile: it never gets
\ saved away. The first 16Kbytes of the RAM section is reserved for the
\ memory-management page tables. The remainder is used for data storage
\ (for example, if you define a VARIABLE its value gets stored there)

TAR_EBSA110 [IF]
	\ load into SSRAM, 128Kbytes maximum (faster than DRAM)
40010000 CONSTANT MMU0		\ start of page tables in RAM
40014000 EQU RAM0		\ first usable RAM location
40020000 CONSTANT RAMEnd	\ first unusable RAM location => 48Kbytes RAM
40010000 CONSTANT ROMEnd	\ first unusable ROM location => 64Kbytes ROM

MM_BOOT MM_DEMON OR [IF]
	\ code in EPROM or for DEMON is linked to run at the start of
	\ SSRAM. For EPROM, the initial piece of code will copy the
	\ image to SSRAM.
40000000 CONSTANT ROM0		\ first usable ROM location
[ELSE] \ MM_PBLOADED
	\ code in Flash is linked to run at start of SSRAM plus c0. The
	\ PBL copies the image to SSRAM. The gap of c0 leaves room to
	\ form a formated AIF/Flash header for programming an updated
	\ image back to Flash.
400000C0 CONSTANT ROM0		\ first usable ROM location
[THEN] \ MM
[THEN] \ TAR_EBSA110

TAR_COGENT [IF]
00008000 CONSTANT ROM0		\ first usable ROM location
00018000 CONSTANT ROMEnd       \ first unusable ROM location => 64Kbytes ROM
00018000 CONSTANT MMU0		\ start of page tables in RAM
0001C000 EQU RAM0		\ first usable RAM location
0002C000 CONSTANT RAMEnd       \ first unusable ROM location => 64Kbytes RAM
[THEN] \ TAR_COGENT

TAR_EBSA285 [IF]

MM_PBLOADED [IF]
	\ code in Flash is linked to run at start of SSRAM plus c0. The gap of
	\ c0 leaves room to form a formated AIF/Flash header for programming
	\ an updated image back to Flash. The PBL will switch the memory map,
	\ init the X-Bus and SDRAM and copy the image from Flash to SDRAM,
	\ then branch to it. Thereby, the image is magically invoked at the
	\ right place.
000100c0 CONSTANT ROM0		\ first usable ROM location
00020000 CONSTANT ROMEnd       \ first unusable ROM location => 64Kbytes ROM
	\ load into SDRAM. Ignore the first 64Kbytes of memory
	\ for now. Eventually want a version that will run
	\ at 0 but then will need to leave room for the vectors there.
00020000 CONSTANT MMU0		\ start of page tables in RAM
00024000 EQU RAM0		\ first usable RAM location
00030000 CONSTANT RAMEnd       \ first unusable ROM location => 48Kbytes RAM
[ELSE] \ must be MM_DEMON
00008000 CONSTANT ROM0		\ first usable ROM location
00018000 CONSTANT ROMEnd       \ first unusable ROM location => 64Kbytes ROM
00018000 CONSTANT MMU0		\ start of page tables in RAM
0001C000 EQU RAM0		\ first usable RAM location
0002C000 CONSTANT RAMEnd       \ first unusable ROM location => 64Kbytes RAM
[THEN] \ MM
[THEN] \ TAR_EBSA285

base !


ROM0	VALUE	 _CODE  \ initial code space pointer


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 2: Define target sections
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Memory allocation for writable ROM:
\   ROMbottom||code>WORDworkarea|--//--|PAD|TIB|reserved<name||ROMtop
\   RAMbottom||variable>--//--<sp|rp||RAMtop
\ Memory allocation for unwritable ROM:
\   ROMbottom||initial-code>--//--<initial-name||ROMtop
\   RAMbottom||code/data>WORDworkarea|--//--|PAD|TIB|reserved<name|sp|rp||RAMtop

\ RAM0 MMU0 RAMEnd ROM0 ROMEnd must be defined by this point, to define the
\ memory map of the target image; currently they are defined in hmeta.fth


\ TODO define INITdata as the number of bytes of data in the pre-inititialised
\ section
45 CONSTANT INITdata  \ TODO just a terrible guess


IDATA VARIABLES RAM0 RAM0 INITdata 1- + SECTION target-idata
UDATA RAM0 INITdata + RAMEnd 1-         SECTION target-udata
CDATA ROM0 ROMEnd 1-                    SECTION target-code

\ TODO define these as EQUs

RAMEnd			CONSTANT RPP   \ start of return stack (RP0)
RPP RTCells CELLL * - 	CONSTANT SPP   \ start of data stack (SP0)
SPP DTCells CELLL * - 	CONSTANT RAMT0 \ top of free RAM area


\ Now we're ready to generate code for the target..
TARGET

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 3: Target-dependent startup code
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ This is the entry point for the executable image.

\ The origin is RAM0; the entry point for the executable. This piece of
\ assembler code is used to start the system up; it is outside the context
\ of any real Forth definition. It does any low-level hardware setup, then
\ initialises the virtual machine and branches to the Forth COLD definition


+asm
init-asm
10 10 30 MK-SYM-TABLE
0 LTORG-HEAD ! \ make sure no other puddle exists

00 L# B, \ branch past literal pool
20 LTORG \ create the first puddle in the literal pool
00 L.

0 [IF] \ TODO

MM_DEMON [IF]
	16 SWI, \ DEMON SWI to go into Supervisor mode
	\ should turn off all interrupts, too?
[THEN]

TAR_EBSA110 MM_BOOT AND [IF]

\ Image is programmed into EPROM and we have to take the responsibility of
\ copying ourself into RAM. The code is built to run at 4000.0000 and is
\ running at 0, but after the first WRITE we will be magically running
\ at 8000.00XX

		03 L# R0 =,
		0000FFFF R1 =,
		R1 R0 R0 AND,		\ get the low-order part

		80000000 # R0 R0 ORR,	\ high-order alias
		R0 PC MOV,		\ jump to high-order alias

03 L.		0 # R0 MOV,
		[ R1 ] R0 STR,		\ switch memory map

\ now running in ROM alias at &8000.00xx copy image from ROM to SSRAM
		40000000 R0 =,	\ destination
		80000000 R1 =,	\ source
		10000 4 / R2 =,	\ number of Dwords (64Kbytes)

04 L.		4 # [ R1 ] R3 LDR,
		4 # [ R0 ] R3 STR,
		1 # R2 R2 SUBS,
		04 L# NE B,

\ now we're going to jump to the RAM copy..
SYM-STATS
		05 L# R0 =,
		0000FFFF R1 =,
		R1 R0 R0 AND,		\ get the low-order part

		40000000 # R0 R0 ORR,	\ point to RAM copy
		R0 PC MOV,		\ jump to RAM copy 
05 L.
[THEN]


TAR_EBSA110 [IF]
	\ turn off the DEBUG LED to prove we got here.
		ledport R0 =,
		[ R0 ] R1 LDR,
		80 # R1 R1 ORR,		\ don't corrupt the DRAM type..
		[ R0 ] R1 STR,
[THEN]

TAR_EBSA285 [IF]
	\ turn on GREEN, YELLOW LEDs to prove we got here.
		4 R0 =,
		ledport R4 =,
		[ R4 ] R0 STR,
[THEN]

\ common startup code for every TARGET and every MEMMAP
		RPP rsp =,		\ init return stack pointer
		SPP dsp =,		\ init data stack pointer

\ TODO this is a forward defn.
\		GetVaxAdr trapfpc R0 =,
		0 # R1 MOV,
		[ R0 ] R1 STR,		\ turn off udebug

\ COLD (right at the end of this listing) is the execution address for the
\ high-level cold start - go do it!
		02 G# B,
		$ALIGN

[THEN] \ TODO


LOCAL-RESOLVED \ Check that all labels used in the init code are resolved
-asm \ remove ASSEMBLER from search list


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 5: Define primitives used by defining and immediate words
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

S-DEF

CODE doVALUE	( -- x )
\		Run-time routine of VALUE. Return the value of VALUE word.
\		This is like an invocation of doCONST for a VARIABLE but
\		instead of returning the address of the variable, we return
\		the value of the variable -- in other words, there is another
\		level of indirection.
		tos pushD,
		[ R14 ] R0 LDR,
		[ R0 ] tos LDR,
		END-CODE COMPILE-ONLY
 
CODE doCONST	( -- x )
\		Run-time routine of CONSTANT and VARIABLE. When you quote a
\		constant or variable you execute its code, which consists of a
\		call to here, followed by an inline literal. The literal is a
\		constant (for a CONSTANT) or the address at which a VARIABLE's
\		value is stored. Although you come here as the result of a
\		native machine call, you never go back to the return address
\		-- you jump back up a level by continuing at the new fpc value.
\		For 8086, Z80 the inline literal is at the return address
\		stored on the top of the hardware stack. For the ARM, we came
\		here through a bl (branch-and-link) so the return address is
\		in r14
		tos pushD,
		[ R14 ] tos LDR,	\ inline address from calling point
		END-CODE COMPILE-ONLY

CODE doUSER	( -- a-addr )
\		Run-time routine of USER. Return address of data space.
\		This is like doCONST but a variable offset is added to the
\		result. By changing the value at AddruserP (which happens
\		on a taskswap) the whole set of user variables is switched
\		to the set for the new task.
		tos pushD,
		[ R14 ] tos LDR,
\ TODO need equivalent		GetVaxAdr userP R0 =,
		[ R0 ] R1 LDR,
		R1 TOS TOS ADD,
		END-CODE COMPILE-ONLY

CODE doLIT	( -- x )
\		Push an inline literal. The inline literal is at the current
\		value of the fpc, so put it onto the stack and point past it.
		tos pushD,
		CELLL # [ fpc ] tos LDR, \ get literal, inc to next forth word
		END-CODE COMPILE-ONLY

CODE doCREATE	( -- a-addr )
\		Run-time routine of CREATE. For CREATEd words with an
\		associated DOES>, get the address of the CREATEd word's data
\		space and execute the DOES> actions. For CREATEd word without
\		an associated DOES>, return the address of the CREATE'd word's
\		data space. A CREATEd word starts its execution through this
\		routine in exactly the same way as a colon definition uses
\		doLIST. In other words, we come here through a native machine
\		branch. For ARM, r14 holds the address of the next word at the
\		called point and fpc holds next word above that.
\
\		Structure of CREATEd word:
\		        | call-doCREATE | 0 or DOES> code addr | a-addr |
\
\		The DOES> address holds a native call to doLIST. This routine
\		doesn't alter the fpc. We never come back *here* so we never
\		need to preserve an address that would bring us back *here*. 
\
\		For ARM, r14 points to the "0 or DOES>" address. The DOES>
\
\		Example : myVARIABLE CREATE , DOES> \
\		56 myVARIABLE JIM
\		JIM \ stacks the address of the data cell that contains 56
\
\   : doCREATE    SWAP            \ switch BX and top of 8086 stack item
\		  DUP CELL+ @ SWAP @ ?DUP IF EXECUTE THEN ; COMPILE-ONLY
\
		tos pushD,
		CELLL # [ R14 ] R0 LDR,		\ 0 or DOES> address
		[ R14 ] tos LDR,		\ a-addr
		0 # R0 R0 ORRS,			\ set flags..
		R0 PC NE MOV,			\ a DOES> address.. go there
						\ (and never come back)
		END-CODE COMPILE-ONLY		\ no DOES> actions

CODE doTO	( x -- )
\		Run-time routine of TO. Store x at the address in the
\		following cell. The inline literal holds the address
\		to be modified.
		\ get the address to be modified and point past
		CELLL # [ fpc ] R0 LDR,
		\ update to new value from tos
		[ R0 ] tos STR,
		tos popD,
		END-CODE COMPILE-ONLY

CODE doLIST	( -- ) ( R: -- nest-sys )
\		Process colon list.
\		The first word of a definition (the xt for the word) is a
\		native machine-code instruction for the target machine. For
\		high-level definitions, that code is emitted by xt, and
\		performs a call to doLIST. doLIST executes the list of xt that
\		make up the definition. The final xt in the definition is EXIT.
\		The address of the first xt to be executed is passed to doLIST
\		in a target-specific way. Two examples:
\		Z80, 8086: native machine call, leaves the return address on
\		the hardware stack pointer, which is used for the data stack.
\		ARM: branch-and-link, so the return address is in r14,
\		not on the data stack.
\
		fpc pushR,			\ preserve forth PC
		R14 fpc MOV,			\ first xt of definition
		END-CODE COMPILE-ONLY

CODE doLOOP	( -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Run time routine for LOOP.
\
		R0 popR,			\ loop count
		1 # R0 R0 ADDS,
		01 L# VS B,			\ overflow -> end loop
		R0 pushR,			\ update loop count
		[ fpc ] fpc LDR,	\ get branch dest. to loop again
		END-CODE-EARLY
01 L.		CELLL # fpc fpc ADD,		\ ignore branch offset
		R0 popR,			\ NAC what's this dropping?? -- can just bump rsp rather than doing memory access
		END-CODE COMPILE-ONLY

CODE do+LOOP	( n -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Run time routine for +LOOP.
\
		R0 popR,			\ loop count
		tos R0 R0 ADDS,
		tos popD,
		01 L# VS B,			\ overflow -> end loop
		R0 pushR,			\ update loop count
		[ fpc ] fpc LDR,		\ loop again
		END-CODE-EARLY
01 L.		CELLL # fpc fpc ADD,		\ ignore branch offset
		R0 popR,			\ NAC what's this dropping? -- can just bump rsp rather than doing memory access
		END-CODE COMPILE-ONLY

CODE branch	( -- )
\		Branch to an inline address.
		[ fpc ] fpc LDR,		\ get branch destination
		END-CODE COMPILE-ONLY		\ .. and go there

CODE 0branch	( flag -- )
\		Branch if flag is zero.
		tos tos tos ORRS,		\ test top of stack
\ ARM's cool conditional-execution saves pipeline-draining branches 
		CELLL # FPC FPC NE ADD,		\ don't branch, point past dest
		[ fpc ] fpc EQ LDR,		\ branch; get destination
		tos popD,			\ tidy up the stack
		END-CODE COMPILE-ONLY

F-DEF
CODE EXIT	( -- ) ( R: nest-sys -- )	\ CORE
\		Return control to the calling definition.
		fpc popR,			\ where call doLIST left it
		END-CODE COMPILE-ONLY


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 6: Set vectors for xts of primitives
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ The "' doVALUE" part searches the target dictionary.
\ The "TO xt-doVALUE" part changes a HOST value.
' doVALUE ' doCONST   ' doUSER
' doLIT   ' doCREATE  ' doTO
' doLIST  ' doLOOP    ' do+LOOP
' branch  ' 0branch ' EXIT

HOST
TO xt-branch  TO xt-EXIT     TO xt-0branch
TO xt-do+LOOP TO xt-doLOOP   TO xt-doLIST
TO xt-doTO    TO xt-doCREATE TO xt-doLIT
TO xt-doUSER  TO xt-doCONST  TO xt-doVALUE
TARGET


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 7: Define data structures used for memory allocation
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Defining words allocate data, code and name space in the target sections.
\ Normally, the pointers to allocate this stuff are stored in VALUEs that
\ are themselves defined in the target sections. This causes a problem at
\ startup: how to define the VALUEs that are used to hold the pointers when
\ we need to access those VALUEs in order to define themselves.
\ The solution is that we duplicate the pointers in HOST definitions. Once
\ we get started we flick a flag value and switch over to using the proper
\ pointers in the target sections.
\ The HOST version of the pointers has restricted functionality. They only
\ allow:
\ -- definitions in the NON-STANDARD-WORDLIST
\ -- CODE definitions
\ -- VALUE definitions.
\ Until now, we have just used them to build the primitives so that we could
\ set the vectors for execution tokens that are spat out by defining words
\ and immediate words.. these are used by TODO?COMPILER definitions.

0	VALUE cpVar	\ Point to the top of the code dictionary.
0	VALUE npVar	\ Point to the bottom of the name dictionary.
0	VALUE hereVar	\ Point to the RAM/ROM data space pointer. Used by , or ALLOT.

\ Now set them to the values that the host has been using (couldn't do
\ that *whilst* they were being defined, as the values change)

\ TODO the text above is out of date.. decided to always keep the v
\ values in the host.


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 8: Define remaining data structures
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ TODO initialisation is handled differently.. going to build 2 copies of
\ this table through another route.
\ tables of initialisation values for some variables.
\ There are two copies and they start off identical. In a RAM system, the
\ first version gets updated as you add words. The second copy allows you to
\ reset the state of the system to the way it was originally.
\ _CODE TO UZERO
\ _CODE INIT_SIZE + TO _CODE
\ _CODE CONSTANT ULAST
\ _CODE TO UZERO0
\ _CODE INIT_SIZE + TO _CODE

S-DEF

\ TODO-GFORTH -- doesn't see ( as a comment in this context -- looks like a string parsing problem
\ .. the problem is that there is a TAB character instead of a space...

host CR .( ...QUITting in hforth.fth for now..) quit \ TODO got to here.

\ Start of system variable area.
RAM0 CONSTANT var0

\ Start of initial value table of system variables.
UZERO CONSTANT sysVar0

\ Start of original initial value table of system variables.
\ (used by RESET-SYSTEM)
UZERO0 CONSTANT sysVar00
ULAST CONSTANT sysVar0End

\ Start address of the THROW message table.
AddrTHROWMsgTbl CONSTANT THROWMsgTbl

\ The following VALUEs get initialised from a table when hForth is started
\ The table is generated (at the end of this file) using $INIT

\ Vectored words
foo	VALUE 'ekey?	\ Execution vector of ekey?
foo	VALUE 'ekey	\ Execution vector of ekey
foo	VALUE 'emit?	\ Execution vector of emit?
foo	VALUE 'emit	\ Execution vector of emit
foo	VALUE 'init-i/o	\ Execution vector of init-i/o
foo	VALUE 'prompt	\ Execution vector of .prompt
foo	VALUE 'boot	\ Application's execution vector called by COLD

\		Identify i/o options
foo	VALUE IOBYTES

F-DEF

\ Identify the input source. -1 for string (via EVALUATE) and 0 for user input
\ device.
foo	VALUE SOURCE-ID ( -- 0 | -1 ) \ CORE EXT

\ Return the address of the radix base for numeric I/O.
VARIABLE BASE ( -- a-addr ) \ CORE


\   'doWord	( -- a-addr )
\		Execution vectors for 'interpret'.
\
$VAR 'doWord _VAR CELLL 5 * + TO _VAR

VARIABLE ROMB	\ Bottom of free ROM area.
VARIABLE ROMT	\ Top of free ROM area.
VARIABLE RAMB	\ Bottom of free RAM area.
VARIABLE RAMT	\ Top of free RAM area.

0 VALUE bal	\ The depth of control-flow stack.

foo VALUE notNONAME? \ Whether to do 'linkLast' or not (used by ';')

VARIABLE rakeVar \ Used by 'rake' to gather LEAVE.

VARIABLE #order \ Hold the search order stack depth.

\ TODO need to find the TARGET value of CELLL
_VAR OrderDepth CELLL * + TO _VAR  \ search order stack

VARIABLE current \ Point to the wordlist to be extended.
\ ie the compilation wordlist. current @ is the wid of the compilation wordlist

F-DEF
\   FORTH-WORDLIST   ( -- wid )			\ SEARCH
\		Return wid of Forth wordlist.
\
VARIABLE FORTH-WORDLIST 
_VAR CELLL 2 * + TO _VAR

\   NONSTANDARD-WORDLIST   ( -- wid )
\		Return wid of non-standard wordlist.
\
VARIABLE NONSTANDARD-WORDLIST
_VAR CELLL 2 * + TO _VAR

_VAR MaxWLISTS 2 - 3 * CELLL * + TO _VAR \ wordlist area

S-DEF


\ Return wid of ENVIRONMENT? string list. Never put this wid in search-order.
\ It should be used only by SET-CURRENT to add new environment query string
\ after addition of a complete wordset.
VARIABLE envQList ( -- wid )


VARIABLE userP \ Return address of USER variable area of current task.
	_VAR CONSTANT SysTask
	_VAR CELLL + TO _VAR
	_VAR CONSTANT SysUser1		\ user1
	_VAR CELLL + TO _VAR
	_VAR CONSTANT SysTaskName	\ taskName
	_VAR CELLL + TO _VAR
	_VAR CONSTANT SysThrowFrame	\ throwFrame
	_VAR CELLL + TO _VAR
	_VAR CONSTANT SysStackTop	\ stackTop
	_VAR CELLL + TO _VAR
	_VAR CONSTANT SysStatus	\ status
	_VAR CELLL + TO _VAR
	_VAR CONSTANT SysUserP
	_VAR CONSTANT SysFollower	\ follower
	_VAR CELLL + TO _VAR
	_VAR CELLL + TO _VAR		\ SP0 for system task
	_VAR CELLL + TO _VAR		\ RP0 for system task


$VARIABLE trapfpc \ Return address of a variable holding trap address for microdebugger


SysTask CONSTANT SystemTask \ Return system task's tid.


\   follower	( -- a-addr )
\		Point next task's 'status' USER variable.
\
SysFollower SysUserP - $USER follower

\   status	( -- a-addr )
\		Status of current task. Point 'pass' or 'wake'.
\
SysStatus SysUserP - $USER status

\   stackTop	( -- a-addr )
\		Store current task's top of stack position.
\
SysStackTop SysUserP - $USER stackTop

\   throwFrame	( -- a-addr )
\		THROW frame for CATCH and THROW need to be saved for eack task.
\
SysThrowFrame SysUserP - $USER throwFrame

\   taskName	( -- a-addr )
\		Current task's task ID.
\
SysTaskName SysUserP - $USER taskName

\   user1	( -- a-addr )
\		One free USER variable for each task.
\
SysUser1 SysUserP - $USER user1

\ That is the end of the variables that get initialised from the table. The
\ following variables must be initialised explicitly after cold start.

F-DEF

\ Return the address of a cell containing compilation-state flag
\ which is true in compilation state or false otherwise.
VARIABLE STATE ( -- a-addr )			\ CORE

\ Hold the character pointer while parsing input stream.
VARIABLE >IN ( -- a-addr )			\ CORE

S-DEF
\   errWord	( -- a-addr )
\		Last found word. To be used to display the word causing error.
\
$VARIABLE errWord	_VAR CELLL + TO _VAR

$VAR hld        ( -- a-addr )
\		Hold a pointer in building a numeric output string.

\   sourceVar	( -- a-addr )
\		Hold the current count and addr of the terminal input buffer.
\
$VAR sourceVar	_VAR CELLL + TO _VAR

\   abort"msg	( -- a-addr )
\		Abort" error message string address.
\
$VAR abort"msg _VAR CELLL + TO _VAR


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 9: Remaining required primitives
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

S-DEF

CODE rp@	( -- a-addr )
\		Push the current RP to the data stack.
		tos pushD,
		rsp tos MOV,
		END-CODE COMPILE-ONLY

CODE rp!	( a-addr -- )
\		Set the return stack pointer.
		tos rsp MOV,
		tos popD,
		END-CODE COMPILE-ONLY

CODE sp@	( -- a-addr )
\		Push the current data stack pointer.
		tos pushD,
		dsp tos MOV,
		END-CODE COMPILE-ONLY

CODE sp!	( a-addr -- )
\		Set the data stack pointer.
		tos dsp MOV,
		tos popD,
		END-CODE COMPILE-ONLY

CODE um+	( u1 u2 -- u3 1|0 )
\		Add two unsigned numbers, return the sum and carry.
		R1 popD,
		tos R1 R1 ADDS,			\ combine
		tos tos tos EOR,		\ clear register
		0 # tos tos ADC,		\ get carry flag
		R1 pushD,			\ store sum
		END-CODE

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Standard words - Processor-dependent definitions: 32-bit Forth for ARM RISC

F-DEF

CODE @		( a-addr -- x )			\ CORE
\		Push the contents at a-addr to the data stack.
		[ tos ] tos LDR,
		END-CODE

CODE !		( x a-addr -- )			\ CORE
\		Store x at a aligned address.
		R1 popD,			\ char to store
		[ tos ] R1 STR,
		tos popD,			\ clear up stack
		END-CODE

CODE C@		( c-addr -- char )		\ CORE
\		Fetch the character stored at c-addr.
		[ tos ] tos LDRB,
		END-CODE

CODE C!		( char c-addr -- )		\ CORE
\		Store char at c-addr.
		R1 popD,			\ char to store
		[ tos ] R1 STRB,
		tos popD,			\ clear up stack
		END-CODE

CODE 0<		( n -- flag )			\ CORE
\		Return true if n is negative.
		0 # R0 MOV,			\ get zeroes for dummy arg
						\ 20H = 32 decimal
		20 # ASR tos R0 tos ADD,	\ echo bit 32 value through r1
		END-CODE

CODE 0=		( x -- flag )			\ CORE
\		Return true if x is zero.
		0 # tos CMP,
		TRUEE tos EQ =,
		FALSEE tos NE =,
		END-CODE

CODE 2*		( x1 -- x2 )			\ CORE
\		Bit-shift left, filling the least significant bit with 0.
		1 # LSL tos tos MOV,
		END-CODE

CODE 2/		( x1 -- x2 )			\ CORE
\		Bit-shift right, leaving the most significant bit unchanged.
		1 # ASR tos tos MOV,
		END-CODE

CODE >R		( x -- ) ( R: -- x )		\ CORE
\		Move top of the data stack item to the return stack.
		tos pushR,
		tos popD,
		END-CODE COMPILE-ONLY

CODE R>		( -- x ) ( R: x -- )		\ CORE
\		Move x from the return stack to the data stack.
		tos pushD,
		tos popR,
		END-CODE COMPILE-ONLY

CODE R@		( -- x ) ( R: x -- x )		\ CORE
\		Copy top of return stack to the data stack.
		tos pushD,
		[ rsp ] tos LDR,
		END-CODE COMPILE-ONLY

CODE AND	( x1 x2 -- x3 )			\ CORE
\		Bitwise AND.
		R1 popD,
		R1 tos tos AND,
		END-CODE

CODE OR		( x1 x2 -- x3 )			\ CORE
\		Return bitwise inclusive-or of x1 with x2.
		R1 popD,
		R1 tos tos ORR,
		END-CODE

CODE XOR	( x1 x2 -- x3 )                 \ CORE
\		Bitwise exclusive OR.
		R1 popD,
		R1 tos tos EOR,
		END-CODE

CODE DROP	( x -- )			\ CORE
\		Discard top stack item.
		tos popD,
		END-CODE

CODE SWAP	( x1 x2 -- x2 x1 )		\ CORE
\		Exchange top two stack items.
		R1 popD,
		tos pushD,
		R1 tos MOV,
		END-CODE

CODE DUP	( x -- x x )			\ CORE
\		Duplicate the top stack item.
		tos pushD,
		END-CODE

CODE OVER	( x1 x2 -- x1 x2 x1 )		\ CORE
\		Copy second stack item to top of the stack.
		[ dsp ] R1 LDR,			\ copy 2nd stack item
		tos pushD,
		R1 tos MOV,
		END-CODE

CODE EXECUTE	( i*x xt -- j*x )		\ CORE
\		Perform the semantics indentified by execution token, xt.
		tos R1 MOV,
		tos popD,
		R1 PC MOV,			\ jump to the code address
		END-CODE-FLOW			\ tidy up

CODE MOVE	( addr1 addr2 u -- )		\ CORE
\		Copy u address units from addr1 to addr2 if u is greater
\		than zero. This word is CODE defined since no other Standard
\		words can handle address unit directly.
		R0 popD,			\ dest
		R1 popD,			\ source
		R1 R0 CMP,
		01 L# MI B,			\ copy from end backwards
02 L.		1 # [ R1 ] R2 LDRB,
		1 # [ R0 ] R2 STRB,
		1 # tos tos SUBS,
		02 L# NE B,
		tos popD,
		END-CODE-EARLY
01 L.		tos R0 R0 ADD,			\ point past the last bytes
		tos R1 R1 ADD,
03 L.		![ -1 # R1 ] R2 LDRB,		\ load with predecrement
		![ -1 # R0 ] R2 STRB,
		1 # tos tos SUBS,
		03 L# NE B,
		tos popD,
		END-CODE


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 10: Target-specific I/O routines.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Default IOBYTES value
\ Byte 3, 2 are undefined.
\ Byte 1 holds the DEFBAUD value
\ Byte 0 holds the com port value (FF=COMDSWI)
IO_DEFBAUD 8 LSHIFT IO_DEFIO + CONSTANT InitIOBYTES


\ TODO really want to abstract this to another file..
\ Equates for host I/O

TAR_EBSA110 [IF]
F2400000 CONSTANT ledport
F0000000 CONSTANT SuperIObase
[THEN]

TAR_EBSA285 [IF]
40012000 CONSTANT ledport
40011000 CONSTANT SuperIObase

13C CONSTANT SA_CONTROL
160 CONSTANT UARTDR
164 CONSTANT UMSEOI
164 CONSTANT RXSTAT
168 CONSTANT H_UBRLCR
16C CONSTANT M_UBRLCR
170 CONSTANT L_UBRLCR
174 CONSTANT UARTCON
178 CONSTANT UARTFLG

\ register constants
1 CONSTANT INIT_COMPLETE
[THEN]

TAR_EBSA110 TAR_EBSA285 OR [IF]
SuperIObase 3F8 2 LSHIFT + CONSTANT COM1Port
SuperIObase 2F8 2 LSHIFT + CONSTANT COM2Port

\ UART registers
	
	\ Receive, Transmit, Interrupt enable, Interrupt Identification and
	\ FIFO are only 
	\ accessable when the Divisor latch access bit in the line control
	\ register is 0
00 CONSTANT Rx \ Receive port, read only
00 CONSTANT Tx \ Transmit port, write only
04 CONSTANT IntEnable \ Interrupt enable, read/write
08 CONSTANT IntId \ Interrupt ID, read only
08 CONSTANT FIFOcntl \ FIFO control, write only

	\ With the Divisor latch access bit set the first 2 registers set
	\ the divisor, which controls the line speed.
00 CONSTANT Dllsb
04 CONSTANT Dlmsb

	\ The remaining registers are always accessable and read/write
0C CONSTANT LineCntl \ Line control, the main control register
10 CONSTANT ModemCntl \ Modem control
14 CONSTANT LineStatus
18 CONSTANT ModemStatus
	
\ Masks etc. for useful fields

	\ Line control register bits
80 CONSTANT DLABMsk \ Divisor latch.

	\ Word length values
03 CONSTANT WordLen8

	\ Line status register
01 CONSTANT LineDRMsk \ Data ready
20 CONSTANT LineTHREMsk \ Tx holding register empty (i.e. ready for next char)

	\ Useful line speed values for divider
0C CONSTANT Baud9600low
00 CONSTANT Baud9600high
06 CONSTANT Baud19200low
00 CONSTANT Baud19200high
03 CONSTANT Baud38400low
00 CONSTANT Baud38400high
02 CONSTANT Baud56000low
00 CONSTANT Baud56000high
[THEN] \ TAR_EBSA110 TAR_EBSA285 OR

\  *** System-dependent I/O -- Must be re-defined for each system.
S-DEF

CODE !IO	( -- )  "store i o"
\               Initialize the serial devices for terminal I/O

\ This is a general purpose Reset for the serial chip, cancelling the current
\ transmission and reception and setting the baudrate according to IOBYTES

\ TODO this is only coded for SuperIO uarts, and it isn't exactly *efficient*!
		GetVaxAdr IOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R2 ANDS,
		1 # R2 CMP,
		COM1Port R4 EQ =,
		COM2Port R4 NE =,
		0FF # R2 CMP,
		00 L# EQ B,		\ DEMON SWI in use so don't touch the
					\ UART
		FF00 # R1 R1 ANDS,	\ just look at baudrate
		500 # R1 R1 SUBS,
		04 L# EQ B,		\ value of 5
		100 # R1 R1 SUBS,
		03 L# EQ B,		\ value of 6
		100 # R1 R1 SUBS,
		02 L# EQ B,		\ value of 7
		\ not recognised or 9600 selected
		Baud9600low R0 =,
		Baud9600high R1 =,
		01 L# B,
04 L.		Baud19200low R0 =,
		Baud19200high R1 =,
	 	01 L# B,
03 L.		Baud38400low R0 =,
		Baud38400high R1 =,
		01 L# B,
02 L.		Baud56000low R0 =,
		Baud56000high R1 =,
\ Prevent serial interrupts while setting divisor because the divisor latch
\ makes it difficult to resume this from an interrupt
01 L.		0 R3 =,
		[ IntEnable # R4 ] R3 STRB,
		WordLen8 DLABMsk + R3 =,	\ Set divisor access
		[ LineCntl # R4 ] R3 STRB,
	
		\ Set the actual rate in the divisor.
		[ Dllsb # R4 ] R0 STRB,
		[ Dlmsb # R4 ] R1 STRB,
		\ Clear the divisor access and simultaneously set the
		\ line control register for 8 bit data, no parity, 1 stop bit.
		WordLen8 R1 =,
		[ LineCntl # R4 ] R1 STRB,

 		\ Turn on the FIFOs and clear them
		7 R1 =, 		\ All bits clear disables the FIFOs
		[ FIFOcntl # R4 ] R1 STRB,
	
		\ Clear out any errors by reading the line status
TAR_COGENT [IF]
		100 R3 =,
[THEN]
05 L.		[ LineStatus # R4 ] R1 LDRB,

TAR_COGENT [IF] \ TODO Carey reads the line status many times
		1 # R3 R3 SUBS,
		05 L# NE B,
[THEN]

	\ Disable receive and error interrupts
		0 R1 =,
		[ IntEnable # R4 ] R1 STRB,

TAR_COGENT [IF] \ TODO Carey has a delay loop here.
		10000 R1 =,
06 L.		1 # R1 R1 SUBS,
		06 L# EQ B,
[THEN]
00 L.		END-CODE


CODE RX?	( -- flag )
\		Return true if key is pressed
		tos pushD,			\ make room for flag
		GetVaxAdr IOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R1 ANDS,
		1 # R1 CMP,
		COM1Port R4 =,
		COM2Port R4 =,
		[ LineStatus # R4 ] tos LDRB,	\ look for a character
		\ reading LineStatus also clears any errors
		LineDRMsk # tos TST,
		0 # tos MOV,			\ predict no char available
		1 # tos tos NE SUB,		\ change flag to -1 (TRUE)
		END-CODE

CODE RX@	( -- u )
\		Receive one keyboard event u.
		tos pushD,			\ make room
		GetVaxAdr IOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R1 ANDS,
		1 # R1 CMP,
		COM1Port R4 =,
		COM2Port R4 =,
		[ Rx # R4 ] tos LDRB,		\ Read the character
		END-CODE

CODE DRX@	( -- u )
\		Receive one keyboard event u using DEMON SWI calls
		tos pushD,			\ make room
		4 SWI,				\ SWI_ReadC - byte is in r0
		R0 tos MOV,
\ There is a BUG (IMO) in ARM's terminal emulator that it will never return CR
\ - only LF. Since we expect CR to signal end-of-line, do a substitution here.
		LFF R0 =,
		R0 tos CMP,
		CRR tos EQ =,
		END-CODE

CODE TX?	( -- flag )
\		Return true if output device is ready or device state is
\		indeterminate.
		tos pushD,			\ make room
		GetVaxAdr IOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R1 ANDS,
		1 # R1 CMP,
		COM1Port R4 EQ =,
		COM2Port R4 NE =,
		[ LineStatus # R4 ] R2 LDRB,
		LineTHREMsk # R2 TST,		\ Wait until ready to queue character
		TRUEE tos =,			\ predict ready
		FALSEE tos NE =,
		END-CODE

CODE TX!	( u -- )
\		Send char to the output device. Have to wait until the output
\		device is ready because TX? is NOT called first
		GetVaxAdr IOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R1 ANDS,
		1 # R1 CMP,
		COM1Port R4 EQ =,
		COM2Port R4 NE =,
00 L.		[ LineStatus # R4 ] R2 LDRB,
		LineTHREMsk # R2 TST,		\ Wait until ready to queue character
		00 L# EQ B,
		[ Tx # R4 ] tos STRB,		\ Queue the character
		tos popD,
		END-CODE

CODE DTX!	( u -- )
\		Send char to the output device using DEMON SWI calls
		tos R0 MOV,			\ pop character to r0
		tos popD,
		0 SWI,				\ SWI_WriteC - write character
		END-CODE

\ *** MS-DOS only words -- not necessary for other systems.


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 11: THROW strings
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ TODO

\ THROW code messages resides in top of name space. Messages must be
\ placed before any Forth words were defined.
\ Before the text of the messages is defined, reserve room for a table of
\ vectors to the messages

_NAME NumTHROWMsgs CELLL * - TO _NAME
							  \ THROW code
$THROWMSG' ABORT'                                         \ -01
$THROWMSG' ABORT"'                                        \ -02
$THROWMSG' stack overflow'                                \ -03
$THROWMSG' stack underflow'                               \ -04
$THROWMSG' return stack overflow'                         \ -05
$THROWMSG' return stack underflow'                        \ -06
$THROWMSG' do-loops nested too deeply during execution'   \ -07
$THROWMSG' dictionary overflow'                           \ -08
$THROWMSG' invalid memory address'                        \ -09
$THROWMSG' division by zero'                              \ -10
$THROWMSG' result out of range'                           \ -11
$THROWMSG' argument type mismatch'                        \ -12
$THROWMSG' undefined word'                                \ -13
$THROWMSG' interpreting a compile-only word'              \ -14
$THROWMSG' invalid FORGET'                                \ -15
$THROWMSG' attempt to use zero-length string as a name'   \ -16
$THROWMSG' pictured numeric output string overflow'       \ -17
$THROWMSG' parsed string overflow'                        \ -18
$THROWMSG' definition name too long'                      \ -19
$THROWMSG' write to a read-only location'                 \ -20
$THROWMSG' unsupported operation (e.g., AT-XY on a too-dumb terminal)' \ -21
$THROWMSG' control structure mismatch'                    \ -22
$THROWMSG' address alignment exception'                   \ -23
$THROWMSG' invalid numeric argument'                      \ -24
$THROWMSG' return stack imbalance'                        \ -25
$THROWMSG' loop parameters unavailable'                   \ -26
$THROWMSG' invalid recursion'                             \ -27
$THROWMSG' user interrupt'                                \ -28
$THROWMSG' compiler nesting'                              \ -29
$THROWMSG' obsolescent feature'                           \ -30
$THROWMSG' >BODY used on non-CREATEd definition'          \ -31
$THROWMSG' invalid name argument (e.g., TO xxx)'          \ -32
$THROWMSG' block read exception'                          \ -33
$THROWMSG' block write exception'                         \ -34
$THROWMSG' invalid block number'                          \ -35
$THROWMSG' invalid file position'                         \ -36
$THROWMSG' file I/O exception'                            \ -37
$THROWMSG' non-existent file'                             \ -38
$THROWMSG' unexpected end of file'                        \ -39
$THROWMSG' invalid BASE for floating point conversion'    \ -40
$THROWMSG' loss of precision'                             \ -41
$THROWMSG' floating-point divide by zero'                 \ -42
$THROWMSG' floating-point result out of range'            \ -43
$THROWMSG' floating-point stack overflow'                 \ -44
$THROWMSG' floating-point stack underflow'                \ -45
$THROWMSG' floating-point invalid argument'               \ -46
$THROWMSG' compilation word list deleted'                 \ -47
$THROWMSG' invalid POSTPONE'                              \ -48
$THROWMSG' search-order overflow'                         \ -49
$THROWMSG' search-order underflow'                        \ -50
$THROWMSG' compilation word list changed'                 \ -51
$THROWMSG' control-flow stack overflow'                   \ -52
$THROWMSG' exception stack overflow'                      \ -53
$THROWMSG' floating-point underflow'                      \ -54
$THROWMSG' floating-point unidentified fault'             \ -55
$THROWMSG' QUIT'                                          \ -56
$THROWMSG' exception in sending or receiving a character' \ -57
$THROWMSG' [IF], [ELSE], or [THEN] exception'             \ -58

$ENVSTR" StrongARM" CPUStr \ TODO -- should be ARM4T or somesuch
$ENVSTR" ROM Model" ModelStr
$ENVSTR" 0.9.9" VersionStr

\ NAC at this point need to ALIGN the value of _NAME, since we packed
\ the strings of the throw table but  $CODE assumes that NAME is aligned.
	_NAME $ALIGN_DOWN TO _NAME


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 12: Environment strings
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ TODO -- could move these environment strings anywhere .. to the start
\ or preferably the end.. to get them out of the main flow.

CR .( *** ENVIRONMENT? Strings)
\ ENVIRONMENT? strings can be searched using SEARCH-WORDLIST and can be
\ EXECUTEd. This wordlist is completely hidden to Forth system except
\ ENVIRONMENT? .
ENV-DEF

: CPU [ CPUStr ] LITERAL COUNT ;
: model [ ModelStr ] LITERAL COUNT ;
: version [ VersionStr ] LITERAL COUNT ;
: /COUNTED-STRING [ MaxCountedString ] LITERAL ;
: /HOLD [ PADSize ] LITERAL ;
: /PAD [ PADSize ] LITERAL ;
: ADDRESS-UNIT-BITS D# 8 ;
: CORE [ TRUEE ] LITERAL ;
: FLOORED [ TRUEE ] LITERAL ;
: MAX-CHAR [ MaxChar ] LITERAL ; \ max value of character set
: MAX-D [ MaxUnsigned ] LITERAL [ MaxSigned ] LITERAL ;
: MAX-N [ MaxSigned ] LITERAL ;
: MAX-U [ MaxUnsigned ] LITERAL ;
: MAX-UD [ MaxUnsigned ] LITERAL [ MaxUnsigned ] LITERAL ;
: RETURN-STACK-CELLS [ RTCells ] LITERAL ;
: STACK-CELLS [ DTCells ] LITERAL ;
: EXCEPTION [ TRUEE ] LITERAL ;
: EXCEPTION-EXT [ TRUEE ] LITERAL ;
: WORDLISTS [ OrderDepth ] LITERAL ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 13: Remaining definitions
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Trivial high-level definitions

S-DEF

META-HI [IF]
0 CONSTANT 0
[ELSE]
CODE 0		( -- 0 )
		tos pushD,
		0 # tos MOV,
		END-CODE
[THEN]

META-HI [IF]
1 CONSTANT 1
[ELSE]
CODE 1		( -- 1 )
		tos pushD,
		1 # tos MOV,
		END-CODE
[THEN]

META-HI [IF]
-1 CONSTANT -1
[ELSE]
CODE -1		( -- -1 )
		tos pushD,
		-1 tos =,
		END-CODE
[THEN]

META-HI [IF]
: .prompt	( -- )
\		Display Forth prompt. This word is vectored.
		'prompt EXECUTE ;
[ELSE]
CODE .prompt
		GetVaxAdr 'prompt R0 =,
		[ R0 ] R1 LDR,
		R1 PC MOV,
		END-CODE-FLOW			\ tidy up
[THEN]

META-HI [IF]
: cell-		( a-addr1 -- a-addr2 )
\		Return previous aligned cell address.
		[ 0 CELLL - ] LITERAL + ;
[ELSE]
CODE cell-
		CELLL # tos tos SUB,
		END-CODE           
[THEN]

META-HI [IF]
: name>xt	( c-addr -- xt )
\		Return execution token using counted string at c-addr.
		cell- cell- @ ;
[ELSE]
CODE name>xt
		CELLL 2 * # tos R0 SUB,		\ point back past link to xt
		[ R0 ] tos LDR,			\ get the xt
		END-CODE
[THEN]

F-DEF
\   TRUE	( -- f )			\ CORE EXT
\		Return the TRUE flag
TRUEE CONSTANT TRUE

\   BL		( -- char )			\ CORE
\		Return the value of the blank character.
SPC CONSTANT BL

META-HI [IF]
: EMIT		( x -- )			\ CORE
\		Send a character to the output device.
		'emit EXECUTE ;
[ELSE]
CODE EMIT
01 G.		GetVaxAdr 'emit R0 =,
		[ R0 ] R1 LDR,
		R1 PC MOV,
		END-CODE-FLOW
[THEN]

: EMIT?		( -- flag )			\ FACILITY EXT
\		flag is true if the user output device is ready to accept data
\		and the execution of EMIT in place of EMIT? would not have
\		suffered an indefinite delay. If device state is indeterminate,
\		flag is true.
		'emit? EXECUTE ;

: EKEY?		( -- flag )			\ FACILITY EXT
\		If a keyboard event is available, return true.
		'ekey? EXECUTE ;

META-HI [IF]
: NIP		( n1 n2 -- n2 )			\ CORE EXT
\		Discard the second stack item.
		SWAP DROP ;
[ELSE]
CODE NIP	R0 popD,
		END-CODE
[THEN]

META-HI [IF]
: ROT		( x1 x2 x3 -- x2 x3 x1 )        \ CORE
\		Rotate the top three data stack items.
		>R SWAP R> SWAP ;
[ELSE]
CODE ROT	R0 popD,			\ x2 (tos=x3)
		R1 popD,			\ x1
		R0 pushD,
		tos pushD,
		R1 tos MOV,
		END-CODE
[THEN]

META-HI [IF]
: S>D		( n -- d )			\ CORE
\		Convert a single-cell number n to double-cell number.
		DUP 0< ;
[ELSE]
CODE S>D	tos pushD,
		0 # R1 MOV,
		\ sign-extend tos into tos to form ms of double
		20 # ASR tos R1 tos ADD, \ 20H = 32 decimal
		END-CODE
[THEN]

META-HI [IF]
: +		( n1|u1 n2|u2 -- n3|u3 )	\ CORE
\		Add top two items and gives the sum.
		um+ DROP ;
[ELSE]
CODE +		R1 popD,
		R1 tos tos ADD,
		END-CODE
[THEN]

META-HI [IF]
: CELL+		( a-addr1 -- a-addr2 )		\ CORE
\		Return next aligned cell address.
		[ CELLL ] LITERAL + ;
[ELSE]
CODE CELL+	CELLL # tos tos ADD,
		END-CODE
[THEN]

META-HI [IF]
: CHAR+		( c-addr1 -- c-addr2 )          \ CORE
\		Return next character-aligned address.
		[ CHARR ] LITERAL + ;
[ELSE]
CODE CHAR+	CHARR # tos tos ADD,
		END-CODE
[THEN]

META-HI [IF]
: 2!		( x1 x2 a-addr -- )		\ CORE
\		Store the cell pair x1 x2 at a-addr, with x2 at a-addr and
\		x1 at the next consecutive cell.
		SWAP OVER ! CELL+ ! ;
[ELSE]
CODE 2!		R0 popD,
		R1 popD,
		CELLL # [ tos ] R0 STR,
		[ tos ] R1 STR,
		tos popD,
		END-CODE
[THEN]

META-HI [IF]
: 2@		( a-addr -- x1 x2 )		\ CORE
\		Fetch the cell pair stored at a-addr. x2 is stored at a-addr
\		and x1 at the next consecutive cell.
		DUP CELL+ @ SWAP @ ;
[ELSE]
CODE 2@		4 # [ tos ] R0 LDR,	\ x2
		[ tos ] R1 LDR,
		R1 pushD,
		R0 tos MOV,
		END-CODE
[THEN]

META-HI [IF]
: 2DROP		( x1 x2 -- )			\ CORE
\		Drop cell pair x1 x2 from the stack.
		DROP DROP ;
[ELSE]
\ faster than the obvious "tos popD, tos popD," -- saves one data access
CODE 2DROP	dsp CELLL # dsp ADD,	\ bump up the stack pointer 
		tos popD,
		END-CODE
[THEN]

META-HI [IF]
: 2DUP		( x1 x2 -- x1 x2 x1 x2 )	\ CORE
\		Duplicate cell pair x1 x2.
		OVER OVER ;
[ELSE]
CODE 2DUP	[ dsp ] R0 LDR,	\ copy x1 without popping stack
		tos pushD,
		R0 pushD,
		END-CODE
[THEN]

META-HI [IF]
: 2SWAP		( x1 x2 x3 x4 -- x3 x4 x1 x2 )	\ CORE
\		Exchange the top two cell pairs.
		ROT >R ROT R> ;
[ELSE]
CODE 2SWAP	R0 popD, 		\ x3
		R1 popD,		\ x2
		R2 popD,		\ x1
		R0 pushD,
		tos pushD,
		R2 pushD,
		R1 tos MOV,
		END-CODE
[THEN]

META-HI [IF]
: 2OVER		( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )	\ CORE
\		Copy cell pair x1 x2 to the top of the stack.
		>R >R 2DUP R> R> 2SWAP ;
[ELSE]
CODE 2OVER	tos pushD,
		[ CELLL 3 * # dsp ] R0 LDR, 	\ x1
		[ CELLL 2 * # dsp ] tos LDR,	\ x2
		R0 pushD,
		END-CODE
[THEN]

META-HI [IF]
: 1+		( n1|u1 -- n2|u2 )		\ CORE
\		Increase top of the stack item by 1.
		1 + ;
[ELSE]
CODE 1+		1 # tos tos ADD,
		END-CODE
[THEN]

META-HI [IF]
: 1-		( n1|u1 -- n2|u2 )		\ CORE
\		Decrease top of the stack item by 1.
		D# -1 + ;
[ELSE]
CODE 1-		1 # tos tos SUB,
		END-CODE
[THEN]

META-HI [IF]
: INVERT	( x1 -- x2 )			\ CORE
\		Return one's complement of x1.
		D# -1 XOR ;
[ELSE]
CODE INVERT	tos tos MVN,
		END-CODE
[THEN]

META-HI [IF]
: NEGATE	( n1 -- n2 )			\ CORE
\		Return two's complement of n1.
		INVERT 1+ ;
[ELSE]
CODE NEGATE	tos tos MVN,
		1 # tos tos ADD,
		END-CODE
[THEN]

META-HI [IF]
: -		( n1|u1 n2|u2 -- n3|u3 )	\ CORE
\		Subtract n2|u2 from n1|u1, giving the difference n3|u3.
		NEGATE + ;
[ELSE]
CODE -		R1 popD,
		tos R1 tos SUB,
		END-CODE
[THEN]

META-HI [IF]
: =		( x1 x2 -- flag )		\ CORE
\		Return true if top two are equal.
		XOR 0= ;
[ELSE]
CODE =		R0 popD,
		R0 tos tos SUBS,		\ equal =>tos will be 0 (FALSE)
		TRUEE tos EQ =,
		FALSEE tos NE =,
		END-CODE
[THEN]

META-HI [IF]
: +!		( n|u a-addr -- )		\ CORE
\		Add n|u to the contents at a-addr.
		SWAP OVER @ + SWAP ! ;
[ELSE]
CODE +!		R0 popD,
		[ tos ] R1 LDR,
		R0 R1 R1 ADD,
		[ tos ] R1 STR,
		tos popD,
		END-CODE
[THEN]

META-HI [IF]
: COUNT		( c-addr1 -- c-addr2 u )	\ CORE
\		Convert counted string to string specification. c-addr2 is
\		the next char-aligned address after c-addr1 and u is the
\		contents at c-addr1.
		DUP CHAR+ SWAP C@ ;
[ELSE]
CODE COUNT	[ tos ] R0 LDRB,		\ u
		1 # tos tos ADD,
		tos pushD,
		R0 tos MOV,
		END-CODE
[THEN]

META-HI [IF]
: UNLOOP	( -- ) ( R: loop-sys -- )	\ CORE
\		Discard loop-control parameters for the current nesting level.
\		An UNLOOP is required for each nesting level before the
\		definition may be EXITed.
		R> R> R> 2DROP >R ; COMPILE-ONLY
[ELSE]
\ The code version doesn't save fpc on return stack so it doesn't have to do
\ the same save/restore as the colon version
CODE UNLOOP	rsp CELLL 2 * # rsp ADD, \ 2DROP from return stack
		END-CODE COMPILE-ONLY
[THEN]

: SOURCE	( -- c-addr u )			\ CORE
\		Return input buffer string.
		sourceVar 2@ ;

META-HI [IF]
: D+		( d1|ud1 d2|ud2 -- d3|ud3 )	\ DOUBLE
\		Add double-cell numbers.
		>R SWAP >R um+ R> R> + + ;
[ELSE]
CODE D+		R0 popD,			\ d2L (tos is d2H)
		R1 popD,			\ d1H
		R2 popD,			\ d1L
		R2 R0 R3 ADDS,
		R3 pushD,
		R1 tos tos ADC,
		END-CODE
[THEN]

META-HI [IF]
: HERE		( -- addr )			\ CORE
\		Return data space pointer.
		hereVar @ ;
[ELSE]
CODE HERE	tos pushD,
		GetVaxAdr hereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] tos LDR,			\ get its value
		END-CODE
[THEN]

META-HI [IF]
: ALLOT		( n -- )			\ CORE
\		Allocate n bytes in RAM or ROM data space.
		hereVar +! ;
[ELSE]
CODE ALLOT	GetVaxAdr hereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		R2 tos R2 ADD,
		tos popD,
		[ R1 ] R2 STR,
		END-CODE
[THEN]

S-DEF

META-HI [IF]
: rp0		( -- a-addr )
\		Pointer to bottom of the return stack.
		userP @ CELL+ CELL+ @ ;
[ELSE]
CODE rp0	tos pushD,
		GetVaxAdr userP R0 =,
		[ R0 ] R1 LDR,
		CELLL 2 * # R1 R1 ADD,
		[ R1 ] tos LDR,
		END-CODE
[THEN]

META-HI [IF]
: sp0		( -- a-addr )
\		Pointer to bottom of the data stack.
		userP @ CELL+ @ ;
[ELSE]
CODE sp0	tos pushD,
		GetVaxAdr userP R0 =,
		[ R0 ] R1 LDR,
		CELLL # R1 R1 ADD,
		[ R1 ] tos LDR,
		END-CODE
[THEN]

META-HI [IF]
: lastName	( -- c-addr )
\		Return the address of the last definition name.
		npVar @ CELL+ CELL+ ;
[ELSE]
CODE lastName	tos pushD,
		GetVaxAdr npVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] tos LDR,
		CELLL 2 * # tos tos ADD,
		END-CODE
[THEN]

META-HI [IF]
: xhere		( -- a-addr )
\		Return next available code space address.
		cpVar @ ;
[ELSE]
CODE xhere	tos pushD,
		GetVaxAdr cpVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] tos LDR,
		END-CODE
[THEN]

META-HI [IF]
: TOxhere	( a-addr -- )
\		Set the next available code space address as a-addr.
		cpVar ! ;
[ELSE]
CODE TOxhere	GetVaxAdr cpVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] tos STR,
		tos popD,
		END-CODE
[THEN]

META-HI [IF]
: code,		( x -- )
\		Reserve one cell in code space and store x in it.
		xhere DUP CELL+ TOxhere ! ;
[ELSE]
CODE code,	GetVaxAdr cpVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL # [ R2 ] tos STR,
		tos popD,
		[ R1 ] R2 STR,
		END-CODE
[THEN]




