\ $Id$
\ $Log$
\ Revision 1.2  1998/06/14 18:31:55  crook
\ meta compilation debug. Some colon definitions.
\
\ Revision 1.1  1998/06/07 22:50:31  crook
\ Initial revision
\

\ experimental meta-compiled version. See how many dependecies there are
\ on words defined earlier and whether these can be removed by re-ordering
\ Start with as many high-level definitions as possible.

\ Look for META-TODO for areas that I have not converted yet
0 CONSTANT META-TODO

\ ** the pieces below are notes or definitions to go in the metacompiler.


\ TODO --- could change all xts to be BL instructions - then need
\ to rearrange the return stack and FPC somewhat but can remove the
\ call-dolist interpeter - super speedup and the only cost is the
\ restraint that all forth code must be in a 23-bit window.. big deal!
\ The compiler can range-check to ensure that no out-of-range code was
\ ever generated.

\ ** BUGs in my source
\ the hi-level source for FILE was wrong.. missing @?
\ and HAND 
\ FLOW-ON had typo: # after XON

\ ** "Bugs" in Wonyong's source
\ 2DROP 2DUP had no high-level definition
\ "=mask" in head, should be =MASK
\ "=imed" should be "=IMED" in IMMEDIATE
\ "=comp" should be "=COMP" in COMPILE-ONLY
\ .... think "[ =MASK ] LITERAL" could just be "=MASK" etc.
\ .. or maybe they should all change to MASKK IMMED COMPO to match the
\ assembler constants
\ UNLOOP hi-level defn. needs COMPILE-ONLY
\ the label NOrder0 in UZERO table is not needed.
\ IMMEDIATE is typo'd in BEGIN and ELSE
\ high-level defn for ( should be IMMEDIATE
\ CHARS and ALIGNED and use char-size and cell-size whereas assembler
\ source uses CHARR and CELLL
\ call-code in FORTH, CALLL in assembler
\ carriage-return-char, linefeed-char  in FORTH, CRR, LFF in assembler
\ max-negative in FORTH, MaxNegative in assembler
\ .ok asm def not the same as high-level def. Equivalent hi-level def
\  is S" ok" TYPE
\ >IN should be labelled \ CORE

\ ** things that have been re-ordered


\ TODO the pieces below are definitions to control the flavour of compilation
\ .. they should be in hmeta.fth
\ Force high-level definitions where possible
FALSE hCONSTANT META-HI
\ Force unproven definitions where available
TRUE hCONSTANT META-EXPERIMENTAL
\ Force code endings to branch through micro-debugger
FALSE hCONSTANT MICRO-DEBUG


\ TODO forward references. These will be cured by the compiler and by
\ re-ordering the definitions. For now, I patched in the address from the
\ reference binary hf.lst
40000A04 hCONSTANT DoVALUE
400009F8 hCONSTANT DoCONST
40000A3C hCONSTANT DoUSER
400009EC hCONSTANT DoLit
40001090 hCONSTANT DoSQuote
4000002C hCONSTANT UZERO
400001E0 hCONSTANT ULAST
4001401C hCONSTANT AddrIOBYTES
4001418C hCONSTANT AddrUserP
40014014 hCONSTANT AddrTprompt
4001405C hCONSTANT AddrBal
40014028 hCONSTANT AddrNPVar
40014094 hCONSTANT LocCurrent
40001524 hCONSTANT xtSkipPARSE
40014024 hCONSTANT LocCPVar
400141C8 hCONSTANT LocHLD
400023F4 hCONSTANT xtTHROW
40001700 hCONSTANT CodeComma
40014048 hCONSTANT LocBASE
4001400C hCONSTANT AddrTEMIT
400141D8 hCONSTANT LocSTATE
400141CC hCONSTANT LocSourceVar
400141D4 hCONSTANT LocToIN
40000724 hCONSTANT udebug


\ This is derived from 1.17 of hfsarom.asm
\ which in turn is derived from Wonyong's v0.9.9
\ $Id$

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

CR .( *** Assembly constants)

HEX -1		hCONSTANT TRUEE
0		hCONSTANT FALSEE
1		hCONSTANT CHARR \ byte size of a character
4		hCONSTANT CELLL \ byte size of a cell
07F		hCONSTANT MaxChar \ use ASCII only
0FF		hCONSTANT MaxString \ max length of a string
07FFFFFFF	hCONSTANT MaxSigned \ max value of a signed integer
0FFFFFFFF	hCONSTANT MaxUnsigned \ max value of an unsigned integer
080000000	hCONSTANT MaxNegative \ max value of a negative integer
DECIMAL 134	hCONSTANT PADSize \ PAD area size
64		hCONSTANT RTCells \ return stack size
256		hCONSTANT DTCells \ data stack size
10		hCONSTANT BASEE \ default radix
10		hCONSTANT OrderDepth \ depth of search order stack
20		hCONSTANT MaxWLISTS \ maximum number of wordlists
				\ 2 are used by the system
				\ 18 are available to Forth programs
58		hCONSTANT NumTHROWMsgs \ number of throw messages
\ COMPO IMMED MASKK are now =MASK =COMP =IMED to match the colon defns
HEX 1F		hCONSTANT =MASK \ lexicon compile-only bit
20		hCONSTANT =COMP \ lexicon immediate bit
40		hCONSTANT =IMED \ lexicon bit mask
				  \ extended character set
				  \ maximum name length = 0x1F

\ NAC: added SPC, XON, XOFF
DECIMAL 32	hCONSTANT SPC \ space - blank
17		hCONSTANT XON \ CTRL-Q - flow control for FILE
19		hCONSTANT XOFF \ CTRL-S - flow control for FILE
8		hCONSTANT BKSPP \ backspace
9		hCONSTANT TABB \ tab
10		hCONSTANT LFF \ line feed
13		hCONSTANT CRR \ carriage return
127		hCONSTANT DEL \ delete
\ For the ARM, this is the branch-with-link op-code; the offset gets ORed in.
\ Used by ?call and xt, but only xt, actually emits opcodes.
HEX 0EB000000	hCONSTANT CALLL

\ Memory allocation for writable ROM
\  ROMbottom||code>WORDworkarea|--//--|PAD|TIB|reserved<name||ROMtop
\  RAMbottom||variable>--//--<sp|rp||RAMtop
\ Memory allocation for unwritable ROM
\  ROMbottom||initial-code>--//--<initial-name||ROMtop
\  RAMbottom||code/data>WORDworkarea|--//--|PAD|TIB|reserved<name|sp|rp||RAMtop

\ RAM0 MMU0 RAMEnd ROM0 ROMEnd must be defined by this point, to define the
\ memory map of the target image; currently they are defined in hmeta.fth

ROM0		hCONSTANT COLDD \ cold start vector
RAMEnd		hCONSTANT RPP \ start of return stack (RP0)
RPP RTCells - CELLL * hCONSTANT SPP \ start of data stack (SP0)
SPP DTCells - CELLL * hCONSTANT RAMT0 \ top of free RAM area

\ *******************************************************************
\
\ Default IOBYTES value
\ Byte 3, 2 are undefined.
\ Byte 1 holds the DEFBAUD value
\ Byte 0 holds the com port value (FF=COMDSWI)

META-TODO [IF] \ work through conditional to compute proper value for IOBYTES
04FF		hCONSTANT IOBYTES

	IF (DEFIO = "COM0")
IOBYTES         EQU (DEFBAUD * 256)
	ENDIF
	IF (DEFIO = "COM1")
IOBYTES         EQU (DEFBAUD * 256) + 1
	ENDIF
	IF (DEFIO = "COM2")
IOBYTES         EQU (DEFBAUD * 256) + 2
	ENDIF
	IF (DEFIO = "COMDSWI")
IOBYTES         EQU (DEFBAUD * 256) + &ff
	ENDIF
[THEN]

\ TODO really want to abstract this to another file..
\ Equates for host I/O

TAR_EBSA110 [IF]
F2400000 hCONSTANT ledport
F0000000 hCONSTANT SuperIObase
[THEN]

TAR_EBSA285 [IF]
40012000 hCONSTANT ledport
40011000 hCONSTANT SuperIObase

13C hCONSTANT SA_CONTROL
160 hCONSTANT UARTDR
164 hCONSTANT UMSEOI
164 hCONSTANT RXSTAT
168 hCONSTANT H_UBRLCR
16C hCONSTANT M_UBRLCR
170 hCONSTANT L_UBRLCR
174 hCONSTANT UARTCON
178 hCONSTANT UARTFLG

\ register constants
1 hCONSTANT INIT_COMPLETE
[THEN]

TAR_EBSA110 TAR_EBSA285 OR [IF]
SuperIObase 3F8 2 LSHIFT + hCONSTANT COM1Port
SuperIObase 2F8 2 LSHIFT + hCONSTANT COM2Port

\ UART registers
	
	\ Receive, Transmit, Interrupt enable, Interrupt Identification and
	\ FIFO are only 
	\ accessable when the Divisor latch access bit in the line control
	\ register is 0
00 hCONSTANT Rx \ Receive port, read only
00 hCONSTANT Tx \ Transmit port, write only
04 hCONSTANT IntEnable \ Interrupt enable, read/write
08 hCONSTANT IntId \ Interrupt ID, read only
08 hCONSTANT FIFOcntl \ FIFO control, write only

	\ With the Divisor latch access bit set the first 2 registers set
	\ the divisor, which controls the line speed.
00 hCONSTANT Dllsb
04 hCONSTANT Dlmsb

	\ The remaining registers are always accessable and read/write
0C hCONSTANT LineCntl \ Line control, the main control register
10 hCONSTANT ModemCntl \ Modem control
14 hCONSTANT LineStatus
18 hCONSTANT ModemStatus
	
\ Masks etc. for useful fields

	\ Line control register bits
80 hCONSTANT DLABMsk \ Divisor latch.

	\ Word length values
03 hCONSTANT WordLen8

	\ Line status register
01 hCONSTANT LineDRMsk \ Data ready
20 hCONSTANT LineTHREMsk \ Tx holding register empty (i.e. ready for next char)

	\ Useful line speed values for divider
0C hCONSTANT Baud9600low
00 hCONSTANT Baud9600high
06 hCONSTANT Baud19200low
00 hCONSTANT Baud19200high
03 hCONSTANT Baud38400low
00 hCONSTANT Baud38400high
02 hCONSTANT Baud56000low
00 hCONSTANT Baud56000high
[THEN] \ TAR_EBSA110 TAR_EBSA285 OR

\ This set of variables is used in the assembler source to track where
\ to emit stuff. The final values are not exposed in the target Forth.
\ Addresses are in target space.
\ The LINK addresses have to be variables rather than values since
\ accesses are vectored.
0	VALUE _THROW \ current throw string
RAM0	VALUE _VAR   \ initial data space pointer
ROM0	VALUE _CODE  \ initial code space pointer
ROMEnd	VALUE     _NAME  \ initial name space pointer.. decrement before use
VARIABLE _SLINK 0 _SLINK ! \ force a null link
VARIABLE _FLINK 0 _FLINK ! \ force a null link

\ Code generation - emit n at the target PC and increment the target PC
\ TODO change _CODE to use cpVar and xhere like hForth source
\ ..problem is that xhere is not defined yet.
\ and tidy up defns. accordingly
: meta-asm,32 ( n -- )
	_CODE t! _CODE CELLL + TO _CODE ;

\ Return the target PC - an absolue address in TARGET space
: meta-asm. ( -- n )
	_CODE ;

\ Change the target PC (not used by the assembler)
: meta-asm! ( n -- )
	TO _CODE ;

\ install routines in the assembler's vectors.
' meta-asm,32 'ASM,32 ! 
' meta-asm. 'ASM. !
' t@ 'ASM@ !
' t! 'ASM! !

\ Ready to start definitions for the system. First, load the meta-compiler
\ that allows colon definitions.
LOAD-COLON


CR .( *** Assembly macros)

\ Align an address to the next cell boundary by rounding it UP
\ TODO should make the rounding word for 16-bit and 32-bit by
\ checking CELLL first
: $ALIGN ( n -- n )
	3 + FFFFFFFC AND ;

\ Align an address to the next cell boundary by rounding it DOWN
\ - use this for name space, which grows towards low memory
\ TODO should make the rounding word for 16-bit and 32-bit by
\ checking CELLL first
: $ALIGN_DOWN ( n -- n )
	FFFFFFFC AND ;
ALSO its-words
\ Add a name to name space of dictionary.
\ TODO - do I need to add this to TDEF? TDEF Can't currently cope with
\ quote-delimited strings.
: $STR' \ $STR' string' label
	[CHAR] ' PARSE \ addr len
	\ hForth originally used aligned strings but I pack them on any
	\ boundary because ARM allows it and it saves space
	DUP >R _NAME OVER - \ save length and calculate destination addr
	DUP 1- TO _NAME
\ TODO - need new version of CMOVE that can move to Target space directly
	t2h SWAP CMOVE \ move string to name space
	R> _NAME tC! \ store length 
	_NAME CONSTANT ;

\ TODO had to move this up to avoid foward reference in $THROWMSG'
_NAME CONSTANT AddrTHROWMsgTbl

\ Add a THROW message in name space. THROW messages won't be
\ needed if target system does not need names of Forth words.
: $THROWMSG' \ $THROWMSG' string'
	[CHAR] ' PARSE \ addr len
	\ hForth originally used aligned strings but I pack them on any
	\ boundary because ARM allows it and it saves space
	DUP >R _NAME OVER - \ save length and calculate destination addr
	DUP 1- TO _NAME
\ TODO - need new version of CMOVE that can move to Target space directly
	t2h SWAP CMOVE \ move string to name space
	R> _NAME tC! \ store length 
	_THROW CELLL + TO _THROW
	_NAME AddrTHROWMsgTbl _THROW - t! ;

\ TODO add similar variable in ASMARM
VARIABLE META-DEPTH


\ TODO have a flag that is set by $CODE and cleared by $END-CODE etc.
\ that way, $CODE can tell whether the previous defn was terminated
\ correctly, to avoid those nasty wordlist errors that occur due to this
\ mistake.

\ General-purpose routine for producing a dictionary entry. n is the address
\ of the variable used as the link address.
\ $CODE leaves the assembler in the search path. For real code words,
\ the search order will be restored by an eventual $NEXT. For other
\ words, the search order must be restored before the end of the word.
\ The dstack depth is saved at the start; it can be checked at the end
\ if desired.
\ Words to set flags in the dictionary (eg immediate, compile-only bits)
\ can be used at any time before a new definition is started, but a
\ consistent approach is recommended.

: $CODE ( n -- )
	>R
	DEPTH META-DEPTH ! \ should finish with 1 fewer than we start
	_CODE $ALIGN TO _CODE
	TDEF PARSE-WORD \ addr len
	DUP 1+ $ALIGN \ name + length takes an integral number of CELLs
	CELLL 2* + \ total size of dictionary entry
	_NAME SWAP - TO _NAME
	_CODE _NAME t!
	R@ @ _NAME CELLL + t!
	_NAME CELLL 2* + DUP R> !
\ TODO - need new version of CMOVE that can move to Target space directly
	t2h \ from len to
	2DUP C! 1+ \ store length
	SWAP CMOVE 
	ALSO ASSEMBLER ; \ leave assembler in the search order

PREVIOUS \ remove its-words

: $SCODE _SLINK $CODE ;
: $FCODE _FLINK $CODE ;

\ End a code definition. Use $NEXT-EARLY if there are multiple exit points,
\ or $END-CODE if the code doesn't need the fpc updating for some reason
ALSO ASSEMBLER

: $END-CODE
	LOCAL-RESOLVED PREVIOUS DEPTH META-DEPTH @ <> IF
		ABORT" $NEXT terminated definition with unbalanced stack"
	THEN ;

MICRO-DEBUG [IF]
: $NEXT
	udebug B, $END-CODE ;
: $NEXT-EARLY
	udebug B, ;
[ELSE]
: $NEXT
	CELLL # [ fpc ] PC LDR,	$END-CODE ;
: $NEXT-EARLY
	CELLL # [ fpc ] PC LDR, ;
[THEN]

PREVIOUS \ take ASSEMBLER off search order

\ TODO ultimately these will be replaced by COMPILE-ONLY and IMMEDIATE
\ Set COMPILE bit of most recent definition
: $COMPO
	_NAME CELLL 2* + DUP tC@ =COMP OR SWAP tC! ;
\ Set IMMEDIATE bit of most recent definition
: $IMMED
	_NAME CELLL 2* + DUP tC@ =IMED OR SWAP tC! ;


ALSO ASSEMBLER \ make BL, available
\ $xVALUE string <label> - compile a system/Forth VALUE header
: $SVALUE
	_SLINK $CODE PREVIOUS
	DoVALUE BL, \ source and dest are target space addresses.
	_VAR meta-asm,32 \ TODO -- meta-asm,32 is really target COMPILE,
	_VAR CELLL + TO _VAR ;

: $FVALUE
	_FLINK $CODE PREVIOUS
	DoVALUE BL,
	_VAR meta-asm,32 \ TODO -- meta-asm,32 is really target COMPILE,
	_VAR CELLL + TO _VAR ;

\ TODO this (and $VAR, $VAL) should really be expressed using the target
\ versions of CONSTANT, VARIABLE, VALUE.
\ <value> $xCONST string - compile a system/Forth CONSTANT header
: $SCONST
	_SLINK $CODE PREVIOUS
	DoCONST BL,
	\ emit the value of the constant (at tos) inline
	meta-asm,32 ; \ TODO -- meta-asm,32 is really target COMPILE,

: $FCONST
	_FLINK $CODE PREVIOUS
	DoCONST BL,
	\ emit the value of the constant (at tos) inline
	meta-asm,32 ; \ TODO -- meta-asm,32 is really target COMPILE,

\ $xVAR string - compile a system/Forth VARIABLE header
\ (uses DoCONST just as CONST does)
: $SVAR
	_SLINK $CODE PREVIOUS
	DoCONST BL,
	\ emit the address of the variable inline
	_VAR meta-asm,32 \ TODO -- meta-asm,32 is really target COMPILE,
	_VAR CELLL + TO _VAR ;

: $FVAR
	_FLINK $CODE PREVIOUS
	DoCONST BL,
	\ emit the address of the variable inline
	_VAR meta-asm,32 \ TODO -- meta-asm,32 is really target COMPILE,
	_VAR CELLL + TO _VAR ;

\ <offset> $SUSER string <value> - compile a system USER header
: $SUSER
	_SLINK $CODE PREVIOUS
	DoUSER BL,
	\ emit the value of the offset (at tos) inline
	meta-asm,32 ; \ TODO -- meta-asm,32 is really target COMPILE,

\ TODO - may be better to perform this in the true compiler.
\ Compile an inline string
\ .. definitely would be.
: $INSTR'
	DoLIT meta-asm,32
	[CHAR] ' PARSE
	DUP DUP >R meta-asm,32		\ emit count and save on R for later
	DoSQuote meta-asm,32		\ doS"
\ TODO - need new version of CMOVE that can move to Target space directly
	_CODE t2h SWAP CMOVE		\ store string
	R> _CODE + $ALIGN TO _CODE ;	\ update code pointer


PREVIOUS \ take ASSEMBLER off search order

META-TODO [IF]

;       Compile a environment query string header.

$ENVIR  MACRO   LEX,NAME
	$ALIGN                                  ;force to cell boundary
	_CODE   = $                             ;save code pointer
	_LEN    = (LEX AND MASKK)/CELLL         ;string cell count, round down
	_NAME   = _NAME-((_LEN+3)*CELLL)        ;new header on cell boundary
ORG     _NAME                                   ;set name pointer
	DW      _CODE,_ENVLINK                  ;token pointer and link
	_ENVLINK = $                            ;link points to a name string
	DB      LEX,NAME                        ;name string
ORG     _CODE
	NOP
	CALL    DoLIST
	ENDM

[THEN]


CR .( *** Make : invoke t: and ; invoke t; )
ALSO it-words
\ put these into FORTH
: h: : ;
: h; hPOSTPONE ; ; IMMEDIATE
: : t: ;
\ put this into it-words
ALSO it-words DEFINITIONS
h: ; nit; h; IMMEDIATE
PREVIOUS DEFINITIONS
PREVIOUS



CR .( *** Main entry points and COLD start data)

\ bit of assembler code that is outside a real definition
ALSO ASSEMBLER init-asm
10 10 10 MK-SYM-TABLE
0 LTORG-HEAD ! \ make sure no other puddle exists
20 LTORG \ create the first puddle in the literal pool

\ ORG is RAM0.

MM_DEMON [IF]
	16 SWI, \ DEMON SWI to go into Supervisor mode
	\ should turn off all interrupts, too?
[THEN]

TAR_EBSA110 MM_BOOT AND [IF]

\ Image is programmed into EPROM and we have to take the responsibility of
\ copying ourself into RAM. The code is built to run at 4000.0000 and is
\ running at 0, but after the first WRITE we will be magically running
\ at 8000.00XX

\ TODO - convert the following

\ ROM equivalent of whereabouts we are
hialias         EQU     ((herefar1 :AND: &0fffffff) :OR: &80000000)
\ SSRAM equivalent of whereabouts we are
loalias         EQU     ((herefar2 :AND: &0fffffff) :OR: &40000000)

		ldr     pc,=hialias             ;jump to to high ROM alias
herefar1        mov     r0, #0
		str     r0, [r1]                ;switch memory map

\ now running in ROM alias at &8000.00xx copy image from ROM to SSRAM
		mov     r0, #&40000000          ;destination
		mov     r1, #&80000000          ;source
		mov     r2, #(&10000/4)         ;number of Dwords (64Kbytes)

movit           ldr     r3,[r1],#4
		str     r3,[r0],#4
		subs    r2,r2,#1
		bne     movit

		ldr     pc,=loalias             ;jump to the RAM copy
herefar2
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

\ TODO		ldr     r0,=AddrTrapfpc
		0 # R1 MOV,
		[ R0 ] R1 STR,		\ turn off udebug

\ COLD (right at the end of this listing) is the execution address for the
\ high-level cold start - go do it!
\ TODO - doing this code line causes something bad to happen..
\		COLD B,			
		$ALIGN


\ TODO these are all forward references. Better to move the whole thing
\ to the end where the table copy is.
META-TODO [IF]

\ COLD start moves the following to system variables.
\ MUST BE IN SAME ORDER AS SYSTEM VARIABLES.

		$ALIGN                          ;align to cell boundary
UZERO           DW      RXQ                     ;'ekey?
		DW      RXFetch                 ;'ekey
		DW      TXQ                     ;'emit?
		DW      TXStore                 ;'emit
		DW      Set_IO                  ;'init-i/o
		DW      DotOK                   ;'prompt
		DW      HI                      ;'boot
		DW      IOBYTES                 ;control i/o
		DW      0                       ;SOURCE-ID
		DW      AddrROMB                ;CPVar
		DW      AddrROMT                ;NPVar
		DW      AddrRAMB                ;HereVar points RAM space.
		DW      OptiCOMPILEComma        ;'doWord nonimmediate word - compilation
		DW      EXECUTE                 ;nonimmediate word - interpretation
		DW      DoubleAlsoComma         ;not found word - compilateion
		DW      DoubleAlso              ;not found word - interpretation
		DW      EXECUTE                 ;immediate word - compilation
		DW      EXECUTE                 ;immediate word - interpretation
		DW      10                      ;BASE
		DW      CTOP                    ;ROMB
		DW      NTOP                    ;ROMT
		DW      VTOP                    ;RAMB
		DW      RAMT0                   ;RAMT
		DW      0                       ;bal
		DW      0                       ;notNONAME?
		DW      0                       ;rakeVar
		DW      2                       ;#order
		DW      FORTH_WORDLISTAddr      ;search order stack
		DW      NONSTANDARD_WORDLISTAddr
		DS      (OrderDepth-2) * CELLL
		DW      FORTH_WORDLISTAddr      ;current pointer
		DW      LASTFORTH               ;FORTH-WORDLIST
		DW      NONSTANDARD_WORDLISTAddr        ;wordlist link
		DW      FORTH_WORDLISTName      ;name of the WORDLIST
		DW      LASTSYSTEM              ;NONSTANDARD-WORDLIST
		DW      0                       ;wordlist link
		DW      NONSTANDARD_WORDLISTName ;name of the WORDLIST
		DS      3*(MaxWLISTS-2) * CELLL ;wordlist area
		DW      LASTENV                 ;envQList
		DW      SysUserP                ;user pointer
		DW      SysUserP                ;system task's tid
		DS      CELLL                   ;user1
		DW      SystemTaskName          ;taskName
		DS      CELLL                   ;throwFrame
		DS      CELLL                   ;stackTop
		DW      Wake                    ;status
		DW      SysStatus               ;follower
		DW      SPP                     ;system task's sp0
		DW      RPP                     ;system task's rp0
ULAST

[THEN]
PREVIOUS

\ THROW code messages resides in top of name space. Messages must be
\ placed before any Forth words were defined.
\ Before the text of the messages is defined, reserve room for a table of
\ vectors to the messages

\ TODO - had to put in the -3 to match the prebuilt image, but don't see
\ how it got there.. could be a bug in the awk scripts.
_NAME NumTHROWMsgs CELLL * - 3 - TO _NAME

	$STR' StrongARM' CPUStr
	$STR' ROM Model' ModelStr
	$STR' 0.9.9' VersionStr
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
	$THROWMSG' [IF], [ELSE], or [THEN] exception'            \ -58

\ NAC at this point need to ALIGN the value of _NAME, since we packed
\ the strings of the throw table but  $CODE assumes that NAME is aligned.
	_NAME $ALIGN_DOWN TO _NAME


CR .( *** System dependent words -- Must be re-defined for each system.)


\   !IO         ( -- )  "store i o"
\               Initialize the serial devices for terminal I/O

		$SCODE !IO

\ This is a general purpose Reset for the serial chip, cancelling the current
\ transmission and reception and setting the baudrate according to IOBYTES

\ TODO this is only coded for SuperIO uarts, and it isn't exactly *efficient*!
		AddrIOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R2 ANDS,
		1 # R2 CMP,
		COM1Port R4 EQ =,
		COM2Port R4 NE =,
		0FF # R1 R2 ANDS,	\ TODO - don't need this instruction
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
00 L.		$NEXT


\   TRUE	( -- f )
\		Return the TRUE flag

		TRUEE $SCONST TRUE

\   RX?		( -- flag )
\		Return true if key is pressed.

		$SCODE RX?
		tos pushD,			\ make room for flag
		AddrIOBYTES R0 =,
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
		$NEXT

\   RX@		( -- u )
\		Receive one keyboard event u.

		$SCODE RX@
		tos pushD,			\ make room
		AddrIOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R1 ANDS,
		1 # R1 CMP,
		COM1Port R4 =,
		COM2Port R4 =,
		[ Rx # R4 ] tos LDRB,		\ Read the character
		$NEXT

\   DRX@	( -- u )
\		Receive one keyboard event u using DEMON SWI calls

		$SCODE DRX@
		tos pushD,			\ make room
		4 SWI,				\ SWI_ReadC - byte is in r0
		R0 tos MOV,
\ There is a BUG (IMO) in ARM's terminal emulator that it will never return CR
\ - only LF. Since we expect CR to signal end-of-line, do a substitution here.
		LFF R0 =,
		R0 tos CMP,
		CRR tos EQ =,
		$NEXT

\   TX?		( -- flag )
\		Return true if output device is ready or device state is
\		indeterminate.

		$SCODE TX?
		tos pushD,			\ make room
		AddrIOBYTES R0 =,
		[ R0 ] R1 LDR,
		0FF # R1 R1 ANDS,
		1 # R1 CMP,
		COM1Port R4 EQ =,
		COM2Port R4 NE =,
		[ LineStatus # R4 ] R2 LDRB,
		LineTHREMsk # R2 TST,		\ Wait until ready to queue character
		TRUEE tos =,			\ predict ready
		FALSEE tos NE =,
		$NEXT

\   TX!		( u -- )
\		Send char to the output device. Have to wait until the output
\		device is ready because TX? is NOT called first

		$SCODE TX!
		AddrIOBYTES R0 =,
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
		$NEXT

\   DTX!	( u -- )
\		Send char to the output device using DEMON SWI calls

		$SCODE DTX!
		tos R0 MOV,			\ pop character to r0
		tos popD,
		0 SWI,				\ SWI_WriteC - write character
		$NEXT


\   CR		( -- )				\ CORE
\		Carriage return and linefeed.
\
\ TODO - this is a defn. change; a bug in wonyong's source
xf-DEF
: CR  [ CRR ] LITERAL EMIT [ LFF ] LITERAL EMIT ;


\   BYE		( -- )				\ TOOLS EXT
\		Return control to the host operation system, if any.

		$FCODE BYE
MM_DEMON [IF]
		11 SWI,				\ SWI_Exit - halt execution and
						\ return to debugger
[ELSE]
\ TODO
\		bl      DoLIST                  ;fake a colon definition
\		$INSTR  ' Sorry - nowhere to go!'
\		DW      TYPEE,CR,EXIT
[THEN]
		$END-CODE

META-TODO [IF]

;   hi          ( -- )
;
xS-DEF
  : hi		CR ." hForth "
		S" CPU" ENVIRONMENT? DROP TYPE SPACE
		S" model" ENVIRONMENT? DROP TYPE SPACE [CHAR] v EMIT
		S" version"  ENVIRONMENT? DROP TYPE
		."  by Wonyong Koh, 1997" CR
		." StrongARM port by neal.crook@reo.mts.dec.com" CR
		." ALL noncommercial and commercial uses are granted." CR
		." Please send comment, bug report and suggestions to:" CR
		."   wykoh@pado.krict.re.kr or wykoh@hitel.kol.co.kr" CR ;


;   INIT-BUS ( -- )
;               For the 21285, need to init some PCI stuff to avoid hanging
;               the host system (if any)
;
;   : INIT-BUS 40012000 @ 40 AND 0= IF
;       \ init PCI because stand-alone bit is clear - see
;	\ EBSA-285 reference manual for explanation of these
;       0C 42000034 !
;       00 42000150 !
;	00 42000154 !
;       00 42000140 !
;       200 4200013C !
;       7C0000 42000100 !
;       40000000 42000010 !
;	F000 42000018 !
;       17 42000004 !
;       1  4200013C !
;     THEN ;

		$COLON  8,'INIT-BUS',InitBus,_SLINK
	IF (TARGET = "EBSA285")
		DW DoLIT,&40012000,Fetch,DoLIT
		DW &40,ANDD,ZeroEquals,ZBranch,IBDONE
		DW DoLIT,&0C,DoLIT,&42000034,Store
		DW Zero,DoLIT,&42000150,Store
		DW Zero,DoLIT,&42000154,Store
		DW Zero,DoLIT,&42000140,Store
		DW DoLIT,&200,DoLIT,&4200013C,Store
		DW DoLIT,&7C0000,DoLIT,&42000100,Store
		DW DoLIT,&40000000,DoLIT,&42000010,Store
		DW DoLIT,&0F000,DoLIT,&42000018,Store
		DW DoLIT,&017,DoLIT,&42000004,Store
		DW DoLIT,One,DoLIT,&4200013C,Store
	ENDIF
IBDONE		DW      EXIT

;   COLD        ( -- )
;               The cold start sequence execution word.
;
;   : COLD      sysVar0 var0 [ sysVar0End sysVar0 - ] LITERAL
;               MOVE                            \ initialize system variable
;               xhere DUP @                     \ free-ROM [free-ROM]
;               INVERT SWAP 2DUP ! @ XOR        \ writable ROM?
;               IF RAMB TO cpVar RAMT TO npVar THEN
;               sp0 sp! rp0 rp!                 \ initialize stack
;               'init-i/o EXECUTE
;               'boot EXECUTE
;		INIT-BUS
;               QUIT ;                          \ start interpretation

		$COLON  4,'COLD',COLD,_SLINK
		DW      SysVar0,VarZero,DoLIT,ULAST-UZERO,MOVE
		DW      XHere,DUPP,Fetch,INVERT,SWAP,TwoDUP,Store,Fetch,XORR
		DW      ZBranch,COLD1
		DW      RAMB,DoTO,AddrCPVar,RAMT,DoTO,AddrNPVar
COLD1           DW      SPZero,SPStore,RPZero,RPStore
		DW      TickINIT_IO,EXECUTE,TickBoot,EXECUTE,InitBus
		DW      QUIT

\   set-i/o	( -- )
\		Set input/output device. (Includes initialising any hardware)
\
;   : set-i/o   set-i/ov !IO ;

		$COLON  7,'set-i/o',Set_IO,_SLINK
		DW      Set_IOV,STOIO,EXIT

\   set-i/ov	( -- )
\		Set input/output vectors, according to IOBYTES. Easiest way to
\		do this is to assume that the default vectors will be used
\		then check IOBYTES; if DEMON vectors are required, overwrite
\		them
\
;   : set-i/ov  sysVar0 var0 4 CELLS MOVE       \ set i/o vectors
;               AddrIOBYTES @ FF AND FF =
;               IF
;                       ' DTX! TO 'emit
;                       ' DRX@ TO 'ekey
;                       ' TRUE TO 'emit?
;                       ' TRUE TO 'ekey?
;               THEN ;

		$COLON  8,'set-i/ov',Set_IOV,_SLINK
		DW      SysVar0,VarZero,DoLIT,4*CELLL,MOVE
		DW      DoLIT,AddrIOBYTES,Fetch,DoLIT,0ffh,ANDD
		DW      DoLIT,0ffh,Equals,ZBranch,SETIOV2
		DW      DoLIT,DTXStore,DoTO,AddrTEMIT
		DW      DoLIT,DRXFetch,DoTO,AddrTEKEY
		DW      DoLIT,FTRUE,DoTO,AddrTEMITQ
		DW      DoLIT,FTRUE,DoTO,AddrTEKEYQ
SETIOV2         DW      EXIT


\   FILE	( -- )
\		Set FILE bit in IOBYTES to disable local echo and impose
\		XON/XOFF flow control for host file download
;   : FILE	IOBYTES 10000000 OR TO IOBYTES ;

		$COLON 4,'FILE',FILE,_SLINK
		DW      DoLIT,AddrIOBYTES,Fetch,DoLIT,010000000h,ORR
		DW      DoTO,AddrIOBYTES,EXIT


;   HAND        ( -- )
;               Clear FILE bit in IOBYTES to turn off XON/XOFF handshake
;               and turn on local echo
;   : HAND      IOBYTES EFFFFFFF AND TO IOBYTES ;

		$COLON 4,'HAND',HAND,_SLINK
		DW      DoLIT,AddrIOBYTES
		DW      Fetch,DoLIT,0efffffffh,ANDD,DoTO,AddrIOBYTES,EXIT

[THEN]

\   FLOW-ON	( -- )
\		Send an XON character for the file downloading process.
xS-DEF
\ TODO another change from previous high-level code
: FLOW-ON [ XON ] LITERAL EMIT ;

META-TODO [IF]

\   FLOW-OFF	( -- )
\		Send an XOFF character for the file downloading process; ONLY
\		if FILE bit is set
;   : FLOW-OFF  IOBYTES 10000000 AND IF XOFF EMIT THEN ;

		$COLON  8,'FLOW-OFF',FLOW_OFF,_SLINK
		DW      DoLIT,AddrIOBYTES,Fetch,DoLIT,010000000h,ANDD
		DW      ZBranch,FLOF1
		DW      DoLIT,XOFF,EMIT
FLOF1           DW      EXIT

[THEN]
CR .( *** MS-DOS only words -- not necessary for other systems.)


CR .( *** Non-Standard words - Processor-dependent definitions)
CR .( ***        32-bit Forth for ARM RISC)

\ microdebugger for debugging new hForth ports by NAC.
\
\ The major problem with debugging Forth code at the assembler level is that
\ most of the definitions are lists of execution tokens that get interpreted
\ (using doLIST) rather than executed directly. As far as the native processor
\ is concerned, these xt are data, and a debugger cannot be set to trap on
\ them.
\
\ The solution to that problem would seem to be to trap on the native-machine
\ 'call' instruction at the start of each definition. However, the threaded
\ nature of the code makes it very difficult to follow a particular definition
\ through: many definitions are used repeatedly through the code. Simply
\ trapping on the 'call' leads to multiple unwanted traps.
\
\ Consider, for example, the code for doS" --
\
\          DW      RFrom,SWAP,TwoDUP,Plus,ALIGNED,ToR,EXIT
\
\ It would be useful to run each word in turn; at the end of each word the
\ effect upon the stacks could be checked until the faulty word is found.
\
\ This technique allows you to do exactly that.
\
\ All definitions end with $NEXT -- either directly (code definitions) or
\ indirectly (colon definitions terminating in EXIT, which is itself a code
\ definition). The action of $NEXT is to use the fpc for the next word to
\ fetch the xt and jumps to it.
\
\ To use the udebug routine, replace the $NEXT expansion with a jump (not a
\ call) to the routine udebug (this requires you to reassemble the code)
\
\ When you want to debug a word, trap at the CALL doLIST at the start of the
\ word and then load the location trapfpc with the address of the first xt
\ of the word. Make your debugger trap when you execute the final instruction
\ in the udebug routine. Now execute your code and your debugger will trap
\ after the completion of the first xt in the definition. To stop debugging,
\ simply set trapfpc to 0.
\
\ This technique has a number of limitations:
\ - It is an assumption that an xt of 0 is illegal
\ - You cannot automatically debug a code stream that includes inline string
\   definitions, or any other kind of inline literal. You must step into the
\   word that includes the definition then hand-edit the appropriate new value
\   into trapfpc
\ Clearly, you could overcome these limitations by making udebug more
\ complex -- but then you run the risk of introducing bugs in that code.

META-TODO [IF]
ALSO ASSEMBLER
\ udebug
		RAM0 R0 =,		\ where UZERO table will go to
		[ R0 ] R1 LDR,
		fpc R1 CMP,		\ compare the stored address with
					\ the address we're about to get the
					\ next xt from
		CELLL # [ fpc ] pc NE LDR, \ <> trap address, so we're done
		CELLL # FPC R1 ADD,	\ next time trap on the next xt
		[ R0 ] R1 STR,
		CELLL # [ fpc ] pc LDR,	\ make debugger TRAP at this address
PREVIOUS
[THEN]

\   W!		( c a -- )      "w store"
\		Store word (16-bit) value at word address.

		$SCODE W!
		R1 popD,			\ pop data
						\ only interested in bits15:0
		[ tos ] R1 STRH,		\ store word at address
		tos popD,
		$NEXT

\   W@		( a -- c )      "w fetch"
\		Fetch word value from word address.

		$SCODE W@
		[ tos ] R1 LDRH,		\ get the word
		R1 tos MOV,			\ bung back on the stack
		$NEXT

\ ===============================================================
CR .( *** StrongARM-specific words for getting at the internals of )
   .( the chip )

\   MMUPTAB	( -- a-addr )
\		Return the start address of the MMU page table

		MMU0 $SCONST MMUPTAB


1000 hCONSTANT conm
FFFFEFFF hCONSTANT coffm

\   CHIPID	( -- n )
\		Read chip ID

		$SCODE CHIPID
		tos pushD,
\ TODO		mrc     p15,0,tos,c0,c1,0
0 meta-asm,32
		$NEXT

\   ION		( -- )
\		Turn on Icache

		$SCODE ION
		conm R1 =,		\ mask Icache ON
\ TODO		mrc     p15,0,r2,c1,c1,0
0 meta-asm,32
		R1 R2 R2 ORR,		\ Icache ON
\ TODO		mcr     p15,0,r2,c1,c1,0
0 meta-asm,32
		$NEXT

\   IOFF	( -- )
\		Turn off Icache

		$SCODE IOFF
		coffm R1 =,		\ mask Icache OFF
\ TODO		mrc     p15,0,r2,c1,c1,0
0 meta-asm,32
		R1 R2 R2 AND,		\ Icache OFF
\ TODO		mcr     p15,0,r2,c1,c1,0
0 meta-asm,32
		$NEXT

\   IFLUSH	( -- )
\		Flush the Icache

		$SCODE IFLUSH
\ TODO		mcr     p15,0,r2,c7,c5,0
0 meta-asm,32
		$NEXT

\   DFLUSH	( -- )
\		Flush the Dcache by forcing data out to memory
\		This algorithm comes from the SA-110 spec. Could optimise
\		it to half the loopcount by selecting an unused
\		address range and adding an MCR.

		$SCODE DFLUSH
		0 # R0 MOV,
		8000 # R0 R1 ADD,	\ 32768 decimal entries
00 L.		20 # [ R0 ] R2 LDR,
		R0 R1 TEQ,
		00 L# NE B,
		$NEXT

\   TLBFLUSH	( -- )
\		Flush the I and D TLBs

		$SCODE TLBFLUSH
\ TODO		mcr     p15,0,r2,c8,c6,0
0 meta-asm,32
		$NEXT

\   MMUON	( -- )
\		Turn on the MMU, WB, Icache, Dcache

		$SCODE MMUON
		100D R1 =,
\ TODO		mrc     p15,0,r2,c1,c1,0
0 meta-asm,32
		R1 R2 R2 ORR,
\ TODO		mcr     p15,0,r2,c1,c1,0
0 meta-asm,32
		$NEXT

\   MMUOFF	( -- )
\		Turn off the MMU, WB, Icache, Dcache

		$SCODE MMUOFF
		EFF2 R1 =,
\ TODO		mrc     p15,0,r2,c1,c1,0
0 meta-asm,32
		R1 R2 R3 AND,
\ TODO		mcr     p15,0,r2,c1,c1,0
0 meta-asm,32
		$NEXT

\   WBDRAIN	( -- )
\		Drain the write buffer

		$SCODE WBDRAIN
\ TODO		mcr     p15,0,r2,c7,c10,4
0 meta-asm,32
		$NEXT

\   CKSWON	( -- )
\		clock switching ON

		$SCODE CKSWON
\ TODO		mcr     p15,0,r2,c15,c1,2
0 meta-asm,32
		$NEXT

\   CKSWOFF	( -- )
\		clock switching OFF

		$SCODE CKSWOFF
\ TODO		mcr     p15,0,r2,c15,c2,2
0 meta-asm,32
		$NEXT

\   WAI		( -- )
\		Wait for interrupt

		$SCODE WAI
\ TODO		mcr     p15,0,r2,c15,c8,2
0 meta-asm,32
		$NEXT

\   TTB!	( n -- )
\		Store n as the translation table base address

		$SCODE TTB!
\ TODO		mcr     p15,0,tos,c2,c1,0
0 meta-asm,32
		tos popD,
		$NEXT

\   DAC@	( -- n )
\		Read the domain access control register

		$SCODE DAC@
		tos pushD,
\ TODO		mrc     p15,0,tos,c3,c1,0
0 meta-asm,32
		$NEXT

\   DAC!	( n -- )
\		Store n in the domain access control register

		$SCODE DAC!
\ TODO		mcr     p15,0,tos,c3,c1,0
0 meta-asm,32
		tos popD,
		$NEXT

\   IDflushline	( a-addr -- )
\		Ensure cache coherency after generating a new op-code
\		at a-addr. For machines with unified caches this is a DROP but
\		for machines with separate I and D caches you must flush the
\		Dcache (in case the code word is in a dirty block there)
\		and then flush the Icache (in case it has a copy of the old
\		value at that location).
\		Currently, the only word that uses this is xt,

		$SCODE IDflushline
\                       p15,??, arm_reg, co-proc_register, CRm, OPC_2
\ TODO		mcr     p15,0,tos,c7,c10,1      ;clean Dcache entry 
\ 		mcr     p15,0,tos,c7,c6,1       ;flush Dcache entry 
\		mcr     p15,0,tos,c7,c10,4      ;drain write buffer
\		mcr     p15,0,tos,c7,c5,0       ;flush Icache
0 meta-asm,32
0 meta-asm,32
0 meta-asm,32
0 meta-asm,32
		tos popD,
		$NEXT

\   same?	( c-addr1 c-addr2 u -- -1|0|1 )
\		Return 0 if two strings, ca1 u and ca2 u, are same\ -1 if
\		string, ca1 u is smaller than ca2 u\ 1 otherwise. Used by
\		'(search-wordlist)'. Code definition is preferred to speed up
\		interpretation. Colon definition is shown below.
META-HI [IF]
   : same?      ?DUP IF         \ null strings are always same
                  0 DO OVER C@ OVER C@ XOR \ 0 (false) if match.. so continue
                       IF UNLOOP C@ SWAP C@ > 2* 1+ EXIT THEN
                       CHAR+ SWAP CHAR+ SWAP
                  LOOP
               THEN 2DROP 0 ;
[ELSE]
		$SCODE same?
		R0 popD,		\ c-addr2
		R1 popD,		\ c-addr1
		tos tos tos ORRS,
		01 L# EQ B,	\ null string always matches; exit with tos=0
02 L.		1 # [ R0 ] R2 LDRB,	\ char at c-addr2
		1 # [ R1 ] R3 LDRB,	\ char at c-addr1
		R3 R2 CMP,		\ flags for c2>c1
		01 L# NE B,		\ mismatch
		1 # tos tos SUBS,
		02 L# NE B,		\ match so far, more string to do
		\ match, tos=0
		$NEXT-EARLY		\ could simply fall through the movCOND
01 L.		1 # tos GT MOV,
		-1 tos LT =,
		$NEXT
[THEN]

\   (search-wordlist)   ( c-addr u wid -- 0 | xt f 1 | xt f -1)
\		Search word list for a match with the given name.
\		Return execution token and not-compile-only flag and
\		-1 or 1 ( IMMEDIATE) if found. Return 0 if not found.
\
\		format is: wid---->[   a    ]
\		                       |
\		                       V
\		[   xt'  ][   a'   ][ccbbaann][ggffeedd]...
\		              |
\		              +--------+
\		                       V
\		[   xt'' ][   a''  ][ccbbaann][ggffeedd]...
\
\		a, a' etc. point to the cell that contains the name of the
\		word. The length is in the low byte of the cell (little byte
\		for little-endian, big byte for big-endian).
\		Eventually, a''' contains 0 to indicate the end of the wordlist
\		(oldest entry). a=0 indicates an empty wordlist.
\		xt is the xt of the word. aabbccddeedd etc. is the name of
\		the word, packed into cells.
META-HI [IF]
: (search-wordlist)
		ROT >R SWAP DUP 0= IF -16 THROW THEN
				\ attempt to use zero-length string as a name
		>R		\ wid  R: ca1 u
		BEGIN @		\ ca2  R: ca1 u
		  DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
		  DUP COUNT [ =MASK ] LITERAL AND R@ = \ ca2 ca2+char f
		  IF R> R@ SWAP DUP >R           \ ca2 ca2+char ca1 u
		    same?                        \ ca2 flag
		\ ELSE DROP -1      \ unnecessary since ca2+char is not 0.
		  THEN
		WHILE cell-             \ pointer to next word in wordlist
		REPEAT
		R> R> 2DROP DUP name>xt SWAP            \ xt ca2
		C@ DUP [ =COMP ] LITERAL AND 0= SWAP
		[ =IMED ] LITERAL AND 0= 2* 1+ ;
[ELSE]
		$SCODE (search-wordlist)
		R0 popD,		\ u
		R1 popD,		\ c-addr
		R0 R0 R0 ORRS,
		10 # tos EQ MVN,	\ message -16 decimal
\ TODO fix up	THROW EQ B,		\ can't use 0-length string as a name
		R0 R1 R6 ADD,		\ point past reference string
02 L.		[ tos ] tos LDR,	\ link to next word
		tos tos tos ORRS,
		03 L# EQ B,		\ link of 0 indicates end of word list
		\ get length byte and point to next-word link
		CELLL NEGATE # [ tos ] R5 LDRB,
		=MASK # R5 R2 AND,	\ get length .. is it the same length
		R0 R2 CMP,		\ as the word we're looking for?
		02 L# NE B,		\ no. Try next word in the list
		5 # tos R2 ADD,		\ point to 1st byte of string in dict'y
		R1 R7 MOV,		\ take a copy of the start address
		\ now compare strings at r7, r2 until r7=r6
04 L.		1 # [ R7 ] R3 LDRB,
		1 # [ R2 ] R4 LDRB,
		R3 R4 R3 SUBS,
		02 L# NE B,  		\ mismatch. Try next word in the list
		R6 R7 R4 SUBS,
		04 L# NE B,		\ match so far, keep going
\ match!! and we tricked 0 into r3 and r4 for later use..
		[ CELLL NEGATE # tos ] R0 LDR,
		R0 pushD,		\ stack the xt
		=COMP # R5 TST,
		\ eq => !=COMP, want to push -1. ne => =COMP ; push 0
		1 # R3 R3 EQ SUB,	\ r3 started as 0
		R3 pushD,
		=IMED # R5 tos ANDS,
		\ eq => tos=0 => =IMED clear
		2 # tos NE MOV,
		1 # tos tos SUB,	\ 1=>SET, -1=>CLEAR
03 L.
		$NEXT
[THEN]

META-TODO [IF]
\   ?call	( xt1 -- xt1 0 | a-addr xt2 )
\		If xt1 starts with a machine CALL instruction then leave
\		xt of the CALLed routine and the address of the cell following
\		the machine call at xt1. Otherwise leave xt1 and 0. See
\		optiCOMPILE for an example of usage
\
\ 8086 version:
\   : ?call	DUP @ call-code =
\		IF   CELL+ DUP @ SWAP CELL+ DUP ROT + EXIT THEN
\		        \ Direct Threaded Code 8086 relative call
\		0 ;
\
\ ARM version: call is one cell and contains a signed 24-bit relative
\ offset. The offset is fixed up for pipeline prefetch:
\   : ?call	DUP @ 0ff000000h AND call-code =
\		\ that call-detection is crude - not an exact check..
\		IF DUP DUP @ 00ffffffh AND    \ it's a branch.. get offset
\		DUP 007fffffh > IF
\		              00ff000000h OR \ sign extend the offset
\		            THEN
\		2 LSHIFT                      \ convert to byte offset
\		+ CELL+ CELL+                 \ fix up for pipeline prefetch
\		SWAP CELL+ SWAP EXIT THEN
\		0 ;

		$COLON  5,'?call',QCall,_SLINK
		DW      DUPP,Fetch,DoLIT,0ff000000h,ANDD,DoLIT,CALLL,Equals
		DW      ZBranch,QCALL1
		DW      DUPP,DUPP,Fetch,DoLIT,0ffffffh,ANDD,DUPP
		DW      DoLIT,07fffffh,GreaterThan,ZBranch,QCALL2
		DW      DoLIT,0ff000000h,ORR
QCALL2          DW      DoLIT,2,LSHIFT,Plus,CELLPlus,CELLPlus
		DW      SWAP,CELLPlus,SWAP,EXIT
QCALL1          DW      Zero,EXIT

\   xt,		( xt1 -- xt2 )
\		Take a run-time word xt1 for :NONAME , CONSTANT , VARIABLE and
\		CREATE . Return xt2 of current definition.
\
\ 8086 version:
\   : xt,	xhere ALIGNED DUP TOxhere SWAP
\		call-code code,         \ Direct Threaded Code
\		xhere CELL+ - code, ;   \ 8086 relative call
\
\ ARM version: call is one cell and contains a signed 24-bit relative
\ offset. The offset must be fixed up for pipeline prefetch:
\   : xt,	xhere ALIGNED DUP TOxhere SWAP
\		xhere - CELL- CELL- 2 RSHIFT    \ get signed offset
\		00ffffffh AND                   \ mask off high-order sign bits
\		call_code OR                    \ make the opcode 
\		xhere swap                      \ remember where it will go
\		code, IDflushline ;             \ emit it and purge the block

		$COLON  3,'xt,',xtComma,_SLINK
		DW      XHere,ALIGNED,DUPP,TOXHere,SWAP
		DW      XHere,Minus,CellMinus,CellMinus,DoLIT,2,RSHIFT
		DW      DoLIT,0FFFFFFH,ANDD
		DW      DoLIT,CALLL,ORR,XHere,SWAP
		DW      CodeComma,IDflushline,EXIT
[THEN]

\   doLIT	( -- x )
\		Push an inline literal. The inline literal is at the current
\		value of the fpc, so put it onto the stack and point past it.

		$SCODE doLIT
		tos pushD,
		CELLL # [ fpc ] tos LDR, \ get literal and inc to next forth word
		$NEXT $COMPO

\   doCONST	( -- x )
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

		$SCODE doCONST
		tos pushD,
		[ R14 ] tos LDR,	\ inline address from calling point
		$NEXT $COMPO

\   doVALUE	( -- x )
\		Run-time routine of VALUE. Return the value of VALUE word.
\		This is like an invocation of doCONST for a VARIABLE but
\		instead of returning the address of the variable, we return
\		the value of the variable -- in other words, there is another
\		level of indirection.

		$SCODE doVALUE
		tos pushD,
		[ R14 ] R0 LDR,
		[ R0 ] tos LDR,
		$NEXT $COMPO

\   doCREATE	( -- a-addr )
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
\		  $COLON  COMPO+8,'doCREATE',DoCREATE,_SLINK
\		  DW      SWAP,DUPP,CELLPlus,Fetch,SWAP,Fetch,QuestionDUP
\		  DW      ZBranch,DOCREAT1
\		  DW      EXECUTE
\ DOCREAT1:       DW      EXIT

		$SCODE doCREATE
		tos pushD,
		CELLL # [ R14 ] R0 LDR,		\ 0 or DOES> address
		[ R14 ] tos LDR,		\ a-addr
		0 # R0 R0 ORRS,			\ set flags..
		R0 PC NE MOV,			\ a DOES> address.. go there
						\ (and never come back)
		$NEXT $COMPO			\ no DOES> actions

\   doTO        ( x -- )
\		Run-time routine of TO. Store x at the address in the
\		following cell. The inline literal holds the address
\		to be modified.

		$SCODE doTO
		\ get the address to be modified and point past
		CELLL # [ fpc ] R0 LDR,
		\ update to new value from tos
		[ R0 ] tos STR,
		tos popD,
		$NEXT $COMPO

\   doUSER      ( -- a-addr )
\		Run-time routine of USER. Return address of data space.
\		This is like doCONST but a variable offset is added to the
\		result. By changing the value at AddrUserP (which happens
\		on a taskswap) the whole set of user variables is switched
\		to the set for the new task.

		$SCODE doUSER
		tos pushD,
		[ R14 ] tos LDR,
		AddrUserP R0 =,
		[ R0 ] R1 LDR,
		R1 TOS TOS ADD,
		$NEXT $COMPO

\   doLIST      ( -- ) ( R: -- nest-sys )
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

		$SCODE doLIST
		fpc pushR,			\ preserve forth PC
		R14 fpc MOV,			\ first xt of definition
		$NEXT $COMPO

\   doLOOP      ( -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Run time routine for LOOP.

		$SCODE doLOOP
		R0 popR,			\ loop count
		1 # R0 R0 ADDS,
		01 L# VS B,			\ overflow -> end loop
		R0 pushR,			\ update loop count
		[ fpc ] fpc LDR,	\ get branch dest. to loop again
		$NEXT-EARLY
01 L.		CELLL # fpc fpc ADD,		\ ignore branch offset
		R0 popR,			\ NAC what's this dropping?? -- can just bump rsp rather than doing memory access
		$NEXT $COMPO

\   do+LOOP     ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Run time routine for +LOOP.

		$SCODE do+LOOP
		R0 popR,			\ loop count
		tos R0 R0 ADDS,
		tos popD,
		01 L# VS B,			\ overflow -> end loop
		R0 pushR,			\ update loop count
		[ fpc ] fpc LDR,		\ loop again
		$NEXT-EARLY
01 L.		CELLL # fpc fpc ADD,		\ ignore branch offset
		R0 popR,			\ NAC what's this dropping? -- can just bump rsp rather than doing memory access
		$NEXT $COMPO

\   0branch     ( flag -- )
\		Branch if flag is zero.

		$SCODE 0branch
		tos tos tos ORRS,		\ test top of stack
\ ARM's cool conditional-execution saves pipeline-draining branches 
		CELLL # FPC FPC NE ADD,		\ don't branch, point past dest
		[ fpc ] fpc EQ LDR,		\ branch; get destination
		tos popD,			\ tidy up the stack
		$NEXT $COMPO

\   branch      ( -- )
\		Branch to an inline address.

		$SCODE branch
		[ fpc ] fpc LDR,		\ get branch destination
		$NEXT $COMPO			\ .. and go there

\   rp@         ( -- a-addr )
\		Push the current RP to the data stack.

		$SCODE rp@
		tos pushD,
		rsp tos MOV,
		$NEXT $COMPO

\   rp!         ( a-addr -- )
\		Set the return stack pointer.

		$SCODE rp!
		tos rsp MOV,
		tos popD,
		$NEXT $COMPO

\   sp@         ( -- a-addr )
\		Push the current data stack pointer.

		$SCODE sp@
		tos pushD,
		dsp tos MOV,
		$NEXT $COMPO

\   sp!         ( a-addr -- )
\		Set the data stack pointer.

		$SCODE sp!
		tos dsp MOV,
		tos popD,
		$NEXT $COMPO

\   um+         ( u1 u2 -- u3 1|0 )
\		Add two unsigned numbers, return the sum and carry.

		$SCODE um+
		R1 popD,
		tos R1 R1 ADDS,			\ combine
		tos tos tos EOR,		\ clear register
		0 # tos tos ADC,		\ get carry flag
		R1 pushD,			\ store sum
		$NEXT

\   1chars/     ( n1 -- n2 )
\		Calculate number of chars for n1 address units.
META-HI [IF]
   : 1chars/    1 CHARS / ;     \ slow, very portable
\   : 1chars/   ;               \ fast, must be redefined for each system
[ELSE]
		$SCODE 1chars/
		$NEXT
[THEN]


CR .( *** Standard words - Processor-dependent definitions)
CR .( ***        32-bit Forth for ARM RISC)
xF-DEF

\   ALIGN	( -- )				\ CORE
\		Align the data space pointer.
\
META-HI [IF]
: ALIGN		hereVar DUP @ ALIGNED SWAP ! ;
[ELSE]
		0 CONSTANT LocHereVar \ TODO real definition
		$FCODE ALIGN
		LocHereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		3 # R2 R2 ADD,
		3 # R2 R2 BIC,			\ round it up
		[ R1 ] R2 STR,
		$NEXT
[THEN]


\   ALIGNED	( addr -- a-addr )		\ CORE
\		Align address to the cell boundary.
\
META-HI [IF]
: ALIGNED	DUP 0 CELLL UM/MOD DROP DUP
		IF CELLL SWAP - THEN + ;    \ slow, very portable
[ELSE]
		$FCODE ALIGNED
		3 # tos tos ADD,
		3 # tos tos BIC,		\ round it up
		$NEXT
[THEN]


\   CELLS	( n1 -- n2 )			\ CORE
\		Calculate number of address units for n1 cells.
\
META-HI [IF]
: CELLS     CELLL * ;   \ slow, very portable
[ELSE]
		$FCODE CELLS
		2 # LSL tos tos MOV,		\ multiply by 4
		$NEXT
[THEN]

\   CHARS	( n1 -- n2 )			\ CORE
\		Calculate number of address units for n1 characters.
\
META-HI [IF]
: CHARS CHARR * ;   \ slow, very portable
[ELSE]
		$FCODE CHARS	\ no-op for ARM
		$NEXT
[THEN]

\   !		( x a-addr -- )			\ CORE
\		Store x at a aligned address.
\
		$FCODE !
		R1 popD,			\ char to store
		[ tos ] R1 STR,
		tos popD,			\ clear up stack
		$NEXT

\   0<		( n -- flag )			\ CORE
\		Return true if n is negative.
\
		$FCODE 0<
		0 # R0 MOV,			\ get zeroes for dummy arg
\ TODO assembler bug - should recognise 32 = 0 for this instruction
						\ 20H = 32 decimal
		0 # ASR tos R0 tos ADD,	\ echo bit 32 value through r1
		$NEXT


\   0=		( x -- flag )			\ CORE
\		Return true if x is zero.
\
		$FCODE 0=
		0 # tos CMP,
		TRUEE tos EQ =,
		FALSEE tos NE =,
		$NEXT


\   2*		( x1 -- x2 )			\ CORE
\		Bit-shift left, filling the least significant bit with 0.
\
		$FCODE 2*
		1 # LSL tos tos MOV,
		$NEXT


\   2/		( x1 -- x2 )			\ CORE
\		Bit-shift right, leaving the most significant bit unchanged.
\
		$FCODE 2/
		1 # ASR tos tos MOV,
		$NEXT


\   >R		( x -- ) ( R: -- x )		\ CORE
\		Move top of the data stack item to the return stack.
\
		$FCODE >R
		tos pushR,
		tos popD,
		$NEXT $COMPO


\   @		( a-addr -- x )			\ CORE
\		Push the contents at a-addr to the data stack.
\
		$FCODE @
		[ tos ] tos LDR,
		$NEXT


\   AND		( x1 x2 -- x3 )			\ CORE
\		Bitwise AND.
\
		$FCODE AND
		R1 popD,
		R1 tos tos AND,
		$NEXT


\   C!		( char c-addr -- )		\ CORE
\		Store char at c-addr.
\
		$FCODE C!
		R1 popD,			\ char to store
		[ tos ] R1 STRB,
		tos popD,			\ clear up stack
		$NEXT


\   C@		( c-addr -- char )		\ CORE
\		Fetch the character stored at c-addr.
\
		$FCODE C@
		[ tos ] tos LDRB,
		$NEXT


\   DROP	( x -- )			\ CORE
\		Discard top stack item.
\
		$FCODE DROP
		tos popD,
		$NEXT


\   DUP		( x -- x x )			\ CORE
\		Duplicate the top stack item.
\
		$FCODE DUP
		tos pushD,
		$NEXT


\   EXECUTE	( i*x xt -- j*x )		\ CORE
\		Perform the semantics indentified by execution token, xt.
\
		$FCODE EXECUTE
		tos R1 MOV,
		tos popD,
		R1 PC MOV,			\ jump to the code address
		$END-CODE			\ tidy up


\   EXIT	( -- ) ( R: nest-sys -- )	\ CORE
\		Return control to the calling definition.
\
		$FCODE EXIT
		fpc popR,			\ where call doLIST left it
		$NEXT $COMPO


\   MOVE	( addr1 addr2 u -- )		\ CORE
\		Copy u address units from addr1 to addr2 if u is greater
\		than zero. This word is CODE defined since no other Standard
\		words can handle address unit directly.
\
		$FCODE MOVE
		R0 popD,			\ dest
		R1 popD,			\ source
		R1 R0 CMP,
		01 L# MI B,			\ copy from end backwards
02 L.		1 # [ R1 ] R2 LDRB,
		1 # [ R0 ] R2 STRB,
		1 # tos tos SUBS,
		02 L# NE B,
		tos popD,
		$NEXT-EARLY
01 L.		tos R0 R0 ADD,			\ point past the last bytes
		tos R1 R1 ADD,
03 L.		![ -1 # R1 ] R2 LDRB,		\ load with predecrement
		![ -1 # R0 ] R2 STRB,
		1 # tos tos SUBS,
		03 L# NE B,
		tos popD,
		$NEXT


\   OR		( x1 x2 -- x3 )			\ CORE
\		Return bitwise inclusive-or of x1 with x2.
\
		$FCODE OR
		R1 popD,
		R1 tos tos ORR,
		$NEXT

\   OVER	( x1 x2 -- x1 x2 x1 )		\ CORE
\		Copy second stack item to top of the stack.
\
		$FCODE OVER
		[ dsp ] R1 LDR,			\ copy 2nd stack item
		tos pushD,
		R1 tos MOV,
		$NEXT


\   R>		( -- x ) ( R: x -- )		\ CORE
\		Move x from the return stack to the data stack.
\
		$FCODE R>
		tos pushD,
		tos popR,
		$NEXT $COMPO


\   R@		( -- x ) ( R: x -- x )		\ CORE
\		Copy top of return stack to the data stack.
\
		$FCODE R@
		tos pushD,
		[ rsp ] tos LDR,
		$NEXT $COMPO


\   SWAP	( x1 x2 -- x2 x1 )		\ CORE
\		Exchange top two stack items.
\
		$FCODE SWAP
		R1 popD,
		tos pushD,
		R1 tos MOV,
		$NEXT


\   XOR         ( x1 x2 -- x3 )                 \ CORE
\		Bitwise exclusive OR.
\
		$FCODE XOR
		R1 popD,
		R1 tos tos EOR,
		$NEXT


CR .( *** Non-Standard words - Processor-dependent definitions)
CR .( ***        32-bit Forth for ARM RISC)
xS-DEF

\ pack" is dependent of cell alignment.
\
\   pack"       ( c-addr u a-addr -- a-addr2 )
\		Place a string c-addr u at a-addr and gives the next
\		cell-aligned address. Fill the rest of the last cell with
\		null character.
\
META-HI [IF]
: pack" 2DUP SWAP CHARS + CHAR+ DUP >R  \ ca u aa aa+u+1
	1 CHARS - 0 SWAP !              \ fill 0 at the end of string
	2DUP C! CHAR+ SWAP              \ c-addr a-addr+1 u
	CHARS MOVE R> ; COMPILE-ONLY
[ELSE]
		$SCODE pack"
		\ assume strings don't overlap
		R0 popD,			\ u
		R1 popD,			\ c-addr
		CHARR # [ tos ] R0 STRB,	\ store length
01 L.		CHARR # [ R1 ] R2 LDRB,
		CHARR # [ tos ] R2 STRB,
		CHARR # R0 R0 SUBS,
		01 L# NE B,
		\ next address is in tos. Need to align, filling any
		\ space with 0. r0 holds 0
		3 # tos R1 AND,			\ number of runt bytes
		4 # R1 R1 RSB,
		4 # R1 CMP,			\ EQ if aligned
02 L.		CHARR # [ tos ] R0 NE STRB,
		1 # R1 R1 NE SUBS,
		02 L# NE B,
\ drop through if no bytes to fill or at end
		$NEXT $COMPO
[THEN]


CR .( *** System constants and variables)

\   var0	( -- a-addr )
\		Start of system variable area.
\
RAM0 $SCONST var0

\   sysVar0	( -- a-addr )
\		Start of initial value table of system variables.
\
UZERO $SCONST sysVar0

\   sysVar0End	( -- a-addr )
\		End of initial value table of system variables.
\
ULAST $SCONST sysVar0End

\   'ekey?	( -- a-addr )
\		Execution vector of EKEY?.
\
$SVALUE 'ekey?

\   'ekey	( -- a-addr )
\		Execution vector of EKEY.
\
$SVALUE 'ekey

\   'emit?	( -- a-addr )
\		Execution vector of EMIT?.
\
$SVALUE 'emit?

\   'emit	( -- a-addr )
\		Execution vector of EMIT.
\
$SVALUE 'emit

\   'init-i/o	( -- a-addr )
\		Execution vector to initialize input/output devices.
\
$SVALUE 'init-i/o

\   'prompt	( -- a-addr )
\		Execution vector of '.prompt'.
\
$SVALUE 'prompt

\   'boot	( -- a-addr )
\		Execution vector of COLD.
\
$SVALUE 'boot

\   IOBYTES	( -- a-addr )
\		Identify i/o options
\
$SVALUE IOBYTES

\   SOURCE-ID	( -- 0 | -1 )			\ CORE EXT
\		Identify the input source. -1 for string (via EVALUATE) and
\		0 for user input device.
\
$FVALUE SOURCE-ID

\   cpVar	( -- a-addr )
\		Point to the top of the code dictionary.
\
$SVALUE cpVar

\   npVar	( -- a-addr )
\		Point to the bottom of the name dictionary.
\
$SVALUE npVar

\   hereVar	( -- a-addr )
\		Point to the RAM/ROM data space pointer. Used by , or ALLOT.
\
$SVALUE hereVar

\   'doWord	( -- a-addr )
\		Execution vectors for 'interpret'.
\
$SVAR 'doWord _VAR CELLL 5 * + TO _VAR

\   BASE	( -- a-addr )			\ CORE
\		Return the address of the radix base for numeric I/O.
\
$FVAR BASE

\   THROWMsgTbl ( -- a-addr )			\ CORE - TODO - is it really?
\		Return the address of the THROW message table.
\
AddrTHROWMsgTbl $SCONST THROWMsgTbl

\   ROMB	( -- a-addr )
\		Bottom of free ROM area.
\
$SVAR ROMB

\   ROMT	( -- a-addr )
\		Top of free ROM area.
\
$SVAR ROMT

\   RAMB        ( -- a-addr )
\               Bottom of free RAM area.
\
$SVAR RAMB

\   RAMT	( -- a-addr )
\		Top of free RAM area.
\
$SVAR RAMT

\   bal		( -- n )
\		Return the depth of control-flow stack.
\
$SVALUE bal

\   notNONAME?	( -- f )
\		Used by ';' whether to do 'linkLast' or not
\
$SVALUE notNONAME?

\   rakeVar	( -- a-addr )
\		Used by 'rake' to gather LEAVE.
\
$SVAR rakeVar

\   #order	( -- a-addr )
\		Hold the search order stack depth.
\
$SVAR #order
_VAR OrderDepth CELLL * + TO _VAR  \ search order stack

\   current	( -- a-addr )
\		Point to the wordlist to be extended.
\ NAC:		.. ie the compilation wordlist
\ NAC:		current @ is the wid of the compilation wordlist
\
$SVAR current

\   FORTH-WORDLIST   ( -- wid )			\ SEARCH
\		Return wid of Forth wordlist.
\
$FVAR FORTH-WORDLIST _VAR CELLL 2 * + TO _VAR

\   NONSTANDARD-WORDLIST   ( -- wid )
\		Return wid of non-standard wordlist.
\
$FVAR NONSTANDARD-WORDLIST

_VAR CELLL 2 * + TO _VAR
_VAR MaxWLISTS 2 - 3 * CELLL * + TO _VAR

\   envQList	( -- wid )
\		Return wid of ENVIRONMENT? string list. Never put this wid in
\		search-order. It should be used only by SET-CURRENT to add new
\		environment query string after addition of a complete wordset.
\
$SVAR envQList

\   userP	( -- a-addr )
\		Return address of USER variable area of current task.
\
$SVAR userP

	_VAR hCONSTANT SysTask
	_VAR CELLL + TO _VAR
	_VAR hCONSTANT SysUser1		\ user1
	_VAR CELLL + TO _VAR
	_VAR hCONSTANT SysTaskName	\ taskName
	_VAR CELLL + TO _VAR
	_VAR hCONSTANT SysThrowFrame	\ throwFrame
	_VAR CELLL + TO _VAR
	_VAR hCONSTANT SysStackTop	\ stackTop
	_VAR CELLL + TO _VAR
	_VAR hCONSTANT SysStatus	\ status
	_VAR CELLL + TO _VAR
	_VAR hCONSTANT SysUserP
	_VAR hCONSTANT SysFollower	\ follower
	_VAR CELLL + TO _VAR
	_VAR CELLL + TO _VAR		\ SP0 for system task
	_VAR CELLL + TO _VAR		\ RP0 for system task


\   trapfpc	( -- a-addr )
\		Return address of a variable holding trap address for
\		microdebugger
\
$SVAR trapfpc

\   SystemTask	( -- a-addr )
\		Return system task's tid.
\
SysTask $SCONST SystemTask

META-TODO [IF]
;;;NAC what's it *for* and what is it doing in the name dictionary?
;;;SystemTaskName  EQU     _NAME-0
		$GET_NAME "+0" SystemTaskName
[THEN]


\   follower	( -- a-addr )
\		Point next task's 'status' USER variable.
\
SysFollower SysUserP - $SUSER follower

\   status	( -- a-addr )
\		Status of current task. Point 'pass' or 'wake'.
\
SysStatus SysUserP - $SUSER status

\   stackTop	( -- a-addr )
\		Store current task's top of stack position.
\
SysStackTop SysUserP - $SUSER stackTop

\   throwFrame	( -- a-addr )
\		THROW frame for CATCH and THROW need to be saved for eack task.
\
SysThrowFrame SysUserP - $SUSER throwFrame

\   taskName	( -- a-addr )
\		Current task's task ID.
\
SysTaskName SysUserP - $SUSER taskName

\   user1	( -- a-addr )
\		One free USER variable for each task.
\
SysUser1 SysUserP - $SUSER user1



\ ENVIRONMENT? strings can be searched using SEARCH-WORDLIST and can be
\ EXECUTEd. This wordlist is completely hidden to Forth system except
\ ENVIRONMENT? .

$ENVIR" CPU" [ CPUStr ] LITERAL COUNT $NEXT
$ENVIR" model" [ ModelStr ] LITERAL COUNT $NEXT
$ENVIR" version" [ VersionStr ] LITERAL COUNT $NEXT
$ENVIR" /COUNTED-STRING" [ MaxString ] LITERAL $NEXT
$ENVIR" /HOLD" [ PADSize ] LITERAL $NEXT
$ENVIR" /PAD" [ PADSize ] LITERAL $NEXT
$ENVIR" ADDRESS-UNIT-BITS" 8 $NEXT
$ENVIR" CORE" [ TRUEE ] LITERAL $NEXT
$ENVIR" FLOORED" [ TRUEE ] LITERAL $NEXT
$ENVIR" MAX-CHAR" [ MaxChar ] LITERAL $NEXT \ max value of character set
$ENVIR" MAX-D" [ MaxUnsigned ] LITERAL [ MaxSigned ] LITERAL $NEXT
$ENVIR" MAX-N" [ MaxSigned ] $NEXT
$ENVIR" MAX-U" [ MaxUnsigned ] $NEXT
$ENVIR" MAX-UD" [ MaxUnsigned] LITERAL [ MaxUnsigned ] LITERAL $NEXT
$ENVIR" RETURN-STACK-CELLS" [ RTCells ] LITERAL $NEXT
$ENVIR" STACK-CELLS" [ DTCells ] LITERAL $NEXT
$ENVIR" EXCEPTION" [ TRUEE ] LITERAL $NEXT
$ENVIR" EXCEPTION-EXT" [ TRUEE ] LITERAL $NEXT
$ENVIR" WORDLISTS" [ OrderDepth ] LITERAL $NEXT

CR .( *** Non-Standard words - Colon definitions)
xS-DEF

\   (')		( "<spaces>name" -- xt 1 | xt -1 )
\		Parse a name, find it and return execution token and
\		-1 or 1 ( IMMEDIATE) if found
\
: (')		PARSE-WORD search-word ?DUP IF NIP EXIT THEN
		errWord 2!      \ if not found error
		-13 THROW ;     \ undefined word


\   (d.)	( d -- c-addr u )
\		Convert a double number to a string.
\
: (d.)		SWAP OVER  DUP 0< IF  DNEGATE  THEN
		<#  #S ROT SIGN  #> ;


\   .ok         ( -- )
\		Display 'ok'.
\
: .ok		S" ok" TYPE ;


\   .prompt	( -- )
\		Display Forth prompt. This word is vectored.
\
META-HI [IF]
: .prompt	'prompt EXECUTE ;
[ELSE]
		$SCODE .prompt
		AddrTprompt R0 =,
		[ R0 ] R1 LDR,
		R1 PC MOV,
		$END-CODE			\ tidy up
[THEN]

\   0		( -- 0 )
\		Return zero.
\
META-HI [IF]
		0 $SCONST 0
[ELSE]
		$SCODE 0
		tos pushD,
		0 # tos MOV,
		$NEXT
[THEN]

\   1		( -- 1 )
\		Return one.
\
META-HI [IF]
		1 $SCONST 1
[ELSE]
		$SCODE 1
		tos pushD,
		1 # tos MOV,
		$NEXT

[THEN]


\   -1		( -- -1 )
\		Return -1.
\
META-HI [IF]
		-1 $SCONST -1
[ELSE]
		$SCODE -1
		tos pushD,
		1 # tos MVN, \ TODO make assembler accept -1 & gen MVN
		$NEXT
[THEN]


\   abort"msg	( -- a-addr )
\		Abort" error message string address.
\
$SVAR abort"msg _VAR CELLL + TO _VAR


\   bal+        ( -- )
\		Increase bal by 1.
\
META-HI [IF]
: bal+		bal 1+ TO bal ;
[ELSE]
		$SCODE bal+
		AddrBal R0 =,
		[ R0 ] R1 LDR,
		1 # R1 R1 ADD,
		[ R0 ] R1 STR,
		$NEXT
[THEN]


\   bal-	( -- )
\		Decrease bal by 1.
\
META-HI [IF]
: bal-		bal 1- TO bal ;
[ELSE]
		$SCODE bal-
		AddrBal R0 =,
		[ R0 ] R1 LDR,
		1 # R1 R1 SUB,
		[ R0 ] R1 STR,
		$NEXT
[THEN]


\ TODO		LTORG


\   cell-	( a-addr1 -- a-addr2 )
\		Return previous aligned cell address.
\
\
META-HI [IF]
: cell-		-(cell-size) + ; \ TODO change to CELLL
[ELSE]
		$SCODE cell-
		CELLL # tos tos SUB,
		$NEXT           
[THEN]


\   COMPILE-ONLY   ( -- )
\		Make the most recent definition an compile-only word.
\
META-HI [IF]
: COMPILE-ONLY	lastName [ =COMP ] LITERAL OVER @ OR SWAP ! ;
[ELSE]
		$SCODE COMPILE-ONLY
		AddrNPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL 2 * # R2 R2 ADD,	\ Lastname
		[ R2 ] R3 LDR,		\ Get length
		=COMP # R3 R3 ORR,	\ Set flag
		[ R2 ] R3 STR,
		$NEXT
[THEN]


\ TODO		LTORG


\   doS"	( u -- c-addr u )
\		Run-time function of S" .
\
META-HI [IF]
: doS"		R> SWAP 2DUP + ALIGNED >R ; COMPILE-ONLY
[ELSE]
		$SCODE doS"
		\ in the colon case the 'next address' is on the
		\ return stack. In the code case it is still in fpc
		fpc pushD,		\ start of string
		tos fpc fpc ADD,	\ point to end of string
		3 # fpc fpc ADD,	\ and align
		3 # fpc fpc BIC,	\ by rounding it up
		$NEXT $COMPO
[THEN]


\   doDO	( n1|u1 n2|u2 -- ) ( R: -- n1 n2-n1-max_negative )
\		Run-time funtion of DO.
\
: doDO		>R MaxNegative + R> OVER - SWAP R> SWAP >R SWAP >R >R ;


\   errWord	( -- a-addr )
\		Last found word. To be used to display the word causing error.
\
$SVAR errWord	_VAR CELLL + TO _VAR


\   head,	( xt "<spaces>name" -- )
\		Parse a word and build a dictionary entry using xt and name.
\
: head,		PARSE-WORD DUP 0=
		IF errWord 2! -16 THROW THEN
		               \ attempt to use zero-length string as a name
		DUP =MASK > IF -19 THROW THEN   \ definition name too long
		2DUP GET-CURRENT SEARCH-WORDLIST  \ name exist?
		IF DROP ." redefine " 2DUP TYPE SPACE THEN \ warn if redefined
		npVar @ OVER CELL+ - ALIGNED
		DUP >R pack" DROP R>            \ pack the name in dictionary
		cell- GET-CURRENT @ OVER !      \ build wordlist link
		cell- DUP npVar !  ! ;          \ adjust name space pointer
						\ and store xt at code field


\   hld		( -- a-addr )
\		Hold a pointer in building a numeric output string.
\
$SVAR hld


\   interpret	( i*x -- j*x )
\		Interpret input string.
\
: interpret	BEGIN  DEPTH 0< IF -4 THROW THEN        \ stack underflow
		       PARSE-WORD DUP
		WHILE  2DUP errWord 2!
		       search-word          \ ca u 0 | xt f -1 | xt f 1
		       DUP IF
		         SWAP STATE @ OR 0= \ compile-only in interpretation
		         IF -14 THROW THEN  \ interpreting a compile-only word
		       THEN
		       1+ 2* STATE @ 1+ + CELLS 'doWord + @ EXECUTE
		REPEAT 2DROP ;


\   optiCOMPILE, ( xt -- )
\		Optimized COMPILE, . Reduce doLIST ... EXIT sequence if
\		xt is COLON definition which contains less than two words.
\
: optiCOMPILE,
		DUP ?call ['] doLIST = IF
		    DUP @ ['] EXIT = IF         \ if first word is EXIT
		      2DROP EXIT THEN
		    DUP CELL+ @ ['] EXIT = IF   \ if second word is EXIT
		      @ DUP ['] doLIT XOR  \ make sure it is not literal value
		      IF SWAP THEN THEN
		THEN THEN DROP COMPILE, ;


\   singleOnly	( c-addr u -- x )
\		Handle the word not found in the search-order. If the string
\		is legal, leave a single cell number in interpretation state.
\
: singleOnly
		0 DUP 2SWAP OVER C@ [CHAR] -
		= DUP >R IF 1 /STRING THEN
		>NUMBER IF -13 THROW THEN       \ undefined word
		2DROP R> IF NEGATE THEN ;

\ singleOnly, ( c-addr u -- )
\		Handle the word not found in the search-order. Compile a
\		single cell number in compilation state.
: singleOnly,
		singleOnly LITERAL ;


\ (doubleAlso)	( c-addr u -- x 1 | x x 2 )
\		If the string is legal, leave a single or double cell number
\		and size of the number.
\
: (doubleAlso)
		0 DUP 2SWAP OVER C@ [CHAR] -
		= DUP >R IF 1 /STRING THEN
		>NUMBER ?DUP
		IF   1- IF -13 THROW THEN     \ more than one char is remained
		     DUP C@ [CHAR] . XOR      \ last char is not '.'
		     IF -13 THROW THEN        \ undefined word
		     R> IF DNEGATE THEN
		     2 EXIT		THEN
		2DROP R> IF NEGATE THEN       \ single number
		1 ;


\ doubleAlso	( c-addr u -- x | x x )
\		Handle the word not found in the search-order. If the string
\		is legal, leave a single or double cell number in
\		interpretation state.
\
: doubleAlso	(doubleAlso) DROP ;


\ doubleAlso,	( c-addr u -- )
\		Handle the word not found in the search-order. If the string
\		is legal, compile a single or double cell number in
\		compilation state.
\
: doubleAlso,
		(doubleAlso) 1- IF SWAP LITERAL THEN LITERAL ;


\  -.		( -- )
\		You don't need this word unless you care that '-.' returns
\		double cell number 0. Catching illegal number '-.' in this way
\		is easier than make 'interpret' catch this exception.
\
: -.		-13 THROW ; IMMEDIATE   \ undefined word


\ lastName	( -- c-addr )
\		Return the address of the last definition name.
\
META-HI [IF]
: lastName	npVar @ CELL+ CELL+ ;
[ELSE]
		$SCODE lastName
		tos pushD,
		AddrNPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] tos LDR,
		CELLL 2 * # tos tos ADD,
		$NEXT
[THEN]


\   linkLast	( -- )
\		Link the word being defined to the current wordlist.
\		Do nothing if the last definition is made by :NONAME .
\
\ TODO: that comment about doing nothing.. is is really true? Looks like
\ the caller already checks, and only calls linklast if the defn is not
\ made with :NONAME.
META-HI [IF]
: linkLast	lastName GET-CURRENT ! ;
[ELSE]
		$SCODE linkLast
		AddrNPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL 2 * # R2 R2 ADD,	\ address of last definition name
		LocCurrent R0 =,
		[ R0 ] R1 LDR,		\ wid of compilation wordlist
		[ R1 ] R2 STR,		\ gets bumped up by new addition
		$NEXT
[THEN]


\   name>xt	( c-addr -- xt )
\		Return execution token using counted string at c-addr.
\
META-HI [IF]
: name>xt	cell- cell- @ ;
[ELSE]
		$SCODE name>xt
		CELLL 2 * # tos R0 SUB,		\ point back past link to xt
		[ R0 ] tos LDR,			\ get the xt
		$NEXT
[THEN]


\   PARSE-WORD  ( "<spaces>ccc<space>" -- c-addr u )
\		Skip leading spaces and parse a word. Return the name.
\
META-HI [IF]
\ TODO that won't compile and isn't right anyway..
: PARSE-WORD	BL skipPARSE ;
[ELSE]
		$SCODE PARSE-WORD
		tos pushD,
		SPC # tos MOV,
		xtSkipPARSE B,
		$END-CODE			\ tidy up
[THEN]


\   pipe        ( -- ) ( R: xt -- )
\		Connect most recently defined word to code following DOES>.
\		Structure of CREATEd word:
\		        | call-doCREATE | 0 or DOES> code addr | a-addr |
\
: pipe		lastName name>xt ?call DUP IF   \ code-addr xt2
		    ['] doCREATE = IF
		    R> SWAP !           \ change DOES> code of CREATEd word
		    EXIT
		THEN THEN
		-32 THROW       \ invalid name argument, no-CREATEd last name
		; COMPILE-ONLY


\   skipPARSE   ( char "<chars>ccc<char>" -- c-addr u )
\		Skip leading chars and parse a word using char as a
\		delimeter. Return the name.
\
: skipPARSE
		>R SOURCE >IN @ /STRING    \ c_addr u  R: char
		DUP IF
		   BEGIN  OVER C@ R@ =
		   WHILE  1- SWAP CHAR+ SWAP DUP 0=
		   UNTIL  R> DROP EXIT
		   ELSE THEN
		   DROP SOURCE DROP - 1chars/ >IN ! R> PARSE EXIT
		THEN R> DROP ;


\   rake        ( C: do-sys -- )
\		Gathers LEAVEs.
\
: rake		DUP code, rakeVar @
		BEGIN  2DUP U<
		WHILE  DUP @ xhere ROT !
		REPEAT rakeVar ! DROP
		?DUP IF		  \ check for ?DO
		   1 bal+ POSTPONE THEN \ orig type is 1
		THEN bal- ; COMPILE-ONLY


\   rp0         ( -- a-addr )
\		Pointer to bottom of the return stack.
\
META-HI [IF]
: rp0		userP @ CELL+ CELL+ @ ;
[ELSE]
		$SCODE rp0
		tos pushD,
		AddrUserP R0 =,
		[ R0 ] R1 LDR,
		CELLL 2 * # R1 R1 ADD,
		[ R1 ] tos LDR,
		$NEXT
[THEN]


\   search-word ( c-addr u -- c-addr u 0 | xt f 1 | xt f -1)
\		Search dictionary for a match with the given name. Return
\		execution token, not-compile-only flag and -1 or 1
\		( IMMEDIATE) if found; c-addr u 0 if not.
\
: search-word
		#order @ DUP		     \ not found if #order is 0
		IF 0
		   DO 2DUP		       \ ca u ca u
		      I CELLS #order CELL+ + @  \ ca u ca u wid
		      (search-wordlist)         \ ca u; 0 | w f 1 | w f -1
		      ?DUP IF		    \ ca u; 0 | w f 1 | w f -1
		         >R 2SWAP 2DROP R> UNLOOP EXIT \ xt f 1 | xt f -1
		      THEN		       \ ca u
		   LOOP 0		        \ ca u 0
		THEN ;


\   sourceVar	( -- a-addr )
\		Hold the current count and addr of the terminal input buffer.
\
		$SVAR sourceVar _VAR CELLL + TO _VAR


\   sp0		( -- a-addr )
\		Pointer to bottom of the data stack.
\
META-HI [IF]
: sp0		userP @ CELL+ @ ;
[ELSE]
		$SCODE sp0
		tos pushD,
		AddrUserP R0 =,
		[ R0 ] R1 LDR,
		CELLL # R1 R1 ADD,
		[ R1 ] tos LDR,
		$NEXT
[THEN]


\   TOxhere	( a-addr -- )
\		Set the next available code space address as a-addr.
\
META-HI [IF]
: TOxhere	cpVar ! ;
[ELSE]
		$SCODE TOxhere
		LocCPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] tos STR,
		tos popD,
		$NEXT
[THEN]


\   xhere	( -- a-addr )
\		Return next available code space address.
\
META-HI [IF]
: xhere		cpVar @ ;
[ELSE]
		$SCODE xhere
		tos pushD,
		LocCPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] tos LDR,
		$NEXT
[THEN]


\   code,	( x -- )
\		Reserve one cell in code space and store x in it.
\
META-HI [IF]
: code,		xhere DUP CELL+ TOxhere ! ;
[ELSE]
		$SCODE code,
		LocCPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL # [ R2 ] tos STR,
		tos popD,
		[ R1 ] R2 STR,
		$NEXT


CR .( *** Words for multitasking)
xS-DEF

\   PAUSE       ( -- )
\		Stop current task and transfer control to the task of which
\		'status' USER variable is stored in 'follower' USER variable
\		of current task.
\
: PAUSE rp@ sp@ stackTop !  follower @ >R ; COMPILE-ONLY


\   wake        ( -- )
\		Wake current task.
\
: wake		R> userP !      \ userP points 'follower' of current task
		stackTop @ sp!          \ set data stack
		rp! ; COMPILE-ONLY      \ set return stack


CR .( *** Essential Standard words - Colon definitions)
xF-DEF

\   #           ( ud1 -- ud2 )		   \ CORE
\		Extract one digit from ud1 and append the digit to
\		pictured numeric output string. ( ud2 = ud1 / BASE )
\
: #		0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP
		9 OVER < [ CHAR A CHAR 9 1 + - ] LITERAL AND +
		[ CHAR 0 ] LITERAL + HOLD R> ;


\   #>          ( xd -- c-addr u )              \ CORE
\		Prepare the output string to be TYPE'd.
\		||xhere>WORD/#-work-area|
\
: #>		2DROP hld @ xhere size-of-PAD + OVER - 1chars/ ;


\   #S          ( ud -- 0 0 )		    \ CORE
\		Convert ud until all digits are added to the output string.
\
: #S		BEGIN # 2DUP OR 0= UNTIL ;


\   '           ( "<spaces>name" -- xt )        \ CORE
\		Parse a name, find it and return xt.
\
: '		(') DROP ;


\   +           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
\		Add top two items and gives the sum.
\
META-HI [IF]
: +		um+ DROP ;
[ELSE]
		$FCODE +
		R1 popD,
		R1 tos tos ADD,
		$NEXT
[THEN]


\   +!          ( n|u a-addr -- )		\ CORE
\		Add n|u to the contents at a-addr.
\
META-HI [IF]
: +!		SWAP OVER @ + SWAP ! ;
[ELSE]
		$FCODE +!
		R0 popD,
		[ tos ] R1 LDR,
		R0 R1 R1 ADD,
		[ tos ] R1 STR,
		tos popD,
		$NEXT
[THEN]


\   ,           ( x -- )		         \ CORE
\		Reserve one cell in RAM or ROM data space and store x in it.
\
META-HI [IF]
: ,		HERE ! CELLL hereVar +! ;
[ELSE]
		$FCODE ,
		LocHereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		CELLL # [ R2 ] tos STR,
		tos popD,
		[ R1 ] R2 STR,
		$NEXT
[THEN]


\   -           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
\		Subtract n2|u2 from n1|u1, giving the difference n3|u3.
\
META-HI [IF]
: -		NEGATE + ;
[ELSE]
		$FCODE -
		R1 popD,
		tos R1 tos SUB,
		$NEXT
[THEN]


\   .           ( n -- )		         \ CORE
\		Display a signed number followed by a space.
\
META-HI [IF]
: .		S>D D. ;
[ELSE]
		$FCODE .
		tos pushD,
		0 # R1 MOV,
		\ sign-extend tos into tos to form ms of double
		0 # ASR tos R1 tos ADD, \ asm TODO - should accept 20 (32dec)
\ TODO fix up		DDot B,
		$END-CODE			\ tidy up
[THEN]


\   /           ( n1 n2 -- n3 )		  \ CORE
\		Divide n1 by n2, giving single-cell quotient n3.
\
: /		/MOD NIP ;


\   /MOD        ( n1 n2 -- n3 n4 )              \ CORE
\		Divide n1 by n2, giving single-cell remainder n3 and
\		single-cell quotient n4.
\
: /MOD		>R S>D R> FM/MOD ;


\   /STRING     ( c-addr1 u1 n -- c-addr2 u2 )  \ STRING
\		Adjust the char string at c-addr1 by n chars.
\
META-HI [IF]
: /STRING	DUP >R - SWAP R> CHARS + SWAP ;
[ELSE]
		$FCODE /STRING
		R0 popD,		\ u1 (tos is n)
		R1 popD,		\ c-addr1
		tos R1 R1 ADD,
		R1 pushD,		\ c-addr2 <= c-addr1 + n
		tos R0 tos SUB,		\ new count u2 <= u1 - n
		$NEXT
[THEN]


\   1+          ( n1|u1 -- n2|u2 )              \ CORE
\		Increase top of the stack item by 1.
\
META-HI [IF]
: 1+		1 + ;
[ELSE]
		$FCODE 1+
		1 # tos tos ADD,
		$NEXT
[THEN]


\   1-		( n1|u1 -- n2|u2 )              \ CORE
\		Decrease top of the stack item by 1.
\
META-HI [IF]
: 1-		-1 + ;
[ELSE]
		$FCODE 1-
		1 # tos tos SUB,
		$NEXT
[THEN]


\   2!		( x1 x2 a-addr -- )		\ CORE
\		Store the cell pare x1 x2 at a-addr, with x2 at a-addr and
\		x1 at the next consecutive cell.
\
META-HI [IF]
: 2!		SWAP OVER ! CELL+ ! ;
[ELSE]
		$FCODE 2!
		R0 popD,
		R1 popD,
		CELLL # [ tos ] R0 STR,
		[ tos ] R1 STR,
		tos popD,
		$NEXT
[THEN]


\   2@		( a-addr -- x1 x2 )		\ CORE
\		Fetch the cell pair stored at a-addr. x2 is stored at a-addr
\		and x1 at the next consecutive cell.
\
META-HI [IF]
: 2@		DUP CELL+ @ SWAP @ ;
[ELSE]
		$FCODE 2@
		4 # [ tos ] R0 LDR,	\ x2
		[ tos ] R1 LDR,
		R1 pushD,
		R0 tos MOV,
		$NEXT
[THEN]


\   2DROP       ( x1 x2 -- )			\ CORE
\		Drop cell pair x1 x2 from the stack.
\
META-HI [IF]
: 2DROP		DROP DROP ;
[ELSE]
		$FCODE 2DROP
		tos popD,			\ TODO could do this in 1 op..
		tos popD,
		$NEXT
[THEN]


\   2DUP	( x1 x2 -- x1 x2 x1 x2 )	\ CORE
\		Duplicate cell pair x1 x2.
\
META-HI [IF]
: 2DUP		OVER OVER ;
[ELSE]
		$FCODE 2DUP
		R0 popD,			\ TODO could save 1 op here..
		R0 pushD,
		tos pushD,
		R0 pushD,
		$NEXT
[THEN]


\   2SWAP       ( x1 x2 x3 x4 -- x3 x4 x1 x2 )  \ CORE
\		Exchange the top two cell pairs.
\
META-HI [IF]
: 2SWAP		ROT >R ROT R> ;
[ELSE]
		$FCODE 2SWAP
		R0 popD, 		\ x3
		R1 popD,		\ x2
		R2 popD,		\ x1
		R0 pushD,
		tos pushD,
		R2 pushD,
		R1 tos MOV,
		$NEXT
[THEN]

\   :           ( "<spaces>name" -- colon-sys ) \ CORE
\		Start a new colon definition using next word as its name.
\
: :		:NONAME ROT head,  -1 TO notNONAME? ;


\   :NONAME     ( -- xt colon-sys )             \ CORE EXT
\		Create an execution token xt, enter compilation state and
\		start the current definition.
\
: :NONAME	bal IF -29 THROW THEN           \ compiler nesting
		['] doLIST xt, DUP -1
		0 TO notNONAME?  1 TO bal  ] ;

\ TODO - the above compiles OK except it barfs on "]" which it ought to
\ understand. Perhaps the defn of : above is messing it up?
\ also, can't expect it to work because the IF isn't ported yet


\   ;           ( colon-sys -- )		 \ CORE
\		Terminate a colon definition.
\
: ;		bal 1- IF -22 THROW THEN        \ control structure mismatch
		NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
		notNONAME? IF   \ if the last definition is not created by ':'
		  linkLast  0 TO notNONAME?     \ link the word to wordlist
		THEN  POSTPONE EXIT     \ add EXIT at the end of the definition
		0 TO bal POSTPONE [ ; COMPILE-ONLY IMMEDIATE


\   <           ( n1 n2 -- flag )		\ CORE
\		Returns true if n1 is less than n2.
\
: <		2DUP XOR 0<             \ same sign?
		IF DROP 0< EXIT THEN    \ different signs, true if n1 <0
		- 0< ;			\ same signs, true if n1-n2 <0


\   <#          ( -- )		           \ CORE
\		Initiate the numeric output conversion process.
\		||xhere>WORD/#-work-area|
\
META-HI [IF]
: <#		xhere size-of-PAD + hld ! ;
[ELSE]
		$FCODE <#
		LocCPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,		\ xhere address
		PADSize CHARR * # R2 R2 ADD,
		LocHLD R3 =,
		[ R3 ] R2 STR,
		$NEXT
[THEN]


\   =           ( x1 x2 -- flag )		\ CORE
\		Return true if top two are equal.
\
META-HI [IF]
: =		XORR 0= ;
[ELSE]
		$FCODE =
		R0 popD,
		R0 tos tos SUBS,		\ equal =>tos will be 0 (FALSE)
		TRUEE tos EQ =,
		FALSEE tos NE =,
		$NEXT
[THEN]

\   >           ( n1 n2 -- flag )		\ CORE
\		Returns true if n1 is greater than n2.
\
: >		SWAP < ;


\   >IN		( -- a-addr )			\ CORE
\		Hold the character pointer while parsing input stream.
\
$FVAR >IN


\   >NUMBER     ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )    \ CORE
\		Add number string's value to ud1. Leaves string of any
\		unconverted chars.
\
: >NUMBER	BEGIN  DUP
		WHILE  >R  DUP >R C@		     \ ud char  R: u c-addr
		       DUP [ CHAR 9 1+ ] LITERAL [CHAR] A WITHIN
		           IF DROP R> R> EXIT THEN
		       [ CHAR 0 ] LITERAL - 9 OVER <
		       [ CHAR A CHAR 9 1 + - ] LITERAL AND -
		       DUP 0 BASE @ WITHIN
		WHILE  SWAP BASE @ UM* DROP ROT BASE @ UM* D+ R> R> 1 /STRING
		REPEAT DROP R> R>
		THEN ;


\   ?DUP        ( x -- x x | 0 )		 \ CORE
\		Duplicate top of the stack if it is not zero.
\
META-HI [IF]
: ?DUP		DUP IF DUP THEN ;
[ELSE]
		$FCODE ?DUP
		0 # tos tos ORRS,		\ set flags
		![ CELLL NEGATE # dsp ] tos NE STR,	\ pushD if ne
		$NEXT
[THEN]


\   ABORT       ( i*x -- ) ( R: j*x -- )        \ EXCEPTION EXT
\		Reset data stack and jump to QUIT.
\
META-HI [IF]
: ABORT		-1 THROW ;
[ELSE]
		$FCODE ABORT
		tos pushD,	\ TODO gonna trash the stack so no need..
		1 # tos MVN,
		xtTHROW B,
		$END-CODE			\ tidy up
[THEN]


\ NAC mods for handshaking: FLOW-ON at start, FLOW-OFF at end and use EMITE for
\ all output in between.
\   ACCEPT      ( c-addr +n1 -- +n2 )           \ CORE
\		Accept a string of up to +n1 chars. Return with actual count.
\		Implementation-defined editing. Stops at EOL# .
\		Supports backspace and delete editing.
\
: ACCEPT    FLOW-ON >R 0
		BEGIN  DUP R@ <		  \ ca n2 f  R: n1
		WHILE  EKEY max-char AND
		       DUP BL <
		       IF DUP  cr# = IF ROT 2DROP R> DROP FLOW-OFF EXIT THEN
		            DUP  tab# =
		            IF   DROP 2DUP + BL DUP EMITE SWAP C! 1+
		            ELSE DUP  bsp# =
				  SWAP del# = OR
				  IF DROP DUP
			            \ discard the last char if not 1st char
				  IF 1- bsp# EMITE BL EMITE bsp# EMITE THEN THEN
		            THEN
		       ELSE >R 2DUP CHARS + R> DUP EMITE SWAP C! 1+  THEN
		       THEN
		REPEAT SWAP R> 2DROP FLOW-OFF ;


\   AGAIN       ( C: dest -- )		   \ CORE EXT
\		Resolve backward reference dest. Typically used as
\		BEGIN ... AGAIN . Move control to the location specified by
\		dest on execution.
\
: AGAIN		IF -22 THROW THEN  \ control structure mismatch; dest type is 0
		POSTPONE branch code, bal- ; COMPILE-ONLY IMMEDIATE


\   AHEAD       ( C: -- orig )		   \ TOOLS EXT
\		Put the location of a new unresolved forward reference onto
\		control-flow stack.
\
: AHEAD		POSTPONE branch xhere 0 code,
		1 bal+          \ orig type is 1
		; COMPILE-ONLY IMMEDIATE


\   BL		( -- char )			\ CORE
\		Return the value of the blank character.
\
\   : BL        blank-char-value EXIT ;

		SPC $FCONST BL


\   CATCH       ( i*x xt -- j*x 0 | i*x n )     \ EXCEPTION
\		Push an exception frame on the exception stack and then execute
\		the execution token xt in such a way that control can be
\		transferred to a point just after CATCH if THROW is executed
\		during the execution of xt.
\
: CATCH		sp@ >R throwFrame @ >R          \ save error frame
		rp@ throwFrame !  EXECUTE       \ execute
		R> throwFrame !		  \ restore error frame
		R> DROP  0 ;		     \ no error


\   CELL+       ( a-addr1 -- a-addr2 )          \ CORE
\		Return next aligned cell address.
\
META-HI [IF]
: CELL+		CELLL + ;
[ELSE]
		$FCODE CELL+
		CELLL # tos tos ADD,
		$NEXT
[THEN]


\   CHAR+       ( c-addr1 -- c-addr2 )          \ CORE
\		Returns next character-aligned address.
\
META-HI [IF]
: CHAR+		char-size + ;
[ELSE]
		$FCODE CHAR+
		CHARR # tos tos ADD,
		$NEXT
[THEN]


\   COMPILE,    ( xt -- )		        \ CORE EXT
\		Compile the execution token on data stack into current
\		colon definition.
\
META-HI [IF]
: COMPILE,	code, ; COMPILE-ONLY
[ELSE]
		$FCODE COMPILE,
		CodeComma B,
		$END-CODE $COMPO
[THEN]


\   CONSTANT    ( x "<spaces>name" -- )         \ CORE
\		name Execution: ( -- x )
\		Create a definition for name which pushes x on the stack on
\		execution.
\
: CONSTANT	bal IF -29 THROW THEN           \ compiler nesting
		['] doCONST xt, head, code, linkLast ;


\   COUNT       ( c-addr1 -- c-addr2 u )        \ CORE
\		Convert counted string to string specification. c-addr2 is
\		the next char-aligned address after c-addr1 and u is the
\		contents at c-addr1.
\
META-HI [IF]
: COUNT		DUP CHAR+ SWAP C@ ;
[ELSE]
		$FCODE COUNT
		[ tos ] R0 LDRB,		\ u
		1 # tos tos ADD,
		tos pushD,
		R0 tos MOV,
		$NEXT
[THEN]


\   CREATE      ( "<spaces>name" -- )           \ CORE
\		name Execution: ( -- a-addr )
\		Create a data object in RAM/ROM data space, which return
\		data object address on execution
\
: CREATE	bal IF -29 THROW THEN           \ compiler nesting
		['] doCREATE xt, head,
		xhere DUP CELL+ CELL+ TOxhere   \ reserve two cells
		0 OVER !		 \ no DOES> code yet
		ALIGN HERE SWAP CELL+ ! \ >BODY returns this address
		linkLast ;              \ link CREATEd word to current wordlist


\   D+          ( d1|ud1 d2|ud2 -- d3|ud3 )	\ DOUBLE
\		Add double-cell numbers.
META-HI [IF]
: D+		>R SWAP >R um+ R> R> + + ;
[ELSE]
		$FCODE D+
		R0 popD,			\ d2L (tos is d2H)
		R1 popD,			\ d1H
		R2 popD,			\ d1L
		R2 R0 R3 ADDS,
		R3 pushD,
		R1 tos tos ADC,
		$NEXT
[THEN]


\   D.          ( d -- )		         \ DOUBLE
\		Display d in free field format followed by a space.
\
: D.		(d.) TYPE SPACE ;


\   DECIMAL     ( -- )		           \ CORE
\		Set the numeric conversion radix to decimal 10.
\
META-HI [IF]
: DECIMAL	0A BASE ! ;
[ELSE]
		$FCODE DECIMAL
		LocBASE R0 =,
		0A # R1 MOV,
		[ R0 ] R1 STR,
		$NEXT
[THEN]


\   DEPTH       ( -- +n )		        \ CORE
\		Return the depth of the data stack.
\
: DEPTH		sp@ sp0 SWAP - CELLL / ;


\   DNEGATE     ( d1 -- d2 )			\ DOUBLE
\		Two's complement of double-cell number.
\
META-HI [IF]
: DNEGATE	INVERT >R INVERT 1 um+ R> + ;
[ELSE]
		$FCODE DNEGATE
		R0 popD,
		tos tos MVN,			\ 1's complement of high part
		R0 R0 MVN,			\ 1's complement of lo part
		1 # R0 R0 ADDS,
		0 # tos tos ADC,
		R0 pushD,
		$NEXT
[THEN]


\ EKEY		( -- u )		         \ FACILITY EXT
\		Receive one keyboard event u.
\
: EKEY		BEGIN PAUSE EKEY? UNTIL 'ekey EXECUTE ;


\   EMIT        ( x -- )				\ CORE
\		Send a character to the output device.
\
META-HI [IF]
: EMIT		'emit EXECUTE ;
[ELSE]
		$FCODE EMIT
01 G.		AddrTEMIT R0 =,
		[ R0 ] R1 LDR,
		R1 PC MOV,
		$END-CODE
[THEN]


\ NAC added for file handshake. TODO should it be slink instead? -- yes
\   EMITE       ( x -- )
\		Send a character to the output device unless FILE bit
\		is set in IOBYTES
META-HI [IF]
: EMITE		IOBYTES 10000000 AND 
		IF 'emit EXECUTE THEN DROP ;
\ TODO test the high-level definition
[ELSE]
		$FCODE EMITE
		AddrIOBYTES R0 =,		\ find out whether to emit it
		[ R0 ] R1 LDR,
		10000000 # R1 R1 ANDS,
		01 G# EQ B,			\ yes, do it
		tos popD,
		$NEXT
[THEN]


\ FM/MOD	( d n1 -- n2 n3 )		\ CORE
\		Signed floored divide of double by single. Return mod n2
\		and quotient n3.
\
: FM/MOD	DUP >R 2DUP XOR >R >R DUP 0< IF DNEGATE THEN
		R@ ABS UM/MOD
		R> 0< IF SWAP NEGATE SWAP THEN
		R> 0< IF NEGATE         \ negative quotient
		    OVER IF R@ ROT - SWAP 1- THEN
		    R> DROP
		    0 OVER < IF -11 THROW THEN          \ result out of range
		    EXIT		         THEN
		R> DROP  DUP 0< IF -11 THROW THEN ;     \ result out of range


\   GET-CURRENT   ( -- wid )		     \ SEARCH
\		Return the indentifier of the compilation wordlist.
\
: GET-CURRENT	current @ ;


\   HERE	( -- addr )			\ CORE
\		Return data space pointer.
\
META-HI [IF]
: HERE		hereVar @ ;
[ELSE]
		$FCODE HERE
		tos pushD,
		LocHereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] tos LDR,			\ get its value
		$NEXT
[THEN]


\   HOLD	( char -- )			\ CORE
\		Add char to the beginning of pictured numeric output string.
\
META-HI [IF]
: HOLD		hld @  1 CHARS - DUP hld ! C! ;
[ELSE]
		$FCODE HOLD
		LocHLD R0 =,
		[ R0 ] R1 LDR,
		CHARR # R1 R1 SUB,
		[ R1 ] tos STRB,
		[ R0 ] R1 STR,
		tos popD,
		$NEXT
[THEN]

\   I		( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
\		Push the innermost loop index.
\
META-HI [IF]
: I		rp@ [ 1 CELLS ] LITERAL + @
		rp@ [ 2 CELLS ] LITERAL + @  +  \ COMPILE-ONLY
[ELSE]
		$FCODE I
		tos pushD,
		[ rsp ] R0 LDR,		\ note that we're one CELLL closer to
					\ the tos in CODE
		[ CELLL # rsp ] tos LDR,
		R0 tos tos ADD,
		$NEXT $COMPO
[THEN]


\   IF          Compilation: ( C: -- orig )             \ CORE
\		Run-time: ( x -- )
\		Put the location of a new unresolved forward reference orig
\		onto the control flow stack. On execution jump to location
\		specified by the resolution of orig if x is zero.
\
: IF		POSTPONE 0branch xhere 0 code,
		1 bal+          \ orig type is 1
		; COMPILE-ONLY IMMEDIATE


\   INVERT      ( x1 -- x2 )		     \ CORE
\		Return one's complement of x1.
\
META-HI [IF]
: INVERT	-1 XOR ;
[ELSE]
		$FCODE INVERT
		tos tos MVN,
		$NEXT
[THEN]


\   KEY         ( -- char )		      \ CORE
\		Receive a character. Do not display char.
\
: KEY		EKEY max-char AND ;


\   LITERAL     Compilation: ( x -- )           \ CORE
\		Run-time: ( -- x )
\		Append following run-time semantics. Put x on the stack on
\		execution
\
: LITERAL	POSTPONE doLIT code, ; COMPILE-ONLY IMMEDIATE


\   NEGATE	( n1 -- n2 )		\ CORE
\		Return two's complement of n1.
\
META-HI [IF]
: NEGATE	INVERT 1+ ;
[ELSE]
		$FCODE NEGATE
		tos tos MVN,
		1 # tos tos ADD,
		$NEXT
[THEN]


\ NIP		( n1 n2 -- n2 )		\ CORE EXT
\		Discard the second stack item.
\
META-HI [IF]
: NIP		SWAP DROP ;
[ELSE]
		$FCODE NIP
		R0 popD,
		$NEXT
[THEN]


\ TODO		LTORG

\   PARSE       ( char "ccc<char>"-- c-addr u )         \ CORE EXT
\		Scan input stream and return counted string delimited by char.
\
: PARSE		>R  SOURCE >IN @ /STRING        \ c-addr u  R: char
		DUP IF
		   OVER CHARS + OVER       \ c-addr c-addr+u c-addr  R: char
		   BEGIN  DUP C@ R@ XOR
		   WHILE  CHAR+ 2DUP =
		   UNTIL  DROP OVER - 1chars/ DUP
		   ELSE   NIP  OVER - 1chars/ DUP CHAR+
		   THEN   >IN +!
		THEN   R> DROP EXIT ;


\ TODO - 58 should be expressed as 'numthrowmsgs' and need to be in DECIMAL
\ to just express it as a raw number
\   QUIT        ( -- ) ( R: i*x -- )            \ CORE
\		Empty the return stack, store zero in SOURCE-ID, make the user
\		input device the input source, and start text interpreter.
\
: QUIT		BEGIN
		  rp0 rp!  0 TO SOURCE-ID  0 TO bal  POSTPONE [
		  BEGIN CR REFILL DROP SPACE    \ REFILL returns always true
		        ['] interpret CATCH ?DUP 0=
		  WHILE STATE @ 0= IF .prompt THEN
		  REPEAT
		  DUP -1 XOR IF				   \ ABORT
		  DUP -2 = IF SPACE abort"msg 2@ TYPE    ELSE   \ ABORT"
		  SPACE errWord 2@ TYPE
		  SPACE [CHAR] ? EMIT SPACE
		  DUP -1 -58 WITHIN IF ." Exception # " . ELSE \ undefined exception
		  CELLS THROWMsgTbl + @ COUNT TYPE       THEN THEN THEN
		  sp0 sp!
		AGAIN ;


\   REFILL      ( -- flag )		      \ CORE EXT
\		Attempt to fill the input buffer from the input source. Make
\		the result the input buffer, set >IN to zero, and return true
\		if successful. Return false if the input source is a string
\		from EVALUATE.
\
: REFILL	SOURCE-ID IF 0 EXIT THEN
		npVar @ [ size-of-PAD CHARS 2* ] LITERAL - DUP
		size-of-PAD ACCEPT sourceVar 2!
		0 >IN ! -1 ;


\   ROT         ( x1 x2 x3 -- x2 x3 x1 )        \ CORE
\		Rotate the top three data stack items.
META-HI [IF]
: ROT		>R SWAP R> SWAP ;
[ELSE]
		$FCODE ROT
		R0 popD,			\ x2 (tos=x3)
		R1 popD,			\ x1
		R0 pushD,
		tos pushD,
		R1 tos MOV,
		$NEXT
[THEN]

\   S>D         ( n -- d )		       \ CORE
\		Convert a single-cell number n to double-cell number.
\
META-HI [IF]
: S>D		DUP 0< ;
[ELSE]
		$FCODE S>D
		tos pushD,
		0 # R1 MOV,
		\ sign-extend tos into tos to form ms of double
		0 # ASR tos R1 tos ADD, \ TODO 0 s/b 32 really..
		$NEXT
[THEN]


\   SEARCH-WORDLIST     ( c-addr u wid -- 0 | xt 1 | xt -1)     \ SEARCH
\		Search word list for a match with the given name.
\		Return execution token and -1 or 1 ( IMMEDIATE) if found.
\		Return 0 if not found.
\
: SEARCH-WORDLIST
		(search-wordlist) DUP IF NIP THEN ;


\   SIGN        ( n -- )		         \ CORE
\		Add a minus sign to the numeric output string if n is negative.
\
: SIGN		0< IF [CHAR] - HOLD THEN ;


\   SOURCE      ( -- c-addr u )		  \ CORE
\		Return input buffer string.
\
: SOURCE	sourceVar 2@ ;


\   SPACE       ( -- )		           \ CORE
\		Send the blank character to the output device.
META-HI [IF]
: SPACE		32 EMIT ;
[ELSE]
		$FCODE SPACE
		tos pushD,
		SPC # tos MOV,
\ TODO		xtEMIT B,
		$END-CODE
[THEN]

\   STATE	( -- a-addr )			\ CORE
\		Return the address of a cell containing compilation-state flag
\		which is true in compilation state or false otherwise.
\
$FVAR STATE


\   THEN        Compilation: ( C: orig -- )     \ CORE
\		Run-time: ( -- )
\		Resolve the forward reference orig.
\
: THEN		1- IF -22 THROW THEN    \ control structure mismatch
				        \ orig type is 1
		xhere SWAP ! bal- ; COMPILE-ONLY IMMEDIATE


\   THROW       ( k*x n -- k*x | i*x n )        \ EXCEPTION
\		If n is not zero, pop the topmost exception frame from the
\		exception stack, along with everything on the return stack
\		above the frame. Then restore the condition before CATCH and
\		transfer control just after the CATCH that pushed that
\		exception frame.
\
: THROW		?DUP
		IF   throwFrame @ rp!   \ restore return stack
		     R> throwFrame !    \ restore THROW frame
		     R> SWAP >R sp!     \ restore data stack
		     DROP R>
		     'init-i/o EXECUTE
		THEN ;


\   TYPE        ( c-addr u -- )		  \ CORE
\		Display the character string if u is greater than zero.
\
: TYPE		?DUP IF 0 DO DUP C@ EMIT CHAR+ LOOP THEN DROP ;


\   U<          ( u1 u2 -- flag )		\ CORE
\		Unsigned compare of top two items. True if u1 < u2.
\
META-HI [IF]
: U<		2DUP XOR 0< IF NIP 0< EXIT THEN - 0< ;
[ELSE]
		$FCODE U<
		R0 popD,			\ u1
		R0 tos CMP,			\ u2-u1
		TRUEE tos =,
		FALSEE tos LS =,		\ C or Z
		$NEXT
[THEN]

\   UM*         ( u1 u2 -- ud )				\ CORE
\		Unsigned multiply. Return double-cell product.
\
META-HI [IF]
: UM*		0 SWAP cell-size-in-bits 0 DO
		   DUP um+ >R >R DUP um+ R> +
		   R> IF >R OVER um+ R> + THEN     \ if carry
		LOOP ROT DROP ;
[ELSE]
		$FCODE UM*
		R0 popD,
		tos R0 tos R1 UMULL,
		R1 pushD,		\ ms 32-bits are at top of stack
		$NEXT
[THEN]

META-TODO [IF]

\   UM/MOD      ( ud u1 -- u2 u3 )              	\ CORE
\		Unsigned division of a double-cell number ud by a single-cell
\		number u1. Return remainder u2 and quotient u3.
\
: UM/MOD	DUP 0= IF -10 THROW THEN        \ divide by zero
		2DUP U< IF
		   NEGATE cell-size-in-bits 0
		   DO   >R DUP um+ >R >R DUP um+ R> + DUP
		        R> R@ SWAP >R um+ R> OR
		        IF >R DROP 1+ R> THEN
		        ELSE DROP THEN
		        R>
		   LOOP DROP SWAP EXIT
		ELSE -11 THROW          \ result out of range
		THEN ;


\   UNLOOP      ( -- ) ( R: loop-sys -- )       \ CORE
\		Discard loop-control parameters for the current nesting level.
\		An UNLOOP is required for each nesting level before the
\		definition may be EXITed.
\
META-HI [IF]
: UNLOOP	R> R> R> 2DROP >R ; COMPILE-ONLY
[ELSE]
		$FCODE UNLOOP
		\ The code version doesn't save fpc on return stack so
		\ it doesn't have to do the same save/restore as the
		\ colon version
		R0 popR,
		R0 popR,	\ TODO could just bump it up.. save time
		$NEXT $COMPO
[THEN]


\   WITHIN      ( n1|u1 n2|n2 n3|u3 -- flag )   \ CORE EXT
\		Return true if (n2|u2<=n1|u1 and n1|u1<n3|u3) or
\		(n2|u2>n3|u3 and (n2|u2<=n1|u1 or n1|u1<n3|u3)).
\
: WITHIN	OVER - >R - R> U< ;


\   [           ( -- )		           \ CORE
\		Enter interpretation state.
\
META-HI [IF]
: [		0 STATE ! ; COMPILE-ONLY IMMEDIATE
[ELSE]
		$FCODE [
		LocSTATE R0 =,
		0 # R1 MOV,
		[ R0 ] R1 STR,
		$NEXT $COMPO $IMMED
[THEN]


\   ]           ( -- )		           \ CORE
\		Enter compilation state.
\
META-HI [IF]
: ]		-1 STATE ! ;
[ELSE]
		$FCODE ]
		LocSTATE R0 =,
		1 # R1 MVN,
		[ R0 ] R1 STR,
		$NEXT
[THEN]

CR .( *** Rest of CORE words and two facility words, EKEY? and EMIT?)
xF-DEF

\       Following definitions can be removed from assembler source and
\       can be colon-defined later.

\   (           ( "ccc<)>" -- )		  \ CORE
\		Ignore following string up to next ) . A comment.
\
: (		[CHAR] ) PARSE 2DROP ; IMMEDIATE


\   *           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
\		Multiply n1|u1 by n2|u2 giving a single product.
\
META-HI [IF]
: *		UM* DROP ;
[ELSE]
		$FCODE *
		R0 popD,
		tos R0 tos MUL,
		$NEXT
[THEN]


\   */          ( n1 n2 n3 -- n4 )              \ CORE
\		Multiply n1 by n2 producing double-cell intermediate,
\		then divide it by n3. Return single-cell quotient.
\
: */		*/MOD NIP ;


\   */MOD       ( n1 n2 n3 -- n4 n5 )           \ CORE
\		Multiply n1 by n2 producing double-cell intermediate,
\		then divide it by n3. Return single-cell remainder and
\		single-cell quotient.
\
: */MOD		>R M* R> FM/MOD ;


\   +LOOP       Compilation: ( C: do-sys -- )   \ CORE
\		Run-time: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Terminate a DO-+LOOP structure. Resolve the destination of all
\		unresolved occurences of LEAVE.
\		On execution add n to the loop index. If loop index did not
\		cross the boundary between loop_limit-1 and loop_limit,
\		continue execution at the beginning of the loop. Otherwise,
\		finish the loop.
\
: +LOOP		POSTPONE do+LOOP  rake ; COMPILE-ONLY IMMEDIATE


\   ."          ( "ccc<">" -- )		  \ CORE
\		Run-time ( -- )
\		Compile an inline string literal to be typed out at run time.
\
: ."		POSTPONE S" POSTPONE TYPE ; COMPILE-ONLY IMMEDIATE


\   2OVER       ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )      \ CORE
\		Copy cell pair x1 x2 to the top of the stack.
\
META-HI [IF]
: 2OVER		>R >R 2DUP R> R> 2SWAP ;
[ELSE]
		$FCODE 2OVER
		tos pushD,
		[ CELLL 3 * # dsp ] R0 LDR, 	\ x1
		[ CELLL 2 * # dsp ] tos LDR,	\ x2
		R0 pushD,
		$NEXT
[THEN]


\   >BODY       ( xt -- a-addr )		 \ CORE
\		Push data field address of CREATEd word.
\		Structure of CREATEd word:
\		        | call-doCREATE | 0 or DOES> code addr | a-addr |
\
: >BODY		?call DUP IF		     \ code-addr xt2
		    ['] doCREATE = IF           \ should be call-doCREATE
		    CELL+ @ EXIT
		THEN THEN
		-31 THROW ;             \ >BODY used on non-CREATEd definition


\   ABORT"      ( "ccc<">" -- )		  \ EXCEPTION EXT
\		Run-time ( i*x x1 -- | i*x ) ( R: j*x -- | j*x )
\		Conditional abort with an error message.
\
: ABORT"	S" POSTPONE ROT
		POSTPONE IF POSTPONE abort"msg POSTPONE 2!
		-2 POSTPONE LITERAL POSTPONE THROW
		POSTPONE ELSE POSTPONE 2DROP POSTPONE THEN
		;  COMPILE-ONLY IMMEDIATE


\   ABS         ( n -- u )		       \ CORE
\		Return the absolute value of n.
\
META-HI [IF]
: ABS		DUP 0< IF NEGATE THEN ;
[ELSE]
		$FCODE ABS
		0 # tos tos ORRS,		\ set flags
		0 R0 =,
		tos R0 tos MI SUB,
		$NEXT
[THEN]

\   ALLOT       ( n -- )		         \ CORE
\		Allocate n bytes in RAM or ROM data space.
\
META-HI [IF]
: ALLOT		hereVar +! ;
[ELSE]
		$FCODE ALLOT
		LocHereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		R2 tos R2 ADD,
		tos popD,
		[ R1 ] R2 STR,
		$NEXT
[THEN]


\   BEGIN       ( C: -- dest )		   \ CORE
\		Start an infinite or indefinite loop structure. Put the next
\		location for a transfer of control, dest, onto the data
\		control stack.
\
: BEGIN		xhere 0 bal+            \ dest type is 0
		; COMPILE-ONLY IMMEDIATE


\   C,          ( char -- )			\ CORE
\		Compile a character into data space.
\
META-HI [IF]
: C,		HERE C! char-size hereVar +! ;
[ELSE]
		$FCODE C,
		LocHereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		CHARR # [ R2 ] tos STRB,
		[ R1 ] R2 STR,
		tos popD,
		$NEXT
[THEN]


\   CHAR        ( "<spaces>ccc" -- char )       \ CORE
\		Parse next word and return the value of first character.
\
: CHAR		PARSE-WORD DROP C@ ;


\   DO          Compilation: ( C: -- do-sys )   \ CORE
\		Run-time: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
\		Start a DO-LOOP structure in a colon definition. Place do-sys
\		on control-flow stack, which will be resolved by LOOP or +LOOP.
\
: DO		0 rakeVar !  0		   \ ?DO-orig is 0 for DO
		POSTPONE doDO xhere  bal+       \ DO-dest
		; COMPILE-ONLY IMMEDIATE


\   DOES>       ( C: colon-sys1 -- colon-sys2 ) \ CORE
\		Build run time code of the data object CREATEd.
\
: DOES>		bal 1- IF -22 THROW THEN        \ control structure mismatch
		NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
		POSTPONE pipe ['] doLIST xt, -1 ; COMPILE-ONLY IMMEDIATE


\   ELSE        Compilation: ( C: orig1 -- orig2 )      \ CORE
\		Run-time: ( -- )
\		Start the false clause in an IF-ELSE-THEN structure.
\		Put the location of new unresolved forward reference orig2
\		onto control-flow stack.
\
: ELSE		POSTPONE AHEAD 2SWAP POSTPONE THEN ; COMPILE-ONLY IMMEDIATE


\   ENVIRONMENT?   ( c-addr u -- false | i*x true )     \ CORE
\		Environment query.
\
: ENVIRONMENT?
		envQList SEARCH-WORDLIST
		DUP >R IF EXECUTE THEN R> ;


\   EVALUATE    ( i*x c-addr u -- j*x )         \ CORE
\		Evaluate the string. Save the input source specification.
\		Store -1 in SOURCE-ID.
\
: EVALUATE	SOURCE >R >R >IN @ >R  SOURCE-ID >R
		-1 TO SOURCE-ID
		sourceVar 2!  0 >IN !  interpret
		R> TO SOURCE-ID
		R> >IN ! R> R> sourceVar 2! ;


\   FILL        ( c-addr u char -- )            \ CORE
\		Store char in each of u consecutive characters of memory
\		beginning at c-addr.
\
META-HI [IF]
: FILL		ROT ROT ?DUP IF 0 DO 2DUP C! CHAR+ LOOP THEN 2DROP ;
[ELSE]
		$FCODE FILL
		\ tos holds char to fill with
		R0 popD,		\ count
		R1 popD,	      	\ dest
		R0 R0 R0 ORRS,		\ drop straight through if length is 0
01 L.		CHARR # [ R1 ] tos NE STRB,
		CHARR # R0 R0 NE SUBS,
		01 L# NE B,
		tos popD,
		$NEXT
[THEN]


\   FIND        ( c-addr -- c-addr 0 | xt 1 | xt -1)     \ SEARCH
\		Search dictionary for a match with the given counted name.
\		Return execution token and -1 or 1 ( IMMEDIATE) if found\
\		c-addr 0 if not found.
\
: FIND		DUP COUNT search-word ?DUP IF NIP ROT DROP EXIT THEN
		2DROP 0 ;


\   IMMEDIATE   ( -- )				\ CORE
\		Make the most recent definition an immediate word.
\
META-HI [IF]
: IMMEDIATE	lastName [ =IMED ] LITERAL OVER @ OR SWAP ! ;
[ELSE]
		$FCODE IMMEDIATE
		AddrNPVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL 2 * # R2 R2 ADD,		\ Lastname
		[ R2 ] R3 LDR,			\ get length
		=IMED # R3 R3 ORR,		\ set flag
		[ R2 ] R3 STR,
		$NEXT
[THEN]

\   J           ( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
\		Push the index of next outer loop.
\
META-HI [IF]
: J		rp@ [ 3 CELLS ] LITERAL + @
		rp@ [ 4 CELLS ] LITERAL + @  +  ; COMPILE-ONLY
[ELSE]
		$FCODE J
		tos pushD,
		\ note that we're one CELLL closer to the tos in CODE
		[ CELLL 2 * # rsp ] R0 LDR,
		[ CELLL 3 * # rsp ] tos LDR,
		R0 tos tos ADD,
		$NEXT $COMPO
[THEN]


\   LEAVE       ( -- ) ( R: loop-sys -- )       \ CORE
\		Terminate definite loop, DO|?DO  ... LOOP|+LOOP, immediately.
\
: LEAVE		POSTPONE UNLOOP POSTPONE branch
		xhere rakeVar DUP @ code, ! ; COMPILE-ONLY IMMEDIATE


\   LOOP        Compilation: ( C: do-sys -- )   \ CORE
\		Run-time: ( -- ) ( R: loop-sys1 -- loop-sys2 )
\		Terminate a DO|?DO ... LOOP structure. Resolve the destination
\		of all unresolved occurences of LEAVE.
\
: LOOP		POSTPONE doLOOP  rake ; COMPILE-ONLY IMMEDIATE


\   LSHIFT      ( x1 u -- x2 )		   \ CORE
\		Perform a logical left shift of u bit-places on x1, giving x2.
\		Put 0 into the least significant bits vacated by the shift.
\
META-HI [IF]
: LSHIFT	?DUP IF 0 DO 2* LOOP THEN ;
[ELSE]
		$FCODE LSHIFT
		R0 popD,
		tos LSL R0 tos MOV,
		$NEXT
[THEN]


\   M*          ( n1 n2 -- d )			\ CORE
\		Signed multiply. Return double product.
\
META-HI [IF]
: M*		2DUP XOR 0< >R ABS SWAP ABS UM* R> IF DNEGATE THEN ;
[ELSE]
		$FCODE M*
		R0 popD,
		tos R0 tos R1 SMULL,
		R1 pushD,		\ ms 32-bits are at top of stack
		$NEXT
[THEN]

\   MAX         ( n1 n2 -- n3 )			\ CORE
\		Return the greater of two top stack items.
META-HI [IF]
: MAX		2DUP < IF SWAP THEN DROP ;
[ELSE]
		$FCODE MAX
		R0 popD,
		tos R0 CMP,
		R0 tos GT MOV,
		$NEXT
[THEN]

\   MIN         ( n1 n2 -- n3 )		  \ CORE
\		Return the smaller of top two stack items.
META-HI [IF]
: MIN		2DUP > IF SWAP THEN DROP ;
[ELSE]
		$FCODE MIN
		R0 popD,
		tos R0 CMP,
		R0 tos LT MOV,
		$NEXT
[THEN]


\   MOD         ( n1 n2 -- n3 )		  \ CORE
\		Divide n1 by n2, giving the single cell remainder n3.
\		Returns modulo of floored division in this implementation.
\
: MOD		/MOD DROP ;


\   PICK        ( x_u ... x1 x0 u -- x_u ... x1 x0 x_u )        \ CORE EXT
\		Remove u and copy the uth stack item to top of the stack. An
\		ambiguous condition exists if there are less than u+2 items
\		on the stack before PICK is executed.
\
: PICK		DEPTH DUP 2 < IF -4 THROW THEN    \ stack underflow
		2 - OVER U< IF -4 THROW THEN
		1+ CELLS sp@ + @ ;


\   POSTPONE    ( "<spaces>name" -- )           \ CORE
\		Parse name and find it. Append compilation semantics of name
\		to current definition.
\
: POSTPONE	(') 0< IF POSTPONE LITERAL
		          POSTPONE COMPILE, EXIT THEN   \ non-IMMEDIATE
		COMPILE, ; COMPILE-ONLY IMMEDIATE       \ IMMEDIATE


\   RECURSE     ( -- )		           \ CORE
\		Append the execution semactics of the current definition to
\		the current definition.
\
: RECURSE	bal 1- 2* PICK 1+ IF -22 THROW THEN
		        \ control structure mismatch; colon-sys type is -1
		bal 1- 2* 1+ PICK       \ xt of current definition
		COMPILE, ; COMPILE-ONLY IMMEDIATE


\   REPEAT      ( C: orig dest -- )             \ CORE
\		Terminate a BEGIN-WHILE-REPEAT indefinite loop. Resolve
\		backward reference dest and forward reference orig.
\
: REPEAT	AGAIN THEN ; COMPILE-ONLY IMMEDIATE


\   RSHIFT      ( x1 u -- x2 )		   \ CORE
\		Perform a logical right shift of u bit-places on x1, giving x2.
\		Put 0 into the most significant bits vacated by the shift.
\
META-HI [IF]
: RSHIFT	?DUP IF
		        0 SWAP  cell-size-in-bits SWAP -
		        0 DO  2DUP D+  LOOP
		        NIP
		THEN ;
[ELSE]
		$FCODE RSHIFT
		R0 popD,
		tos LSR R0 tos MOV,
		$NEXT
[THEN]


\ SLITERAL	( c-addr1 u -- )		 \ STRING
\		Run-time ( -- c-addr2 u )
\		Compile a string literal. Return the string on execution.
\
: SLITERAL	DUP LITERAL POSTPONE doS"
		CHARS xhere 2DUP + ALIGNED TOxhere
		SWAP MOVE ; COMPILE-ONLY IMMEDIATE


\   S"          Compilation: ( "ccc<">" -- )    \ CORE
\		Run-time: ( -- c-addr u )
\		Parse ccc delimetered by " . Return the string specification
\		c-addr u on execution.
\
: S"		[CHAR] " PARSE POSTPONE SLITERAL ; COMPILE-ONLY IMMEDIATE


\   SM/REM      ( d n1 -- n2 n3 )		\ CORE
\		Symmetric divide of double by single. Return remainder n2
\		and quotient n3.
\
: SM/REM	2DUP XOR >R OVER >R >R DUP 0< IF DNEGATE THEN
		R> ABS UM/MOD
		R> 0< IF SWAP NEGATE SWAP THEN
		R> 0< IF        \ negative quotient
		    NEGATE 0 OVER < 0= IF EXIT THEN
		    -11 THROW		    THEN    \ result out of range
		DUP 0< IF -11 THROW THEN ;              \ result out of range


\   SPACES      ( n -- )		         \ CORE
\		Send n spaces to the output device if n is greater than zero.
\
: SPACES	?DUP IF 0 DO SPACE LOOP THEN ;


\   TO          Interpretation: ( x "<spaces>name" -- ) \ CORE EXT
\		Compilation:    ( "<spaces>name" -- )
\		Run-time:       ( x -- )
\		Store x in name.
\
: TO		' ?call DUP IF          \ should be call-doVALUE
		  ['] doVALUE =         \ verify VALUE marker
		  IF @ STATE @
		     IF POSTPONE doTO code, EXIT THEN
		     ! EXIT
		     THEN THEN
		-32 THROW ; IMMEDIATE   \ invalid name argument (e.g. TO xxx)


\   U.          ( u -- )		         \ CORE
\		Display u in free field format followed by space.
\
META-HI [IF]
: U.		0 D. ;
[ELSE]
		$FCODE U.
		tos pushD,
		0 # tos MOV,
\ TODO		DDot B,
		$END-CODE
[THEN]


\   UNTIL       ( C: dest -- )		   \ CORE
\		Terminate a BEGIN-UNTIL indefinite loop structure.
\
: UNTIL		IF -22 THROW THEN  \ control structure mismatch; dest type is 0
		POSTPONE 0branch code, bal- ; COMPILE-ONLY IMMEDIATE


\   VALUE       ( x "<spaces>name" -- )         \ CORE EXT
\		name Execution: ( -- x )
\		Create a value object with initial value x.
\
: VALUE		bal IF -29 THROW THEN           \ compiler nesting
		['] doVALUE xt, head,
		xhere DUP CELL+ TOxhere
		RAMB @ SWAP !
		, linkLast ; \ store x and link VALUE word to current wordlist


\   VARIABLE    ( "<spaces>name" -- )           \ CORE
\		name Execution: ( -- a-addr )
\		Parse a name and create a variable with the name.
\		Resolve one cell of data space at an aligned address.
\		Return the address on execution.
\
: VARIABLE	bal IF -29 THROW THEN           \ compiler nesting
		['] doCONST xt, head,
		xhere DUP CELL+ TOxhere
		RAMB @ DUP CELL+ RAMB ! \ allocate one cell in RAM area
		SWAP ! linkLast ;


\   WHILE       ( C: dest -- orig dest )        \ CORE
\		Put the location of a new unresolved forward reference orig
\		onto the control flow stack under the existing dest. Typically
\		used in BEGIN ... WHILE ... REPEAT structure.
\
: WHILE		POSTPONE IF 2SWAP ; COMPILE-ONLY IMMEDIATE


\   WORD        ( char "<chars>ccc<char>" -- c-addr )   \ CORE
\		Skip leading delimeters and parse a word. Return the address
\		of a transient region containing the word as counted string.
\
: WORD		skipPARSE xhere pack" DROP xhere ;


\   [']         Compilation: ( "<spaces>name" -- )      \ CORE
\		Run-time: ( -- xt )
\		Parse name. Return the execution token of name on execution.
\
: [']		' POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE


\   [CHAR]      Compilation: ( "<spaces>name" -- )      \ CORE
\		Run-time: ( -- char )
\		Parse name. Return the value of the first character of name
\		on execution.
\
: [CHAR]	CHAR POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE


\   \           ( "ccc<eol>" -- )		\ CORE EXT
\		Parse and discard the remainder of the parse area.
\
META-HI [IF]
: \		SOURCE >IN ! DROP ; IMMEDIATE
[ELSE]
		$FCODE \
		LocSourceVar R0 =,
		[ R0 ] R1 LDR,
		LocToIN R3 =,
		[ R3 ] R1 STR,
		$NEXT $IMMED
[THEN]




\ TODO		LTORG

\ Optional Facility words

\   EKEY?       ( -- flag )		      \ FACILITY EXT
\		If a keyboard event is available, return true.
\
: EKEY?		'ekey? EXECUTE ;


\   EMIT?       ( -- flag )		      \ FACILITY EXT
\		flag is true if the user output device is ready to accept data
\		and the execution of EMIT in place of EMIT? would not have
\		suffered an indefinite delay. If device state is indeterminate,
\		flag is true.
\
: EMIT?		'emit? EXECUTE ;


CR .( *** RAM/ROM System Only)
xS-DEF

\ RESET-SYSTEM   ( -- )
\		Reset the system. Restore initialization values of system
\		variables.
\
: RESET-SYSTEM
		sysVar00 sysVar0 [ sysVar0End sysVar0 - ] LITERAL  MOVE COLD ;

\		$COLON  12,'RESET-SYSTEM',RESET_SYSTEM,_SLINK
\		DW      DoLIT,UZERO0,SysVar0,DoLIT,ULAST-UZERO
\		DW      MOVE,COLD,EXIT

META-TODO [IF]

UZERO0          DW      RXQ
		DW      RXFetch
		DW      TXQ
		DW      TXStore
		DW      Set_IO
		DW      DotOK
		DW      HI
		DW      IOBYTES
		DW      0
		DW      AddrROMB
		DW      AddrROMT
		DW      AddrRAMB
		DW      OptiCOMPILEComma
		DW      EXECUTE
		DW      DoubleAlsoComma
		DW      DoubleAlso
		DW      EXECUTE
		DW      EXECUTE
		DW      10
		DW      CTOP
		DW      NTOP
		DW      VTOP
		DW      RAMT0
		DW      0
		DW      0
		DW      0
		DW      2
		DW      FORTH_WORDLISTAddr
		DW      NONSTANDARD_WORDLISTAddr
		DS      (OrderDepth-2) * CELLL
		DW      FORTH_WORDLISTAddr
		DW      LASTFORTH
		DW      NONSTANDARD_WORDLISTAddr
		DW      FORTH_WORDLISTName
		DW      LASTSYSTEM
		DW      0
		DW      NONSTANDARD_WORDLISTName
		DS      3*(MaxWLISTS-2) * CELLL
		DW      LASTENV
		DW      SysUserP
		DW      SysUserP
		DS      CELLL
		DW      SystemTaskName
		DS      CELLL
		DS      CELLL
		DW      Wake
		DW      SysStatus
		DW      SPP
		DW      RPP
[THEN]

META-TODO [IF]
\ ===============================================================
\ NAC: these are put in by makesrc.awk -- CTOP and VTOP are OK because _VAR
\ NAC: is SETA to the right value as the awk script emits new source
\ LASTENV         EQU     _ENVLINK-0
\ LASTSYSTEM      EQU     _SLINK-0        ;last SYSTEM word name address
\ LASTFORTH       EQU     _FLINK-0        ;last FORTH word name address

NTOP            EQU     _NAME-0         ;next available memory in name dictionary
CTOP            EQU     .-0             ;next available memory in code dictionary
VTOP            EQU     _VAR-0          ;next available memory in variable area

\ nMAIN    ENDS
\ nEND     ORIG

\===============================================================

[THEN]

CR CR .( Start definition of test words )

: tester ALIGN ALIGN + - + * ;
: jim tester ;
\ expect this to be found
\ TODO immediate version of S" must parse
\ : test1 S" ALIGN" mFORTH-WORDLIST mSEARCH-WORDLIST .S ;
\ expect this to NOT be found
\ : test2 S" ALIGN" NONSTANDARD-WORDLIST mSEARCH-WORDLIST .S ;
\ expect this to be found - search all in search list
\ : test3 S" ALIGN" search-word .S ;

\ t: neal ." Hello" t;
\ t: forb neal t;

: freddy IF THEN ;
: freddy1 IF SWAP ALIGN THEN ;
\ : frederic IF ELSE THEN ;
\ : fred 	(doubleAlso) 1- IF SWAP LITERAL THEN LITERAL ;
