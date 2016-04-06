TITLE hForth Z80 RAM Model

PAGE 62,132	;62 lines per page, 132 characters per line

;===============================================================
;
;	hForth Z80 RAM model v0.9.9 by Wonyong Koh, 1995
;
;
; 1997. 6. 4.
;	8086 hForth 0.9.9 is re-ported to Z80.
;
;;	hForth Z80 RAM ¡¡•I·e hForth 8086 RAM ¡¡•I·i Z80 CP/MµA¬á ³i ®
;;	·¶•¡¢ ¤aŽ… ˆõ·³“¡”a.
;;
;;	hForth 8086 RAM ¡¡•I‰Á ”aŸe ¸ñ—i·i ´aœµA ¸â´ö·s“¡”a. ¯¡¯aÉQµA
;;	‰Åe–E ·³Â‰b { i —w‰Á ‹¡‰´á ¸÷·—i e·i CP/M‰Á Z80µA  xÂ´á
;;	¤aŽ´ö·s“¡”a. XOR ‹¡‰´á ¸÷· ”a·q· ´á­Q§iŸ¡ ¤aÈw¥¥·e ¸åÑa
;;	­¥¥¡»¡ ´g´v·s“¡”a.
;;
;;	1. CP/M· BDOS Ðq®Ÿi ³a•¡¢ RX? , RX@ , TX!· ·³Â‰b { i·i
;;	   ‰¡Áv·s“¡”a. { i BYE•¡ BDOS Ðq®Ÿi °á¬á ”a¯¡ ¸÷·Ð–·s“¡”a.
;;	   MS-DOSµA e ‰Åe–E stdin , << —w· { i·i ¨–·s“¡”a.
;;
;;	2. Á¡‹¡ÑÁ ¡ww‰Á ˆb ‹¡‰´á ¸÷·—i·i Z80 ‹¡‰´á¡ ”a¯¡
;;	   ¸÷·Ð–·s“¡”a.
;;
;;	3. Z80· CALL‰Á JP ‹¡‰´á ¡ww·e ¬w” º­¡Ÿi ³a“e 8086‰Á ”iŸ¡
;;	   ¸é” º­¡Ÿi ³a‹¡ ˜¢…µA ?call‰Á xt,Ÿi ‰¡Áv·s“¡”a.
;;
;;	4. 8§¡Ëa  a·¡Ça¡Ïa¡­A¬áµA¬á“e ¡A¡¡Ÿ¡ º­¡ˆa aœeÐi Ï©¶aˆa
;;	   ´ô‹¡ ˜¢…µA ALIGN‰Á ALIGNEDŸi ´a¢ ·©•¡ Ða»¡ ´g•¡¢
;;	   ¤aŽ´ö·s“¡”a. $ALIGN  Ça¡•¡ ¡¡– ´ô´–·s“¡”a.
;;
;;	5. hForth 8086 ¡¡•IµA¬áÀáœñ É·µb ­¢•¡Ÿi ¨aŸa‰A Ða‹¡ ¶áÐ
;;	   same?µÁ (search-wordlist)Ÿi ‹¡‰´á¡ ¸÷·Ða‰¡ Z80
;;	   ¡¡•IµA¬á“e ¢…¸aµi·i ˆt·a¡, ˆt·i ¢…¸aµi¡ ¤aŽ“e ˆõ·i ¨iŸ¡
;;	   Ða‹¡ ¶áÐ UM*µÁ UM/MOD•¡ ‹¡‰´á¡ ¸÷·Ð–·s“¡”a. UM*“e
;;	   >NUMBER· ¸÷·µA, UM/MOD“e #· ¸÷·µA —i´á ·¶·s“¡”a.
;;
;	hForth Z80 RAM model is derived from hForth 8086 RAM model to
;	be used for Z80 CP/M.
;
;	Changes from 8086 RAM model is described below. Only code
;	definitions and I/O related words were rewritten. No
;	modification was made in the assembly source after the code
;	definition of XOR.
;
;	1. 'RX?', 'RX@' and 'TX!' were redefined to use BDOS function
;	   of CP/M. BYE was redefined using BDOS fuction also. Words
;	   only for MS-DOS such as 'stdin' and '<<' in 8086 RAM model
;	   were removed which were only for MS-DOS.
;
;	2. Initialization code and low level definitions were
;	   rewritten in Z80 machine codes.
;
;	3. '?call' and 'xt,' were rewritten since Z80 CALL and JP
;	   machine instructions use absolute address while 8086 uses
;	   relative address.
;
;	4. Make ALIGN and ALIGNED null definition since aligning
;	   address is not necessary for 8-bit processor. Remove all
;	   $ALIGN macros also.
;
;	5. 'same?' and '(search-wordlist)' were rewritten in machine
;	   codes as in 8086 RAM model. Also 'UM*' and 'UM/MOD' were
;	   rewritten in Z80 machine to speed up conversion between a
;	   character string and a number. 'UM*' is used in '>NUMBER'
;	   and 'UM/MOD' is used in '#'.
;
;===============================================================
;
;	Z80 register usages
;	SP:	data stack pointer
;	IX:	return stack pointer
;	DE:	Forth virtual machine instruction pointer
;	BC:	top of data stack item
;	All other registers including alternate registers are free.
;
;	Structure of a task
;	userP points follower.
;	//userP//<return_stack//<data_stack//
;	//user_area/user1/taskName/throwFrame/stackTop/status/follower/sp0/rp0
;
;===============================================================

;;;;;;;;;;;;;;;;
; Assembly Constants
;;;;;;;;;;;;;;;;

TRUEE		EQU	-1
FALSEE		EQU	0

CHARR		EQU	1		;byte size of a character
CELLL		EQU	2		;byte size of a cell
MaxChar 	EQU	0FFh		;Extended character set
					;  Use 07Fh for ASCII only
MaxSigned	EQU	07FFFh		;max value of signed integer
MaxUnsigned	EQU	0FFFFh		;max value of unsigned integer
MaxNegative	EQU	8000h		;max value of negative integer
					;  Used in doDO

PADSize 	EQU	130		;PAD area size
RTCells 	EQU	64		;return stack size
DTCells 	EQU	256		;data stack size

BASEE		EQU	10		;default radix
OrderDepth	EQU	10		;depth of search order stack

COMPO		EQU	020h		;lexicon compile only bit
IMMED		EQU	040h		;lexicon immediate bit
MASKK		EQU	1Fh		;lexicon bit mask
					;extended character set
					;maximum name length = 1Fh

BKSPP		EQU	8		;backspace
TABB		EQU	9		;tab
LFF		EQU	10		;line feed
CRR		EQU	13		;carriage return
DEL		EQU	127		;delete

CALLL		EQU	0CD00h		;NOP CALL opcodes

; Memory allocation
;	RAMbottom||code/name/data>WORDworkarea|--//--|PAD|TIB||MemTop

COLDD		EQU	00100h			;cold start vector

; Initialize assembly variables

_SLINK	= 0					;force a null link
_FLINK	= 0					;force a null link
_ENVLINK = 0					;farce a null link
_THROW	= 0					;current throw str addr offset

;;;;;;;;;;;;;;;;
; Assembly macros
;;;;;;;;;;;;;;;;


;	Add a name to name space of dictionary. To be used to store THROW
;	message in name space. THROW messages won't be needed if target
;	system do not need names of Forth words.

;	Add a name to name space of dictionary.

$STR	MACRO	LABEL,STRING
LABEL:
	_LEN	= $
	DB	0,STRING
	_CODE	= $
ORG	_LEN
	DB	_CODE-_LEN-1
ORG	_CODE
	ENDM

;	Add a THROW message in name space. THROW messages won't be
;	needed if target system do not need names of Forth words.

$THROWMSG MACRO STRING
	_LEN	= $
	DB	0,STRING
	_CODE	= $
ORG	_LEN
	DB	_CODE-_LEN-1
	_THROW	= _THROW + CELLL
ORG	AddrTHROWMsgTbl - _THROW
	DW	_LEN
ORG	_CODE
	ENDM

;	Compile a code definition header.

$CODE	MACRO	LEX,NAME,LABEL,LINK
	DW	LINK
	_NAME	= $
	LINK	= $				;link points to a name string
	DB	LEX,NAME			;name string
	DW	_NAME
LABEL:						;assembly label
	ENDM

;	Compile a colon definition header.

$COLON	MACRO	LEX,NAME,LABEL,LINK
	$CODE	LEX,NAME,LABEL,LINK
	DW	CALLL	; NOP			;align to cell boundary
	DW	DoLIST	; CALL	DoLIST		;include CALL doLIST
	ENDM

;	Compile a system CONSTANT header.

$CONST	MACRO	LEX,NAME,LABEL,VALUE,LINK
	$CODE	LEX,NAME,LABEL,LINK
	DW	CALLL
	DW	DoCONST
	DW	VALUE
	ENDM

;	Compile a system VALUE header.

$VALUE	MACRO	LEX,NAME,LABEL,VALUE,LINK
	$CODE	LEX,NAME,LABEL,LINK
	DW	CALLL
	DW	DoVALUE
	DW	VALUE
	ENDM

;	Compile a non-initialized system VARIABLE header.

$VAR	MACRO	LEX,NAME,LABEL,N_CELLS,LINK
	$CODE	LEX,NAME,LABEL,LINK
	DW	CALLL
	DW	DoVAR
	DW	N_CELLS DUP (?)
	ENDM

;	Compile a system USER header.

$USER	MACRO	LEX,NAME,LABEL,OFFSET,LINK
	$CODE	LEX,NAME,LABEL,LINK
	DW	CALLL
	DW	DoUSER
	DW	OFFSET
	ENDM

;	Compile an inline string.

$INSTR	MACRO	STRNG
	DW	DoLIT
	_LEN	= $				;save address of count
	DW	0				;count
	DW	DoSQuote			;doS"
	DB	STRNG				;store string
	_CODE	= $				;save code pointer
ORG	_LEN					;point to count byte
	DW	_CODE-_LEN-2*CELLL		;set count
ORG	_CODE					;restore code pointer
	ENDM

;	Compile a environment query string header.

$ENVIR	MACRO	LEX,NAME
	DW	_ENVLINK			;link
	_ENVLINK = $				;link points to a name string
	DB	LEX,NAME			;name string
	DW	_ENVLINK
	DB	000h	; NOP			;align to cell boundary
	DB	0CDh	; CALL	DoLIST		;include CALL doLIST
	DW	DoLIST
	ENDM

;	Assemble inline direct threaded code ending.

$NEXT	MACRO
	DB	0EBh	; EX	DE,HL	; 4t
	DB	05Eh	; LD	E,(HL)	; 7t
	DB	023h	; INC	HL	; 6t
	DB	056h	; LD	D,(HL)	; 7t
	DB	023h	; INC	HL	; 6t
	DB	0EBh	; EX	DE,HL	; 4t
	DB	0E9h	; JP	(HL)	; 4t
			;		;38t==(10MHz)3.8 usec.
	ENDM

;	Assemble inline direct threaded code ending.
;	NEXTHL is used when the IP is already in HL.

$NEXTHL MACRO
	DB	05Eh	; LD	E,(HL)	; 7t
	DB	023h	; INC	HL	; 6t
	DB	056h	; LD	D,(HL)	; 7t
	DB	023h	; INC	HL	; 6t
	DB	0EBh	; EX	DE,HL	; 4t
	DB	0E9h	; JP	(HL)	; 4t
			;		;34t==(10MHz)3.4 usec.
	ENDM

;===============================================================

;;;;;;;;;;;;;;;;
; Main entry points and COLD start data
;;;;;;;;;;;;;;;;

MAIN	SEGMENT
ASSUME	CS:MAIN,DS:MAIN,SS:MAIN

ORG		COLDD				;beginning of cold boot

ORIG:
	DB	02Ah	; LD	HL,(06h)	; BDOS address, rounded down
	DW	0006h				;   = end of avail.mem (memTop)
	DB	02Eh,0	; LD	L,0
	DB	022h	; LD	(AddrMemTop),HL
	DW	AddrMemTop
	DB	021h	; LD	HL,SPP
	DW	SPP
	DB	0F9h	; LD	SP,HL
	DB	021h	; LD	HL,RPP
	DW	RPP
	DB	0E5h	; PUSH	HL
	DB	0DDh,0E1h ; POP IX
	DB	011h	; LD	DE,0001 ;Z80 cold start vecotr
	DW	0001
	DB	0C3h	; JP	COLD
	DW	COLD

		$STR	CPUStr,'Z80'
		$STR	ModelStr,'RAM Model'
		$STR	VersionStr,'0.9.9'

; system variables.

ValueTickEKEYQ	EQU	RXQ			;'ekey?
ValueTickEKEY	EQU	RXFetch 		;'ekey
ValueTickEMITQ	EQU	TXQ			;'emit?
ValueTickEMIT	EQU	TXStore 		;'emit
ValueTickINIT_IO EQU	Set_IO			;'init-i/o
ValueTickPrompt EQU	DotOK			;'prompt
ValueTickBoot	EQU	HI			;'boot
ValueSOURCE_ID	EQU	0			;SOURCE-ID
ValueHERE	EQU	CTOP			;data space pointer
AddrTickDoWord	DW	OptiCOMPILEComma	;nonimmediate word - compilation
		DW	EXECUTE 		;nonimmediate word - interpretation
		DW	DoubleAlsoComma 	;not found word - compilateion
		DW	DoubleAlso		;not found word - interpretation
		DW	EXECUTE 		;immediate word - compilation
		DW	EXECUTE 		;immediate word - interpretation
AddrBASE	DW	10			;BASE
AddrRakeVar	DW	0			;rakeVar
AddrNumberOrder DW	2			;#order
		DW	AddrFORTH_WORDLIST	;search order stack
		DW	AddrNONSTANDARD_WORDLIST
		DW	(OrderDepth-2) DUP (0)
AddrCurrent	DW	AddrFORTH_WORDLIST	;current pointer
AddrFORTH_WORDLIST DW	LASTFORTH		;FORTH-WORDLIST
		DW	AddrNONSTANDARD_WORDLIST;wordlist link
		DW	FORTH_WORDLISTName	;name of the WORDLIST
AddrNONSTANDARD_WORDLIST DW	 LASTSYSTEM	;NONSTANDARD-WORDLIST
		DW	0			;wordlist link
		DW	NONSTANDARD_WORDLISTName;name of the WORDLIST
AddrEnvQList	DW	LASTENV 		;envQList
AddrUserP	DW	SysUserP		;user pointer
SysTask 	DW	SysUserP		;system task's tid
SysUser1	DW	?			;user1
SysTaskName	DW	SystemTaskName		;taskName
SysThrowFrame	DW	?			;throwFrame
SysStackTop	DW	?			;stackTop
SysStatus	DW	Wake			;status
SysUserP:
SysFollower	DW	SysStatus		;follower
		DW	SPP			;system task's sp0
		DW	RPP			;system task's rp0

AddrNumberOrder0 DW	2			;#order
		DW	AddrFORTH_WORDLIST	;search order stack
		DW	AddrNONSTANDARD_WORDLIST
		DW	(OrderDepth-2) DUP (0)

RStack		DW	RTCells DUP (0AAAAh)	;to see how deep stack grows
RPP		EQU	$-CELLL
DStack		DW	DTCells DUP (05555h)	;to see how deep stack grows
SPP		EQU	$-CELLL

; THROW code messages

	DW	58 DUP (?)		;number of throw messages = 58
AddrTHROWMsgTbl:
								    ;THROW code
	$THROWMSG	'ABORT'                                         ;-01
	$THROWMSG	'ABORT"'                                        ;-02
	$THROWMSG	'stack overflow'                                ;-03
	$THROWMSG	'stack underflow'                               ;-04
	$THROWMSG	'return stack overflow'                         ;-05
	$THROWMSG	'return stack underflow'                        ;-06
	$THROWMSG	'do-loops nested too deeply during execution'   ;-07
	$THROWMSG	'dictionary overflow'                           ;-08
	$THROWMSG	'invalid memory address'                        ;-09
	$THROWMSG	'division by zero'                              ;-10
	$THROWMSG	'result out of range'                           ;-11
	$THROWMSG	'argument type mismatch'                        ;-12
	$THROWMSG	'undefined word'                                ;-13
	$THROWMSG	'interpreting a compile-only word'              ;-14
	$THROWMSG	'invalid FORGET'                                ;-15
	$THROWMSG	'attempt to use zero-length string as a name'   ;-16
	$THROWMSG	'pictured numeric output string overflow'       ;-17
	$THROWMSG	'parsed string overflow'                        ;-18
	$THROWMSG	'definition name too long'                      ;-19
	$THROWMSG	'write to a read-only location'                 ;-20
	$THROWMSG	'unsupported operation (e.g., AT-XY on a too-dumb terminal)' ;-21
	$THROWMSG	'control structure mismatch'                    ;-22
	$THROWMSG	'address alignment exception'                   ;-23
	$THROWMSG	'invalid numeric argument'                      ;-24
	$THROWMSG	'return stack imbalance'                        ;-25
	$THROWMSG	'loop parameters unavailable'                   ;-26
	$THROWMSG	'invalid recursion'                             ;-27
	$THROWMSG	'user interrupt'                                ;-28
	$THROWMSG	'compiler nesting'                              ;-29
	$THROWMSG	'obsolescent feature'                           ;-30
	$THROWMSG	'>BODY used on non-CREATEd definition'          ;-31
	$THROWMSG	'invalid name argument (e.g., TO xxx)'          ;-32
	$THROWMSG	'block read exception'                          ;-33
	$THROWMSG	'block write exception'                         ;-34
	$THROWMSG	'invalid block number'                          ;-35
	$THROWMSG	'invalid file position'                         ;-36
	$THROWMSG	'file I/O exception'                            ;-37
	$THROWMSG	'non-existent file'                             ;-38
	$THROWMSG	'unexpected end of file'                        ;-39
	$THROWMSG	'invalid BASE for floating point conversion'    ;-40
	$THROWMSG	'loss of precision'                             ;-41
	$THROWMSG	'floating-point divide by zero'                 ;-42
	$THROWMSG	'floating-point result out of range'            ;-43
	$THROWMSG	'floating-point stack overflow'                 ;-44
	$THROWMSG	'floating-point stack underflow'                ;-45
	$THROWMSG	'floating-point invalid argument'               ;-46
	$THROWMSG	'compilation word list deleted'                 ;-47
	$THROWMSG	'invalid POSTPONE'                              ;-48
	$THROWMSG	'search-order overflow'                         ;-49
	$THROWMSG	'search-order underflow'                        ;-50
	$THROWMSG	'compilation word list changed'                 ;-51
	$THROWMSG	'control-flow stack overflow'                   ;-52
	$THROWMSG	'exception stack overflow'                      ;-53
	$THROWMSG	'floating-point underflow'                      ;-54
	$THROWMSG	'floating-point unidentified fault'             ;-55
	$THROWMSG	'QUIT'                                          ;-56
	$THROWMSG	'exception in sending or receiving a character' ;-57
	$THROWMSG	'[IF], [ELSE], or [THEN] exception'             ;-58

;;;;;;;;;;;;;;;;
; System dependent words -- Must be re-definded for each system.
;;;;;;;;;;;;;;;;
; I/O words must be redefined if serial communication is used instead of
; keyboard. Following words are for CP/M system.

;   bdos	( register_DE register_C -- register_A )
;		Call CP/M BDOS fuction.

		$CODE	4,'bdos',BDOS,_SLINK
		DB	0EBh	; EX	DE,HL
		DB	0D1h	; POP	DE
		DB	0E5h	; PUSH	HL
		DB	0DDh,0E5h ;PUSH IX
		DB	0CDh	; CALL	05	; CP/M BDOS entry point
		DW	0005h	;
		DB	04Fh	; LD	C,A
		DB	006h,0	; LD	B,0
		DB	0DDh,0E1h ; POP IX
		DB	0D1h	; POP	DE
		$NEXT

;   keysave	( -- a-addr )
;		Temporary storage for keyboard input.

		$VAR	7,'keysave',KeySave,1,_SLINK

;   ?key	( -- 0:u )
;		Returns u if keyboard event occured. Otherwise return 0.
;
;   : ?key	0FF 6 bdos DUP keysave C! ;

		$COLON	4,'key?',QKey,_SLINK
		DW	DoLIT,0FFh,DoLIT,06h,BDOS,DUPP,KeySave,CStore,EXIT

;   RX? 	( -- flag )
;		Return true if key is pressed.
;
;   : RX?	keysave C@ IF -1 EXIT THEN
;		?key 0= 0= ;

		$COLON	3,'RX?',RXQ,_SLINK
		DW	KeySave,CFetch,ZBranch,RXQ1
		DW	MinusOne,EXIT
RXQ1		DW	QKey,ZeroEquals,ZeroEquals,EXIT

;   RX@ 	( -- u )
;		Receive one keyboard event u.
;
;   : RX@	keysave C@ 0= IF BEGIN ?key UNTIL THEN
;		keysave C@ 0 keysave C! ;

		$COLON	3,'RX@',RXFetch,_SLINK
		DW	KeySave,CFetch,ZeroEquals,ZBranch,RXFET1
RXFET2		DW	QKey,ZBranch,RXFET2
RXFET1		DW	KeySave,CFetch,Zero,KeySave,CStore,EXIT

;   TX? 	( -- flag )
;		Return true if output device is ready or device state is
;		indeterminate.

		$CONST	3,'TX?',TXQ,-1,_SLINK   ;always true for CP/M

;   TX! 	( u -- )
;		Send char to the output device.
;
;   : TX!	DUP 0FF <	    \ 0FF 6 BDOS calls console input
;		IF 6 bdos DROP EXIT THEN
;		-21 THROW ;	    \ unsupported operation

		$COLON	3,'TX!',TXStore,_SLINK
		DW	DUPP,DoLIT,0FFh,LessThan,ZBranch,TXSTO1
		DW	DoLIT,6,BDOS,DROP,EXIT
TXSTO1		DW	DoLIT,-21,THROW

;   CR		( -- )				\ CORE
;		Carriage return and linefeed.
;
;   : CR	carriage-return-char EMIT  linefeed-char EMIT ;

		$COLON	2,'CR',CR,_FLINK
		DW	DoLIT,CRR,EMIT,DoLIT,LFF,EMIT,EXIT

;   BYE 	( -- )				\ TOOLS EXT
;		Return control to the host operation system, if any.

		$CODE	3,'BYE',BYE,_FLINK
		DB	0C3h	; JP	0
		DW	0

;   hi		( -- )
;
;   : hi	CR ." hForth "
;		S" CPU" ENVIRONMENT? DROP TYPE SPACE
;		S" model" ENVIRONMENT? DROP TYPE SPACE [CHAR] v EMIT
;		S" version"  ENVIRONMENT? DROP TYPE
;		."  by Wonyong Koh, 1997" CR
;		." ALL noncommercial and commercial uses are granted." CR
;		." Please send comment, bug report and suggestions to:" CR
;		."   wykoh@pado.krict.re.kr or wykoh@hitel.kol.co.kr" CR ;

		$COLON	2,'hi',HI,_SLINK
		DW	CR
		$INSTR	'hForth '
		DW	TYPEE
		$INSTR	'CPU'
		DW	ENVIRONMENTQuery,DROP,TYPEE,SPACE
		$INSTR	'model'
		DW	ENVIRONMENTQuery,DROP,TYPEE,SPACE,DoLIT,'v',EMIT
		$INSTR	'version'
		DW	ENVIRONMENTQuery,DROP,TYPEE
		$INSTR	' by Wonyong Koh, 1997'
		DW	TYPEE,CR
		$INSTR	'All noncommercial and commercial uses are granted.'
		DW	TYPEE,CR
		$INSTR	'Please send comment, bug report and suggestions to:'
		DW	TYPEE,CR
		$INSTR	'  wykoh@pado.krict.re.kr or wykoh@hitel.kol.co.kr'
		DW	TYPEE,CR,EXIT

;   COLD	( -- )
;		The cold start sequence execution word.
;
;   : COLD	sp0 sp! rp0 rp! 		\ initialize stack
;		'init-i/o EXECUTE
;		'boot EXECUTE
;		QUIT ;				\ start interpretation

		$COLON	4,'COLD',COLD,_SLINK
		DW	SPZero,SPStore,RPZero,RPStore
		DW	TickINIT_IO,EXECUTE,TickBoot,EXECUTE
		DW	QUIT

;   set-i/o ( -- )
;		Set input/output device.
;
;   : set-i/o	;				\ do nothing for CP/M

		$COLON	7,'set-i/o',Set_IO,_SLINK
		DW	EXIT

;;;;;;;;;;;;;;;;
; Non-Standard words - Processor-dependent definitions
;	16 bit Forth for Z80
;;;;;;;;;;;;;;;;

; microdebugger for debugging new hForth ports by NAC.
;
; The major problem with debugging Forth code at the assembler level is that
; most of the definitions are lists of execution tokens that get interpreted
; (using doLIST) rather than executed directly. As far as the native processor
; is concerned, these xt are data, and a debugger cannot be set to trap on
; them.
;
; The solution to that problem would seem to be to trap on the native-machine
; 'call' instruction at the start of each definition. However, the threaded
; nature of the code makes it very difficult to follow a particular definition
; through: many definitions are used repeatedly through the code. Simply
; trapping on the 'call' leads to multiple unwanted traps.
;
; Consider, for example, the code for doS" --
;
;	   DW	   RFrom,SWAP,TwoDUP,Plus,ALIGNED,ToR,EXIT
;
; It would be useful to run each word in turn; at the end of each word the
; effect upon the stacks could be checked until the faulty word is found.
;
; This technique allows you to do exactly that.
;
; All definitions end with $NEXT -- either directly (code definitions) or
; indirectly (colon definitions terminating in EXIT, which is itself a code
; definition). The action of $NEXT is to use the fpc for the next word to
; fetch the xt and jumps to it.
;
; To use the udebug routine, replace the $NEXT expansion with a jump (not a
; call) to the routine udebug (this requires you to reassemble the code)
;
; When you want to debug a word, trap at the CALL doLIST at the start of the
; word and then load the location trapfpc with the address of the first xt
; of the word. Make your debugger trap when you execute the final instruction
; in the udebug routine. Now execute your code and your debugger will trap
; after the completion of the first xt in the definition. To stop debugging,
; simply set trapfpc to 0.
;
; This technique has a number of limitations:
; - It is an assumption that an xt of 0 is illegal
; - You cannot automatically debug a code stream that includes inline string
;   definitions, or any other kind of inline literal. You must step into the
;   word that includes the definition then hand-edit the appropriate new value
;   into trapfpc
; Clearly, you could overcome these limitations by making udebug more
; complex -- but then you run the risk of introducing bugs in that code.

; Microdebugger is not yet implemented.
;
; uDebug:	  MOV	  AX,trapfpc
;		  CMP	  AX,SI 	  ; compare the stored address with
;					  ; the address we're about to get the
;					  ; next xt from
;		  JNE	  uDebug1	  ; not the trap address, so we're done
;		  ADD	  AX,CELLL	  ; next time trap on the next xt
;		  MOV	  trapfpc,AX
;		  NOP			  ; make debugger TRAP at this address
; uDebug1:	  LODSW
;		  JMP	  AX
;
; trapfpc	  DW	  0

;   same?	( c-addr1 c-addr2 u -- -1|0|1 )
;		Return 0 if two strings, ca1 u and ca2 u, are same; -1 if
;		string, ca1 u is smaller than ca2 u; 1 otherwise. Used by
;		'(search-wordlist)'. Code definition is preferred to speed up
;		interpretation. Colon definition is shown below.
;
;   : same?	?DUP IF 	\ null strings are always same
;		   0 DO OVER C@ OVER C@ XOR
;			IF UNLOOP C@ SWAP C@ > 2* 1+ EXIT THEN
;			CHAR+ SWAP CHAR+ SWAP
;		   LOOP
;		THEN 2DROP 0 ;
;
;		  $COLON  5,'same?',SameQ,_SLINK
;		  DW	  QuestionDUP,ZBranch,SAMEQ4
;		  DW	  Zero,DoDO
; SAMEQ3	  DW	  OVER,CFetch,OVER,CFetch,XORR,ZBranch,SAMEQ2
;		  DW	  UNLOOP,CFetch,SWAP,CFetch,GreaterThan
;		  DW	  TwoStar,OnePlus,EXIT
; SAMEQ2	  DW	  CHARPlus,SWAP,CHARPlus
;		  DW	  DoLOOP,SAMEQ3
; SAMEQ4	  DW	  TwoDROP,Zero,EXIT

		$CODE  5,'same?',SameQ,_SLINK
		DB	0C5h		; PUSH	BC
		DB	0D9h		; EXX
		DB	0C1h		; POP	BC	; count
		DB	0E1h		; POP	HL	; destination addr
		DB	0D1h		; POP	DE	; source addr
		DB	078h		; LD	A,B
		DB	0B1h		; OR	C
		DB	028h		; JR	Z,SAMEQ1
		DB	SAMEQ1-SAMEQ2	;
SAMEQ2		DB	01Ah		; LD	A,(DE)
		DB	013h		; INC	DE
		DB	0EDh,0A1h	; CPI
		DB	020h		; JR	NZ,SAMEQ3
		DB	SAMEQ3-SAMEQ4	;
SAMEQ4		DB	078h		; LD	A,B
		DB	0B1h		; OR	C
		DB	020h		; JR	NZ,SAMEQ2
		DB	SAMEQ2-SAMEQ1	;
SAMEQ1		DB	0D9h		; EXX
		DB	0AFh		; XOR	A
		DB	047h		; LD	B,A
		DB	04Fh		; LD	C,A
		$NEXT
SAMEQ3		DB	02Bh		; DEC	HL
		DB	0BEh		; CP	(HL)
		DB	09Fh		; SBC	A,A
		DB	0D9h		; EXX
		DB	047h		; LD	B,A
		DB	04Fh		; LD	C,A
		DB	0CBh,021h	; SLA	C
		DB	0CBh,010h	; RL	B
		DB	003h		; INC	BC
		$NEXT

;   (search-wordlist)	( c-addr u wid -- 0 | xt f 1 | xt f -1)
;		Search word list for a match with the given name.
;		Return execution token and not-compile-only flag and
;		-1 or 1 ( IMMEDIATE) if found. Return 0 if not found.
;
;   : (search-wordlist)
;		ROT >R SWAP DUP 0= IF -16 THROW THEN
;				\ attempt to use zero-length string as a name
;		>R		\ wid  R: ca1 u
;		BEGIN @ 	\ ca2  R: ca1 u
;		   DUP 0= IF R> R> 2DROP EXIT THEN	\ not found
;		   DUP COUNT [ =MASK ] LITERAL AND R@ = \ ca2 ca2+char f
;		      IF   R> R@ SWAP DUP >R		\ ca2 ca2+char ca1 u
;			   same?			\ ca2 flag
;		    \ ELSE DROP -1	\ unnecessary since ca2+char is not 0.
;		      THEN
;		WHILE cell-		\ pointer to next word in wordlist
;		REPEAT
;		R> R> 2DROP DUP name>xt SWAP		\ xt ca2
;		C@ DUP [ =COMP ] LITERAL AND 0= SWAP
;		[ =IMED ] LITERAL AND 0= 2* 1+ ;

;		  $COLON  17,'(search-wordlist)',ParenSearch_Wordlist,_SLINK
;		  DW	  ROT,ToR,SWAP,DUPP,ZBranch,PSRCH6
;		  DW	  ToR
; PSRCH1	  DW	  Fetch
;		  DW	  DUPP,ZBranch,PSRCH9
;		  DW	  DUPP,COUNT,DoLIT,MASKK,ANDD,RFetch,Equals
;		  DW	  ZBranch,PSRCH5
;		  DW	  RFrom,RFetch,SWAP,DUPP,ToR,SameQ
; PSRCH5	  DW	  ZBranch,PSRCH3
;		  DW	  CellMinus,Branch,PSRCH1
; PSRCH3	  DW	  RFrom,RFrom,TwoDROP,DUPP,NameToXT,SWAP
;		  DW	  CFetch,DUPP,DoLIT,COMPO,ANDD,ZeroEquals,SWAP
;		  DW	  DoLIT,IMMED,ANDD,ZeroEquals,TwoStar,OnePlus,EXIT
; PSRCH9	  DW	  RFrom,RFrom,TwoDROP,EXIT
; PSRCH6	  DW	  DoLIT,-16,THROW

		$CODE	17,'(search-wordlist)',ParenSearch_Wordlist,_SLINK
		DB	0C5h		; PUSH	BC
		DB	0D9h		; EXX
		DB	0E1h		; POP	HL	; wid
		DB	0C1h		; POP	BC	; u
		DB	0D1h		; POP	DE	; c-addr
		DB	078h		; LD	A,B
		DB	0B1h		; OR	C
		DB	028h		; JR	Z,PSRCH5
		DB	PSRCH5-PSRCH1
PSRCH1		DB	07Eh		; LD	A,(HL)
		DB	023h		; INC	HL
		DB	066h		; LD	H,(HL)
		DB	06Fh		; LD	L,A
		DB	0B4h		; OR	H
		DB	028H		; JR	Z,PSRCH4
		DB	PSRCH4-PSRCH11
PSRCH11 	DB	07Eh		; LD	A,(HL)
		DB	02Bh		; DEC	HL	;pointer to nextword
		DB	02Bh		; DEC	HL
		DB	0E6h,MASKK	; AND	31     ;max name length = MASKK
		DB	0B9h		; CP	C
		DB	020h		; JR	NZ,PSRCH1
		DB	PSRCH1-PSRCH12
PSRCH12 	DB	0E5h		; PUSH	HL
		DB	0D5h		; PUSH	DE
		DB	0C5h		; PUSH	BC
		DB	023h		; INC	HL
		DB	023h		; INC	HL
		DB	023h		; INC	HL
PSRCH2		DB	078h		; LD	A,B
		DB	0B1h		; OR	C
		DB	028h		; JR	Z,PSRCH3
		DB	PSRCH3-PSRCH13
PSRCH13 	DB	01Ah		; LD	A,(DE)
		DB	013h		; INC	DE
		DB	0EDh,0A1h	; CPI
		DB	028h		; JR	Z,PSRCH2
		DB	PSRCH2-PSRCH14
PSRCH14 	DB	0C1h		; POP	BC
		DB	0D1h		; POP	DE
		DB	0E1h		; POP	HL
		DB	018h		; JR	PSRCH1
		DB	PSRCH1-PSRCH5
PSRCH5		DB	001h,0F0h,0FFh	; LD	BC,-16
				;attempt to use zero-length string as a name
		DB	0C3h		; JP	THROW
		DW	THROW
PSRCH4		DB	0D9h		; EXX
		DB	047h		; LD	B,A
		DB	04Fh		; LD	C,A
		$NEXT
PSRCH3		DB	023h		; INC	HL
		DB	023h		; INC	HL
		DB	0C1h		; POP	BC
		DB	0D1h		; POP	DE
		DB	0E3h		; EX	(SP),HL
		DB	023h		; INC	HL
		DB	023h		; INC	HL
		DB	07Eh		; LD	A,(HL)
		DB	0D9h		; EXX
		DB	001h		; LD	BC,-1
		DW	-1
		DB	0CBh,06Fh	; BIT	5,A	;COMPO=020h
		DB	028h		; JR	Z,PSRCH6
		DB	PSRCH6-PSRCH16
PSRCH16 	DB	003h		; INC	BC
PSRCH6		DB	0C5h		; PUSH	BC
		DB	001h		; LD	BC,1
		DW	1
		DB	0CBh,077h	; BIT	6,A	;IMMED=040h
		DB	020h		; JR	NZ,PSRCH7
		DB	PSRCH7-PSRCH17
PSRCH17 	DB	00Bh		; DEC	BC
		DB	00Bh		; DEC	BC
PSRCH7: 	$NEXT

;   ?call	( xt1 -- xt1 0 | a-addr xt2 )
;		Return xt of the CALLed run-time word if xt starts with machine
;		CALL instruction and leaves the next cell address after the
;		CALL instruction. Otherwise leaves the original xt1 and zero.
;
;   : ?call	DUP @ call-code = IF CELL+ CELL+ DUP cell- @ EXIT THEN
;			\ Direct Threaded Code Z80 absolute call
;		0 ;

		$COLON	5,'?call',QCall,_SLINK
		DW	DUPP,Fetch,DoLIT,CALLL,Equals,ZBranch,QCALL1
		DW	CELLPlus,CELLPlus,DUPP,CellMinus,Fetch,EXIT
QCALL1		DW	Zero,EXIT

;   xt, 	( xt1 -- xt2 )
;		Take a run-time word xt1 for :NONAME , CONSTANT , VARIABLE and
;		CREATE . Return xt2 of current definition.
;
;   : xt,	HERE ALIGNED DUP TO HERE SWAP	\ Direct Threaded Code
;		call-code COMPILE, COMPILE, ;	\ Z80 absolute call

		$COLON	3,'xt,',xtComma,_SLINK
		DW	HERE,ALIGNED,DUPP,DoTO,AddrHERE,SWAP
		DW	DoLIT,CALLL,COMPILEComma,COMPILEComma,EXIT

;   doLIT	( -- x )
;		Push an inline literal.

		$CODE	COMPO+5,'doLIT',DoLIT,_SLINK
		DB	0C5h	; PUSH	BC	;11t
		DB	0EBh	; EX	DE,HL	; 4t
		DB	04Eh	; LD	C,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		DB	046h	; LD	B,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		$NEXTHL

;   doCONST	( -- x )
;		Run-time routine of CONSTANT and VARIABLE.

		$CODE	COMPO+7,'doCONST',DoCONST,_SLINK
		DB	0E1h	; POP	HL	;10t
		DB	0C5h	; PUSH	BC	;11t
		DB	04Eh	; LD	C,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		DB	046h	; LD	B,(HL)	; 7t
		$NEXT

;   doVALUE	( -- x )
;               Run-time routine of VALUE. Same as doCONSTANT. Used as a
;		marker for TO.

		$CODE	COMPO+7,'doVALUE',DoVALUE,_SLINK
		DB	0E1h	; POP	HL	;10t
		DB	0C5h	; PUSH	BC	;11t
		DB	04Eh	; LD	C,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		DB	046h	; LD	B,(HL)	; 7t
		$NEXT

;   doVAR	( -- x )
;		Run-time routine of VARIABLE.

		$CODE	COMPO+5,'doVAR',DoVAR,_SLINK
		DB	0E1h	; POP	HL	;10t
		DB	0C5h	; PUSH	BC	;11t
		DB	044h	; LD	B,H	; 4t
		DB	04Dh	; LD	C,L	; 4t
		$NEXT

;   doCREATE	( -- a-addr )
;		Run-time routine of CREATE. Return address of data space.
;		Structure of CREATEd word:
;		    | call-doCREATE | 0 or DOES> code addr | >BODY points here

		$CODE	COMPO+8,'doCREATE',DoCREATE,_SLINK
		DB	0E1h	; POP	HL	;10t
		DB	0C5h	; PUSH	BC	;11t
		DB	07Eh	; LD	A,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		DB	044h	; LD	B,H	; 4t
		DB	04Dh	; LD	C,L	; 4t
		DB	003h	; INC	BC	; 6t
		DB	0B6h	; OR	(HL)	; 7t
		DB	020h	; JR	NZ,DOCREAT1
		DB	DOCREAT1-DOCREAT2
DOCREAT2:	$NEXT
DOCREAT1	DB	07Eh	; LD	A,(HL)	; 7t
		DB	02Bh	; DEC	HL
		DB	06Eh	; LD	L,(HL)
		DB	067h	; LD	H,A
		DB	0E9h	; JP	(HL)	; 4t

;   doTO	( x -- )
;		Run-time routine of TO. Store x at the address in the
;		following cell.

		$CODE	COMPO+4,'doTO',DoTO,_SLINK
		DB	0EBh	; EX	DE,HL	; 4t
		DB	05Eh	; LD	E,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		DB	056h	; LD	D,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		DB	0EBh	; EX	DE,HL	; 4t
		DB	071h	; LD	(HL),C	; 7t
		DB	023h	; INC	HL	; 6t
		DB	070h	; LD	(HL),B	; 7t
		DB	0C1h	; POP	BC	; 10t
		$NEXT

;   doUSER	( -- a-addr )
;		Run-time routine of USER. Return address of data space.

		$CODE	COMPO+6,'doUSER',DoUSER,_SLINK
		DB	0E1h	; POP	HL	;10t
		DB	0C5h	; PUSH	BC	;11t
		DB	04Eh	; LD	C,(HL)	; 7t
		DB	023h	; INC	HL	; 6t
		DB	046h	; LD	B,(HL)	; 7t
		DB	02Ah	; LD	HL,(AddrUserP)
		DW	AddrUserP		;16t
		DB	009h	; ADD	HL,BC	;11t
		DB	044h	; LD	B,H	; 4t
		DB	04Dh	; LD	C,L	; 4t
		$NEXT

;   doLIST	( -- ) ( R: -- nest-sys )
;		Process colon list.

		$CODE	COMPO+6,'doLIST',DoLIST,_SLINK
		DB	0DDh,02Bh	; DEC IX	;10t
		DB	0DDh,072h,0	; LD  (IX+0),D	;19t
		DB	0DDh,02Bh	; DEC IX	;10t
		DB	0DDh,073h,0	; LD  (IX+0),E	;19t
		DB	0E1h		; POP	HL	;10t
							;68t total
		$NEXTHL

;   doLOOP	( -- ) ( R: loop-sys1 -- | loop-sys2 )
;		Run time routine for LOOP.

		$CODE	COMPO+6,'doLOOP',DoLOOP,_SLINK
		DB	0DDh,0E5h ; PUSH IX	; 15t
		DB	0E1h	; POP	HL	;10t
		DB	034h	; INC	(HL)	;11t
		DB	028h	; JR Z,DOLOOP2	;12/7t a fast dec is ok, only
		DB	DOLOOP2-DOLOOP1 	;      failed every 255 time
DOLOOP1 	DB	01Ah	; LD	A,(DE)	; 7t go back to the loop
		DB	06Fh	; LD	L,A	; 4t
		DB	013h	; INC	DE	; 6t
		DB	01Ah	; LD	A,(DE)	; 7t
		DB	067h	; LD	H,A	; 4t
		$NEXTHL
DOLOOP2 	DB	023h	; INC	HL	; 6t
		DB	034h	; INC	(HL)	;11t
		DB	0E2h	; JP PO,DOLOOP1 ;10/7t ?loop end
		DW	DOLOOP1
		DB	0EBh	; EX	DE,HL	; 4t yes,continue past the branch offset
		DB	011h	; LD	DE,4	;10t clear return stack
		DW	4
		DB	0DDh,019h ; ADD IX,DE	;15h
		DB	023h	; INC	HL	; 6t
		DB	023h	; INC	HL	; 6t
		$NEXTHL

;   do+LOOP	( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;		Run time routine for +LOOP.

		$CODE	COMPO+7,'do+LOOP',DoPLOOP,_SLINK
		DB	0E1h	; POP	HL	;10t this will be the new TOS
		DB	0C5h	; PUSH	BC	;11t
		DB	044h	; LD	B,H	; 4t
		DB	04Dh	; LD	C,L	; 4t
		DB	0D9h	; EXX		; 4t
		DB	0C1h	; POP	BC	;10t old TOS = loop increment
		DB	0DDh,06Eh,0 ;LD L,(IX+0);19t
		DB	0DDh,066h,1 ;LD H,(IX+1);19t
		DB	0B7h	; OR	A	; 4t clear carry
		DB	0EDh,04Ah ; ADC HL,BC	;15t
		DB	0EAh	; JP PE,DOPLP1	;10/7t ?loop end
		DW	DOPLP1
		DB	0DDh,075h,0 ;LD (IX+0),L;19t   no, go back
		DB	0DDh,074h,1 ;LD (IX+1),H;19t
		DB	0D9h	; EXX		; 4t
		DB	01Ah	; LD	A,(DE)	; 7t go back to the loop
		DB	06Fh	; LD	L,A	; 4t
		DB	013h	; INC	DE	; 6t
		DB	01Ah	; LD	A,(DE)	; 7t
		DB	067h	; LD	H,A	; 4t
		$NEXTHL
DOPLP1		DB	001h	; LD	BC,4	;10t clear return stack
		DW	4
		DB	0DDh,009h ; ADD IX,BC	;15h
		DB	0D9h	; EXX		; 4t
		DB	013h	; INC	DE	; 6t yes,continue past the branch offset
		DB	013h	; INC	DE	; 6t
		$NEXT

;   0branch	( flag -- )
;		Branch if flag is zero.

		$CODE	COMPO+7,'0branch',ZBranch,_SLINK
		DB	078h	; LD	A,B	; 4t
		DB	0B1h	; OR	C	; 4t
		DB	0C1h	; POP	BC	;10t
		DB	0CAh	; JP   Z,Branch ;10/7t a fast dec is ok, only
		DW	Branch	;		       failed every 255 time
ZBRAN1		DB	013h	; INC	DE	; 6t yes,continue past the branch offset
		DB	013h	; INC	DE	; 6t
		$NEXT

;   branch	( -- )
;		Branch to an inline address.

		$CODE	COMPO+6,'branch',Branch,_SLINK
		DB	01Ah	; LD	A,(DE)	; 7t go back to the loop
		DB	06Fh	; LD	L,A	; 4t
		DB	013h	; INC	DE	; 6t
		DB	01Ah	; LD	A,(DE)	; 7t
		DB	067h	; LD	H,A	; 4t
		$NEXTHL

;   rp@ 	( -- a-addr )
;		Push the current RP to the data stack.

		$CODE	COMPO+3,'rp@',RPFetch,_SLINK
		DB	0C5h	; PUSH	BC	;11t
		DB	0DDh,0E5h ; PUSH IX	;15t
		DB	0C1h	; POP	BC	;10t
		$NEXT

;   rp! 	( a-addr -- )
;		Set the return stack pointer.

		$CODE	COMPO+3,'rp!',RPStore,_SLINK
		DB	0C5h	; PUSH	BC	;11t
		DB	0DDh,0E1h ; POP IX	;14t
		DB	0C1h	; POP	BC	;10t
		$NEXT

;   sp@ 	( -- a-addr )
;		Push the current data stack pointer.

		$CODE	3,'sp@',SPFetch,_SLINK
		DB	0C5h	; PUSH	BC	;11t
		DB	021h	; LD	HL,0	;10t
		DW	0
		DB	039h	; ADD	HL,SP	;11t
		DB	044h	; LD	B,H	; 4t
		DB	04Dh	; LD	C,L	; 4t
		$NEXT

;   sp! 	( a-addr -- )
;		Set the data stack pointer.

		$CODE	3,'sp!',SPStore,_SLINK
		DB	060h	; LD	H,B	; 4t
		DB	069h	; LD	L,C	; 4t
		DB	0F9h	; LD	SP,HL	; 6t
		DB	0C1h	; POP	BC	;10t
		$NEXT

;   um+ 	( u1 u2 -- u3 1|0 )
;		Add two unsigned numbers, return the sum and carry.

		$CODE	3,'um+',UMPlus,_SLINK
		DB	0E1h	; POP	HL	;10t
		DB	009h	; ADD	HL,BC	;11t
		DB	0E5h	; PUSH	HL	;11t
		DB	001h	; LD	BC,0	;10t
		DW	0
		DB	030h	; JR	NC,UMP1 ;12/7t
		DB	UMP1-UMP2
UMP2		DB	003h	; INC	BC	; 6t
UMP1:		$NEXT

;   1chars/	( n1 -- n2 )
;		Calculate number of chars for n1 address units.
;
;   : 1chars/	1 CHARS / ;	\ slow, very portable
;   : 1chars/	;		\ fast, must be redefined for each system

		$COLON	7,'1chars/',OneCharsSlash,_SLINK
		DW	EXIT

;;;;;;;;;;;;;;;;
; Standard words - Processor-dependent definitions
;	16 bit Forth for Z80
;;;;;;;;;;;;;;;;

; No need to align address for 8 bit processor

;   ALIGN	( -- )				\ CORE
;		Align the data space pointer.
;
;   : ALIGN	;

		$COLON	5,'ALIGN',ALIGNN,_FLINK
		DW	EXIT

;   ALIGNED	( addr -- a-addr )		   \ CORE
;		Align address to the cell boundary.
;
;   : ALIGNED	;

		$COLON	7,'ALIGNED',ALIGNED,_FLINK
		DW	EXIT

; pack" is dependent of cell alignment.
;
;   pack"       ( c-addr u a-addr -- a-addr2 )
;		Place a string c-addr u at a-addr and gives the next
;		cell-aligned address. Fill the rest of the last cell with
;		null character.
;
;   : pack"     2DUP SWAP CHARS + CHAR+ DUP >R  \ ca u aa aa+u+1
;		1 CHARS - 0 SWAP !		\ fill 0 at the end of string
;		2DUP C! CHAR+ SWAP		\ c-addr a-addr+1 u
;		CHARS MOVE R> ; COMPILE-ONLY

		$COLON	5,'pack"',PackQuote,_SLINK
		DW	TwoDUP,SWAP,CHARS,Plus,CHARPlus,DUPP,ToR
		DW	DoLIT,CHARR,Minus,Zero,SWAP,Store
		DW	TwoDUP,CStore,CHARPlus,SWAP
		DW	CHARS,MOVE,RFrom,EXIT

;   CELLS	( n1 -- n2 )			\ CORE
;		Calculate number of address units for n1 cells.
;
;   : CELLS	cell-size * ;	\ slow, very portable
;   : CELLS	2* ;		\ fast, must be redefined for each system

		$COLON	5,'CELLS',CELLS,_FLINK
		DW	TwoStar,EXIT

;   CHARS	( n1 -- n2 )			\ CORE
;		Calculate number of address units for n1 characters.
;
;   : CHARS	char-size * ;	\ slow, very portable
;   : CHARS	;		\ fast, must be redefined for each system

		$COLON	5,'CHARS',CHARS,_FLINK
		DW	EXIT

;   !		( x a-addr -- ) 		\ CORE
;		Store x at a aligned address.

		$CODE	1,'!',Store,_FLINK
		DB	060h	; LD	H,B
		DB	069h	; LD	L,C
		DB	0C1h	; POP	BC
		DB	071h	; LD	(HL),C
		DB	023h	; INC	HL
		DB	070h	; LD	(HL),B
		DB	0C1h	; POP	BC
		$NEXT

;   0<		( x -- flag )			\ CORE
;		Return true if n is negative.

		$CODE	2,'0<',ZeroLess,_FLINK
		DB	0CBh,020h ; SLA B	; sign bit -> cy flag
		DB	09Fh	; SBC	A,A	; propagate cy through A
		DB	047h	; LD	B,A	; put 0000 or FFFF in TOS
		DB	04Fh	; LD	C,A
		$NEXT

;   0=		( x -- flag )			\ CORE
;		Return true if x is zero.

		$CODE	2,'0=',ZeroEquals,_FLINK
		DB	078h	; LD	A,B
		DB	0B1h	; OR	C	; result=0 if bc was 0
		DB	0D6h,1	; SUB	1	; cy set   if bc was 0
		DB	09Fh	; SBC	A,A	; propagate cy through A
		DB	047h	; LD	B,A	; put 0000 or FFFF in TOS
		DB	04Fh	; LD	C,A
		$NEXT

;   2*		( x1 -- x2 )			\ CORE
;		Bit-shift left, filling the least significant bit with 0.

		$CODE	2,'2*',TwoStar,_FLINK
		DB	0CBh,021h ; SLA C
		DB	0CBh,010h ; RL	B
		$NEXT

;   2/		( x1 -- x2 )			\ CORE
;		Bit-shift right, leaving the most significant bit unchanged.

		$CODE	2,'2/',TwoSlash,_FLINK
		DB	0CBh,028h ; SRA B
		DB	0CBh,019h ; RR	C
		$NEXT

;   >R		( x -- ) ( R: -- x )		\ CORE
;		Move top of the data stack item to the return stack.

		$CODE	COMPO+2,'>R',ToR,_FLINK
		DB	0DDh,2Bh	; DEC  IX
		DB	0DDh,070h,0	; LD  (IX+0),B
		DB	0DDh,2Bh	; DEC  IX
		DB	0DDh,071h,0	; LD  (IX+0),C
		DB	0C1h		; POP	BC
		$NEXT

;   @		( a-addr -- x ) 		\ CORE
;		Push the contents at a-addr to the data stack.

		$CODE	1,'@',Fetch,_FLINK
		DB	060h	; LD	H,B
		DB	069h	; LD	L,C
		DB	04Eh	; LD	C,(HL)
		DB	023h	; INC	HL
		DB	046h	; LD	B,(HL)
		$NEXT

;   AND 	( x1 x2 -- x3 ) 		\ CORE
;		Bitwise AND.

		$CODE	3,'AND',ANDD,_FLINK
		DB	0E1h	; POP	HL
		DB	078h	; LD	A,B
		DB	0A4h	; AND	H
		DB	047h	; LD	B,A
		DB	079h	; LD	A,C
		DB	0A5h	; AND	L
		DB	04Fh	; LD	C,A
		$NEXT

;   C!		( char c-addr -- )		\ CORE
;		Store char at c-addr.

		$CODE	2,'C!',CStore,_FLINK
		DB	060h	; LD	H,B
		DB	069h	; LD	L,C
		DB	0C1h	; POP	BC
		DB	071h	; LD	(HL),C
		DB	0C1h	; POP	BC
		$NEXT

;   C@		( c-addr -- char )		\ CORE
;		Fetch the character stored at c-addr.

		$CODE	2,'C@',CFetch,_FLINK
		DB	00Ah	; LD	A,(BC)
		DB	04Fh	; LD	C,A
		DB	006h,0	; LD	B,0
		$NEXT

;   DROP	( x -- )			\ CORE
;		Discard top stack item.

		$CODE	4,'DROP',DROP,_FLINK
		DB	0C1h	; POP	BC
		$NEXT

;   DUP 	( x -- x x )			\ CORE
;		Duplicate the top stack item.

		$CODE	3,'DUP',DUPP,_FLINK
		DB	0C5h	; PUSH	BC
		$NEXT

;   EXECUTE	( i*x xt -- j*x )		\ CORE
;		Perform the semantics indentified by execution token, xt.

		$CODE	7,'EXECUTE',EXECUTE,_FLINK
		DB	060h	; LD	H,B
		DB	069h	; LD	L,C
		DB	0C1h	; POP	BC
		DB	0E9h	; JP	(HL)

;   EXIT	( -- ) ( R: nest-sys -- )	\ CORE
;		Return control to the calling definition.

		$CODE	COMPO+4,'EXIT',EXIT,_FLINK
		DB	0DDh,06Eh,0	; LD	L,(IX+0)
		DB	0DDh,023h	; INC	IX
		DB	0DDh,066h,0	; LD	H,(IX+0)
		DB	0DDh,023h	; INC	IX
		$NEXTHL

;   MOVE	( addr1 addr2 u -- )		\ CORE
;		Copy u address units from addr1 to addr2 if u is greater
;		than zero. This word is CODE defined since no other Standard
;		words can handle address unit directly.

		$CODE	4,'MOVE',MOVE,_FLINK
		DB	078h	; LD	A,B
		DB	0B1h	; OR	C
		DB	028h	; JR	Z,SAMEQ1
		DB	MOVE6-MOVE7
MOVE7		DB	0C5h	; PUSH	BC
		DB	0D9h	; EXX
		DB	0C1h	; POP	BC	; count
		DB	0D1h	; POP	DE	; destination addr
		DB	0E1h	; POP	HL	; source addr
		DB	078h	; LD	A,B
		DB	0B1h	; OR	C
		DB	028h	; JR	Z,MOVE1
		DB	MOVE1-MOVE2
MOVE2		DB	0EDh,052h ; SBC HL,DE	; carry is clear
		DB	038h	; JR	C,MOVE4
		DB	MOVE4-MOVE3
MOVE3		DB	019h	; ADD	HL,DE
		DB	0EDh,0B0h ; LDIR   ; source adr is larger than dest adr
MOVE1		DB	0D9h	; EXX
		DB	0C1h	; POP	BC
		$NEXT
MOVE6		DB	0C1h	; POP	BC
		DB	0C1h	; POP	BC
		DB	0C1h	; POP	BC
		$NEXT
MOVE4		DB	019h	; ADD	HL,DE
		DB	009h	; ADD	HL,BC
		DB	02Bh	; DEC	HL
		DB	0EBh	; EX	DE,HL
		DB	009h	; ADD	HL,BC
		DB	02Bh	; DEC	HL
		DB	0EBh	; EX	DE,HL
		DB	0EDh,0B8h ; LDDR
		DB	0D9h	; EXX
		DB	0C1h	; POP	BC
		$NEXT

;   OR		( x1 x2 -- x3 ) 		\ CORE
;		Return bitwise inclusive-or of x1 with x2.

		$CODE	2,'OR',ORR,_FLINK
		DB	0E1h	; POP	HL
		DB	078h	; LD	A,B
		DB	0B4h	; OR	H
		DB	047h	; LD	B,A
		DB	079h	; LD	A,C
		DB	0B5h	; OR	L
		DB	04Fh	; LD	C,A
		$NEXT

;   OVER	( x1 x2 -- x1 x2 x1 )		\ CORE
;		Copy second stack item to top of the stack.

		$CODE	4,'OVER',OVER,_FLINK
		DB	0E1h	; POP	HL
		DB	0E5h	; PUSH	HL
		DB	0C5h	; PUSH	BC
		DB	044h	; LD	B,H
		DB	04Dh	; LD	C,L
		$NEXT

;   R>		( -- x ) ( R: x -- )		\ CORE
;		Move x from the return stack to the data stack.

		$CODE	COMPO+2,'R>',RFrom,_FLINK
		DB	0C5h		; PUSH	BC
		DB	0DDh,04Eh,0	; LD	C,(IX+0)
		DB	0DDh,023h	; INC	IX
		DB	0DDh,046h,0	; LD	B,(IX+0)
		DB	0DDh,023h	; INC	IX
		$NEXT

;   R@		( -- x ) ( R: x -- x )		\ CORE
;		Copy top of return stack to the data stack.

		$CODE	COMPO+2,'R@',RFetch,_FLINK
		DB	0C5h		; PUSH	BC
		DB	0DDh,04Eh,0	; LD	C,(IX+0)
		DB	0DDh,046h,1	; LD	B,(IX+1)
		$NEXT

;   SWAP	( x1 x2 -- x2 x1 )		\ CORE
;		Exchange top two stack items.

		$CODE	4,'SWAP',SWAP,_FLINK
		DB	0E1h	; POP	HL
		DB	0C5h	; PUSH	BC
		DB	044h	; LD	B,H
		DB	04Dh	; LD	C,L
		$NEXT

;   UM/MOD	( ud u1 -- u2 u3 )		\ CORE
;		Unsigned division of a double-cell number ud by a single-cell
;		number u1. Return remainder u2 and quotient u3.
;
;   : UM/MOD	DUP 0= IF -10 THROW THEN	\ divide by zero
;		2DUP U< IF
;		   NEGATE cell-size-in-bits 0
;		   DO	>R DUP um+ >R >R DUP um+ R> + DUP
;			R> R@ SWAP >R um+ R> OR
;			IF >R DROP 1+ R> THEN
;			ELSE DROP THEN
;			R>
;		   LOOP DROP SWAP EXIT
;		ELSE -11 THROW		\ result out of range
;		THEN ;
;
;		  $COLON  6,'UM/MOD',UMSlashMOD,_FLINK
;		  DW	  DUPP,ZBranch,UMM5
;		  DW	  TwoDUP,ULess,ZBranch,UMM4
;		  DW	  NEGATE,DoLIT,CELLL*8,Zero,DoDO
; UMM1		  DW	  ToR,DUPP,UMPlus,ToR,ToR,DUPP,UMPlus,RFrom,Plus,DUPP
;		  DW	  RFrom,RFetch,SWAP,ToR,UMPlus,RFrom,ORR,ZBranch,UMM2
;		  DW	  ToR,DROP,OnePlus,RFrom,Branch,UMM3
; UMM2		  DW	  DROP
; UMM3		  DW	  RFrom,DoLOOP,UMM1
;		  DW	  DROP,SWAP,EXIT
; UMM5		  DW	  DoLIT,-10,THROW
; UMM4		  DW	  DoLIT,-11,THROW

		$CODE	6,'UM/MOD',UMSlashMOD,_FLINK
		DB	078h		; LD	A,B
		DB	0B1h		; OR	C
		DB	028h		; JR	Z,UMMOD2	;?zero divisor
		DB	UMMOD2-UMMOD11
UMMOD11 	DB	0C5h		; PUSH	BC
		DB	0D9h		; EXX
		DB	0C1h		; POP	BC
		DB	0E1h		; POP	HL
		DB	0D1h		; POP	DE
		DB	07Dh		; LD	A,L
		DB	091h		; SUB	C
		DB	07Ch		; LD	A,H
		DB	098h		; SBC	A,B
		DB	030h		; JR	NC,UMMOD1	;?too big
		DB	UMMOD1-UMMOD12
UMMOD12 	DB	07Ch		; LD	A,H
		DB	065h		; LD	H,L
		DB	06Ah		; LD	L,D
		DB	016h,8		; LD	D,8
		DB	0D5h		; PUSH	DE
		DB	0CDh		; CALL	UMMOD3
		DW	UMMOD3
		DB	0D1h		; POP	DE
		DB	0E5h		; PUSH	HL
		DB	06Bh		; LD	L,E
		DB	0CDh		; CALL	UMMOD3
		DW	UMMOD3
		DB	057h		; LD	D,A
		DB	05Ch		; LD	E,H
		DB	0C1h		; POP	BC
		DB	061h		; LD	H,C
		DB	0D5h		; PUSH	DE
		DB	0E5h		; PUSH	HL
		DB	0D9h		; EXX
		DB	0C1h		; POP	BC
		$NEXT
UMMOD2		DB	001h		; LD	BC,-10 ; divide by 0
		DW	-10
		DB	0C3h		; JP	THROW
		DW	THROW
UMMOD1		DB	001h		; LD	BC,-10 ; result out of range
		DW	-11
		DB	0C3h		; JP	THROW
		DW	THROW
UMMOD4		DB	05Fh		; LD	E,A
		DB	07Ch		; LD	A,H
		DB	091h		; SUB	C
		DB	067h		; LD	H,A
		DB	07Bh		; LD	A,E
		DB	098h		; SBC	A,B
		DB	030h		; JR	NC,UMMOD5
		DB	UMMOD5-UMMOD13
UMMOD13 	DB	07Ch		; LD	A,H
		DB	081h		; ADD	A,C
		DB	067h		; LD	H,A
		DB	07Bh		; LD	A,E
		DB	015h		; DEC	D
		DB	0C8h		; RET	Z
UMMOD3		DB	029h		; ADD	HL,HL
		DB	017h		; RLA
		DB	030h		; JR	NC,UMMOD4
		DB	UMMOD4-UMMOD14
UMMOD14 	DB	05Fh		; LD	E,A
		DB	07Ch		; LD	A,H
		DB	091h		; SUB	C
		DB	067h		; LD	H,A
		DB	07Bh		; LD	A,E
		DB	098h		; SBC	A,B
UMMOD5		DB	02Ch		; INC	L
		DB	015h		; DEC	D
		DB	020h		; JR	NZ,UMMOD3
		DB	UMMOD3-UMMOD15
UMMOD15 	DB	0C9h		; RET

;   UM* 	( u1 u2 -- ud ) 		\ CORE
;		Unsigned multiply. Return double-cell product.
;
;   : UM*	0 SWAP cell-size-in-bits 0 DO
;		   DUP um+ >R >R DUP um+ R> +
;		   R> IF >R OVER um+ R> + THEN	   \ if carry
;		LOOP ROT DROP ;
;
;		  $COLON  3,'UM*',UMStar,_FLINK
;		  DW	  Zero,SWAP,DoLIT,CELLL*8,Zero,DoDO
; UMST1 	  DW	  DUPP,UMPlus,ToR,ToR
;		  DW	  DUPP,UMPlus,RFrom,Plus,RFrom
;		  DW	  ZBranch,UMST2
;		  DW	  ToR,OVER,UMPlus,RFrom,Plus
; UMST2 	  DW	  DoLOOP,UMST1
;		  DW	  ROT,DROP,EXIT

		$CODE	3,'UM*',UMStar,_FLINK
		DB	0EBh		; EX	DE,HL
		DB	0E3h		; EX	(SP),HL
		DB	0EBh		; EX	DE,HL
		DB	021h		; LD	HL,0
		DW	0
		DB	03Eh,010h	; LD	A,010h
UMSTAR1 	DB	029h		; ADD	HL,HL
		DB	0EBh		; EX	DE,HL
		DB	0EDh,06Ah	; ADC	HL,HL
		DB	0EBh		; EX	DE,HL
		DB	030h		; JR	NC,UMSTAR3
		DB	UMSTAR3-UMSTAR2
UMSTAR2 	DB	009h		; ADD	HL,BC
		DB	030h		; JR	NC,UMSTAR3
		DB	UMSTAR3-UMSTAR5
UMSTAR5 	DB	013h		; INC	DE
UMSTAR3 	DB	03Dh		; DEC	A
		DB	020h		; JR	NZ,UMSTAR1
		DB	UMSTAR1-UMSTAR4
UMSTAR4 	DB	042h		; LD	B,D
		DB	04Bh		; LD	C,E
		DB	0E3h		; EX	(SP),HL
		$NEXTHL

;   XOR 	( x1 x2 -- x3 ) 		\ CORE
;		Bitwise exclusive OR.

		$CODE	3,'XOR',XORR,_FLINK
		DB	0E1h	; POP	HL
		DB	078h	; LD	A,B
		DB	0ACh	; XOR	H
		DB	047h	; LD	B,A
		DB	079h	; LD	A,C
		DB	0ADh	; XOR	L
		DB	04Fh	; LD	C,A
		$NEXT

;;;;;;;;;;;;;;;;
; System constants and variables
;;;;;;;;;;;;;;;;

;   #order0	( -- a-addr )
;		Start address of default search order.

		$CONST	7,'#order0',NumberOrder0,AddrNumberOrder0,_SLINK

;   'ekey?      ( -- a-addr )
;		Execution vector of EKEY?.

		$VALUE	6,"'ekey?",TickEKEYQ,ValueTickEKEYQ,_SLINK

;   'ekey       ( -- a-addr )
;		Execution vector of EKEY.

		$VALUE	5,"'ekey",TickEKEY,ValueTickEKEY,_SLINK

;   'emit?      ( -- a-addr )
;		Execution vector of EMIT?.

		$VALUE	6,"'emit?",TickEMITQ,ValueTickEMITQ,_SLINK

;   'emit       ( -- a-addr )
;		Execution vector of EMIT.

		$VALUE	5,"'emit",TickEMIT,ValueTickEMIT,_SLINK

;   'init-i/o   ( -- a-addr )
;		Execution vector to initialize input/output devices.

		$VALUE	9,"'init-i/o",TickINIT_IO,ValueTickINIT_IO,_SLINK

;   'prompt     ( -- a-addr )
;		Execution vector of '.prompt'.

		$VALUE	7,"'prompt",TickPrompt,ValueTickPrompt,_SLINK

;   'boot       ( -- a-addr )
;		Execution vector of COLD.

		$VALUE	5,"'boot",TickBoot,ValueTickBoot,_SLINK

;   SOURCE-ID	( -- 0 | -1 )			\ CORE EXT
;		Identify the input source. -1 for string (via EVALUATE) and
;		0 for user input device.

		$VALUE	9,'SOURCE-ID',SOURCE_ID,ValueSOURCE_ID,_FLINK
AddrSOURCE_ID	EQU	$-CELLL

;   HERE	( -- addr )			\ CORE
;		Return data space pointer.

		$VALUE	4,'HERE',HERE,ValueHERE,_FLINK
AddrHERE	EQU	$-CELLL

;   'doWord     ( -- a-addr )
;		Execution vectors for 'interpret'.

		$CONST	7,"'doWord",TickDoWord,AddrTickDoWord,_SLINK

;   BASE	( -- a-addr )			\ CORE
;		Return the address of the radix base for numeric I/O.

		$CONST	4,'BASE',BASE,AddrBASE,_FLINK

;   THROWMsgTbl ( -- a-addr )			\ CORE
;		Return the address of the THROW message table.

		$CONST	11,'THROWMsgTbl',THROWMsgTbl,AddrTHROWMsgTbl,_SLINK

;   memTop	( -- a-addr )
;		Top of free RAM area.

		$VALUE	6,'memTop',MemTop,?,_SLINK
AddrMemTop	EQU	$-CELLL

;   bal 	( -- n )
;		Return the depth of control-flow stack.

		$VALUE	3,'bal',Bal,?,_SLINK
AddrBal 	EQU	$-CELLL

;   notNONAME?	( -- f )
;		Used by ';' whether to do 'linkLast' or not

		$VALUE	10,'notNONAME?',NotNONAMEQ,?,_SLINK
AddrNotNONAMEQ	EQU	$-CELLL

;   rakeVar	( -- a-addr )
;		Used by 'rake' to gather LEAVE.

		$CONST	7,'rakeVar',RakeVar,AddrRakeVar,_SLINK

;   #order	( -- a-addr )
;		Hold the search order stack depth.

		$CONST	6,'#order',NumberOrder,AddrNumberOrder,_SLINK

;   current	( -- a-addr )
;		Point to the wordlist to be extended.

		$CONST	7,'current',Current,AddrCurrent,_SLINK

;   FORTH-WORDLIST   ( -- wid ) 		\ SEARCH
;		Return wid of Forth wordlist.

		$CONST	14,'FORTH-WORDLIST',FORTH_WORDLIST,AddrFORTH_WORDLIST,_FLINK
FORTH_WORDLISTName	EQU	_NAME-0

;   NONSTANDARD-WORDLIST   ( -- wid )
;		Return wid of non-standard wordlist.

		$CONST	20,'NONSTANDARD-WORDLIST',NONSTANDARD_WORDLIST,AddrNONSTANDARD_WORDLIST,_FLINK
NONSTANDARD_WORDLISTName EQU	_NAME-0

;   envQList	( -- wid )
;		Return wid of ENVIRONMENT? string list. Never put this wid in
;		search-order. It should be used only by SET-CURRENT to add new
;		environment query string after addition of a complete wordset.

		$CONST	8,'envQList',EnvQList,AddrEnvQList,_SLINK

;   userP	( -- a-addr )
;		Return address of USER variable area of current task.

		$CONST	5,'userP',UserP,AddrUserP,_SLINK

;   SystemTask	( -- a-addr )
;		Return system task's tid.

		$CONST	10,'SystemTask',SystemTask,SysTask,_SLINK
SystemTaskName	EQU	_NAME-0

;   follower	( -- a-addr )
;		Point next task's 'status' USER variable.

		$USER	8,'follower',Follower,SysFollower-SysUserP,_SLINK

;   status	( -- a-addr )
;		Status of current task. Point 'pass' or 'wake'.

		$USER	6,'status',Status,SysStatus-SysUserP,_SLINK

;   stackTop	( -- a-addr )
;		Store current task's top of stack position.

		$USER	8,'stackTop',StackTop,SysStackTop-SysUserP,_SLINK

;   throwFrame	( -- a-addr )
;		THROW frame for CATCH and THROW need to be saved for eack task.

		$USER	10,'throwFrame',ThrowFrame,SysThrowFrame-SysUserP,_SLINK

;   taskName	( -- a-addr )
;		Current task's task ID.

		$USER	8,'taskName',TaskName,SysTaskName-SysUserP,_SLINK

;   user1	( -- a-addr )
;		One free USER variable for each task.

		$USER	5,'user1',User1,SysUser1-SysUserP,_SLINK

; ENVIRONMENT? strings can be searched using SEARCH-WORDLIST and can be
; EXECUTEd. This wordlist is completely hidden to Forth system except
; ENVIRONMENT? .

		$ENVIR	3,'CPU'
		DW	DoLIT,CPUStr,COUNT,EXIT

		$ENVIR	5,'model'
		DW	DoLIT,ModelStr,COUNT,EXIT

		$ENVIR	7,'version'
		DW	DoLIT,VersionStr,COUNT,EXIT

		$ENVIR	15,'/COUNTED-STRING'
		DW	DoLIT,MaxChar,EXIT

		$ENVIR	5,'/HOLD'
		DW	DoLIT,PADSize,EXIT

		$ENVIR	4,'/PAD'
		DW	DoLIT,PADSize,EXIT

		$ENVIR	17,'ADDRESS-UNIT-BITS'
		DW	DoLIT,8,EXIT

		$ENVIR	4,'CORE'
		DW	DoLIT,TRUEE,EXIT

		$ENVIR	7,'FLOORED'
		DW	DoLIT,TRUEE,EXIT

		$ENVIR	8,'MAX-CHAR'
		DW	DoLIT,MaxChar,EXIT	;max value of character set

		$ENVIR	5,'MAX-D'
		DW	DoLIT,MaxUnsigned,DoLIT,MaxSigned,EXIT

		$ENVIR	5,'MAX-N'
		DW	DoLIT,MaxSigned,EXIT

		$ENVIR	5,'MAX-U'
		DW	DoLIT,MaxUnsigned,EXIT

		$ENVIR	6,'MAX-UD'
		DW	DoLIT,MaxUnsigned,DoLIT,MaxUnsigned,EXIT

		$ENVIR	18,'RETURN-STACK-CELLS'
		DW	DoLIT,RTCells,EXIT

		$ENVIR	11,'STACK-CELLS'
		DW	DoLIT,DTCells,EXIT

		$ENVIR	9,'EXCEPTION'
		DW	DoLIT,TRUEE,EXIT

		$ENVIR	13,'EXCEPTION-EXT'
		DW	DoLIT,TRUEE,EXIT

		$ENVIR	9,'WORDLISTS'
		DW	DoLIT,OrderDepth,EXIT

;;;;;;;;;;;;;;;;
; Non-Standard words - Colon definitions
;;;;;;;;;;;;;;;;

;   (')         ( "<spaces>name" -- xt 1 | xt -1 )
;		Parse a name, find it and return execution token and
;		-1 or 1 ( IMMEDIATE) if found
;
;   : (')       PARSE-WORD search-word ?DUP IF NIP EXIT THEN
;		errWord 2!	\ if not found error
;		-13 THROW ;	\ undefined word

		$COLON	3,"(')",ParenTick,_SLINK
		DW	PARSE_WORD,Search_word,QuestionDUP,ZBranch,PTICK1
		DW	NIP,EXIT
PTICK1		DW	ErrWord,TwoStore,DoLIT,-13,THROW

;   (d.)	( d -- c-addr u )
;		Convert a double number to a string.
;
;   : (d.)	SWAP OVER  DUP 0< IF  DNEGATE  THEN
;		<#  #S ROT SIGN  #> ;

		$COLON	4,'(d.)',ParenDDot,_SLINK
		DW	SWAP,OVER,DUPP,ZeroLess,ZBranch,PARDD1
		DW	DNEGATE
PARDD1		DW	LessNumberSign,NumberSignS,ROT
		DW	SIGN,NumberSignGreater,EXIT

;   .ok 	( -- )
;		Display 'ok'.
;
;   : .ok	." ok" ;

		$COLON	3,'.ok',DotOK,_SLINK
		$INSTR	'ok'
		DW	TYPEE,EXIT

;   .prompt	    ( -- )
;		Disply Forth prompt. This word is vectored.
;
;   : .prompt	'prompt EXECUTE ;

		$COLON	7,'.prompt',DotPrompt,_SLINK
		DW	TickPrompt,EXECUTE,EXIT

;   0		( -- 0 )
;		Return zero.

		$CONST	1,'0',Zero,0,_SLINK

;   1		( -- 1 )
;		Return one.

		$CONST	1,'1',One,1,_SLINK

;   -1		( -- -1 )
;		Return -1.

		$CONST	2,'-1',MinusOne,-1,_SLINK

;   abort"msg   ( -- a-addr )
;		Abort" error message string address.

		$VAR	9,'abort"msg',AbortQMsg,2,_SLINK

;   bal+	( -- )
;		Increase bal by 1.
;
;   : bal+	bal 1+ TO bal ;

		$COLON	4,'bal+',BalPlus,_SLINK
		DW	Bal,OnePlus,DoTO,AddrBal,EXIT

;   bal-	( -- )
;		Decrease bal by 1.
;
;   : bal-	bal 1- TO bal ;

		$COLON	4,'bal-',BalMinus,_SLINK
		DW	Bal,OneMinus,DoTO,AddrBal,EXIT

;   cell-	( a-addr1 -- a-addr2 )
;		Return previous aligned cell address.
;
;   : cell-	-(cell-size) + ;

		$COLON	5,'cell-',CellMinus,_SLINK
		DW	DoLIT,0-CELLL,Plus,EXIT

;   COMPILE-ONLY   ( -- )
;		Make the most recent definition an compile-only word.
;
;   : COMPILE-ONLY   lastName [ =comp ] LITERAL OVER @ OR SWAP ! ;

		$COLON	12,'COMPILE-ONLY',COMPILE_ONLY,_SLINK
		DW	LastName,DoLIT,COMPO,OVER,Fetch,ORR,SWAP,Store,EXIT

;   doS"        ( u -- c-addr u )
;		Run-time function of S" .
;
;   : doS"      R> SWAP 2DUP + ALIGNED >R ; COMPILE-ONLY

		$COLON	COMPO+4,'doS"',DoSQuote,_SLINK
		DW	RFrom,SWAP,TwoDUP,Plus,ALIGNED,ToR,EXIT

;   doDO	( n1|u1 n2|u2 -- ) ( R: -- n1 n2-n1-max_negative )
;		Run-time funtion of DO.
;
;   : doDO	>R max-negative + R> OVER - SWAP R> SWAP >R SWAP >R >R ;

		$COLON	COMPO+4,'doDO',DoDO,_SLINK
		DW	ToR,DoLIT,MaxNegative,Plus,RFrom
		DW	OVER,Minus,SWAP,RFrom,SWAP,ToR,SWAP,ToR,ToR,EXIT

;   errWord	( -- a-addr )
;		Last found word. To be used to display the word causing error.

		$VAR	7,'errWord',ErrWord,2,_SLINK

;   head,	( "<spaces>name" -- )
;		Parse a word and build a dictionary entry using a name.
;
;   : head,	PARSE-WORD DUP 0=
;		IF errWord 2! -16 THROW THEN
;				\ attempt to use zero-length string as a name
;		DUP =mask > IF -19 THROW THEN	\ definition name too long
;		2DUP GET-CURRENT SEARCH-WORDLIST  \ name exist?
;		IF DROP ." redefine " 2DUP TYPE SPACE THEN \ warn if redefined
;		HERE ALIGNED TO HERE		\ align
;		GET-CURRENT @ , 		\ build wordlist link
;		HERE DUP >R pack" TO HERE R>    \ pack the name in dictionary
;		DUP , TO lastName ;

		$COLON	5,'head,',HeadComma,_SLINK
		DW	PARSE_WORD,DUPP,ZBranch,HEADC1
		DW	DUPP,DoLIT,MASKK,GreaterThan,ZBranch,HEADC3
		DW	DoLIT,-19,THROW
HEADC3		DW	TwoDUP,GET_CURRENT,SEARCH_WORDLIST,ZBranch,HEADC2
		DW	DROP
		$INSTR	'redefine '
		DW	TYPEE,TwoDUP,TYPEE,SPACE
HEADC2		DW	HERE,ALIGNED,DoTO,AddrHERE
		DW	GET_CURRENT,Fetch,Comma
		DW	HERE,DUPP,ToR,PackQuote,DoTO,AddrHERE,RFrom
		DW	DUPP,Comma,DoTO,AddrLastName,EXIT
HEADC1		DW	ErrWord,TwoStore,DoLIT,-16,THROW

;   hld 	( -- a-addr )
;		Hold a pointer in building a numeric output string.

		$VAR	3,'hld',HLD,1,_SLINK

;   interpret	( i*x -- j*x )
;		Intrepret input string.
;
;   : interpret BEGIN  DEPTH 0< IF -4 THROW THEN	\ stack underflow
;		       PARSE-WORD DUP
;		WHILE  2DUP errWord 2!
;		       search-word	    \ ca u 0 | xt f -1 | xt f 1
;		       DUP IF
;			 SWAP STATE @ OR 0= \ compile-only in interpretation
;			 IF -14 THROW THEN  \ interpreting a compile-only word
;		       THEN
;		       1+ 2* STATE @ 1+ + CELLS 'doWord + @ EXECUTE
;		REPEAT 2DROP ;

		$COLON	9,'interpret',Interpret,_SLINK
INTERP1 	DW	DEPTH,ZeroLess,ZBranch,INTERP2
		DW	DoLIT,-4,THROW
INTERP2 	DW	PARSE_WORD,DUPP,ZBranch,INTERP3
		DW	TwoDUP,ErrWord,TwoStore
		DW	Search_word,DUPP,ZBranch,INTERP5
		DW	SWAP,STATE,Fetch,ORR,ZBranch,INTERP4
INTERP5 	DW	OnePlus,TwoStar,STATE,Fetch,OnePlus,Plus,CELLS
		DW	TickDoWord,Plus,Fetch,EXECUTE
		DW	Branch,INTERP1
INTERP3 	DW	TwoDROP,EXIT
INTERP4 	DW	DoLIT,-14,THROW

;   optiCOMPILE, ( xt -- )
;		Optimized COMPILE, . Reduce doLIST ... EXIT sequence if
;		xt is COLON definition which contains less than two words.
;
;   : optiCOMPILE,
;		DUP ?call ['] doLIST = IF
;		    DUP @ ['] EXIT = IF         \ if first word is EXIT
;		      2DROP EXIT THEN
;		    DUP CELL+ @ ['] EXIT = IF   \ if second word is EXIT
;		      @ DUP ['] doLIT XOR  \ make sure it is not literal value
;		      IF SWAP THEN THEN
;		THEN THEN DROP COMPILE, ;

		$COLON	12,'optiCOMPILE,',OptiCOMPILEComma,_SLINK
		DW	DUPP,QCall,DoLIT,DoLIST,Equals,ZBranch,OPTC2
		DW	DUPP,Fetch,DoLIT,EXIT,Equals,ZBranch,OPTC1
		DW	TwoDROP,EXIT
OPTC1		DW	DUPP,CELLPlus,Fetch,DoLIT,EXIT,Equals,ZBranch,OPTC2
		DW	Fetch,DUPP,DoLIT,DoLIT,XORR,ZBranch,OPTC2
		DW	SWAP
OPTC2		DW	DROP,COMPILEComma,EXIT

;   singleOnly	( c-addr u -- x )
;		Handle the word not found in the search-order. If the string
;		is legal, leave a single cell number in interpretation state.
;
;   : singleOnly
;		0 DUP 2SWAP OVER C@ [CHAR] -
;		= DUP >R IF 1 /STRING THEN
;		>NUMBER IF -13 THROW THEN	\ undefined word
;		2DROP R> IF NEGATE THEN ;

		$COLON	10,'singleOnly',SingleOnly,_SLINK
		DW	Zero,DUPP,TwoSWAP,OVER,CFetch,DoLIT,'-'
		DW	Equals,DUPP,ToR,ZBranch,SINGLEO4
		DW	One,SlashSTRING
SINGLEO4	DW	ToNUMBER,ZBranch,SINGLEO1
		DW	DoLIT,-13,THROW
SINGLEO1	DW	TwoDROP,RFrom,ZBranch,SINGLEO2
		DW	NEGATE
SINGLEO2	DW	EXIT

;   singleOnly, ( c-addr u -- )
;		Handle the word not found in the search-order. Compile a
;		single cell number in compilation state.
;
;   : singleOnly,
;		singleOnly LITERAL ;

		$COLON	11,'singleOnly,',SingleOnlyComma,_SLINK
		DW	SingleOnly,LITERAL,EXIT

;   (doubleAlso) ( c-addr u -- x 1 | x x 2 )
;		If the string is legal, leave a single or double cell number
;		and size of the number.
;
;   : (doubleAlso)
;		0 DUP 2SWAP OVER C@ [CHAR] -
;		= DUP >R IF 1 /STRING THEN
;		>NUMBER ?DUP
;		IF   1- IF -13 THROW THEN     \ more than one char is remained
;		     DUP C@ [CHAR] . XOR      \ last char is not '.'
;		     IF -13 THROW THEN	      \ undefined word
;		     R> IF DNEGATE THEN
;		     2 EXIT		  THEN
;		2DROP R> IF NEGATE THEN       \ single number
;		1 ;

		$COLON	12,'(doubleAlso)',ParenDoubleAlso,_SLINK
		DW	Zero,DUPP,TwoSWAP,OVER,CFetch,DoLIT,'-'
		DW	Equals,DUPP,ToR,ZBranch,DOUBLEA1
		DW	One,SlashSTRING
DOUBLEA1	DW	ToNUMBER,QuestionDUP,ZBranch,DOUBLEA4
		DW	OneMinus,ZBranch,DOUBLEA3
DOUBLEA2	DW	DoLIT,-13,THROW
DOUBLEA3	DW	CFetch,DoLIT,'.',Equals,ZBranch,DOUBLEA2
		DW	RFrom,ZBranch,DOUBLEA5
		DW	DNEGATE
DOUBLEA5	DW	DoLIT,2,EXIT
DOUBLEA4	DW	TwoDROP,RFrom,ZBranch,DOUBLEA6
		DW	NEGATE
DOUBLEA6	DW	One,EXIT

;   doubleAlso	( c-addr u -- x | x x )
;		Handle the word not found in the search-order. If the string
;		is legal, leave a single or double cell number in
;		interpretation state.
;
;   : doubleAlso
;		(doubleAlso) DROP ;

		$COLON	10,'doubleAlso',DoubleAlso,_SLINK
		DW	ParenDoubleAlso,DROP,EXIT

;   doubleAlso, ( c-addr u -- )
;		Handle the word not found in the search-order. If the string
;		is legal, compile a single or double cell number in
;		compilation state.
;
;   : doubleAlso,
;		(doubleAlso) 1- IF SWAP LITERAL THEN LITERAL ;

		$COLON	11,'doubleAlso,',DoubleAlsoComma,_SLINK
		DW	ParenDoubleAlso,OneMinus,ZBranch,DOUBC1
		DW	SWAP,LITERAL
DOUBC1		DW	LITERAL,EXIT

;   -.		( -- )
;		You don't need this word unless you care that '-.' returns
;		double cell number 0. Catching illegal number '-.' in this way
;		is easier than make 'interpret' catch this exception.
;
;   : -.	-13 THROW ; IMMEDIATE	\ undefined word

		$COLON	IMMED+2,'-.',MinusDot,_SLINK
		DW	DoLIT,-13,THROW

;   lastName	( -- c-addr )
;		Return the address of the last definition name.

		$VALUE	8,'lastName',LastName,?,_SLINK
AddrLastName	EQU	$-CELLL

;   linkLast	( -- )
;		Link the word being defined to the current wordlist.
;		Do nothing if the last definition is made by :NONAME .
;
;   : linkLast	lastName GET-CURRENT ! ;

		$COLON	8,'linkLast',LinkLast,_SLINK
		DW	LastName,GET_CURRENT,Store,EXIT

;   name>xt	( c-addr -- xt )
;		Return execution token using counted string at c-addr.
;
;   : name>xt	COUNT [ =MASK ] LITERAL AND + ALIGNED CELL+ ;

		$COLON	7,'name>xt',NameToXT,_SLINK
		DW	COUNT,DoLIT,MASKK,ANDD,Plus,ALIGNED,CELLPlus,EXIT

;   PARSE-WORD	( "<spaces>ccc<space>" -- c-addr u )
;		Skip leading spaces and parse a word. Return the name.
;
;   : PARSE-WORD   BL skipPARSE ;

		$COLON	10,'PARSE-WORD',PARSE_WORD,_SLINK
		DW	BLank,SkipPARSE,EXIT

;   pipe	( -- ) ( R: xt -- )
;		Connect most recently defined word to code following DOES>.
;		Structure of CREATEd word:
;		    | call-doCREATE | 0 or DOES> code addr | >BODY points here
;
;   : pipe	lastName name>xt ?call DUP IF	\ code-addr xt2
;		    ['] doCREATE = IF
;		    R> SWAP !		\ change DOES> code of CREATEd word
;		    EXIT
;		THEN THEN
;		-32 THROW	\ invalid name argument, no-CREATEd last name
;		; COMPILE-ONLY

		$COLON	COMPO+4,'pipe',Pipe,_SLINK
		DW	LastName,NameToXT,QCall,DUPP,ZBranch,PIPE1
		DW	DoLIT,DoCREATE,Equals,ZBranch,PIPE1
		DW	RFrom,SWAP,Store,EXIT
PIPE1		DW	DoLIT,-32,THROW

;   skipPARSE	( char "<chars>ccc<char>" -- c-addr u )
;		Skip leading chars and parse a word using char as a
;		delimeter. Return the name.
;
;   : skipPARSE
;		>R SOURCE >IN @ /STRING    \ c_addr u  R: char
;		DUP IF
;		   BEGIN  OVER C@ R@ =
;		   WHILE  1- SWAP CHAR+ SWAP DUP 0=
;		   UNTIL  R> DROP EXIT
;		   ELSE THEN
;		   DROP SOURCE DROP - 1chars/ >IN ! R> PARSE EXIT
;		THEN R> DROP ;

		$COLON	9,'skipPARSE',SkipPARSE,_SLINK
		DW	ToR,SOURCE,ToIN,Fetch,SlashSTRING
		DW	DUPP,ZBranch,SKPAR1
SKPAR2		DW	OVER,CFetch,RFetch,Equals,ZBranch,SKPAR3
		DW	OneMinus,SWAP,CHARPlus,SWAP
		DW	DUPP,ZeroEquals,ZBranch,SKPAR2
		DW	RFrom,DROP,EXIT
SKPAR3		DW	DROP,SOURCE,DROP,Minus,OneCharsSlash
		DW	ToIN,Store,RFrom,PARSE,EXIT
SKPAR1		DW	RFrom,DROP,EXIT

;   rake	( C: do-sys -- )
;		Gathers LEAVEs.
;
;   : rake	DUP , rakeVar @
;		BEGIN  2DUP U<
;		WHILE  DUP @ HERE ROT !
;		REPEAT rakeVar ! DROP
;		?DUP IF 		\ check for ?DO
;		   1 bal+ POSTPONE THEN \ orig type is 1
;		THEN bal- ; COMPILE-ONLY

		$COLON	COMPO+4,'rake',rake,_SLINK
		DW	DUPP,Comma,RakeVar,Fetch
RAKE1		DW	TwoDUP,ULess,ZBranch,RAKE2
		DW	DUPP,Fetch,HERE,ROT,Store,Branch,RAKE1
RAKE2		DW	RakeVar,Store,DROP
		DW	QuestionDUP,ZBranch,RAKE3
		DW	One,BalPlus,THENN
RAKE3		DW	BalMinus,EXIT

;   rp0 	( -- a-addr )
;		Pointer to bottom of the return stack.
;
;   : rp0	userP @ CELL+ CELL+ @ ;

		$COLON	3,'rp0',RPZero,_SLINK
		DW	UserP,Fetch,CELLPlus,CELLPlus,Fetch,EXIT

;   search-word ( c-addr u -- c-addr u 0 | xt f 1 | xt f -1)
;		Search dictionary for a match with the given name. Return
;		execution token, not-compile-only flag and -1 or 1
;		( IMMEDIATE) if found; c-addr u 0 if not.
;
;   : search-word
;		#order @ DUP			\ not found if #order is 0
;		IF 0
;		   DO 2DUP			\ ca u ca u
;		      I CELLS #order CELL+ + @	\ ca u ca u wid
;		      (search-wordlist) 	\ ca u; 0 | w f 1 | w f -1
;		      ?DUP IF			\ ca u; 0 | w f 1 | w f -1
;			 >R 2SWAP 2DROP R> UNLOOP EXIT \ xt f 1 | xt f -1
;		      THEN			\ ca u
;		   LOOP 0			\ ca u 0
;		THEN ;

		$COLON	11,'search-word',Search_word,_SLINK
		DW	NumberOrder,Fetch,DUPP,ZBranch,SEARCH1
		DW	Zero,DoDO
SEARCH2 	DW	TwoDUP,I,CELLS,NumberOrder,CELLPlus,Plus,Fetch
		DW	ParenSearch_Wordlist,QuestionDUP,ZBranch,SEARCH3
		DW	ToR,TwoSWAP,TwoDROP,RFrom,UNLOOP,EXIT
SEARCH3 	DW	DoLOOP,SEARCH2
		DW	Zero
SEARCH1 	DW	EXIT

;   sourceVar	( -- a-addr )
;		Hold the current count and address of the terminal input buffer.

		$VAR	9,'sourceVar',SourceVar,2,_SLINK

;   sp0 	( -- a-addr )
;		Pointer to bottom of the data stack.
;
;   : sp0	userP @ CELL+ @ ;

		$COLON	3,'sp0',SPZero,_SLINK
		DW	UserP,Fetch,CELLPlus,Fetch,EXIT

;
; Words for multitasking
;

;   PAUSE	( -- )
;		Stop current task and transfer control to the task of which
;		'status' USER variable is stored in 'follower' USER variable
;		of current task.
;
;   : PAUSE	rp@ sp@ stackTop !  follower @ >R ; COMPILE-ONLY

		$COLON	COMPO+5,'PAUSE',PAUSE,_SLINK
		DW	RPFetch,SPFetch,StackTop,Store,Follower,Fetch,ToR,EXIT

;   wake	( -- )
;		Wake current task.
;
;   : wake	R> userP !	\ userP points 'follower' of current task
;		stackTop @ sp!		\ set data stack
;		rp! ; COMPILE-ONLY	\ set return stack

		$COLON	COMPO+4,'wake',Wake,_SLINK
		DW	RFrom,UserP,Store,StackTop,Fetch,SPStore,RPStore,EXIT

;;;;;;;;;;;;;;;;
; Essential Standard words - Colon definitions
;;;;;;;;;;;;;;;;

;   #		( ud1 -- ud2 )			\ CORE
;		Extract one digit from ud1 and append the digit to
;		pictured numeric output string. ( ud2 = ud1 / BASE )
;
;   : # 	0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP
;		9 OVER < [ CHAR A CHAR 9 1 + - ] LITERAL AND +
;		[ CHAR 0 ] LITERAL + HOLD R> ;

		$COLON	1,'#',NumberSign,_FLINK
		DW	Zero,BASE,Fetch,UMSlashMOD,ToR,BASE,Fetch,UMSlashMOD
		DW	SWAP,DoLIT,9,OVER,LessThan,DoLIT,'A'-'9'-1,ANDD,Plus
		DW	DoLIT,'0',Plus,HOLD,RFrom,EXIT

;   #>		( xd -- c-addr u )		\ CORE
;		Prepare the output string to be TYPE'd.
;		||HERE>WORD/#-work-area|
;
;   : #>	2DROP hld @ HERE size-of-PAD + OVER - 1chars/ ;

		$COLON	2,'#>',NumberSignGreater,_FLINK
		DW	TwoDROP,HLD,Fetch,HERE,DoLIT,PADSize*CHARR,Plus
		DW	OVER,Minus,OneCharsSlash,EXIT

;   #S		( ud -- 0 0 )			\ CORE
;		Convert ud until all digits are added to the output string.
;
;   : #S	BEGIN # 2DUP OR 0= UNTIL ;

		$COLON	2,'#S',NumberSignS,_FLINK
NUMSS1		DW	NumberSign,TwoDUP,ORR
		DW	ZeroEquals,ZBranch,NUMSS1
		DW	EXIT

;   '           ( "<spaces>name" -- xt )        \ CORE
;		Parse a name, find it and return xt.
;
;   : '         (') DROP ;

		$COLON	1,"'",Tick,_FLINK
		DW	ParenTick,DROP,EXIT

;   +		( n1|u1 n2|u2 -- n3|u3 )	\ CORE
;		Add top two items and gives the sum.
;
;   : + 	um+ DROP ;

		$COLON	1,'+',Plus,_FLINK
		DW	UMPlus,DROP,EXIT

;   +!		( n|u a-addr -- )		\ CORE
;		Add n|u to the contents at a-addr.
;
;   : +!	SWAP OVER @ + SWAP ! ;

		$COLON	2,'+!',PlusStore,_FLINK
		DW	SWAP,OVER,Fetch,Plus
		DW	SWAP,Store,EXIT

;   ,		( x -- )			\ CORE
;		Reserve one cell in data space and store x in it.
;
;   : , 	HERE DUP CELL+ TO HERE ! ;

		$COLON	1,',',Comma,_FLINK
		DW	HERE,DUPP,CELLPlus,DoTO,AddrHERE,Store,EXIT

;   -		( n1|u1 n2|u2 -- n3|u3 )	\ CORE
;		Subtract n2|u2 from n1|u1, giving the difference n3|u3.
;
;   : - 	NEGATE + ;

		$COLON	1,'-',Minus,_FLINK
		DW	NEGATE,Plus,EXIT

;   .		( n -- )			\ CORE
;		Display a signed number followed by a space.
;
;   : . 	S>D D. ;

		$COLON	1,'.',Dot,_FLINK
		DW	SToD,DDot,EXIT

;   /		( n1 n2 -- n3 ) 		\ CORE
;		Divide n1 by n2, giving single-cell quotient n3.
;
;   : / 	/MOD NIP ;

		$COLON	1,'/',Slash,_FLINK
		DW	SlashMOD,NIP,EXIT

;   /MOD	( n1 n2 -- n3 n4 )		\ CORE
;		Divide n1 by n2, giving single-cell remainder n3 and
;		single-cell quotient n4.
;
;   : /MOD	>R S>D R> FM/MOD ;

		$COLON	4,'/MOD',SlashMOD,_FLINK
		DW	ToR,SToD,RFrom,FMSlashMOD,EXIT

;   /STRING	( c-addr1 u1 n -- c-addr2 u2 )	\ STRING
;		Adjust the char string at c-addr1 by n chars.
;
;   : /STRING	DUP >R - SWAP R> CHARS + SWAP ;

		$COLON	7,'/STRING',SlashSTRING,_FLINK
		DW	DUPP,ToR,Minus,SWAP,RFrom,CHARS,Plus,SWAP,EXIT

;   1+		( n1|u1 -- n2|u2 )		\ CORE
;		Increase top of the stack item by 1.
;
;   : 1+	1 + ;

		$COLON	2,'1+',OnePlus,_FLINK
		DW	One,Plus,EXIT

;   1-		( n1|u1 -- n2|u2 )		\ CORE
;		Decrease top of the stack item by 1.
;
;   : 1-	-1 + ;

		$COLON	2,'1-',OneMinus,_FLINK
		DW	MinusOne,Plus,EXIT

;   2!		( x1 x2 a-addr -- )		\ CORE
;		Store the cell pare x1 x2 at a-addr, with x2 at a-addr and
;		x1 at the next consecutive cell.
;
;   : 2!	SWAP OVER ! CELL+ ! ;

		$COLON	2,'2!',TwoStore,_FLINK
		DW	SWAP,OVER,Store,CELLPlus,Store,EXIT

;   2@		( a-addr -- x1 x2 )		\ CORE
;		Fetch the cell pair stored at a-addr. x2 is stored at a-addr
;		and x1 at the next consecutive cell.
;
;   : 2@	DUP CELL+ @ SWAP @ ;

		$COLON	2,'2@',TwoFetch,_FLINK
		DW	DUPP,CELLPlus,Fetch,SWAP,Fetch,EXIT

;   2DROP	( x1 x2 -- )			\ CORE
;		Drop cell pair x1 x2 from the stack.

		$COLON	5,'2DROP',TwoDROP,_FLINK
		DW	DROP,DROP,EXIT

;   2DUP	( x1 x2 -- x1 x2 x1 x2 )	\ CORE
;		Duplicate cell pair x1 x2.

		$COLON	4,'2DUP',TwoDUP,_FLINK
		DW	OVER,OVER,EXIT

;   2SWAP	( x1 x2 x3 x4 -- x3 x4 x1 x2 )	\ CORE
;		Exchange the top two cell pairs.
;
;   : 2SWAP	ROT >R ROT R> ;

		$COLON	5,'2SWAP',TwoSWAP,_FLINK
		DW	ROT,ToR,ROT,RFrom,EXIT

;   :		( "<spaces>name" -- colon-sys ) \ CORE
;		Start a new colon definition using next word as its name.
;
;   : : 	head, :NONAME ROT DROP	-1 TO notNONAME? ;

		$COLON	1,':',COLON,_FLINK
		DW	HeadComma,ColonNONAME,ROT,DROP
		DW	DoLIT,-1,DoTO,AddrNotNONAMEQ,EXIT

;   :NONAME	( -- xt colon-sys )		\ CORE EXT
;		Create an execution token xt, enter compilation state and
;		start the current definition.
;
;   : :NONAME	bal IF -29 THROW THEN		\ compiler nesting
;		['] doLIST xt, DUP -1
;		0 TO notNONAME?  1 TO bal  ] ;

		$COLON	7,':NONAME',ColonNONAME,_FLINK
		DW	Bal,ZBranch,NONAME1
		DW	DoLIT,-29,THROW
NONAME1 	DW	DoLIT,DoLIST,xtComma,DUPP,DoLIT,-1
		DW	Zero,DoTO,AddrNotNONAMEQ
		DW	One,DoTO,AddrBal,RightBracket,EXIT

;   ;		( colon-sys -- )		\ CORE
;		Terminate a colon definition.
;
;   : ; 	bal 1- IF -22 THROW THEN	\ control structure mismatch
;		NIP 1+ IF -22 THROW THEN	\ colon-sys type is -1
;		notNONAME? IF	\ if the last definition is not created by ':'
;		  linkLast  0 TO notNONAME?	\ link the word to wordlist
;		THEN  POSTPONE EXIT	\ add EXIT at the end of the definition
;		0 TO bal  POSTPONE [ ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+1,';',Semicolon,_FLINK
		DW	Bal,OneMinus,ZBranch,SEMI1
		DW	DoLIT,-22,THROW
SEMI1		DW	NIP,OnePlus,ZBranch,SEMI2
		DW	DoLIT,-22,THROW
SEMI2		DW	NotNONAMEQ,ZBranch,SEMI3
		DW	LinkLast,Zero,DoTO,AddrNotNONAMEQ
SEMI3		DW	DoLIT,EXIT,COMPILEComma
		DW	Zero,DoTO,AddrBal,LeftBracket,EXIT

;   <		( n1 n2 -- flag )		\ CORE
;		Returns true if n1 is less than n2.
;
;   : < 	2DUP XOR 0<		\ same sign?
;		IF DROP 0< EXIT THEN	\ different signs, true if n1 <0
;		- 0< ;			\ same signs, true if n1-n2 <0

		$COLON	1,'<',LessThan,_FLINK
		DW	TwoDUP,XORR,ZeroLess,ZBranch,LESS1
		DW	DROP,ZeroLess,EXIT
LESS1		DW	Minus,ZeroLess,EXIT

;   <#		( -- )				\ CORE
;		Initiate the numeric output conversion process.
;		||HERE>WORD/#-work-area|
;
;   : <#	HERE size-of-PAD + hld ! ;

		$COLON	2,'<#',LessNumberSign,_FLINK
		DW	HERE,DoLIT,PADSize*CHARR,Plus,HLD,Store,EXIT

;   =		( x1 x2 -- flag )		\ CORE
;		Return true if top two are equal.
;
;   : = 	XORR 0= ;

		$COLON	1,'=',Equals,_FLINK
		DW	XORR,ZeroEquals,EXIT

;   >		( n1 n2 -- flag )		\ CORE
;		Returns true if n1 is greater than n2.
;
;   : > 	SWAP < ;

		$COLON	1,'>',GreaterThan,_FLINK
		DW	SWAP,LessThan,EXIT

;   >IN 	( -- a-addr )
;		Hold the character pointer while parsing input stream.

		$VAR	3,'>IN',ToIN,1,_FLINK

;   >NUMBER	( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )	\ CORE
;		Add number string's value to ud1. Leaves string of any
;		unconverted chars.
;
;   : >NUMBER	BEGIN  DUP
;		WHILE  >R  DUP >R C@			\ ud char  R: u c-addr
;		       DUP [ CHAR 9 1+ ] LITERAL [CHAR] A WITHIN
;			   IF DROP R> R> EXIT THEN
;		       [ CHAR 0 ] LITERAL - 9 OVER <
;		       [ CHAR A CHAR 9 1 + - ] LITERAL AND -
;		       DUP 0 BASE @ WITHIN
;		WHILE  SWAP BASE @ UM* DROP ROT BASE @ UM* D+ R> R> 1 /STRING
;		REPEAT DROP R> R>
;		THEN ;

		$COLON	7,'>NUMBER',ToNUMBER,_FLINK
TONUM1		DW	DUPP,ZBranch,TONUM3
		DW	ToR,DUPP,ToR,CFetch,DUPP
		DW	DoLIT,'9'+1,DoLIT,'A',WITHIN,ZeroEquals,ZBranch,TONUM2
		DW	DoLIT,'0',Minus,DoLIT,9,OVER,LessThan
		DW	DoLIT,'A'-'9'-1,ANDD,Minus,DUPP
		DW	Zero,BASE,Fetch,WITHIN,ZBranch,TONUM2
		DW	SWAP,BASE,Fetch,UMStar,DROP,ROT,BASE,Fetch
		DW	UMStar,DPlus,RFrom,RFrom,One,SlashSTRING
		DW	Branch,TONUM1
TONUM2		DW	DROP,RFrom,RFrom
TONUM3		DW	EXIT

;   ?DUP	( x -- x x | 0 )		\ CORE
;		Duplicate top of the stack if it is not zero.
;
;   : ?DUP	DUP IF DUP THEN ;

		$COLON	4,'?DUP',QuestionDUP,_FLINK
		DW	DUPP,ZBranch,QDUP1
		DW	DUPP
QDUP1		DW	EXIT

;   ABORT	( i*x -- ) ( R: j*x -- )	\ EXCEPTION EXT
;		Reset data stack and jump to QUIT.
;
;   : ABORT	-1 THROW ;

		$COLON	5,'ABORT',ABORT,_FLINK
		DW	MinusOne,THROW

;   ACCEPT	( c-addr +n1 -- +n2 )		\ CORE
;		Accept a string of up to +n1 chars. Return with actual count.
;		Implementation-defined editing. Stops at EOL# .
;		Supports backspace and delete editing.
;
;   : ACCEPT	>R 0
;		BEGIN  DUP R@ < 		\ ca n2 f  R: n1
;		WHILE  EKEY max-char AND
;		       DUP BL <
;		       IF   DUP  cr# = IF ROT 2DROP R> DROP EXIT THEN
;			    DUP  tab# =
;			    IF	 DROP 2DUP + BL DUP EMIT SWAP C! 1+
;			    ELSE DUP  bsp# =
;				 SWAP del# = OR
;				 IF DROP DUP
;					\ discard the last char if not 1st char
;				 IF 1- bsp# EMIT BL EMIT bsp# EMIT THEN THEN
;			    THEN
;		       ELSE >R 2DUP CHARS + R> DUP EMIT SWAP C! 1+  THEN
;		       THEN
;		REPEAT SWAP  R> 2DROP ;

		$COLON	6,'ACCEPT',ACCEPT,_FLINK
		DW	ToR,Zero
ACCPT1		DW	DUPP,RFetch,LessThan,ZBranch,ACCPT5
		DW	EKEY,DoLIT,MaxChar,ANDD
		DW	DUPP,BLank,LessThan,ZBranch,ACCPT3
		DW	DUPP,DoLIT,CRR,Equals,ZBranch,ACCPT4
		DW	ROT,TwoDROP,RFrom,DROP,EXIT
ACCPT4		DW	DUPP,DoLIT,TABB,Equals,ZBranch,ACCPT6
		DW	DROP,TwoDUP,Plus,BLank,DUPP,EMIT,SWAP,CStore,OnePlus
		DW	Branch,ACCPT1
ACCPT6		DW	DUPP,DoLIT,BKSPP,Equals
		DW	SWAP,DoLIT,DEL,Equals,ORR,ZBranch,ACCPT1
		DW	DUPP,ZBranch,ACCPT1
		DW	OneMinus,DoLIT,BKSPP,EMIT,BLank,EMIT,DoLIT,BKSPP,EMIT
		DW	Branch,ACCPT1
ACCPT3		DW	ToR,TwoDUP,CHARS,Plus,RFrom,DUPP,EMIT,SWAP,CStore
		DW	OnePlus,Branch,ACCPT1
ACCPT5		DW	SWAP,RFrom,TwoDROP,EXIT

;   AGAIN	( C: dest -- )			\ CORE EXT
;		Resolve backward reference dest. Typically used as
;		BEGIN ... AGAIN . Move control to the location specified by
;		dest on execution.
;
;   : AGAIN	IF -22 THROW THEN  \ control structure mismatch; dest type is 0
;		POSTPONE branch , bal- ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+5,'AGAIN',AGAIN,_FLINK
		DW	ZBranch,AGAIN1
		DW	DoLIT,-22,THROW
AGAIN1		DW	DoLIT,Branch,COMPILEComma,Comma,BalMinus,EXIT

;   AHEAD	( C: -- orig )			\ TOOLS EXT
;		Put the location of a new unresolved forward reference onto
;		control-flow stack.
;
;   : AHEAD	POSTPONE branch  HERE 0 ,
;		1 bal+		\ orig type is 1
;		; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+5,'AHEAD',AHEAD,_FLINK
		DW	DoLIT,Branch,COMPILEComma,HERE,Zero,Comma
		DW	One,BalPlus,EXIT

;   BL		( -- char )			\ CORE
;		Return the value of the blank character.
;
;   : BL	blank-char-value EXIT ;

		$CONST	2,'BL',BLank,' ',_FLINK

;   CATCH	( i*x xt -- j*x 0 | i*x n )	\ EXCEPTION
;		Push an exception frame on the exception stack and then execute
;		the execution token xt in such a way that control can be
;		transferred to a point just after CATCH if THROW is executed
;		during the execution of xt.
;
;   : CATCH	sp@ >R throwFrame @ >R		\ save error frame
;		rp@ throwFrame !  EXECUTE	\ execute
;		R> throwFrame ! 		\ restore error frame
;		R> DROP  0 ;			\ no error

		$COLON	5,'CATCH',CATCH,_FLINK
		DW	SPFetch,ToR,ThrowFrame,Fetch,ToR
		DW	RPFetch,ThrowFrame,Store,EXECUTE
		DW	RFrom,ThrowFrame,Store
		DW	RFrom,DROP,Zero,EXIT

;   CELL+	( a-addr1 -- a-addr2 )		\ CORE
;		Return next aligned cell address.
;
;   : CELL+	cell-size + ;

		$COLON	5,'CELL+',CELLPlus,_FLINK
		DW	DoLIT,CELLL,Plus,EXIT

;   CHAR+	( c-addr1 -- c-addr2 )		\ CORE
;		Returns next character-aligned address.
;
;   : CHAR+	char-size + ;

		$COLON	5,'CHAR+',CHARPlus,_FLINK
		DW	DoLIT,CHARR,Plus,EXIT

;   COMPILE,	( xt -- )			\ CORE EXT
;		Compile the execution token on data stack into current
;		colon definition.
;
;   : COMPILE,	, ; COMPILE-ONLY

		$COLON	COMPO+8,'COMPILE,',COMPILEComma,_FLINK
		DW	Comma,EXIT

;   CONSTANT	( x "<spaces>name" -- )         \ CORE
;		name Execution: ( -- x )
;		Create a definition for name which pushes x on the stack on
;		execution.
;
;   : CONSTANT	bal IF -29 THROW THEN		\ compiler nesting
;		head,  ['] doCONST xt, DROP  ,  linkLast ;

		$COLON	8,'CONSTANT',CONSTANT,_FLINK
		DW	Bal,ZBranch,CONST1
		DW	DoLIT,-29,THROW
CONST1		DW	HeadComma,DoLIT,DoCONST,xtComma,DROP,Comma
		DW	LinkLast,EXIT

;   COUNT	( c-addr1 -- c-addr2 u )	\ CORE
;		Convert counted string to string specification. c-addr2 is
;		the next char-aligned address after c-addr1 and u is the
;		contents at c-addr1.
;
;   : COUNT	DUP CHAR+ SWAP C@ ;

		$COLON	5,'COUNT',COUNT,_FLINK
		DW	DUPP,CHARPlus,SWAP,CFetch,EXIT

;   CREATE	( "<spaces>name" -- )           \ CORE
;		name Execution: ( -- a-addr )
;		Create a data object in data space, which return data
;		object address on execution
;		Structure of CREATEd word:
;		    | call-doCREATE | 0 or DOES> code addr | >BODY points here
;
;   : CREATE	bal IF -29 THROW THEN		\ compiler nesting
;		head,  ['] doCREATE xt, DROP
;		HERE DUP CELL+ TO HERE		\ reserve a cell
;		0 SWAP !		\ no DOES> code yet
;		linkLast ;		\ link CREATEd word to current wordlist

		$COLON	6,'CREATE',CREATE,_FLINK
		DW	Bal,ZBranch,CREAT1
		DW	DoLIT,-29,THROW
CREAT1		DW	HeadComma,DoLIT,DoCREATE,xtComma,DROP
		DW	HERE,DUPP,CELLPlus,DoTO,AddrHERE
		DW	Zero,SWAP,Store,LinkLast,EXIT

;   D+		( d1|ud1 d2|ud2 -- d3|ud3 )	\ DOUBLE
;		Add double-cell numbers.
;
;   : D+	>R SWAP >R um+ R> R> + + ;

		$COLON	2,'D+',DPlus,_FLINK
		DW	ToR,SWAP,ToR,UMPlus
		DW	RFrom,RFrom,Plus,Plus,EXIT

;   D.		( d -- )			\ DOUBLE
;		Display d in free field format followed by a space.
;
;   : D.	(d.) TYPE SPACE ;

		$COLON	2,'D.',DDot,_FLINK
		DW	ParenDDot,TYPEE,SPACE,EXIT

;   DECIMAL	( -- )				\ CORE
;		Set the numeric conversion radix to decimal 10.
;
;   : DECIMAL	10 BASE ! ;

		$COLON	7,'DECIMAL',DECIMAL,_FLINK
		DW	DoLIT,10,BASE,Store,EXIT

;   DEPTH	( -- +n )			\ CORE
;		Return the depth of the data stack.
;
;   : DEPTH	sp@ sp0 SWAP - cell-size / ;

		$COLON	5,'DEPTH',DEPTH,_FLINK
		DW	SPFetch,SPZero,SWAP,Minus
		DW	DoLIT,CELLL,Slash,EXIT

;   DNEGATE	( d1 -- d2 )			\ DOUBLE
;		Two's complement of double-cell number.
;
;   : DNEGATE	INVERT >R INVERT 1 um+ R> + ;

		$COLON	7,'DNEGATE',DNEGATE,_FLINK
		DW	INVERT,ToR,INVERT
		DW	One,UMPlus
		DW	RFrom,Plus,EXIT

;   EKEY	( -- u )			\ FACILITY EXT
;		Receive one keyboard event u.
;
;   : EKEY	BEGIN PAUSE EKEY? UNTIL 'ekey EXECUTE ;

		$COLON	4,'EKEY',EKEY,_FLINK
EKEY1		DW	PAUSE,EKEYQuestion,ZBranch,EKEY1
		DW	TickEKEY,EXECUTE,EXIT

;   EMIT	( x -- )			\ CORE
;		Send a character to the output device.
;
;   : EMIT	'emit EXECUTE ;

		$COLON	4,'EMIT',EMIT,_FLINK
		DW	TickEMIT,EXECUTE,EXIT

;   FM/MOD	( d n1 -- n2 n3 )		\ CORE
;		Signed floored divide of double by single. Return mod n2
;		and quotient n3.
;
;   : FM/MOD	DUP >R 2DUP XOR >R >R DUP 0< IF DNEGATE THEN
;		R@ ABS UM/MOD
;		R> 0< IF SWAP NEGATE SWAP THEN
;		R> 0< IF NEGATE 	\ negative quotient
;		    OVER IF R@ ROT - SWAP 1- THEN
;		    R> DROP
;		    0 OVER < IF -11 THROW THEN		\ result out of range
;		    EXIT			THEN
;		R> DROP  DUP 0< IF -11 THROW THEN ;	\ result out of range

		$COLON	6,'FM/MOD',FMSlashMOD,_FLINK
		DW	DUPP,ToR,TwoDUP,XORR,ToR,ToR,DUPP,ZeroLess
		DW	ZBranch,FMMOD1
		DW	DNEGATE
FMMOD1		DW	RFetch,ABSS,UMSlashMOD
		DW	RFrom,ZeroLess,ZBranch,FMMOD2
		DW	SWAP,NEGATE,SWAP
FMMOD2		DW	RFrom,ZeroLess,ZBranch,FMMOD3
		DW	NEGATE,OVER,ZBranch,FMMOD4
		DW	RFetch,ROT,Minus,SWAP,OneMinus
FMMOD4		DW	RFrom,DROP
		DW	DoLIT,0,OVER,LessThan,ZBranch,FMMOD6
		DW	DoLIT,-11,THROW
FMMOD6		DW	EXIT
FMMOD3		DW	RFrom,DROP,DUPP,ZeroLess,ZBranch,FMMOD6
		DW	DoLIT,-11,THROW

;   GET-CURRENT   ( -- wid )			\ SEARCH
;		Return the indentifier of the compilation wordlist.
;
;   : GET-CURRENT   current @ ;

		$COLON	11,'GET-CURRENT',GET_CURRENT,_FLINK
		DW	Current,Fetch,EXIT

;   HOLD	( char -- )			\ CORE
;		Add char to the beginning of pictured numeric output string.
;
;   : HOLD	hld @  1 CHARS - DUP hld ! C! ;

		$COLON	4,'HOLD',HOLD,_FLINK
		DW	HLD,Fetch,DoLIT,0-CHARR,Plus
		DW	DUPP,HLD,Store,CStore,EXIT

;   I		( -- n|u ) ( R: loop-sys -- loop-sys )	\ CORE
;		Push the innermost loop index.
;
;   : I 	rp@ [ 1 CELLS ] LITERAL + @
;		rp@ [ 2 CELLS ] LITERAL + @  +	; COMPILE-ONLY

		$COLON	COMPO+1,'I',I,_FLINK
		DW	RPFetch,DoLIT,CELLL,Plus,Fetch
		DW	RPFetch,DoLIT,2*CELLL,Plus,Fetch,Plus,EXIT

;   IF		Compilation: ( C: -- orig )		\ CORE
;		Run-time: ( x -- )
;		Put the location of a new unresolved forward reference orig
;		onto the control flow stack. On execution jump to location
;		specified by the resolution of orig if x is zero.
;
;   : IF	POSTPONE 0branch  HERE 0 ,
;		1 bal+		\ orig type is 1
;		; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+2,'IF',IFF,_FLINK
		DW	DoLIT,ZBranch,COMPILEComma,HERE,Zero,Comma
		DW	One,BalPlus,EXIT

;   INVERT	( x1 -- x2 )			\ CORE
;		Return one's complement of x1.
;
;   : INVERT	-1 XOR ;

		$COLON	6,'INVERT',INVERT,_FLINK
		DW	MinusOne,XORR,EXIT

;   KEY 	( -- char )			\ CORE
;		Receive a character. Do not display char.
;
;   : KEY	EKEY max-char AND ;

		$COLON	3,'KEY',KEY,_FLINK
		DW	EKEY,DoLIT,MaxChar,ANDD,EXIT

;   LITERAL	Compilation: ( x -- )		\ CORE
;		Run-time: ( -- x )
;		Append following run-time semantics. Put x on the stack on
;		execution
;
;   : LITERAL	POSTPONE doLIT , ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+7,'LITERAL',LITERAL,_FLINK
		DW	DoLIT,DoLIT,COMPILEComma,Comma,EXIT

;   NEGATE	( n1 -- n2 )			\ CORE
;		Return two's complement of n1.
;
;   : NEGATE	INVERT 1+ ;

		$COLON	6,'NEGATE',NEGATE,_FLINK
		DW	INVERT,OnePlus,EXIT

;   NIP 	( n1 n2 -- n2 ) 		\ CORE EXT
;		Discard the second stack item.
;
;   : NIP	SWAP DROP ;

		$COLON	3,'NIP',NIP,_FLINK
		DW	SWAP,DROP,EXIT

;   PARSE	( char "ccc<char>"-- c-addr u )         \ CORE EXT
;		Scan input stream and return counted string delimited by char.
;
;   : PARSE	>R  SOURCE >IN @ /STRING	\ c-addr u  R: char
;		DUP IF
;		   OVER CHARS + OVER	   \ c-addr c-addr+u c-addr  R: char
;		   BEGIN  DUP C@ R@ XOR
;		   WHILE  CHAR+ 2DUP =
;		   UNTIL  DROP OVER - 1chars/ DUP
;		   ELSE   NIP  OVER - 1chars/ DUP CHAR+
;		   THEN   >IN +!
;		THEN   R> DROP EXIT ;

		$COLON	5,'PARSE',PARSE,_FLINK
		DW	ToR,SOURCE,ToIN,Fetch,SlashSTRING
		DW	DUPP,ZBranch,PARSE4
		DW	OVER,CHARS,Plus,OVER
PARSE1		DW	DUPP,CFetch,RFetch,XORR,ZBranch,PARSE3
		DW	CHARPlus,TwoDUP,Equals,ZBranch,PARSE1
PARSE2		DW	DROP,OVER,Minus,DUPP,OneCharsSlash,Branch,PARSE5
PARSE3		DW	NIP,OVER,Minus,DUPP,OneCharsSlash,CHARPlus
PARSE5		DW	ToIN,PlusStore
PARSE4		DW	RFrom,DROP,EXIT

;   QUIT	( -- ) ( R: i*x -- )		\ CORE
;		Empty the return stack, store zero in SOURCE-ID, make the user
;		input device the input source, and start text interpreter.
;
;   : QUIT	BEGIN
;		  rp0 rp!  0 TO SOURCE-ID  0 TO bal  POSTPONE [
;		  BEGIN CR REFILL DROP SPACE	\ REFILL returns always true
;			['] interpret CATCH ?DUP 0=
;		  WHILE STATE @ 0= IF .prompt THEN
;		  REPEAT
;		  DUP -1 XOR IF 				\ ABORT
;		  DUP -2 = IF SPACE abort"msg 2@ TYPE    ELSE   \ ABORT"
;		  SPACE errWord 2@ TYPE
;		  SPACE [CHAR] ? EMIT SPACE
;		  DUP -1 -58 WITHIN IF ." Exception # " . ELSE \ undefined exception
;		  CELLS THROWMsgTbl + @ COUNT TYPE	 THEN THEN THEN
;		  sp0 sp!
;		AGAIN ;

		$COLON	4,'QUIT',QUIT,_FLINK
QUIT1		DW	RPZero,RPStore,Zero,DoTO,AddrSOURCE_ID
		DW	Zero,DoTO,AddrBal,LeftBracket
QUIT2		DW	CR,REFILL,DROP,SPACE
		DW	DoLIT,Interpret,CATCH,QuestionDUP,ZeroEquals
		DW	ZBranch,QUIT3
		DW	STATE,Fetch,ZeroEquals,ZBranch,QUIT2
		DW	DotPrompt,Branch,QUIT2
QUIT3		DW	DUPP,MinusOne,XORR,ZBranch,QUIT5
		DW	DUPP,DoLIT,-2,Equals,ZBranch,QUIT4
		DW	SPACE,AbortQMsg,TwoFetch,TYPEE,Branch,QUIT5
QUIT4		DW	SPACE,ErrWord,TwoFetch,TYPEE
		DW	SPACE,DoLIT,'?',EMIT,SPACE
		DW	DUPP,MinusOne,DoLIT,-58,WITHIN,ZBranch,QUIT7
		$INSTR	' Exception # '
		DW	TYPEE,Dot,Branch,QUIT5
QUIT7		DW	CELLS,THROWMsgTbl,Plus,Fetch,COUNT,TYPEE
QUIT5		DW	SPZero,SPStore,Branch,QUIT1

;   REFILL	( -- flag )			\ CORE EXT
;		Attempt to fill the input buffer from the input source. Make
;		the result the input buffer, set >IN to zero, and return true
;		if successful. Return false if the input source is a string
;		from EVALUATE.
;
;   : REFILL	SOURCE-ID IF 0 EXIT THEN
;		memTop [ size-of-PAD CHARS ] LITERAL - DUP
;		size-of-PAD ACCEPT sourceVar 2!
;		0 >IN ! -1 ;

		$COLON	6,'REFILL',REFILL,_FLINK
		DW	SOURCE_ID,ZBranch,REFIL1
		DW	Zero,EXIT
REFIL1		DW	MemTop,DoLIT,0-PADSize*CHARR,Plus,DUPP
		DW	DoLIT,PADSize*CHARR,ACCEPT,SourceVar,TwoStore
		DW	Zero,ToIN,Store,MinusOne,EXIT

;   ROT 	( x1 x2 x3 -- x2 x3 x1 )	\ CORE
;		Rotate the top three data stack items.
;
;   : ROT	>R SWAP R> SWAP ;

		$COLON	3,'ROT',ROT,_FLINK
		DW	ToR,SWAP,RFrom,SWAP,EXIT

;   S>D 	( n -- d )			\ CORE
;		Convert a single-cell number n to double-cell number.
;
;   : S>D	DUP 0< ;

		$COLON	3,'S>D',SToD,_FLINK
		DW	DUPP,ZeroLess,EXIT

;   SEARCH-WORDLIST	( c-addr u wid -- 0 | xt 1 | xt -1)	\ SEARCH
;		Search word list for a match with the given name.
;		Return execution token and -1 or 1 ( IMMEDIATE) if found.
;		Return 0 if not found.
;
;   : SEARCH-WORDLIST
;		(search-wordlist) DUP IF NIP THEN ;

		$COLON	15,'SEARCH-WORDLIST',SEARCH_WORDLIST,_FLINK
		DW	ParenSearch_Wordlist,DUPP,ZBranch,SRCHW1
		DW	NIP
SRCHW1		DW	EXIT

;   SIGN	( n -- )			\ CORE
;		Add a minus sign to the numeric output string if n is negative.
;
;   : SIGN	0< IF [CHAR] - HOLD THEN ;

		$COLON	4,'SIGN',SIGN,_FLINK
		DW	ZeroLess,ZBranch,SIGN1
		DW	DoLIT,'-',HOLD
SIGN1		DW	EXIT

;   SOURCE	( -- c-addr u ) 		\ CORE
;		Return input buffer string.
;
;   : SOURCE	sourceVar 2@ ;

		$COLON	6,'SOURCE',SOURCE,_FLINK
		DW	SourceVar,TwoFetch,EXIT

;   SPACE	( -- )				\ CORE
;		Send the blank character to the output device.
;
;   : SPACE	32 EMIT ;

		$COLON	5,'SPACE',SPACE,_FLINK
		DW	BLank,EMIT,EXIT

;   STATE	( -- a-addr )			\ CORE
;		Return the address of a cell containing compilation-state flag
;		which is true in compilation state or false otherwise.

		$VAR	5,'STATE',STATE,1,_FLINK

;   THEN	Compilation: ( C: orig -- )	\ CORE
;		Run-time: ( -- )
;		Resolve the forward reference orig.
;
;   : THEN	1- IF -22 THROW THEN	\ control structure mismatch
;					\ orig type is 1
;		HERE SWAP ! bal- ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+4,'THEN',THENN,_FLINK
		DW	OneMinus,ZBranch,THEN1
		DW	DoLIT,-22,THROW
THEN1		DW	HERE,SWAP,Store,BalMinus,EXIT

;   THROW	( k*x n -- k*x | i*x n )	\ EXCEPTION
;		If n is not zero, pop the topmost exception frame from the
;		exception stack, along with everything on the return stack
;		above the frame. Then restore the condition before CATCH and
;		transfer control just after the CATCH that pushed that
;		exception frame.
;
;   : THROW	?DUP
;		IF   throwFrame @ rp!	\ restore return stack
;		     R> throwFrame !	\ restore THROW frame
;		     R> SWAP >R sp!	\ restore data stack
;		     DROP R>
;		     'init-i/o EXECUTE
;		THEN ;

		$COLON	5,'THROW',THROW,_FLINK
		DW	QuestionDUP,ZBranch,THROW1
		DW	ThrowFrame,Fetch,RPStore,RFrom,ThrowFrame,Store
		DW	RFrom,SWAP,ToR,SPStore,DROP,RFrom
		DW	TickINIT_IO,EXECUTE
THROW1		DW	EXIT

;   TYPE	( c-addr u -- ) 		\ CORE
;		Display the character string if u is greater than zero.
;
;   : TYPE	?DUP IF 0 DO DUP C@ EMIT CHAR+ LOOP THEN DROP ;

		$COLON	4,'TYPE',TYPEE,_FLINK
		DW	QuestionDUP,ZBranch,TYPE2
		DW	Zero,DoDO
TYPE1		DW	DUPP,CFetch,EMIT,CHARPlus,DoLOOP,TYPE1
TYPE2		DW	DROP,EXIT

;   U<		( u1 u2 -- flag )		\ CORE
;		Unsigned compare of top two items. True if u1 < u2.
;
;   : U<	2DUP XOR 0< IF NIP 0< EXIT THEN - 0< ;

		$COLON	2,'U<',ULess,_FLINK
		DW	TwoDUP,XORR,ZeroLess
		DW	ZBranch,ULES1
		DW	NIP,ZeroLess,EXIT
ULES1		DW	Minus,ZeroLess,EXIT

;   UNLOOP	( -- ) ( R: loop-sys -- )	\ CORE
;		Discard loop-control parameters for the current nesting level.
;		An UNLOOP is required for each nesting level before the
;		definition may be EXITed.
;
;   : UNLOOP	R> R> R> 2DROP >R ;

		$COLON	COMPO+6,'UNLOOP',UNLOOP,_FLINK
		DW	RFrom,RFrom,RFrom,TwoDROP,ToR,EXIT

;   WITHIN	( n1|u1 n2|n2 n3|u3 -- flag )	\ CORE EXT
;		Return true if (n2|u2<=n1|u1 and n1|u1<n3|u3) or
;		(n2|u2>n3|u3 and (n2|u2<=n1|u1 or n1|u1<n3|u3)).
;
;   : WITHIN	OVER - >R - R> U< ;

		$COLON	6,'WITHIN',WITHIN,_FLINK
		DW	OVER,Minus,ToR			;ul <= u < uh
		DW	Minus,RFrom,ULess,EXIT

;   [		( -- )				\ CORE
;		Enter interpretation state.
;
;   : [ 	0 STATE ! ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+1,'[',LeftBracket,_FLINK
		DW	Zero,STATE,Store,EXIT

;   ]		( -- )				\ CORE
;		Enter compilation state.
;
;   : ] 	-1 STATE ! ;

		$COLON	1,']',RightBracket,_FLINK
		DW	MinusOne,STATE,Store,EXIT

;;;;;;;;;;;;;;;;
; Rest of CORE words and two facility words, EKEY? and EMIT?
;;;;;;;;;;;;;;;;
;	Following definitions can be removed from assembler source and
;	can be colon-defined later.

;   (		( "ccc<)>" -- )                 \ CORE
;		Ignore following string up to next ) . A comment.
;
;   : ( 	[CHAR] ) PARSE 2DROP ;

		$COLON	IMMED+1,'(',Paren,_FLINK
		DW	DoLIT,')',PARSE,TwoDROP,EXIT

;   *		( n1|u1 n2|u2 -- n3|u3 )	\ CORE
;		Multiply n1|u1 by n2|u2 giving a single product.
;
;   : * 	UM* DROP ;

		$COLON	1,'*',Star,_FLINK
		DW	UMStar,DROP,EXIT

;   */		( n1 n2 n3 -- n4 )		\ CORE
;		Multiply n1 by n2 producing double-cell intermediate,
;		then divide it by n3. Return single-cell quotient.
;
;   : */	*/MOD NIP ;

		$COLON	2,'*/',StarSlash,_FLINK
		DW	StarSlashMOD,NIP,EXIT

;   */MOD	( n1 n2 n3 -- n4 n5 )		\ CORE
;		Multiply n1 by n2 producing double-cell intermediate,
;		then divide it by n3. Return single-cell remainder and
;		single-cell quotient.
;
;   : */MOD	>R M* R> FM/MOD ;

		$COLON	5,'*/MOD',StarSlashMOD,_FLINK
		DW	ToR,MStar,RFrom,FMSlashMOD,EXIT

;   +LOOP	Compilation: ( C: do-sys -- )	\ CORE
;		Run-time: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;		Terminate a DO-+LOOP structure. Resolve the destination of all
;		unresolved occurences of LEAVE.
;		On execution add n to the loop index. If loop index did not
;		cross the boundary between loop_limit-1 and loop_limit,
;		continue execution at the beginning of the loop. Otherwise,
;		finish the loop.
;
;   : +LOOP	POSTPONE do+LOOP  rake ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+5,'+LOOP',PlusLOOP,_FLINK
		DW	DoLIT,DoPLOOP,COMPILEComma,rake,EXIT

;   ."          ( "ccc<">" -- )                 \ CORE
;		Run-time ( -- )
;		Compile an inline string literal to be typed out at run time.
;
;   : ."        POSTPONE S" POSTPONE TYPE ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+2,'."',DotQuote,_FLINK
		DW	SQuote,DoLIT,TYPEE,COMPILEComma,EXIT

;   2OVER	( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )	  \ CORE
;		Copy cell pair x1 x2 to the top of the stack.
;
;   : 2OVER	>R >R 2DUP R> R> 2SWAP ;

		$COLON	5,'2OVER',TwoOVER,_FLINK
		DW	ToR,ToR,TwoDUP,RFrom,RFrom,TwoSWAP,EXIT

;   >BODY	( xt -- a-addr )		\ CORE
;		Push data field address of CREATEd word.
;		Structure of CREATEd word:
;		    | call-doCREATE | 0 or DOES> code addr | >BODY points here
;
;   : >BODY	?call DUP IF			\ code-addr xt2
;		    ['] doCREATE = IF           \ should be call-doCREATE
;		    CELL+ EXIT
;		THEN THEN
;		-31 THROW ;		\ >BODY used on non-CREATEd definition

		$COLON	5,'>BODY',ToBODY,_FLINK
		DW	QCall,DUPP,ZBranch,TBODY1
		DW	DoLIT,DoCREATE,Equals,ZBranch,TBODY1
		DW	CELLPlus,EXIT
TBODY1		DW	DoLIT,-31,THROW

;   ABORT"      ( "ccc<">" -- )                 \ EXCEPTION EXT
;		Run-time ( i*x x1 -- | i*x ) ( R: j*x -- | j*x )
;		Conditional abort with an error message.
;
;   : ABORT"    S" POSTPONE ROT
;		POSTPONE IF POSTPONE abort"msg POSTPONE 2!
;		-2 POSTPONE LITERAL POSTPONE THROW
;		POSTPONE ELSE POSTPONE 2DROP POSTPONE THEN
;		;  COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+6,'ABORT"',ABORTQuote,_FLINK
		DW	SQuote,DoLIT,ROT,COMPILEComma
		DW	IFF,DoLIT,AbortQMsg,COMPILEComma ; IF is immediate
		DW	DoLIT,TwoStore,COMPILEComma
		DW	DoLIT,-2,LITERAL		 ; LITERAL is immediate
		DW	DoLIT,THROW,COMPILEComma
		DW	ELSEE,DoLIT,TwoDROP,COMPILEComma ; ELSE and THEN are
		DW	THENN,EXIT			 ; immediate

;   ABS 	( n -- u )			\ CORE
;		Return the absolute value of n.
;
;   : ABS	DUP 0< IF NEGATE THEN ;

		$COLON	3,'ABS',ABSS,_FLINK
		DW	DUPP,ZeroLess,ZBranch,ABS1
		DW	NEGATE
ABS1		DW	EXIT

;   ALLOT	( n -- )			\ CORE
;		Allocate n address units in data space.
;
;   : ALLOT	HERE + TO HERE ;

		$COLON	5,'ALLOT',ALLOT,_FLINK
		DW	HERE,Plus,DoTO,AddrHERE,EXIT

;   BEGIN	( C: -- dest )			\ CORE
;		Start an infinite or indefinite loop structure. Put the next
;		location for a transfer of control, dest, onto the data
;		control stack.
;
;   : BEGIN	HERE 0 bal+		\ dest type is 0
;		; COMPILE-ONLY IMMDEDIATE

		$COLON	IMMED+COMPO+5,'BEGIN',BEGIN,_FLINK
		DW	HERE,Zero,BalPlus,EXIT

;   C,		( char -- )			\ CORE
;		Compile a character into data space.
;
;   : C,	HERE C!  HERE CHAR+ TO HERE ;

		$COLON	2,'C,',CComma,_FLINK
		DW	HERE,CStore,HERE,CHARPlus,DoTO,AddrHERE,EXIT

;   CHAR	( "<spaces>ccc" -- char )       \ CORE
;		Parse next word and return the value of first character.
;
;   : CHAR	PARSE-WORD DROP C@ ;

		$COLON	4,'CHAR',CHAR,_FLINK
		DW	PARSE_WORD,DROP,CFetch,EXIT

;   DO		Compilation: ( C: -- do-sys )	\ CORE
;		Run-time: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
;		Start a DO-LOOP structure in a colon definition. Place do-sys
;		on control-flow stack, which will be resolved by LOOP or +LOOP.
;
;   : DO	0 rakeVar !  0			\ ?DO-orig is 0 for DO
;		POSTPONE doDO  HERE  bal+	\ DO-dest
;		; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+2,'DO',DO,_FLINK
		DW	Zero,RakeVar,Store,Zero
		DW	DoLIT,DoDO,COMPILEComma,HERE,BalPlus,EXIT

;   DOES>	( C: colon-sys1 -- colon-sys2 ) \ CORE
;		Build run time code of the data object CREATEd.
;
;   : DOES>	bal 1- IF -22 THROW THEN	\ control structure mismatch
;		NIP 1+ IF -22 THROW THEN	\ colon-sys type is -1
;		POSTPONE pipe ['] doLIST xt, -1 ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+5,'DOES>',DOESGreater,_FLINK
		DW	Bal,OneMinus,ZBranch,DOES1
		DW	DoLIT,-22,THROW
DOES1		DW	NIP,OnePlus,ZBranch,DOES2
		DW	DoLIT,-22,THROW
DOES2		DW	DoLIT,Pipe,COMPILEComma
		DW	DoLIT,DoLIST,xtComma,DoLIT,-1,EXIT

;   ELSE	Compilation: ( C: orig1 -- orig2 )	\ CORE
;		Run-time: ( -- )
;		Start the false clause in an IF-ELSE-THEN structure.
;		Put the location of new unresolved forward reference orig2
;		onto control-flow stack.
;
;   : ELSE	POSTPONE AHEAD 2SWAP POSTPONE THEN ; COMPILE-ONLY IMMDEDIATE

		$COLON	IMMED+COMPO+4,'ELSE',ELSEE,_FLINK
		DW	AHEAD,TwoSWAP,THENN,EXIT

;   ENVIRONMENT?   ( c-addr u -- false | i*x true )	\ CORE
;		Environment query.
;
;   : ENVIRONMENT?
;		envQList SEARCH-WORDLIST
;		DUP >R IF EXECUTE THEN R> ;

		$COLON	12,'ENVIRONMENT?',ENVIRONMENTQuery,_FLINK
		DW	EnvQList,SEARCH_WORDLIST
		DW	DUPP,ToR,ZBranch,ENVRN1
		DW	EXECUTE
ENVRN1		DW	RFrom,EXIT

;   EVALUATE	( i*x c-addr u -- j*x ) 	\ CORE
;		Evaluate the string. Save the input source specification.
;		Store -1 in SOURCE-ID.
;
;   : EVALUATE	SOURCE >R >R >IN @ >R  SOURCE-ID >R
;		-1 TO SOURCE-ID
;		sourceVar 2!  0 >IN !  interpret
;		R> TO SOURCE-ID
;		R> >IN ! R> R> sourceVar 2! ;

		$COLON	8,'EVALUATE',EVALUATE,_FLINK
		DW	SOURCE,ToR,ToR,ToIN,Fetch,ToR,SOURCE_ID,ToR
		DW	MinusOne,DoTO,AddrSOURCE_ID
		DW	SourceVar,TwoStore,Zero,ToIN,Store,Interpret
		DW	RFrom,DoTO,AddrSOURCE_ID
		DW	RFrom,ToIN,Store,RFrom,RFrom,SourceVar,TwoStore,EXIT

;   FILL	( c-addr u char -- )		\ CORE
;		Store char in each of u consecutive characters of memory
;		beginning at c-addr.
;
;   : FILL	ROT ROT ?DUP IF 0 DO 2DUP C! CHAR+ LOOP THEN 2DROP ;

		$COLON	4,'FILL',FILL,_FLINK
		DW	ROT,ROT,QuestionDUP,ZBranch,FILL2
		DW	Zero,DoDO
FILL1		DW	TwoDUP,CStore,CHARPlus,DoLOOP,FILL1
FILL2		DW	TwoDROP,EXIT

;   FIND	( c-addr -- c-addr 0 | xt 1 | xt -1)	 \ SEARCH
;		Search dictionary for a match with the given counted name.
;		Return execution token and -1 or 1 ( IMMEDIATE) if found;
;		c-addr 0 if not found.
;
;   : FIND	DUP COUNT search-word ?DUP IF NIP ROT DROP EXIT THEN
;		2DROP 0 ;

		$COLON	4,'FIND',FIND,_FLINK
		DW	DUPP,COUNT,Search_word,QuestionDUP,ZBranch,FIND1
		DW	NIP,ROT,DROP,EXIT
FIND1		DW	TwoDROP,Zero,EXIT

;   IMMEDIATE	( -- )				\ CORE
;		Make the most recent definition an immediate word.
;
;   : IMMEDIATE   lastName [ =imed ] LITERAL OVER @ OR SWAP ! ;

		$COLON	9,'IMMEDIATE',IMMEDIATE,_FLINK
		DW	LastName,DoLIT,IMMED,OVER,Fetch,ORR,SWAP,Store,EXIT

;   J		( -- n|u ) ( R: loop-sys -- loop-sys )	\ CORE
;		Push the index of next outer loop.
;
;   : J 	rp@ [ 3 CELLS ] LITERAL + @
;		rp@ [ 4 CELLS ] LITERAL + @  +	; COMPILE-ONLY

		$COLON	COMPO+1,'J',J,_FLINK
		DW	RPFetch,DoLIT,3*CELLL,Plus,Fetch
		DW	RPFetch,DoLIT,4*CELLL,Plus,Fetch,Plus,EXIT

;   LEAVE	( -- ) ( R: loop-sys -- )	\ CORE
;		Terminate definite loop, DO|?DO  ... LOOP|+LOOP, immediately.
;
;   : LEAVE	POSTPONE UNLOOP POSTPONE branch
;		HERE rakeVar DUP @ , ! ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+5,'LEAVE',LEAVEE,_FLINK
		DW	DoLIT,UNLOOP,COMPILEComma,DoLIT,Branch,COMPILEComma
		DW	HERE,RakeVar,DUPP,Fetch,Comma,Store,EXIT

;   LOOP	Compilation: ( C: do-sys -- )	\ CORE
;		Run-time: ( -- ) ( R: loop-sys1 -- loop-sys2 )
;		Terminate a DO|?DO ... LOOP structure. Resolve the destination
;		of all unresolved occurences of LEAVE.
;
;   : LOOP	POSTPONE doLOOP  rake ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+4,'LOOP',LOOPP,_FLINK
		DW	DoLIT,DoLOOP,COMPILEComma,rake,EXIT

;   LSHIFT	( x1 u -- x2 )			\ CORE
;		Perform a logical left shift of u bit-places on x1, giving x2.
;		Put 0 into the least significant bits vacated by the shift.
;
;   : LSHIFT	?DUP IF 0 DO 2* LOOP THEN ;

		$COLON	6,'LSHIFT',LSHIFT,_FLINK
		DW	QuestionDUP,ZBranch,LSHIFT2
		DW	Zero,DoDO
LSHIFT1 	DW	TwoStar,DoLOOP,LSHIFT1
LSHIFT2 	DW	EXIT

;   M*		( n1 n2 -- d )			\ CORE
;		Signed multiply. Return double product.
;
;   : M*	2DUP XOR 0< >R ABS SWAP ABS UM* R> IF DNEGATE THEN ;

		$COLON	2,'M*',MStar,_FLINK
		DW	TwoDUP,XORR,ZeroLess,ToR,ABSS,SWAP,ABSS
		DW	UMStar,RFrom,ZBranch,MSTAR1
		DW	DNEGATE
MSTAR1		DW	EXIT

;   MAX 	( n1 n2 -- n3 ) 		\ CORE
;		Return the greater of two top stack items.
;
;   : MAX	2DUP < IF SWAP THEN DROP ;

		$COLON	3,'MAX',MAX,_FLINK
		DW	TwoDUP,LessThan,ZBranch,MAX1
		DW	SWAP
MAX1		DW	DROP,EXIT

;   MIN 	( n1 n2 -- n3 ) 		\ CORE
;		Return the smaller of top two stack items.
;
;   : MIN	2DUP > IF SWAP THEN DROP ;

		$COLON	3,'MIN',MIN,_FLINK
		DW	TwoDUP,GreaterThan,ZBranch,MIN1
		DW	SWAP
MIN1		DW	DROP,EXIT

;   MOD 	( n1 n2 -- n3 ) 		\ CORE
;		Divide n1 by n2, giving the single cell remainder n3.
;		Returns modulo of floored division in this implementation.
;
;   : MOD	/MOD DROP ;

		$COLON	3,'MOD',MODD,_FLINK
		DW	SlashMOD,DROP,EXIT

;   PICK	( x_u ... x1 x0 u -- x_u ... x1 x0 x_u )	\ CORE EXT
;		Remove u and copy the uth stack item to top of the stack. An
;		ambiguous condition exists if there are less than u+2 items
;		on the stack before PICK is executed.
;
;   : PICK	DEPTH DUP 2 < IF -4 THROW THEN	  \ stack underflow
;		2 - OVER U< IF -4 THROW THEN
;		1+ CELLS sp@ + @ ;

		$COLON	4,'PICK',PICK,_FLINK
		DW	DEPTH,DUPP,DoLIT,2,LessThan,ZBranch,PICK1
		DW	DoLIT,-4,THROW
PICK1		DW	DoLIT,2,Minus,OVER,ULess,ZBranch,PICK2
		DW	DoLIT,-4,THROW
PICK2		DW	OnePlus,CELLS,SPFetch,Plus,Fetch,EXIT

;   POSTPONE	( "<spaces>name" -- )           \ CORE
;		Parse name and find it. Append compilation semantics of name
;		to current definition.
;
;   : POSTPONE	(') 0< IF POSTPONE LITERAL
;			  POSTPONE COMPILE, EXIT THEN	\ non-IMMEDIATE
;		COMPILE, ; COMPILE-ONLY IMMEDIATE	\ IMMEDIATE

		$COLON	IMMED+COMPO+8,'POSTPONE',POSTPONE,_FLINK
		DW	ParenTick,ZeroLess,ZBranch,POSTP1
		DW	LITERAL,DoLIT,COMPILEComma
POSTP1		DW	COMPILEComma,EXIT

;   RECURSE	( -- )				\ CORE
;		Append the execution semactics of the current definition to
;		the current definition.
;
;   : RECURSE	bal 1- 2* PICK 1+ IF -22 THROW THEN
;			\ control structure mismatch; colon-sys type is -1
;		bal 1- 2* 1+ PICK	\ xt of current definition
;		COMPILE, ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+7,'RECURSE',RECURSE,_FLINK
		DW	Bal,OneMinus,TwoStar,PICK,OnePlus,ZBranch,RECUR1
		DW	DoLIT,-22,THROW
RECUR1		DW	Bal,OneMinus,TwoStar,OnePlus,PICK
		DW	COMPILEComma,EXIT

;   REPEAT	( C: orig dest -- )		\ CORE
;		Terminate a BEGIN-WHILE-REPEAT indefinite loop. Resolve
;		backward reference dest and forward reference orig.
;
;   : REPEAT	AGAIN THEN ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+6,'REPEAT',REPEATT,_FLINK
		DW	AGAIN,THENN,EXIT

;   RSHIFT	( x1 u -- x2 )			\ CORE
;		Perform a logical right shift of u bit-places on x1, giving x2.
;		Put 0 into the most significant bits vacated by the shift.
;
;   : RSHIFT	?DUP IF
;			0 SWAP	cell-size-in-bits SWAP -
;			0 DO  2DUP D+  LOOP
;			NIP
;		     THEN ;

		$COLON	6,'RSHIFT',RSHIFT,_FLINK
		DW	QuestionDUP,ZBranch,RSHIFT2
		DW	Zero,SWAP,DoLIT,CELLL*8,SWAP,Minus,Zero,DoDO
RSHIFT1 	DW	TwoDUP,DPlus,DoLOOP,RSHIFT1
		DW	NIP
RSHIFT2 	DW	EXIT

;   SLITERAL	( c-addr1 u -- )		\ STRING
;		Run-time ( -- c-addr2 u )
;		Compile a string literal. Return the string on execution.
;
;   : SLITERAL	DUP LITERAL POSTPONE doS"
;		CHARS HERE 2DUP + ALIGNED TO HERE
;		SWAP MOVE ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+8,'SLITERAL',SLITERAL,_FLINK
		DW	DUPP,LITERAL,DoLIT,DoSQuote,COMPILEComma
		DW	CHARS,HERE,TwoDUP,Plus,ALIGNED,DoTO,AddrHERE
		DW	SWAP,MOVE,EXIT

;   S"          Compilation: ( "ccc<">" -- )    \ CORE
;		Run-time: ( -- c-addr u )
;		Parse ccc delimetered by " . Return the string specification
;		c-addr u on execution.
;
;   : S"        [CHAR] " PARSE POSTPONE SLITERAL ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+2,'S"',SQuote,_FLINK
		DW	DoLIT,'"',PARSE,SLITERAL,EXIT

;   SM/REM	( d n1 -- n2 n3 )		\ CORE
;		Symmetric divide of double by single. Return remainder n2
;		and quotient n3.
;
;   : SM/REM	2DUP XOR >R OVER >R >R DUP 0< IF DNEGATE THEN
;		R> ABS UM/MOD
;		R> 0< IF SWAP NEGATE SWAP THEN
;		R> 0< IF	\ negative quotient
;		    NEGATE 0 OVER < 0= IF EXIT THEN
;		    -11 THROW			THEN	\ result out of range
;		DUP 0< IF -11 THROW THEN ;		\ result out of range

		$COLON	6,'SM/REM',SMSlashREM,_FLINK
		DW	TwoDUP,XORR,ToR,OVER,ToR,ToR,DUPP,ZeroLess
		DW	ZBranch,SMREM1
		DW	DNEGATE
SMREM1		DW	RFrom,ABSS,UMSlashMOD
		DW	RFrom,ZeroLess,ZBranch,SMREM2
		DW	SWAP,NEGATE,SWAP
SMREM2		DW	RFrom,ZeroLess,ZBranch,SMREM3
		DW	NEGATE,DoLIT,0,OVER,LessThan,ZeroEquals,ZBranch,SMREM4
SMREM5		DW	EXIT
SMREM3		DW	DUPP,ZeroLess,ZBranch,SMREM5
SMREM4		DW	DoLIT,-11,THROW

;   SPACES	( n -- )			\ CORE
;		Send n spaces to the output device if n is greater than zero.
;
;   : SPACES	?DUP IF 0 DO SPACE LOOP THEN ;

		$COLON	6,'SPACES',SPACES,_FLINK
		DW	QuestionDUP,ZBranch,SPACES2
		DW	Zero,DoDO
SPACES1 	DW	SPACE,DoLOOP,SPACES1
SPACES2 	DW	EXIT

;   TO		Interpretation: ( x "<spaces>name" -- ) \ CORE EXT
;		Compilation:	( "<spaces>name" -- )
;		Run-time:	( x -- )
;		Store x in name.
;
;   : TO	' ?call DUP IF          \ should be call-doCONST
;                 ['] doVALUE =         \ verify VALUE marker
;		  IF STATE @
;		     IF POSTPONE doTO , EXIT THEN
;		     ! EXIT
;		     THEN THEN
;		-32 THROW ; IMMEDIATE	\ invalid name argument (e.g. TO xxx)

		$COLON	IMMED+2,'TO',TO,_FLINK
		DW	Tick,QCall,DUPP,ZBranch,TO1
                DW      DoLIT,DoVALUE,Equals,ZBranch,TO1
		DW	STATE,Fetch,ZBranch,TO2
		DW	DoLIT,DoTO,COMPILEComma,Comma,EXIT
TO2		DW	Store,EXIT
TO1		DW	DoLIT,-32,THROW

;   U.		( u -- )			\ CORE
;		Display u in free field format followed by space.
;
;   : U.	0 D. ;

		$COLON	2,'U.',UDot,_FLINK
		DW	Zero,DDot,EXIT

;   UNTIL	( C: dest -- )			\ CORE
;		Terminate a BEGIN-UNTIL indefinite loop structure.
;
;   : UNTIL	IF -22 THROW THEN  \ control structure mismatch; dest type is 0
;		POSTPONE 0branch , bal- ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+5,'UNTIL',UNTIL,_FLINK
		DW	ZBranch,UNTIL1
		DW	DoLIT,-22,THROW
UNTIL1		DW	DoLIT,ZBranch,COMPILEComma,Comma,BalMinus,EXIT

;   VALUE	( x "<spaces>name" -- )         \ CORE EXT
;		name Execution: ( -- x )
;		Create a value object with initial value x.
;
;   : VALUE	bal IF -29 THROW THEN		\ compiler nesting
;               head,  ['] doVALUE xt, DROP
;		, linkLast ; \ store x and link VALUE word to current wordlist

		$COLON	5,'VALUE',VALUE,_FLINK
		DW	Bal,ZBranch,VALUE1
		DW	DoLIT,-29,THROW
VALUE1          DW      HeadComma,DoLIT,DoVALUE,xtComma,DROP
		DW	Comma,LinkLast,EXIT

;   VARIABLE	( "<spaces>name" -- )           \ CORE
;		name Execution: ( -- a-addr )
;		Parse a name and create a variable with the name.
;		Resolve one cell of data space at an aligned address.
;		Return the address on execution.
;
;   : VARIABLE	bal IF -29 THROW THEN		\ compiler nesting
;		head,  ['] doVAR xt, DROP
;		HERE CELL+ TO HERE  linkLast ;

		$COLON	8,'VARIABLE',VARIABLE,_FLINK
		DW	Bal,ZBranch,VARIA1
		DW	DoLIT,-29,THROW
VARIA1		DW	HeadComma,DoLIT,DoVAR,xtComma,DROP
		DW	HERE,CELLPlus,DoTO,AddrHERE,LinkLast,EXIT

;   WHILE	( C: dest -- orig dest )	\ CORE
;		Put the location of a new unresolved forward reference orig
;		onto the control flow stack under the existing dest. Typically
;		used in BEGIN ... WHILE ... REPEAT structure.
;
;   : WHILE	POSTPONE IF 2SWAP ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+5,'WHILE',WHILEE,_FLINK
		DW	IFF,TwoSWAP,EXIT

;   WORD	( char "<chars>ccc<char>" -- c-addr )   \ CORE
;		Skip leading delimeters and parse a word. Return the address
;		of a transient region containing the word as counted string.
;
;   : WORD	skipPARSE HERE pack" DROP HERE ;

		$COLON	4,'WORD',WORDD,_FLINK
		DW	SkipPARSE,HERE,PackQuote,DROP,HERE,EXIT

;   [']         Compilation: ( "<spaces>name" -- )      \ CORE
;		Run-time: ( -- xt )
;		Parse name. Return the execution token of name on execution.
;
;   : [']       ' POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+3,"[']",BracketTick,_FLINK
		DW	Tick,LITERAL,EXIT

;   [CHAR]	Compilation: ( "<spaces>name" -- )      \ CORE
;		Run-time: ( -- char )
;		Parse name. Return the value of the first character of name
;		on execution.
;
;   : [CHAR]	CHAR POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

		$COLON	IMMED+COMPO+6,'[CHAR]',BracketCHAR,_FLINK
		DW	CHAR,LITERAL,EXIT

;   \		( "ccc<eol>" -- )               \ CORE EXT
;		Parse and discard the remainder of the parse area.
;
;   : \ 	SOURCE >IN ! DROP ; IMMEDIATE

		$COLON	IMMED+1,'\',Backslash,_FLINK
		DW	SOURCE,ToIN,Store,DROP,EXIT

; Optional Facility words

;   EKEY?	( -- flag )			\ FACILITY EXT
;		If a keyboard event is available, return true.
;
;   : EKEY?	'ekey? EXECUTE ;

		$COLON	5,'EKEY?',EKEYQuestion,_FLINK
		DW	TickEKEYQ,EXECUTE,EXIT

;   EMIT?	( -- flag )			\ FACILITY EXT
;		flag is true if the user output device is ready to accept data
;		and the execution of EMIT in place of EMIT? would not have
;		suffered an indefinite delay. If device state is indeterminate,
;		flag is true.
;
;   : EMIT?	'emit? EXECUTE ;

		$COLON	5,'EMIT?',EMITQuestion,_FLINK
		DW	TickEMITQ,EXECUTE,EXIT

;===============================================================

LASTENV 	EQU	_ENVLINK-0
LASTSYSTEM	EQU	_SLINK-0	;last SYSTEM word name address
LASTFORTH	EQU	_FLINK-0	;last FORTH word name address

CTOP		EQU	$-0		;next available memory in dictionary

MAIN	ENDS
END	ORIG

;===============================================================
