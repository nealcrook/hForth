;;TITLE hForth ARM ROM Model

;;PAGE 62,132     ;62 lines per page, 132 characters per line

;; $Id$
;;
;; $Log$

;===============================================================
;
;       hForth ARM ROM model v0.9.7 by Neal Crook,
;	ported from 8086 model by Wonyong Koh, 1995
; 
; 1996. 5. 21.
;	Start ARM version derived from 8086 version v0.9.7
;
;;NAC: removed Korean language stuff from here
;;
;       hForth ROM model is designed for small embedded system.
;       Especially it is designed for a minimal development system which
;       uses non-volatile RAM(NVRAM) or ROM emulator in place of ROM so
;       that the content of ROM can be changed during development phase
;       and can be copied to real ROM later for production system. Name
;       space does not need to be included in final system if the system
;       does not require Forth text interpreter. hForth occupies little
;       more than 6 KB of code space for CORE words only and about 8 KB
;       with additional words in OPTIONAL.F such as WORDS, HEX, SEE,
;       etc. hForth ROM model requires at lease 1 KB of RAM.
;
;       ANS Forth Standard divide Forth dictionary into code, name, and
;       data space. When hForth ROM model starts, the code space resides
;       at bottom of ROM, name space at top of ROM, and data space in
;       RAM address space. Code and name parts of new definitions will
;       split into proper spaces if "ROM" is writable. If "ROM" is not
;       writable, code and data part of new definitions goes into bottom
;       of RAM and name part of new definitions goes into top of RAM.
;
;       You can use the words 'RAM' and 'ROM' to switch data space
;       between RAM and ROM address space.
;
;           ROM  CREATE TTABLE  1 , 2 , 3 ,
;
;       will make a preset table in ROM address space while
;
;           RAM  CREATE AARRAY  10 CELLS ALLOT
;
;       will make an array of 10 cells where you write values into.
;
;       hForth is based on eForth model published by Mr. Bill Muench and
;       Dr. C. H. Ting in 1990. The key features of the original eForth
;       model is preserved. Following is quoted from the orginal 8086
;       eForth source.
;
;         > small machine dependent kernel and portable high level code
;         > source code in the MASM format
;         > direct threaded code
;         > separated code and name dictionaries
;         > simple vectored terminal and file interface to host computer
;         > aligned with the proposed ANS Forth Standard
;         > easy upgrade path to optimize for specific CPU
;
;       These are also the characteristics of hForth. For better, hForth
;       is ANS Forth system which complies the Standard, not just
;       alignes with the Standard. Colon definitions for all high level
;       words are also given as comments in TASM source code. The source
;       code would be a working example for a Forth student.
;
;===============================================================
;
;	ARM register usage
;
tos		RN r9				; top of data stack item
fpc		RN r10				; FORTH vm instruction ptr
rsp		RN r11				; RETURN stack pointer
dsp		RN r12				; DATA stack pointer
;
;	r14, as usual, is used for the link address in subroutine calls
;	r??? are used as scratch registers during calculation.
;	All other registers are free.
;
;       Structure of a task
;       userP points follower.
;       //userP//<return_stack//<data_stack//
;       //user_area/user1/taskName/throwFrame/stackTop/status/follower/sp0/rp0
;
;===============================================================

;NAC: search for NAC to see comments on to-do stuff and the mods I have made.
; - there may be an assumption about number of cells taken by call dolist
; in eforth this was defined as calsize and affected see, does> and DOES>
; - for metacompilation it will be necessary to re-order some words to remove
;   forward references.

;***************
; Assembly Constants
;***************

TRUEE		EQU	-1
FALSEE		EQU	0

CHARR           EQU     1               ;byte size of a character
CELLL           EQU     4               ;byte size of a cell
MaxChar         EQU     07Fh            ;Extended character set
                                        ;  Use 07Fh for ASCII only
;NAC: make these generic using shift etc.
MaxSigned       EQU     07FFFFFFFh      ;max value of signed integer
MaxUnsigned     EQU     0FFFFFFFFh	;max value of unsigned integer
MaxNegative     EQU     080000000h	;max value of negative integer
                                        ;  Used in doDO

PADSize         EQU     134             ;PAD area size
RTCells         EQU     64              ;return stack size
DTCells         EQU     256             ;data stack size

BASEE           EQU     10              ;default radix
OrderDepth      EQU     10              ;depth of search order stack
MaxWLISTS       EQU     20              ;maximum number of wordlists
                                        ; 2 is used by the system
                                        ; 18 is available to Forth programs

NumTHROWMsgs	EQU	58		;Number of throw messages

;NAC: might need something better for MASKK.. I did before, and I think I
;made it platform-independent.
COMPO           EQU     020h            ;lexicon compile only bit
IMMED           EQU     040h            ;lexicon immediate bit
MASKK           EQU     1Fh             ;lexicon bit mask
                                        ;extended character set
                                        ;maximum name length = 1Fh

;NAC: added SPC. 
SPC		EQU	32		;space (blank)
BKSPP           EQU     8               ;backspace
TABB            EQU     9               ;tab
LFF             EQU     10              ;line feed
CRR             EQU     13              ;carriage return
DEL             EQU     127             ;delete

;For the ARM, this is the branch-with-link op-code; the offset gets ORed in.
;Used by ?call and xt, -- those are the only words that omit opcodes.
CALLL		EQU	0eb000000h	;****fix modsntx.awk

; Memory allocation for writable ROM
;   ROMbottom||code>WORDworkarea|--//--|PAD|TIB|reserved<name||ROMtop
;   RAMbottom||variable>--//--<sp|rp||RAMtop
; Memory allocation for unwritable ROM
;   ROMbottom||initial-code>--//--<initial-name||ROMtop
;   RAMbottom||code/data>WORDworkarea|--//--|PAD|TIB|reserved<name|sp|rp||RAMtop

;NAC: defined the EBSA SSRAM memory map here.
RAM0            EQU     040010000h              ;bottom of RAM memory ******
RAMEnd          EQU     040020000h              ;top of RAM memory ******
                                                ;RAM size = 64KB
ROM0            EQU     040000000h              ;bottom of ROM memory ******
ROMEnd          EQU     040010000h              ;end of ROM memory ******
                                                ;ROM size = 64KB
COLDD           EQU     ROM0             	;cold start vector ******
;NAC we ignore CODEE for now, which is fine.. We'd need to use it if we blew
;NAC a ROM to start at 0.. would need to leave room for the vectors.
CODEE           EQU     ROM0+200h               ;initial code space ******
RPP             EQU     RAMEnd                  ;start of return stack (RP0)
SPP             EQU     RPP-RTCells*CELLL       ;start of data stack (SP0)
RAMT0           EQU     SPP-DTCells*CELLL       ;top of free RAM area


;NAC added equates for host I/O
;*******************************************************************
; Equates for host I/O

ledport		EQU	&f2400000

	IF (TARGET = "EBSABOOT") :LOR: (TARGET = "EBSAFLASH")

; Real I/O for EBSA-110

IOShift         EQU     2
ISAIOBase       EQU     &F0000000
IOBoff          EQU     0       ;Byte offset in word

;; Macro does I/O port -> memory address conversion for byte I/O
                MACRO
$label            IOBADDR $reg,$io_port
$label            LDR     $reg, =((($io_port):SHL:IOShift)+ISAIOBase+IOBoff)
                MEND

COM1Port	EQU	&3f8		
COM2Port	EQU	&2f8

;;; UART registers
	
	;; Receive, Transmit, Interupt enable, Interrupt Identitication and
	;; FIFO are only 
	;; accessable when the Divisor latch access bit in the line control
	;; register is 0
Rx		EQU	&0	; Receive port, read only
Tx		EQU	&0	; Transmit port, write only
IntEnable	EQU	&1	; Interrupt enable, read/write
FIFOcntl	EQU	&2	; FIFO control, write only

	;; With the Divisor latch access bit set the first 2 registers set
	;; the divisor, which controls the line speed.
Dllsb		EQU	&0
Dlmsb		EQU	&1		

	;; The remaining registers are always accessable and read/write
LineCntl	EQU	&3	; Line control, the main control register
LineStatus	EQU	&5
	
;;; Masks etc. for useful fields

	;; Line control register bits
DLABMsk		EQU	&80	; Divisor latch.

	;; Word length values
WordLen8	EQU	&3

	;; Line status register
LineDRMsk	EQU	&1	; Data ready
LineTHREMsk	EQU	&20	; Transmitter holding register empty (i.e. ready for next char)

	;; Useful line speed values for divider
Baud9600low	EQU	12
Baud9600high	EQU	0

Baud19200low	EQU	6
Baud19200high	EQU	0

Baud38400low	EQU	3
Baud38400high	EQU	0

	ENDIF

	IF (TARGET = "PIE")
;this serial code is derived from ~berent/arm/ebsarm/demon/pie/driver.s
;which is
;Copyright (C) 1991 Advanced RISC Machines Limited. All rights reserved.
;Written by Dave Jaggar.

SerialChipBase  EQU     &80000000
MR1             EQU     &0
MR2             EQU     &0
SR              EQU     &4
CSR             EQU     &4
CMDR            EQU     &8
RHR             EQU     &c
THR             EQU     &c
ACR             EQU     &10
ISR             EQU     &14
IMR             EQU     &14
CTUR            EQU     &18
CTLR            EQU     &1c

; Values for the fields in MR1

BitsPerChar     EQU     3 ; 0 for 5, 1 for 6, 2 for 7, 3 for 8
ParityType      EQU     4 ; 0 for Even, 4 for Odd
ParityMode      EQU     16 ; 0 for on, 8 for force, 16 for off
ErrorMode       EQU     0 ; 0 for char, 32 for block
RxInterrupt     EQU     0 ; 0 for char ready, 64 for FIFO full
RxAutoRTS       EQU     0 ; 0 for off, 128 for on

MR1Value        EQU     BitsPerChar + ParityType + ParityMode + ErrorMode + RxInterrupt + RxAutoRTS

; Values for the fields in MR2

StopBits        EQU     7 ; 7 for 1, 15 for 2 (others in the data sheet)
AutoCTS         EQU     0 ; 0 for off, 16 for on
TxAutoRTS       EQU     0 ; 0 for off, 32 for on

MR2Value        EQU     StopBits + AutoCTS + TxAutoRTS

; Values for the fields in the CSR

; The baud rates are defined both here, and in the top bit of the ACR
; called (the BRG bit). Most useful values are
; BRG = 0, 9 for 4K8, 11 for 9K6, 12 for 38K4
; BRG = 1, 12 for 19K2

Baud9600        EQU     &0000000b
Baud19200       EQU     &8000000c
Baud38400       EQU     &0000000c

; Values for the CMDR

ResetMR1        EQU     &10 ; Reset MR1
ResetRx         EQU     &20 ; Reset Receiver
ResetTx         EQU     &30 ; Reset Transmitter
ResetError      EQU     &40 ; Reset Error Status
ResetBreak      EQU     &50 ; Reset Break Status
ResetMPI        EQU     &c0 ; Reset MPI
DisableRxTx     EQU     &0A ; Disable the receiver and the transmitter
EnableRxTx      EQU     &05 ; Enable the receiver and the transmitter

; Values in the SR

SRRxReady       EQU     1 ; a character has been received
SRRxFull        EQU     2 ; Rx FIFO is full
SRTxReady       EQU     4 ; space in the Tx FIFO
SRTxEmpty       EQU     8 ; Tx FIFO is empty
SROverrunError  EQU     16
SRParityError   EQU     32
SRFrameError    EQU     64
SRReceivedBreak EQU     128

SRNastyError    EQU     SROverrunError + SRParityError + SRFrameError + SRReceivedBreak

; Values for ACR, the BaudRateGroup (BRG) bit is set at run time

MPOOutput       EQU     0 ; see data sheet
PowerDown       EQU     8 ; 0 for on, 8 for off
TimerSource     EQU     &70 ; see data sheet

ACRValue        EQU     MPOOutput + PowerDown + TimerSource

	ENDIF

;NAC end of equates for host I/O

; Initialize assembly variables

		GBLA _SLINK
		GBLA _FLINK
		GBLA _ENVLINK
		GBLA _NAME
		GBLA _CODE
		GBLA _VAR
		GBLA _THROW

_THROW		SETA (-1*NumTHROWMsgs)		;current throw string
_SLINK		SETA 0				;force a null link
_FLINK		SETA 0				;force a null link
_ENVLINK	SETA 0				;force a null link
;NAC the _NAME value is not used in the ARM port.. instead, need to set the
;NAC dict_pc parameter to makesrc.awk to the value _NAME would have had
_NAME		SETA ROMEnd			;initialize name pointer
;NAC: don't think that _CODE needs to be initialised.. it is always used as
;temporary storage in macro expansions. This is a bug in eForth, too.
_CODE		SETA CODEE			;initialize code pointer
_VAR		SETA RAM0			;variable space pointer


;; ARM Macros for push and pop

		MACRO
$label		pushD $reg
$label          str	$reg, [dsp, # - CELLL]!
		MEND

		MACRO
$label		pushR $reg
$label          str	$reg, [rsp, # - CELLL]!
		MEND

		MACRO
$label		popD $reg
$label          ldr	$reg, [dsp], #CELLL
		MEND

		MACRO
$label		popR $reg
$label          ldr	$reg, [rsp], #CELLL
		MEND

;***************
; Assembly macros
;***************

;       Adjust an address to the next cell boundary.

$ALIGN  MACRO
        EVEN                                    ;for 16 bit systems
        ENDM

;       Add a name to name space of dictionary. To be used to store THROW
;       message in name space. THROW messages won't be needed if target
;       system do not need names of Forth words.

$STR    MACRO   LABEL,STRING
        _CODE   = $
        DB      STRING
        _LEN    = $ - _CODE
        _NAME   = _NAME - _LEN + 1		;packed string -not aligned
ORG     _NAME
LABEL:
        DB      _LEN,STRING
ORG     _CODE                                   ;restore code pointer
        ENDM

;NAC need to add $THROWTABLE macro for back-portability to x86

;       Add a name to name space of dictionary. To be used to store THROW
;       message in name space. THROW messages won't be needed if target
;       system do not need names of Forth words.

$THROWSTR MACRO   LABEL,STRING
        _CODE   = $
        DB      STRING
        _LEN    = $ - _CODE
        _NAME   = _NAME - _LEN + 1		;pack the strings
ORG     _NAME
LABEL:
        DB      _LEN,STRING
ORG	AddrTHROWMsgTbl - _THROW
	_THROW	= _THROW + CELLL
	DW	LABEL
ORG     _CODE                                   ;restore code pointer
        ENDM

;       Compile a code definition header.

$CODE   MACRO   LEX,NAME,LABEL,LINK
        $ALIGN                                  ;force to cell boundary
LABEL:                                          ;assembly label
        _CODE   = $                             ;save code pointer
        _LEN    = (LEX AND MASKK)/CELLL         ;string cell count, round down
        _NAME   = _NAME-((_LEN+3)*CELLL)        ;new header on cell boundary
ORG     _NAME                                   ;set name pointer
        DW      _CODE,LINK                      ;token pointer and link
        LINK   = $                              ;link points to a name string
        DB      LEX,NAME                        ;name string
ORG     _CODE                                   ;restore code pointer
        ENDM

;       Compile a colon definition header.

$COLON  MACRO   LEX,NAME,LABEL,LINK
        $CODE   LEX,NAME,LABEL,LINK
        NOP                                     ;align to cell boundary
        CALL    DoLIST                          ;include CALL doLIST
        ENDM

;       Compile a system CONSTANT header.

$CONST  MACRO   LEX,NAME,LABEL,VALUE,LINK
        $CODE   LEX,NAME,LABEL,LINK
        NOP
        CALL    DoCONST
        DW      VALUE
        ENDM

;       Compile a system VALUE header.

$VALUE  MACRO   LEX,NAME,LABEL,LINK
        $CODE   LEX,NAME,LABEL,LINK
        NOP
        CALL    DoVALUE
        DW      _VAR
        _VAR = _VAR +CELLL
        ENDM

;       Compile a system VARIABLE header.

$VAR    MACRO   LEX,NAME,LABEL,LINK
        $CODE   LEX,NAME,LABEL,LINK
        NOP
        CALL    DoCONST
        DW      _VAR
        _VAR = _VAR +CELLL                      ;update variable area offset
        ENDM

;       Compile a system USER header.

$USER   MACRO   LEX,NAME,LABEL,OFFSET,LINK
        $CODE   LEX,NAME,LABEL,LINK
        NOP
        CALL    DoUSER
        DW      OFFSET
        ENDM

;       Compile an inline string.

D$      MACRO   FUNCT,STRNG
        DW      FUNCT                           ;function
        _LEN    = $                             ;save address of count byte
        DB      0,STRNG                         ;count byte and string
        _CODE   = $                             ;save code pointer
ORG     _LEN                                    ;point to count byte
        DB      _CODE-_LEN-1                    ;set count
ORG     _CODE                                   ;restore code pointer
        $ALIGN
        ENDM

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

;       Assemble inline direct threaded code ending.

$NEXT   MACRO
        LODSW                                   ;next code address into AX
        JMP     AX                              ;jump directly to code address
        $ALIGN
        ENDM

;===============================================================

;***************
; Main entry points and COLD start data
;***************


		AREA myprog,ABS,CODE,DATA

	IF	(TARGET = "EBSABOOT")
		; code in EPROM is linked to run at the start of SSRAM
		; the initial piece of code copies the image to SSRAM.
		ORG	&40000000
	ELSE
		IF	(TARGET = "EBSAFLASH")
			; code in Flash is linked to run at start of
			; SSRAM plus c0. The PBL copies the image to
			; SSRAM. The gap of c0 leaves room to form a
			; formated AIF/Flash header for programming
			; an updated image back to Flash.
			ORG &400000c0
		ELSE
			; all other targets are loaded by DEMON and
			; live at low memory just beyond DEMON's
			; workspace area
			ORG &8000
		ENDIF
	ENDIF


;NAC COLDD is where the 1st code byte is generated. Don't want to hard-code
;it because the linker can take care of that
;;ORG		COLDD

	IF ((TARGET = "EBSADEMON") :LOR: (TARGET = "PIEDEMON"))
		swi	0x16			; go into Supervisor mode
; should turn off all interrupts, too

	ENDIF

	IF (TARGET = "EBSABOOT")

; Image is programmed into EPROM and we have to take the 
; responsibility of copying ourself into RAM.
; The code is built to run at &40000000
; and is running at 0, but after the first WRITE we will be
; magically running at &8000.00XX

; ROM equivalent of whereabouts we are
hialias		EQU	((herefar1 :AND: &0fffffff) :OR: &80000000)
; SSRAM equivalent of whereabouts we are
loalias		EQU	((herefar2 :AND: &0fffffff) :OR: &40000000)

;;;debug		ldr	pc,=hialias		;jump to to high ROM alias
herefar1
;;;debug		mov	r0, #0
;;;debug		str	r0, [r1]		;switch memory map

; now we're running at &8000.00xx
;copy image from ROM to SSRAM at &4000.0000
		mov	r0, #&40000000		;destination
		mov	r1, #&80000000		;source
		mov	r2, #(&10000/4)		;length (64Kbytes)

;;;debug movit		ldr	r3,[r1],#4
;;;		str	r3,[r0],#4
;;;		subs	r2,r2,#1
;;;		bne	movit

; jump to the RAM copy
;;;debug		ldr	pc,=loalias
herefar2
	ENDIF

	IF	(TARGET = "EBSABOOT") :LOR: (TARGET = "EBSADEMON") :LOR: (TARGET = "EBSAFLASH")

	; turn off the DEBUG LED to prove we got here.
		ldr	r0,=ledport
		ldr	r1,[r0]
		orr	r1,r1,#0x80		;don't corrupt the DRAM type..
		str	r1,[r0]
	ENDIF

		ldr	rsp, =RPP		;init return stack pointer
		ldr	dsp, =SPP		;init data stack pointer

		b	COLD                    ;cold (right at the end
						;of this listing) is the
						;execution address for the
						;high-level cold start
						;go do it!
                $ALIGN

; COLD start moves the following to system variables.
; MUST BE IN SAME ORDER AS SYSTEM VARIABLES.

                $ALIGN                          ;align to cell boundary
UZERO:          DW      RXQ                     ;'ekey?
                DW      RXFetch                 ;'ekey
                DW      TXQ                     ;'emit?
                DW      TXStore                 ;'emit
                DW      Set_IO                  ;'init-i/o
                DW      DotOK                   ;'prompt
                DW      HI                      ;'boot
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
                DW      0                       ;lastXT
                DW      0                       ;rakeVar
NOrder0:        DW      2                       ;#order
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
ULAST:

;NAC: what is NullString for?

; THROW code messages resides in top of name space. Messages must be
; placed before any Forth words were defined.

;;;NAC assume that the macros put the table in the right place..
;;;NAC the idea of the THROWTABLE macro is that it updates _NAME to
;;;NAC account for the space needed for the messages' jump table.
;;;NAC thereafter, the messages can be emitted at _NAME
;;;ORG	_NAME
;;;AddrTHROWMsgTbl:
;;;
;;;_NAME   SETA _NAME - NumTHROWMsgs*CELLL

	$THROWTABLE AddrTHROWMsgTbl,NumTHROWMsgs

        $STR        SystemIDStr,'hForth ARM ROM Model'
        $STR        VersionStr,'0.9.7'
        $STR        NullString,''
        $THROWSTR   THROWMsg_01,'ABORT'
        $THROWSTR   THROWMsg_02,'ABORT"'
        $THROWSTR   THROWMsg_03,'stack overflow'
        $THROWSTR   THROWMsg_04,'stack underflow'
        $THROWSTR   THROWMsg_05,'return stack overflow'
        $THROWSTR   THROWMsg_06,'return stack underflow'
        $THROWSTR   THROWMsg_07,'do-loops nested too deeply during execution'
        $THROWSTR   THROWMsg_08,'dictionary overflow'
        $THROWSTR   THROWMsg_09,'invalid memory address'
        $THROWSTR   THROWMsg_10,'division by zero'
        $THROWSTR   THROWMsg_11,'result out of range'
        $THROWSTR   THROWMsg_12,'argument type mismatch'
        $THROWSTR   THROWMsg_13,'undefined word'
        $THROWSTR   THROWMsg_14,'interpreting a compile-only word'
        $THROWSTR   THROWMsg_15,'invalid FORGET'
        $THROWSTR   THROWMsg_16,'attempt to use zero-length THROWSTRing as a name'
        $THROWSTR   THROWMsg_17,'pictured numeric output THROWSTRing overflow'
        $THROWSTR   THROWMsg_18,'parsed THROWSTRing overflow'
        $THROWSTR   THROWMsg_19,'definition name too long'
        $THROWSTR   THROWMsg_20,'write to a read-only location'
        $THROWSTR   THROWMsg_21,'unsupported operation (e.g., AT-XY on a too-dumb terminal)'
        $THROWSTR   THROWMsg_22,'control THROWSTRucture mismatch'
        $THROWSTR   THROWMsg_23,'address alignment exception'
        $THROWSTR   THROWMsg_24,'invalid numeric argument'
        $THROWSTR   THROWMsg_25,'return stack imbalance'
        $THROWSTR   THROWMsg_26,'loop parameters unavailable'
        $THROWSTR   THROWMsg_27,'invalid recursion'
        $THROWSTR   THROWMsg_28,'user interrupt'
        $THROWSTR   THROWMsg_29,'compiler nesting'
        $THROWSTR   THROWMsg_30,'obsolescent feature'
        $THROWSTR   THROWMsg_31,'>BODY used on non-CREATEd definition'
        $THROWSTR   THROWMsg_32,'invalid name argument (e.g., TO xxx)'
        $THROWSTR   THROWMsg_33,'block read exception'
        $THROWSTR   THROWMsg_34,'block write exception'
        $THROWSTR   THROWMsg_35,'invalid block number'
        $THROWSTR   THROWMsg_36,'invalid file position'
        $THROWSTR   THROWMsg_37,'file I/O exception'
        $THROWSTR   THROWMsg_38,'non-existent file'
        $THROWSTR   THROWMsg_39,'unexpected end of file'
        $THROWSTR   THROWMsg_40,'invalid BASE for floating point conversion'
        $THROWSTR   THROWMsg_41,'loss of precision'
        $THROWSTR   THROWMsg_42,'floating-point divide by zero'
        $THROWSTR   THROWMsg_43,'floating-point result out of range'
        $THROWSTR   THROWMsg_44,'floating-point stack overflow'
        $THROWSTR   THROWMsg_45,'floating-point stack underflow'
        $THROWSTR   THROWMsg_46,'floating-point invalid argument'
        $THROWSTR   THROWMsg_47,'compilation word list deleted'
        $THROWSTR   THROWMsg_48,'invalid POSTPONE'
        $THROWSTR   THROWMsg_49,'search-order overflow'
        $THROWSTR   THROWMsg_50,'search-order underflow'
        $THROWSTR   THROWMsg_51,'compilation word list changed'
        $THROWSTR   THROWMsg_52,'control-flow stack overflow'
        $THROWSTR   THROWMsg_53,'exception stack overflow'
        $THROWSTR   THROWMsg_54,'floating-point underflow'
        $THROWSTR   THROWMsg_55,'floating-point unidentified fault'
        $THROWSTR   THROWMsg_56,'QUIT'
        $THROWSTR   THROWMsg_57,'exception in sending or receiving a character'
        $THROWSTR   THROWMsg_58,'[IF], [ELSE], or [THEN] exception'

;;;NAC at this point need to ALIGN the value of _NAME, since we packed
;;;the strings of the throw table but  $CODE assumes that NAME is aligned.
	$ALIGN_NAME

;;;NAC this allows a gap between the start of rom and the start of code..
;;;ORG     CODEE                                   ;start code dictionary


;***************
; System dependent words -- Must be re-definded for each system.
;***************
; I/O words must be redefined if serial communication is used instead of
; keyboard. Following words are for MS-DOS system.

;NAC: added stoio and setbaud from eForth


;   !IO		( -- )	"store i o"
;		Initialize the serial devices for terminal I/O

		$CODE   3,'!IO',STOIO,_SLINK
	IF (TARGET = "EBSABOOT") :LOR: (TARGET = "EBSAFLASH")

; This is a general purpose Reset for the serial chip, restoring
; everything to the defaults, and cancelling the current transmission
; and reception.  This routine doesn't touch r0, and other code
; depends on this.

ResetDriver
	;; Set the default line speed
		LDR	r1,=WordLen8+DLABMsk	; Set divisor access
		IOBADDR	r2,(COM2Port+LineCntl)
		STRB	r1,[r2]

		LDR	r1,=Baud9600low		; Set 9600 
		IOBADDR	r2,(COM2Port+Dllsb)
		STRB	r1,[r2]
		LDR	r1,=Baud9600high 
		IOBADDR	r2,(COM2Port+Dlmsb)
		STRB	r1,[r2]

		LDR	r1,=WordLen8		; Clear divisor access
		IOBADDR	r2,(COM2Port+LineCntl)
		STRB	r1,[r2]

	;; Note that when we cleared divisor access we set the other bits we need in the
	;; line control register for 8 bit data, no parity, 1 stop bit. This is what we need.

	;; Disable the FIFOs
		LDR	r1,=0	; All bits clear disables the FIFOs
		IOBADDR r2,(COM2Port+FIFOcntl)
		STRB	r1,[r2]
	
	;; Clear out any errors by reading the line status
		IOBADDR r2,(COM2Port+LineStatus)
		LDRB	r1,[r2]

	;; Disable receive and error interrupts
		LDR	r1,=0
		IOBADDR	r2,(COM2Port+IntEnable)
		STRB	r1,[r2]
	ENDIF

	IF (TARGET = "PIE")
; This is a general purpose Reset for the serial chip, restoring
; everything to the defaults, and cancelling the current transmission
; and reception.
; affects r2, r3
ResetDriver     MOV     r3, #SerialChipBase

                MOV     r2, #DisableRxTx
                STR     r2, [r3, #CMDR]

                MOV     r2, #ResetMR1 ; make sure we write MR1
                STR     r2, [r3, #CMDR]
                MOV     r2, #MR1Value
                STR     r2, [r3, #MR1]
                MOV     r2, #MR2Value
                STR     r2, [r3, #MR2]

                MOV     r2, #Baud9600 ; default baud rate
                ORR     r2, r2, r2, LSL #4
                STR     r2, [r3, #CSR]

                TST     r2, #&80000000
                MOVEQ   r2, #ACRValue
                MOVNE   r2, #ACRValue + &80
                STR     r2, [r3, #ACR]

                MOV     r2, #ResetError
                STR     r2, [r3, #CMDR]
                MOV     r2, #ResetBreak
                STR     r2, [r3, #CMDR]
                MOV     r2, #ResetMPI
                STR     r2, [r3, #CMDR]
                MOV     r2, #ResetRx
                STR     r2, [r3, #CMDR]
                MOV     r2, #ResetTx
                STR     r2, [r3, #CMDR]

                MOV     r2, #0		; disable all interrupts
                STR     r2, [r3, #IMR]

                MOV     r2, #EnableRxTx
                STR     r2, [r3, #CMDR]
	ENDIF

		$NEXT

;   SETBAUD	( n -- )
;		Set the baud rate for serial I/O
;		1 => 9600
;		2 => 19K2
;		3 => 38K4

		$CODE   7,'SETBAUD',SETBAUD,_SLINK

		popD	r0

	IF (TARGET = "EBSABOOT") :LOR: (TARGET = "EBSAFLASH")

DriverSpeed     SUBS	r0,r0,#1	; test for 9600
		BEQ	Set9600
		SUBS	r0,r0,#1	; and 19200
		BEQ	Set19200
	;; So we want 38400
		LDR	r0,=Baud38400low
		LDR	r1,=Baud38400high
		B	SetRate

Set19200	LDR	r0,=Baud19200low
		LDR	r1,=Baud19200high
		B	SetRate

Set9600		LDR	r0,=Baud9600low
		LDR	r1,=Baud9600high

SetRate		IOBADDR	r4,(COM2Port+IntEnable)	; Prevent serial interrupts while setting divisor
		LDR	r5,=0			; This is because the divisor latch makes
		STRB	r5,[r4]			; resuming this from an interrupt difficult
		
		LDR	r3,=WordLen8+DLABMsk	; Set divisor access
		IOBADDR	r2,(COM2Port+LineCntl)
		STRB	r3,[r2]
	
	;; Set the actual rate in the divisor.
		IOBADDR	r2,(COM2Port+Dllsb)
		STRB	r0,[r2]
		IOBADDR	r2,(COM2Port+Dlmsb)
		STRB	r1,[r2]

		LDR	r1,=WordLen8		; Clear divisor access
		IOBADDR	r2,(COM2Port+LineCntl)
		STRB	r1,[r2]

	ENDIF

	IF (TARGET = "PIE")

DriverSpeed	MOV     r3, #SerialChipBase
01              LDR     r1, [r3, #SR] ; get the status register
                TST     r1, #SRTxEmpty ; finished sending ?
                BEQ     %B01

                CMP     r0, #0 ; really change it ?
                BLE     NoChange

                MOV     r1, #DisableRxTx
                STR     r1, [r3, #CMDR]

                MOV     r1, #Baud9600 ; default baud rate
                CMP     r0, #2
                MOVEQ   r1, #Baud19200
                CMP     r0, #3
                MOVEQ   r1, #Baud38400

                ORR     r1, r1, r1, LSL #4
                STR     r1, [r3, #CSR]

                TST     r1, #&80000000
                MOVEQ   r1, #ACRValue
                MOVNE   r1, #ACRValue + &80
                STR     r1, [r3, #ACR]

                MOV     r1, #ResetRx
                STR     r1, [r3, #CMDR]
                MOV     r1, #ResetTx
                STR     r1, [r3, #CMDR]

                MOV     r1, #EnableRxTx
                STR     r1, [r3, #CMDR]
NoChange
	ENDIF

		$NEXT


;   RX?         ( -- flag )
;               Return true if key is pressed.

                $CODE   3,'RX?',RXQ,_SLINK

	IF ((TARGET = "EBSADEMON") :LOR: (TARGET = "PIEDEMON"))

		;Demon actually, *waits* for a key across the link from
		;the host to the target. Therefore, always returns TRUE
		mvn	r0, #0			;push -1 (TRUE)..
		pushD	r0
	ENDIF

	IF (TARGET = "EBSABOOT") :LOR: (TARGET = "EBSAFLASH")

		IOBADDR	r0, (COM2Port+LineStatus)
		LDRB	r1, [r0]		;look for a character, note that reading this
		TST	r1, #LineDRMsk		;register also clears any errors
		mov	r1, #0			;predict no char available
		subne	r1, r1, #1		;change flag to -1 (TRUE)
		pushD	r1			;push flag
	ENDIF

	IF (TARGET = "PIE")

; See if a character has been received. If char available, the Z flag is CLEAR.
; affects r0, r1, r3
QGetChar
                MOV     r3, #SerialChipBase
		LDR     r0, [r3, #SR]		;get the status register
                TST     r0, #SRRxReady		;character arrived
		mov	r1, #0			;predict no char available
		subne	r1, r1, #1		;change flag to -1 (TRUE)
		pushD	r1			;push flag

	ENDIF
		$NEXT

;   RX@         ( -- u )
;               Receive one keyboard event u.

                $CODE   3,'RX@',RXFetch,_SLINK

	IF ((TARGET = "EBSADEMON") :LOR: (TARGET = "PIEDEMON"))

		swi	4			;SWI_ReadC
						;return byte in r0
		pushD	r0			;push char value
	ENDIF

	IF (TARGET = "EBSABOOT") :LOR: (TARGET = "EBSAFLASH")

		IOBADDR	r0, (COM2Port+LineStatus)
		IOBADDR	r0, (COM2Port+Rx)	;Read the character
		LDRB	r0, [r0]			
		pushD	r0			;push char value
	ENDIF

	IF (TARGET = "PIE")

                MOV     r3, #SerialChipBase
		LDR     r0, [r3, #RHR]		;get the character
                AND     r0, r0, #&ff		;only keep ASCII code
		pushD	r0			;push char value
	ENDIF
		$NEXT

;   TX?         ( -- flag )
;               Return true if output device is ready or device state is
;               indeterminate.

                $CONST  3,'TX?',TXQ,TRUEE,_SLINK   ;always true for UART


;   TX!         ( u -- )
;               Send char to the output device.

                $CODE   3,'TX!',TXStore,_SLINK

		ldr	r0, [dsp], #CELLL	;pop character

	IF ((TARGET = "EBSADEMON") :LOR: (TARGET = "PIEDEMON"))
		swi	0			;SWI_WriteC - write character
	ENDIF

	IF (TARGET = "EBSABOOT") :LOR: (TARGET = "EBSAFLASH")

; Send a byte to the serial line

PutByte		IOBADDR	r1, (COM2Port+LineStatus)
PutByteLoop	LDRB	r2,[r1]
		TST	r2,#LineTHREMsk		;Wait until ready to queue character
		BEQ	PutByteLoop

		IOBADDR	r1,(COM2Port+Tx)	;Now queue the character
		STRB	r0,[r1]
	ENDIF

	IF (TARGET = "PIE")
		; affects r1, r3
		popD	r0			;pop character
		MOV     r3, #SerialChipBase
PutByteLoop	LDR     r1, [r3, #SR]		;get the status register
		TST     r1, #SRTxReady		;space for the next character?
                BEQ     PutByteLoop		;spin until there is
                STR     r0, [r3, #THR]		;put the character
	ENDIF

		$NEXT

		LTORG				; assembler-generated literals
						; are stored here.


;   CR          ( -- )                          \ CORE
;               Carriage return and linefeed.
;
;   : CR        carriage-return-char EMIT  linefeed-char EMIT ;

                $COLON  2,'CR',CR,_FLINK
                DW      DoLIT,CRR,EMIT,DoLIT,LFF,EMIT,EXIT

;   BYE         ( -- )                          \ TOOLS EXT
;               Return control to the host operation system, if any.

                $CODE   3,'BYE',BYE,_FLINK

	IF ((TARGET = "EBSADEMON") :LOR: (TARGET = "PIEDEMON"))
		swi	011h			;SWI_Exit - halt execution and
						;return to debugger
		$ALIGN
	ELSE
		bl	DoLIST			;fake a colon definition
		D$      DoDotQuote,' Sorry - nowhere to go! '
		DW	CR,EXIT
	ENDIF

;   Iflushline	( -- )
;		Flush the whole Icache or the Icache block associated with
;		the code word most recently generated (at xhere???). This must
;		be called in a cached system any time that a machine op-code
;		is generated (unless the CPU supports a cache-coherent Icache).
;		Currently, the only word that uses this is xt,

		$COLON 10,'Iflushline',Iflushline,_SLINK
;NAC: todo: code this..
		DW EXIT


;   hi          ( -- )
;
;   : hi        CR S" systemID" ENVIRONMENT? DROP TYPE SPACE [CHAR] v EMIT
;                  S" version"  ENVIRONMENT? DROP TYPE
;               ."  by Wonyong Koh, 1995" CR
;               ." ALL noncommercial and commercial uses are granted." CR
;               ." Please send comment, bug report and suggestions to:" CR
;               ."   wykoh@pado.krict.re.kr or 82-42-861-4245 (FAX)" CR ;

                $COLON  2,'hi',HI,_SLINK
                DW      CR
;;;NAC gets here OK
                D$      DoSQuote,'systemID'
;;;NAC at this point dstack should hold (address length) for string
                DW      ENVIRONMENTQuery,DROP,TYPEE,SPACE,DoLIT,'v',EMIT
;;;NAC gone wrong by here because it never prints the system id
                D$      DoSQuote,'version'
                DW      ENVIRONMENTQuery,DROP,TYPEE
                D$      DoDotQuote,' by Wonyong Koh, 1995'
                DW      CR
                D$      DoDotQuote,'All noncommercial and commercial uses are granted.'
                DW      CR
                D$      DoDotQuote,'Please send comment, bug report and suggestions to:'
                DW      CR
                D$      DoDotQuote,'  wykoh@pado.krict.re.kr or 82-42-861-4245 (FAX)'
                DW      CR,EXIT

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
;               QUIT ;                          \ start interpretation

                $COLON  4,'COLD',COLD,_SLINK
                DW      SysVar0,VarZero,DoLIT,ULAST-UZERO,MOVE
                DW      XHere,DUPP,Fetch,INVERT,SWAP,TwoDUP,Store,Fetch,XORR
                DW      ZBranch,COLD1
                DW      RAMB,DoTO,AddrCPVar,RAMT,DoTO,AddrNPVar
COLD1:          DW      SPZero,SPStore,RPZero,RPStore
                DW      TickINIT_IO,EXECUTE,TickBoot,EXECUTE
                DW      QUIT

;   set-i/o ( -- )
;               Set input/output device. (Includes initialising any hardware)
;
;   : set-i/o   sysVar0 var0 4 CELLS MOVE       \ set i/o vectors
;               S" CON" stdin ;                 \ MS-DOS only

                $COLON  7,'set-i/o',Set_IO,_SLINK
                DW      SysVar0,VarZero,DoLIT,4*CELLL,MOVE
		DW	STOIO,EXIT

; Following words are for MS-DOS only.
; File input using MS-DOS redirection function without using FILE words.

;   redirect    ( c-addr -- flag )
;               Redirect standard input from the device identified by ASCIIZ
;               string stored at c-addr. Return error code.

                $CODE   8,'redirect',Redirect,_SLINK
;NAC: this code is unused for a non-DOS target
;NAC: todo: add a file/hand mechanism like eForth has..
;                MOV     DX,BX
;                MOV     AX,Redirect1stQ
;                OR      AX,AX
;                JZ      REDIRECT2
;                MOV     AH,03Eh
;                MOV     BX,RedirHandle
;                INT     021h            ; close previously opend file
REDIRECT2:
;                MOV     AX,03D00h       ; open file read-only
;                MOV     Redirect1stQ,AX ; set Redirect1stQ true
;                INT     021h
;                JC      REDIRECT1       ; if error
;                MOV     RedirHandle,AX
;                XOR     CX,CX
;                MOV     BX,AX
;                MOV     AX,04600H
;                INT     021H
;                JC      REDIRECT1
;                XOR     AX,AX
REDIRECT1:
;                MOV     BX,AX
                $NEXT
Redirect1stQ    DW      0               ; true after the first redirection
RedirHandle     DS      CELLL           ; redirect file handle

;   asciiz      ( ca1 u -- ca2 )
;               Return ASCIIZ string.
;
;   : asciiz    xhere SWAP 2DUP + 0 SWAP C! CHARS MOVE xhere ;

                $COLON  6,'asciiz',ASCIIZ,_SLINK
                DW      XHere,SWAP,TwoDUP,Plus,Zero
                DW      SWAP,CStore,CHARS,MOVE,XHere,EXIT

;   stdin       ( ca u -- )
;
;   : stdin     asciiz redirect ?DUP
;               IF -38 THROW THEN ; COMPILE-ONLY

                $COLON  5,'stdin',STDIN,_SLINK
                DW      ASCIIZ,Redirect,QuestionDUP,ZBranch,STDIN1
                DW      DoLIT,-38,THROW
STDIN1:         DW      EXIT

;   <<          ( "<spaces>ccc" -- )
;               Redirect input from the file 'ccc'. Should be used only in
;               interpretation state.
;
;   : <<        STATE @ IF ." Do not use '<<' in a definition." ABORT THEN
;               PARSE-WORD stdin SOURCE >IN !  DROP ; IMMEDIATE

                $COLON  IMMED+2,'<<',FROM,_SLINK
                DW      STATE,Fetch,ZBranch,FROM1
                DW      CR
                D$      DoDotQuote,'Do not use << in a definition.'
                DW      ABORT
FROM1:          DW      PARSE_WORD,STDIN,SOURCE,ToIN,Store,DROP,EXIT

;***************
; Non-Standard words - Processor-dependent definitions
;       32-bit Forth for ARM RISC
;***************

;   same?       ( c-addr1 c-addr2 u -- -1|0|1 )
;               Return 0 if two strings, ca1 u and ca2 u, are same; -1 if
;               string, ca1 u is smaller than ca2 u; 1 otherwise. Used by
;               '(search-wordlist)'. Code definition is preferred to speed up
;               interpretation. Colon definition is shown below.
;
;   : same?     ?DUP IF         \ null strings are always same
;                  0 DO OVER C@ OVER C@ XOR \ 0 (false) if match.. so continue
;                       IF UNLOOP C@ SWAP C@ > 2* 1+ EXIT THEN
;                       CHAR+ SWAP CHAR+ SWAP
;                  LOOP
;               THEN 2DROP 0 ;
;
;                 $COLON  5,'same?',SameQ,_SLINK
;                 DW      QuestionDUP,ZBranch,SAMEQ4
;                 DW      Zero,DoDO
; SAMEQ3:         DW      OVER,CFetch,OVER,CFetch,XORR,ZBranch,SAMEQ2
;                 DW      UNLOOP,CFetch,SWAP,CFetch,GreaterThan
;                 DW      TwoStar,OnePlus,EXIT
; SAMEQ2:         DW      CHARPlus,SWAP,CHARPlus
;                 DW      DoLOOP,SAMEQ3
; SAMEQ4:         DW      TwoDROP,Zero,EXIT

                $COLON   5,'same?',SameQ,_SLINK
;NAC: recode this native for speed..

                DW      QuestionDUP,ZBranch,SAMEQ4
                DW      Zero,DoDO
SAMEQ3:         DW      OVER,CFetch,OVER,CFetch,XORR,ZBranch,SAMEQ2
                DW      UNLOOP,CFetch,SWAP,CFetch,GreaterThan
                DW      TwoStar,OnePlus,EXIT
SAMEQ2:         DW      CHARPlus,SWAP,CHARPlus
                DW      DoLOOP,SAMEQ3
SAMEQ4:         DW      TwoDROP,Zero,EXIT


;   (search-wordlist)   ( c-addr u wid -- 0 | xt f 1 | xt f -1)
;               Search word list for a match with the given name.
;               Return execution token and not-compile-only flag and
;               -1 or 1 ( IMMEDIATE) if found. Return 0 if not found.
;
;   : (search-wordlist)
;               ROT >R SWAP DUP 0= IF -16 THROW THEN
;                               \ attempt to use zero-length string as a name
;               >R              \ wid  R: ca1 u
;               BEGIN @         \ ca2  R: ca1 u
;                  DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
;                  DUP COUNT [ =MASK ] LITERAL AND R@ = \ ca2 ca2+char f
;                     IF   R> R@ SWAP DUP >R            \ ca2 ca2+char ca1 u
;                          same?                        \ ca2 flag
;                   \ ELSE DROP -1      \ unnecessary since ca2+char is not 0.
;                     THEN
;               WHILE cell-             \ pointer to next word in wordlist
;               REPEAT
;               R> R> 2DROP DUP name>xt SWAP            \ xt ca2
;               C@ DUP [ =COMP ] LITERAL AND 0= SWAP
;               [ =IMED ] LITERAL AND 0= 2* 1+ ;
;
;                 $COLON  17,'(search-wordlist)',ParenSearch_Wordlist,_SLINK
;                 DW      ROT,ToR,SWAP,DUPP,ZBranch,PSRCH6
;                 DW      ToR
; PSRCH1:         DW      Fetch
;                 DW      DUPP,ZBranch,PSRCH9
;                 DW      DUPP,COUNT,DoLIT,MASKK,ANDD,RFetch,Equals
;                 DW      ZBranch,PSRCH5
;                 DW      RFrom,RFetch,SWAP,DUPP,ToR,SameQ
; PSRCH5:         DW      ZBranch,PSRCH3
;                 DW      CellMinus,Branch,PSRCH1
; PSRCH3:         DW      RFrom,RFrom,TwoDROP,DUPP,NameToXT,SWAP
;                 DW      CFetch,DUPP,DoLIT,COMPO,ANDD,ZeroEquals,SWAP
;                 DW      DoLIT,IMMED,ANDD,ZeroEquals,TwoStar,OnePlus,EXIT
; PSRCH9:         DW      RFrom,RFrom,TwoDROP,EXIT
; PSRCH6:         DW      DoLIT,-16,THROW

                $COLON   17,'(search-wordlist)',ParenSearch_Wordlist,_SLINK
;NAC: recode native for speed..
                DW      ROT,ToR,SWAP,DUPP,ZBranch,PSRCH6
                DW      ToR
PSRCH1:         DW      Fetch
                DW      DUPP,ZBranch,PSRCH9
                DW      DUPP,COUNT,DoLIT,MASKK,ANDD,RFetch,Equals
                DW      ZBranch,PSRCH5
                DW      RFrom,RFetch,SWAP,DUPP,ToR,SameQ
PSRCH5:         DW      ZBranch,PSRCH3
                DW      CellMinus,Branch,PSRCH1
PSRCH3:         DW      RFrom,RFrom,TwoDROP,DUPP,NameToXT,SWAP
                DW      CFetch,DUPP,DoLIT,COMPO,ANDD,ZeroEquals,SWAP
                DW      DoLIT,IMMED,ANDD,ZeroEquals,TwoStar,OnePlus,EXIT
PSRCH9:         DW      RFrom,RFrom,TwoDROP,EXIT
PSRCH6:         DW      DoLIT,-16,THROW

;   ?call       ( xt1 -- xt1 0 | a-addr xt2 )
;		If xt1 starts with a machine CALL instruction then leave
;		xt of the CALLed routine and the address of the cell following
;		the machine call at xt1. Otherwise leave xt1 and 0. See
;		optiCOMPILE for an example of usage
;
; 8086 version:
;   : ?call     DUP @ call-code =
;               IF   CELL+ DUP @ SWAP CELL+ DUP ROT + EXIT THEN
;                       \ Direct Threaded Code 8086 relative call
;               0 ;
;
; ARM version: call is one cell and contains a signed 24-bit relative
; offset. The offset must be fixed up for pipeline prefetch:
;   : ?call     DUP @ 0ff000000h AND call-code =
;		\ that call-detection is crude - not an exact check..
;		IF DUP DUP @ 00ffffffh AND    \ it's a branch.. get offset
;		007fffffh > IF
;			      00ff000000h AND
;			    ENDIF	      \ sign extend
;		2 LSHIFT 		      \ convert to byte offset
;		+ CELL+ CELL+		      \ destination address
;               SWAP CELL+ SWAP EXIT THEN
;               0 ;

                $COLON  5,'?call',QCall,_SLINK
                DW      DUPP,Fetch,DoLIT,0ff000000h,ANDD,DoLIT,CALLL,Equals
		DW	ZBranch,QCALL1,DUPP,DUPP,Fetch,0ffffffh,ANDD
		DW	07fffffh,GreaterThan,ZBranch,QCALL2
		DW	0ff000000h,ANDD
QCALL2:		DW	DoLIT,2,LSHIFT,Plus,CELLPlus,CELLPlus
		DW	SWAP,CELLPlus,SWAP,EXIT
QCALL1:         DW      Zero,EXIT

;   xt,         ( xt1 -- xt2 )
;               Take a run-time word xt1 for :NONAME , CONSTANT , VARIABLE and
;               CREATE . Return xt2 of current definition.
;
; 8086 version:
;   : xt,       xhere ALIGNED DUP TOxhere SWAP
;               call-code code,         \ Direct Threaded Code
;               xhere CELL+ - code, ;   \ 8086 relative call
;
; ARM version: call is one cell and contains a signed 24-bit relative
; offset. The offset must be fixed up for pipeline prefetch:
;   : xt,       xhere ALIGNED DUP TOxhere SWAP
;		xhere - CELL- CELL- 2 RSHIFT    \ get signed offset
;		00ffffffh AND                   \ mask off high-order sign bits
;		call_code OR code, Iflushline ;

                $COLON  3,'xt,',xtComma,_SLINK
                DW      XHere,ALIGNED,DUPP,TOXHere,SWAP
		DW	XHere,Minus,CellMinus,CellMinus,DoLIT,2,RSHIFT
		DW	DoLIT,0FFFFFFH,ANDD
		DW	CALLL,ORR,CodeComma,Iflushline,EXIT

;   doLIT       ( -- x )
;               Push an inline literal.

                $CODE   COMPO+5,'doLIT',DoLIT,_SLINK
		pushD	tos
		ldr	tos, [fpc], #CELLL	;get literal and inc to next forth word
		$NEXT

;   doCONST     ( -- x )
;               Run-time routine of CONSTANT and VARIABLE.
;		For the ARM, invoking a constant results in a bl to doCONST;
;		the return address is in r14; the value of the constant is
;		stored in-line at that address. 

                $CODE   COMPO+7,'doCONST',DoCONST,_SLINK
		pushD	tos
		ldr	tos, [r14]		;inline address from calling point
                $NEXT

;   doVALUE     ( -- x )
;               Run-time routine of VALUE. Return the value of VALUE word.
;NAC: like doCONST but with another level of indirection

                $CODE   COMPO+7,'doVALUE',DoVALUE,_SLINK
		pushD	tos
		ldr	r0, [r14]
		ldr	tos, [r0]
                $NEXT

;   doCREATE    ( -- a-addr )
;               Run-time routine of CREATE. Return address of data space.
;               Structure of CREATEd word:
;                       | call-doCREATE | 0 or DOES> code addr | a-addr |
;
;   : doCREATE    SWAP            \ switch BX and top of 8086 stack item
;                 DUP CELL+ @ SWAP @ ?DUP IF EXECUTE THEN ; COMPILE-ONLY
;
;                 $COLON  COMPO+8,'doCREATE',DoCREATE,_SLINK
;                 DW      SWAP,DUPP,CELLPlus,Fetch,SWAP,Fetch,QuestionDUP
;                 DW      ZBranch,DOCREAT1
;                 DW      EXECUTE
; DOCREAT1:       DW      EXIT

                $CODE   COMPO+8,'doCREATE',DoCREATE,_SLINK
		ldr	r0, [tos], #CELLL
		ldr	tos, [tos]		; want this whatever happens
		adds	r0, r0, #0		; set flags..
		movne	r15, r0			; go there if it wasn't 0
		$NEXT
                $ALIGN

;   doTO        ( x -- )
;               Run-time routine of TO. Store x at the address in the
;               following cell.

                $CODE   COMPO+4,'doTO',DoTO,_SLINK
		; the address to be modified is an in-line literal; get it
		; and point past
		ldr	r0, [fpc], #CELLL
		str	tos, [r0]		;modify the VALUE
		popD	tos
                $NEXT

;   doUSER      ( -- a-addr )
;               Run-time routine of USER. Return address of data space.
;NAC: like doCONST but add a constant offset to the result

                $CODE   COMPO+6,'doUSER',DoUSER,_SLINK
		pushD	tos
		ldr	tos, [r14]
		ldr	r0, = AddrUserP
		add	tos, tos, r0
                $NEXT

		LTORG

;   doLIST      ( -- ) ( R: -- nest-sys )
;               Process colon list.
;		The first word of a definition (the xt for the word) is
;		a native machine-code instruction for the target machine.
;		For high-level definitions, that code is emitted by xt,
;		and performs a call to doLIST. The function of doLIST is
;		to execute the list of xt that make up the definition.
;		The final xt in the definition is EXIT. The address of the
;		first xt to be executed by doLIST is passed in a
;		target-specific way. Two examples:
;		Z80, 8086: native machine call, leaves the return address on
;		the hardware stack pointer, which is used for the data stack
;		ARM: branch-and-link, so the return address is in R14,
;		not on the data stack.

                $CODE   COMPO+6,'doLIST',DoLIST,_SLINK
		pushR	fpc			;preserve forth PC
		mov	fpc, r14		;first xt of definition
		$NEXT

;   doLOOP      ( -- ) ( R: loop-sys1 -- | loop-sys2 )
;               Run time routine for LOOP.

                $CODE   COMPO+6,'doLOOP',DoLOOP,_SLINK
 		popR	r0			;loop count
		adds	r0, r0, #1
		bvs	%f01			;overflow -> end loop
		pushR	r0			;update loop count
		ldr	fpc, [fpc]		;loop again
		$NEXT
01		add	fpc, fpc, #CELLL	;ignore branch offset
		popR	r0			;clear up return stack
		$NEXT

;   do+LOOP     ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;               Run time routine for +LOOP.

                $CODE   COMPO+7,'do+LOOP',DoPLOOP,_SLINK
 		popR	r0
		adds	r1, r1, tos
		popD	tos
		bvs	%f01			;overflow -> end loop
		pushR	r0			;update loop count
		ldr	fpc, [fpc]		;loop again
		$NEXT
01		add	fpc, fpc, #CELLL	;ignore branch offset
		popR	r0			;clear up return stack
		$NEXT

;   0branch     ( flag -- )
;               Branch if flag is zero.

                $CODE   COMPO+7,'0branch',ZBranch,_SLINK
		orrs	tos, tos, tos		;test top of stack
;ARM's cool conditional-execution saves pipeline-draining branches 
		addne	fpc, fpc, #CELLL	;don't branch, point past dest
		ldreq	fpc, [fpc]		;branch; get destination
		popD	tos			;tidy up the stack
                $NEXT

;   branch      ( -- )
;               Branch to an inline address.

                $CODE   COMPO+6,'branch',Branch,_SLINK
		ldr	fpc, [fpc]		;get branch destination
						;and go there
                $NEXT

;   rp@         ( -- a-addr )
;               Push the current RP to the data stack.

                $CODE   COMPO+3,'rp@',RPFetch,_SLINK
		pushD	tos
		mov	tos, rsp
                $NEXT

;   rp!         ( a-addr -- )
;               Set the return stack pointer.

                $CODE   COMPO+3,'rp!',RPStore,_SLINK
		mov	rsp, tos
		popD	tos
                $NEXT

;   sp@         ( -- a-addr )
;               Push the current data stack pointer.

                $CODE   3,'sp@',SPFetch,_SLINK
		pushD	tos
		mov	tos, dsp
                $NEXT

;   sp!         ( a-addr -- )
;               Set the data stack pointer.

                $CODE   3,'sp!',SPStore,_SLINK
		mov	dsp, tos
		popD	tos
                $NEXT

;   um+         ( u1 u2 -- u3 1|0 )
;               Add two unsigned numbers, return the sum and carry.

                $CODE   3,'um+',UMPlus,_SLINK
		popD	r1
		adds	r1, r1, tos		;combine
		eor	tos, tos, tos		;clear register
		adcs	tos, tos, #0		;get carry flag
		pushD	r1			;store sum
                $NEXT

;***************
; Standard words - Processor-dependent definitions
;       32-bit Forth for ARM RISC
;***************

;   ALIGN       ( -- )                          \ CORE
;               Align the data space pointer.
;
;   : ALIGN     hereVar DUP @ ALIGNED SWAP ! ;

                $COLON  5,'ALIGN',ALIGNN,_FLINK
                DW      HereVar,DUPP,Fetch,ALIGNED,SWAP,Store,EXIT

;   ALIGNED     ( addr -- a-addr )              \ CORE
;               Align address to the cell boundary.
;
;   : ALIGNED   DUP 0 cell-size UM/MOD DROP DUP
;               IF cell-size SWAP - THEN + ;    \ slow, very portable
;
;                 $COLON  7,'ALIGNED',ALIGNED,_FLINK
;                 DW      DUPP,Zero,DoLIT,CELLL
;                 DW      UMSlashMOD,DROP,DUPP
;                 DW      ZBranch,ALGN1
;                 DW      DoLIT,CELLL,SWAP,Minus
; ALGN1:          DW      Plus,EXIT

                $CODE   7,'ALIGNED',ALIGNED,_FLINK
		and	tos, tos, #0FFFFFFFCH ;*** fix modsyntx.awk
                $NEXT

;   CELLS       ( n1 -- n2 )                    \ CORE
;               Calculate number of address units for n1 cells.
;
;   : CELLS     cell-size * ;   \ slow, very portable
;   : CELLS     2* ;            \ fast, must be redefined for each system

                $COLON  5,'CELLS',CELLS,_FLINK
                DW      TwoStar,EXIT

;   CHARS       ( n1 -- n2 )                    \ CORE
;               Calculate number of address units for n1 characters.
;
;   : CHARS     char-size * ;   \ slow, very portable
;   : CHARS     ;               \ fast, must be redefined for each system

                $COLON  5,'CHARS',CHARS,_FLINK
                DW      EXIT

;   1chars/     ( n1 -- n2 )
;               Calculate number of chars for n1 address units.
;
;   : 1chars/   1 CHARS / ;     \ slow, very portable
;   : 1chars/   ;               \ fast, must be redefined for each system

                $COLON  7,'1chars/',OneCharsSlash,_SLINK
                DW      EXIT

;   !           ( x a-addr -- )                 \ CORE
;               Store x at a aligned address.

                $CODE   1,'!',Store,_FLINK
		popD	r1			;char to store
		str	r1, [tos]
		popD	tos			;clear up stack
                $NEXT

;   0<          ( n -- flag )                   \ CORE
;               Return true if n is negative.

                $CODE   2,'0<',ZeroLess,_FLINK
		mov	r0, #0			;get zeroes for dummy arg
		add	tos, r0, tos, asr #32	;echo bit 32 value through r1
                $NEXT

;   0=          ( x -- flag )                   \ CORE
;               Return true if x is zero.

                $CODE   2,'0=',ZeroEquals,_FLINK
		cmp	tos, #0
		ldreq	tos,=TRUEE
		ldrne	tos,=FALSEE
		$NEXT

;   2*          ( x1 -- x2 )                    \ CORE
;               Bit-shift left, filling the least significant bit with 0.

                $CODE   2,'2*',TwoStar,_FLINK
		mov	tos, tos, lsl #1
                $NEXT

;   2/          ( x1 -- x2 )                    \ CORE
;               Bit-shift right, leaving the most significant bit unchanged.

                $CODE   2,'2/',TwoSlash,_FLINK
		mov	tos, tos, asr #1
                $NEXT

;   >R          ( x -- ) ( R: -- x )            \ CORE
;               Move top of the data stack item to the return stack.

                $CODE   COMPO+2,'>R',ToR,_FLINK
		pushR	tos
		popD	tos
                $NEXT

;   @           ( a-addr -- x )                 \ CORE
;               Push the contents at a-addr to the data stack.

                $CODE   1,'@',Fetch,_FLINK
		ldr	tos, [tos]
                $NEXT

;   AND         ( x1 x2 -- x3 )                 \ CORE
;               Bitwise AND.

                $CODE   3,'AND',ANDD,_FLINK
		popD	r1
		and	tos, tos, r1
                $NEXT

;   C!          ( char c-addr -- )              \ CORE
;               Store char at c-addr.

                $CODE   2,'C!',CStore,_FLINK
		popD	r1			;char to store
		strb	r1, [tos]
		popD	tos			;clear up stack
                $NEXT

;   C@          ( c-addr -- char )              \ CORE
;               Fetch the character stored at c-addr.

                $CODE   2,'C@',CFetch,_FLINK
		ldrb	tos, [tos]
                $NEXT

;   DROP        ( x -- )                        \ CORE
;               Discard top stack item.

                $CODE   4,'DROP',DROP,_FLINK
                popD	tos
                $NEXT

;   DUP         ( x -- x x )                    \ CORE
;               Duplicate the top stack item.

                $CODE   3,'DUP',DUPP,_FLINK
                pushD	tos
                $NEXT

;   EXECUTE     ( i*x xt -- j*x )               \ CORE
;               Perform the semantics indentified by execution token, xt.

                $CODE   7,'EXECUTE',EXECUTE,_FLINK
		mov	r1,tos
		popD	tos
		mov	pc,r1			;jump to the code address
                $ALIGN

;   EXIT        ( -- ) ( R: nest-sys -- )       \ CORE
;               Return control to the calling definition.

                $CODE   COMPO+4,'EXIT',EXIT,_FLINK
		popR	fpc			;where call doLIST left it
                $NEXT

;   MOVE        ( addr1 addr2 u -- )            \ CORE
;               Copy u address units from addr1 to addr2 if u is greater
;               than zero. This word is CODE defined since no other Standard
;               words can handle address unit directly.

                $CODE   4,'MOVE',MOVE,_FLINK
		popD	r0		;dest
		popD	r1		;source
		cmp	r0, r1
		bmi	%f01		;copy from end backwards
02		ldrb	r2, [r1], #1
		strb	r2, [r0], #1
		subs	tos, tos, #1
		bne	%b02
		popD	tos
		$NEXT
01		add	r0, r0, tos	;point past the last bytes
		add	r1, r1,	tos
		ldrb	r2, [r1, #-1]!	;load with predecrement
		strb	r3, [r0, #-1]!
		subs	tos, tos, #1
		bne	%b01
		popD	tos
		$NEXT

;   OR          ( x1 x2 -- x3 )                 \ CORE
;               Return bitwise inclusive-or of x1 with x2.

                $CODE   2,'OR',ORR,_FLINK
		popD	r1
		orr	tos, tos, r1
                $NEXT

;   OVER        ( x1 x2 -- x1 x2 x1 )           \ CORE
;               Copy second stack item to top of the stack.

                $CODE   4,'OVER',OVER,_FLINK
		ldr	r1, [dsp]		;copy 2nd stack item
		pushD	tos
		mov	tos, r1
                $NEXT

;   R>          ( -- x ) ( R: x -- )            \ CORE
;               Move x from the return stack to the data stack.

                $CODE   COMPO+2,'R>',RFrom,_FLINK
		pushD	tos
		popR	tos
                $NEXT

;   R@          ( -- x ) ( R: x -- x )          \ CORE
;               Copy top of return stack to the data stack.

                $CODE   COMPO+2,'R@',RFetch,_FLINK
		pushD	tos
		ldr	tos, [rsp]
                $NEXT

;   SWAP        ( x1 x2 -- x2 x1 )              \ CORE
;               Exchange top two stack items.

                $CODE   4,'SWAP',SWAP,_FLINK
		popD	r1
		pushD	tos
		mov	tos,r1
                $NEXT

;   XOR         ( x1 x2 -- x3 )                 \ CORE
;               Bitwise exclusive OR.

                $CODE   3,'XOR',XORR,_FLINK
		popD	r1
		eor	tos, tos, r1
                $NEXT

;***************
; System constants and variables
;***************

;   var0        ( -- a-addr )
;               Start of system variable area.

                $CONST  4,'var0',VarZero,RAM0,_SLINK

;   sysVar0     ( -- a-addr )
;               Start of initial value table of system variables.

                $CONST  7,'sysVar0',SysVar0,UZERO,_SLINK

;   sysVar0End  ( -- a-addr )
;               End of initial value table of system variables.

                $CONST  10,'sysVar0End',SysVar0End,ULAST,_SLINK

;   'ekey?      ( -- a-addr )
;               Execution vector of EKEY?.

                $VALUE  6,"'ekey?",TickEKEYQ,_SLINK

;   'ekey       ( -- a-addr )
;               Execution vector of EKEY.

                $VALUE  5,"'ekey",TickEKEY,_SLINK

;   'emit?      ( -- a-addr )
;               Execution vector of EMIT?.

                $VALUE  6,"'emit?",TickEMITQ,_SLINK

;   'emit       ( -- a-addr )
;               Execution vector of EMIT.

                $VALUE  5,"'emit",TickEMIT,_SLINK

;   'init-i/o   ( -- a-addr )
;               Execution vector to initialize input/output devices.

                $VALUE  9,"'init-i/o",TickINIT_IO,_SLINK

;   'prompt     ( -- a-addr )
;               Execution vector of '.prompt'.

                $VALUE  7,"'prompt",TickPrompt,_SLINK

;   'boot       ( -- a-addr )
;               Execution vector of COLD.

                $VALUE  5,"'boot",TickBoot,_SLINK

;   SOURCE-ID   ( -- 0 | -1 )                   \ CORE EXT
;               Identify the input source. -1 for string (via EVALUATE) and
;               0 for user input device.

                $VALUE  9,'SOURCE-ID',SOURCE_ID,_FLINK
AddrSOURCE_ID   EQU     _VAR -CELLL

;   cpVar       ( -- a-addr )
;               Point to the top of the code dictionary.

                $VALUE  5,'cpVar',CPVar,_SLINK
AddrCPVar       EQU     _VAR -CELLL

;   npVar       ( -- a-addr )
;               Point to the bottom of the name dictionary.

                $VALUE  5,'npVar',NPVar,_SLINK
AddrNPVar       EQU     _VAR -CELLL

;   hereVar     ( -- a-addr )
;               Point to the RAM/ROM data space pointer. Used by , or ALLOT.

                $VALUE  7,'hereVar',HereVar,_SLINK
AddrHereVar     EQU     _VAR -CELLL

;   'doWord     ( -- a-addr )
;               Execution vectors for 'interpret'.

                $VAR    7,"'doWord",TickDoWord,_SLINK
_VAR 		SETA _VAR +5*CELLL

;   BASE        ( -- a-addr )                   \ CORE
;               Return the address of the radix base for numeric I/O.

                $VAR    4,'BASE',BASE,_FLINK

;   THROWMsgTbl ( -- a-addr )                   \ CORE
;               Return the address of the THROW message table.

                $CONST  11,'THROWMsgTbl',THROWMsgTbl,AddrTHROWMsgTbl,_SLINK

;   ROMB        ( -- a-addr )
;               Bottom of free ROM area.

                $VAR    4,'ROMB',ROMB,_SLINK
AddrROMB        EQU     _VAR -CELLL

;   ROMT        ( -- a-addr )
;               Top of free ROM area.

                $VAR    4,'ROMT',ROMT,_SLINK
AddrROMT        EQU     _VAR -CELLL

;   RAMB        ( -- a-addr )
;               Bottom of free RAM area.

                $VAR    4,'RAMB',RAMB,_SLINK
AddrRAMB        EQU     _VAR -CELLL

;   RAMT        ( -- a-addr )
;               Top of free RAM area.

                $VAR    4,'RAMT',RAMT,_SLINK
AddrRAMT        EQU     _VAR -CELLL

;   lastXT      ( -- xt )
;               Return xt of the last definition.

                $VALUE  6,'lastXT',LastXT,_SLINK
AddrLastXT      EQU     _VAR -CELLL

;   rakeVar     ( -- a-addr )
;               Used by 'rake' to gather LEAVE.

                $VAR   7,'rakeVar',RakeVar,_SLINK

;   #order      ( -- a-addr )
;               Hold the search order stack depth.

                $VAR    6,'#order',NumberOrder,_SLINK
_VAR		SETA _VAR +OrderDepth*CELLL   ;search order stack

;   current     ( -- a-addr )
;               Point to the wordlist to be extended.

                $VAR    7,'current',Current,_SLINK

;   FORTH-WORDLIST   ( -- wid )                 \ SEARCH
;               Return wid of Forth wordlist.

                $VAR    14,'FORTH-WORDLIST',FORTH_WORDLIST,_FLINK
FORTH_WORDLISTAddr      EQU     _VAR -CELLL
;;;NAC can't use _NAME .. goal is to get the address of the counted string?
;;;FORTH_WORDLISTName      EQU     _NAME +2*CELLL
		$GET_NAME "+2*CELLL" FORTH_WORDLISTName

_VAR 		SETA _VAR +2*CELLL

;   NONSTANDARD-WORDLIST   ( -- wid )
;               Return wid of non-standard wordlist.

                $VAR    20,'NONSTANDARD-WORDLIST',NONSTANDARD_WORDLIST,_FLINK
NONSTANDARD_WORDLISTAddr        EQU     _VAR -CELLL
;;;NAC can't use _NAME
;;;NONSTANDARD_WORDLISTName        EQU     _NAME +2*CELLL
		$GET_NAME "+2*CELLL" NONSTANDARD_WORDLISTName

_VAR		SETA _VAR +2*CELLL
_VAR		SETA _VAR +3*(MaxWLISTS-2)*CELLL

;NAC typo: SET-CURRET should say SET-CURRENT
;   envQList    ( -- wid )
;               Return wid of ENVIRONMENT? string list. Never put this wid in
;               search-order. It should be used only by SET-CURRET to add new
;               environment query string after addition of a complete wordset.

                $VAR    8,'envQList',EnvQList,_SLINK

;   userP       ( -- a-addr )
;               Return address of USER variable area of current task.
                $VAR    5,'userP',UserP,_SLINK
;NAC: My version to set AddrUserP seems simpler and just as portable
AddrUserP	EQU _VAR - CELLL
;_CODE   = $
;ORG _VAR -CELLL
;AddrUserP       DW      ?
;ORG _CODE
;NAC: makesrc.awk uses SETA to keep _VAR correct and so the following stuff
;NAC should work out fine.
SysTask         EQU     _VAR-0
_VAR		SETA _VAR + CELLL

SysUser1        EQU     _VAR-0                  ;user1
_VAR		SETA _VAR + CELLL             
SysTaskName     EQU     _VAR-0                  ;taskName
_VAR		SETA _VAR + CELLL             
SysThrowFrame   EQU     _VAR-0                  ;throwFrame
_VAR		SETA _VAR + CELLL
SysStackTop     EQU     _VAR-0                  ;stackTop
_VAR		SETA _VAR + CELLL
SysStatus       EQU     _VAR-0                  ;status
_VAR		SETA _VAR + CELLL
SysUserP        EQU     _VAR-0
SysFollower     EQU     _VAR-0                  ;follower
_VAR		SETA _VAR + CELLL
_VAR		SETA _VAR + CELLL             ;SP0 for system task
_VAR		SETA _VAR + CELLL             ;RP0 for system task

;   SystemTask  ( -- a-addr )
;               Return system task's tid.

                $CONST  10,'SystemTask',SystemTask,SysTask,_SLINK
;;;NAC what's it *for* and what is it doing in the name dictionary?
;;;SystemTaskName  EQU     _NAME-0
		$GET_NAME "+0" SystemTaskName

;   follower    ( -- a-addr )
;               Point next task's 'status' USER variable.

                $USER   8,'follower',Follower,SysFollower-SysUserP,_SLINK

;   status      ( -- a-addr )
;               Status of current task. Point 'pass' or 'wake'.

                $USER   6,'status',Status,SysStatus-SysUserP,_SLINK

;   stackTop    ( -- a-addr )
;               Store current task's top of stack position.

                $USER   8,'stackTop',StackTop,SysStackTop-SysUserP,_SLINK

;   throwFrame  ( -- a-addr )
;               THROW frame for CATCH and THROW need to be saved for eack task.

                $USER   10,'throwFrame',ThrowFrame,SysThrowFrame-SysUserP,_SLINK

;   taskName    ( -- a-addr )
;               Current task's task ID.

                $USER   8,'taskName',TaskName,SysTaskName-SysUserP,_SLINK

;   user1       ( -- a-addr )
;               One free USER variable for each task.

                $USER   5,'user1',User1,SysUser1-SysUserP,_SLINK

; ENVIRONMETN? strings can be searched using SEARCH-WORDLIST and can be
; EXECUTEd. This wordlist is completely hidden to Forth system except
; ENVIRONMENT? .

                $ENVIR  8,'systemID'
                DW      DoLIT,SystemIDStr,COUNT,EXIT

                $ENVIR  7,'version'
                DW      DoLIT,VersionStr,COUNT,EXIT

                $ENVIR  15,'/COUNTED-STRING'
                DW      DoLIT,MaxChar,EXIT

                $ENVIR  5,'/HOLD'
                DW      DoLIT,PADSize,EXIT

                $ENVIR  4,'/PAD'
                DW      DoLIT,PADSize,EXIT

                $ENVIR  17,'ADDRESS-UNIT-BITS'
                DW      DoLIT,8,EXIT

                $ENVIR  4,'CORE'
                DW      DoLIT,TRUEE,EXIT           ;true

                $ENVIR  7,'FLOORED'
                DW      DoLIT,TRUEE,EXIT

                $ENVIR  8,'MAX-CHAR'
                DW      DoLIT,MaxChar,EXIT      ;max value of character set

                $ENVIR  5,'MAX-D'
                DW      DoLIT,MaxUnsigned,DoLIT,MaxSigned,EXIT

                $ENVIR  5,'MAX-N'
                DW      DoLIT,MaxSigned,EXIT

                $ENVIR  5,'MAX-U'
                DW      DoLIT,MaxUnsigned,EXIT

                $ENVIR  6,'MAX-UD'
                DW      DoLIT,MaxUnsigned,DoLIT,MaxUnsigned,EXIT

                $ENVIR  18,'RETURN-STACK-CELLS'
                DW      DoLIT,RTCells,EXIT

                $ENVIR  11,'STACK-CELLS'
                DW      DoLIT,DTCells,EXIT

                $ENVIR  9,'EXCEPTION'
                DW      DoLIT,TRUEE,EXIT

                $ENVIR  13,'EXCEPTION-EXT'
                DW      DoLIT,TRUEE,EXIT

                $ENVIR  9,'WORDLISTS'
                DW      DoLIT,OrderDepth,EXIT

;***************
; Non-Standard words - Colon definitions
;***************

;   (')         ( "<spaces>name" -- xt 1 | xt -1 )
;               Parse a name, find it and return execution token and
;               -1 or 1 ( IMMEDIATE) if found
;
;   : (')       PARSE-WORD search ?DUP IF NIP EXIT THEN
;               errWord 2!      \ if not found error
;               -13 THROW ;     \ undefined word

                $COLON  3,"(')",ParenTick,_SLINK
                DW      PARSE_WORD,Search,QuestionDUP,ZBranch,PTICK1
                DW      NIP,EXIT
PTICK1:         DW      ErrWord,TwoStore,DoLIT,-13,THROW

;   (d.)        ( d -- c-addr u )
;               Convert a double number to a string.
;
;   : (d.)      SWAP OVER  DUP 0< IF  DNEGATE  THEN
;               <#  #S ROT SIGN  #> ;

                $COLON  4,'(d.)',ParenDDot,_SLINK
                DW      SWAP,OVER,DUPP,ZeroLess,ZBranch,PARDD1
                DW      DNEGATE
PARDD1:         DW      LessNumberSign,NumberSignS,ROT
                DW      SIGN,NumberSignGreater,EXIT

;   .ok         ( -- )
;               Display 'ok'.
;
;   : .ok       ." ok" ;

                $COLON  3,'.ok',DotOK,_SLINK
                D$      DoDotQuote,'ok'
                DW      EXIT

;   .prompt         ( -- )
;               Disply Forth prompt. This word is vectored.
;
;   : .prompt   'prompt EXECUTE ;

                $COLON  7,'.prompt',DotPrompt,_SLINK
                DW      TickPrompt,EXECUTE,EXIT

;   0           ( -- 0 )
;               Return zero.

                $CONST  1,'0',Zero,0,_SLINK

;   1           ( -- 1 )
;               Return one.

                $CONST  1,'1',One,1,_SLINK

;   -1          ( -- -1 )
;               Return -1.

                $CONST  2,'-1',MinusOne,-1,_SLINK

;   abort"msg   ( -- a-addr )
;               Abort" error message string address.

                $VAR    9,'abort"msg',AbortQMsg,_SLINK
_VAR		SETA _VAR +CELLL

;   bal         ( -- a-addr )
;               Check for match of contol structure. bal must be zero at the
;               end of colon definition. Otherwise -22 THROW . From least
;               significant bit, 4 bits are reserved for 'orig'; 4 bits for
;               'dest'; 4 bits for 'do-sys'. Most significant 4 bits are free
;               which may be used for 'case-sys'.

                $VAR    3,'bal',Balance,_SLINK

;   orig+       ( -- )
;		Increase bal by 1.
;
;   : orig+	1 bal +! ;

		$COLON	5,'orig+',OrigPlus,_SLINK
		DW	One,Balance,PlusStore,EXIT

;   orig-	( -- )
;		Decrease bal by 1.
;
;   : orig-	-1 bal +! ;

		$COLON	5,'orig-',OrigMinus,_SLINK
		DW	MinusOne,Balance,PlusStore,EXIT

;   dest+	( -- )
;		Increase bal by 16.
;
;   : dest+	16 bal +! ;

		$COLON	5,'dest+',DestPlus,_SLINK
		DW	DoLIT,16,Balance,PlusStore,EXIT

;   dest-	( -- )
;		Decrease bal by 16.
;
;   : dest-	-16 bal +! ;

		$COLON	5,'dest-',DestMinus,_SLINK
		DW	DoLIT,-16,Balance,PlusStore,EXIT

;   dosys+	 ( -- )
;		Increase bal by 256.
;
;   : dosys+	256 bal +! ;

		$COLON	6,'dosys+',DoSysPlus,_SLINK
		DW	DoLIT,256,Balance,PlusStore,EXIT

;   dosys-	 ( -- )
;		Decrease bal by 256.
;
;   : dosys-	-256 bal +! ;

		$COLON	6,'dosys-',DoSysMinus,_SLINK
		DW	DoLIT,-256,Balance,PlusStore,EXIT

;   cell-       ( a-addr1 -- a-addr2 )
;               Return previous aligned cell address.
;
;   : cell-     -(cell-size) + ;

                $COLON  5,'cell-',CellMinus,_SLINK
                DW      DoLIT,0-CELLL,Plus,EXIT

;   COMPILE-ONLY   ( -- )
;               Make the most recent definition an compile-only word.
;
;   : COMPILE-ONLY   lastName [ =comp ] LITERAL OVER @ OR SWAP ! ;

                $COLON  12,'COMPILE-ONLY',COMPILE_ONLY,_SLINK
                DW      LastName,DoLIT,COMPO,OVER,Fetch,ORR,SWAP,Store,EXIT

;   do."        ( -- )
;               Run time routine of ." . Display a compiled string.
;
;   : do."      R> COUNT 2DUP TYPE + ALIGNED >R ; COMPILE-ONLY

                $COLON  COMPO+4,'do."',DoDotQuote,_SLINK
                DW      RFrom,COUNT,TwoDUP,TYPEE,Plus,ALIGNED,ToR,EXIT

;NAC documentation bug: stack-action is actually ( -- c-addr u)
;   doS"        ( -- c-addr )
;               Run-time function of S" .
;
;   : doS"      R> COUNT 2DUP + ALIGNED >R ; COMPILE-ONLY

                $COLON  COMPO+4,'doS"',DoSQuote,_SLINK
                DW      RFrom,COUNT,TwoDUP,Plus,ALIGNED,ToR,EXIT

;   doDO        ( n1 n2 -- ) ( R: -- n1 n2-n1-max_negative )
;               Run-time funtion of DO.
;
;   : doDO      >R max-negative + R> OVER - SWAP R> SWAP >R SWAP >R >R ;

                $COLON  COMPO+4,'doDO',DoDO,_SLINK
                DW      ToR,DoLIT,MaxNegative,Plus,RFrom
                DW      OVER,Minus,SWAP,RFrom,SWAP,ToR,SWAP,ToR,ToR,EXIT

;   errWord     ( -- a-addr )
;               Last found word. To be used to display the word causing error.

                $VAR   7,'errWord',ErrWord,_SLINK
_VAR		SETA _VAR +CELLL

;   head,       ( xt "<spaces>name" -- )
;               Parse a word and build a dictionary entry using xt and name.
;
;   : head,     DUP TO lastXT
;               PARSE-WORD DUP 0=
;               IF errWord 2! -16 THROW THEN
;                               \ attempt to use zero-length string as a name
;               DUP =mask > IF -19 THROW THEN   \ definition name too long
;               npVar @ OVER CELL+ - ALIGNED
;               DUP >R pack" DROP R>            \ pack the name in dictionary
;               DUP FIND NIP                    \ name exist?
;               IF ." redefine " DUP COUNT TYPE THEN    \ warn if redefined
;               cell- GET-CURRENT @ OVER !      \ build wordlist link
;               cell- DUP npVar !  ! ;        \ adjust name space pointer
;                                               \ and store xt at code field

                $COLON  5,'head,',HeadComma,_SLINK
                DW      DUPP,DoTO,AddrLastXT
                DW      PARSE_WORD,DUPP,ZBranch,HEADC1
                DW      DUPP,DoLIT,MASKK,GreaterThan,ZBranch,HEADC3
                DW      DoLIT,-19,THROW
HEADC3:         DW      NPVar,Fetch,OVER,CELLPlus,Minus,ALIGNED
                DW      DUPP,ToR,PackQuote,DROP,RFrom
                DW      DUPP,FIND,NIP,ZBranch,HEADC2
                D$      DoDotQuote,'redefine '
                DW      DUPP,COUNT,TYPEE
HEADC2:         DW      CellMinus,GET_CURRENT,Fetch,OVER,Store
                DW      CellMinus,DUPP,NPVar,Store,Store,EXIT
HEADC1:         DW      ErrWord,TwoStore,DoLIT,-16,THROW

;   hld         ( -- a-addr )
;               Hold a pointer in building a numeric output string.

                $VAR   3,'hld',HLD,_SLINK

;   interpret   ( i*x -- j*x )
;               Intrepret input string.
;
;   : interpret BEGIN  DEPTH 0< IF -4 THROW THEN        \ stack underflow
;                      PARSE-WORD DUP
;               WHILE  2DUP errWord 2!
;                      search               \ ca u 0 | xt f -1 | xt f 1
;                      DUP IF
;                        SWAP STATE @ OR 0= \ compile-only in interpretation
;                        IF -14 THROW THEN  \ interpreting a compile-only word
;                      THEN
;                      1+ 2* STATE @ 1+ + CELLS 'doWord + @ EXECUTE
;               REPEAT 2DROP ;

                $COLON  9,'interpret',Interpret,_SLINK
INTERP1:        DW      DEPTH,ZeroLess,ZBranch,INTERP2
                DW      DoLIT,-4,THROW
INTERP2:        DW      PARSE_WORD,DUPP,ZBranch,INTERP3
                DW      TwoDUP,ErrWord,TwoStore
                DW      Search,DUPP,ZBranch,INTERP5
                DW      SWAP,STATE,Fetch,ORR,ZBranch,INTERP4
INTERP5:        DW      OnePlus,TwoStar,STATE,Fetch,OnePlus,Plus,CELLS
                DW      TickDoWord,Plus,Fetch,EXECUTE
                DW      Branch,INTERP1
INTERP3:        DW      TwoDROP,EXIT
INTERP4:        DW      DoLIT,-14,THROW

;   optiCOMPILE, ( xt -- )
;               Optimized COMPILE, . Reduce doLIST ... EXIT sequence if
;               xt is COLON definition which contains less than two words.
;
;   : optiCOMPILE,
;               DUP ?call ['] doLIST = IF
;                   DUP @ ['] EXIT = IF         \ if first word is EXIT
;                     2DROP EXIT THEN
;                   DUP CELL+ @ ['] EXIT = IF   \ if second word is EXIT
;                     @ DUP ['] doLIT XOR  \ make sure it is not literal value
;                     IF SWAP THEN THEN
;               THEN THEN DROP COMPILE, ;

                $COLON  12,'optiCOMPILE,',OptiCOMPILEComma,_SLINK
                DW      DUPP,QCall,DoLIT,DoLIST,Equals,ZBranch,OPTC2
                DW      DUPP,Fetch,DoLIT,EXIT,Equals,ZBranch,OPTC1
                DW      TwoDROP,EXIT
OPTC1:          DW      DUPP,CELLPlus,Fetch,DoLIT,EXIT,Equals,ZBranch,OPTC2
                DW      Fetch,DUPP,DoLIT,DoLIT,XORR,ZBranch,OPTC2
                DW      SWAP
OPTC2:          DW      DROP,COMPILEComma,EXIT

;   singleOnly  ( c-addr u -- x )
;               Handle the word not found in the search-order. If the string
;               is legal, leave a single cell number in interpretation state.
;
;   : singleOnly
;               0 DUP 2SWAP OVER C@ [CHAR] -
;               = DUP >R IF 1 /STRING THEN
;               >NUMBER IF -13 THROW THEN       \ undefined word
;               2DROP R> IF NEGATE THEN ;

                $COLON  10,'singleOnly',SingleOnly,_SLINK
                DW      Zero,DUPP,TwoSWAP,OVER,CFetch,DoLIT,'-'
                DW      Equals,DUPP,ToR,ZBranch,SINGLEO4
                DW      One,SlashSTRING
SINGLEO4:       DW      ToNUMBER,ZBranch,SINGLEO1
                DW      DoLIT,-13,THROW
SINGLEO1:       DW      TwoDROP,RFrom,ZBranch,SINGLEO2
                DW      NEGATE
SINGLEO2:       DW      EXIT

;   singleOnly, ( c-addr u -- )
;               Handle the word not found in the search-order. Compile a
;               single cell number in compilation state.
;
;   : singleOnly,
;               singleOnly LITERAL ;

                $COLON  11,'singleOnly,',SingleOnlyComma,_SLINK
                DW      SingleOnly,LITERAL,EXIT

;   (doubleAlso) ( c-addr u -- x 1 | x x 2 )
;               If the string is legal, leave a single or double cell number
;               and size of the number.
;
;   : (doubleAlso)
;               0 DUP 2SWAP OVER C@ [CHAR] -
;               = DUP >R IF 1 /STRING THEN
;               >NUMBER ?DUP
;               IF   1- IF -13 THROW THEN     \ more than one char is remained
;                    DUP C@ [CHAR] . XOR      \ last char is not '.'
;                    IF -13 THROW THEN        \ undefined word
;                    R> IF DNEGATE THEN
;                    2 EXIT               THEN
;               2DROP R> IF NEGATE THEN       \ single number
;               1 ;

                $COLON  12,'(doubleAlso)',ParenDoubleAlso,_SLINK
                DW      Zero,DUPP,TwoSWAP,OVER,CFetch,DoLIT,'-'
                DW      Equals,DUPP,ToR,ZBranch,DOUBLEA1
                DW      One,SlashSTRING
DOUBLEA1:       DW      ToNUMBER,QuestionDUP,ZBranch,DOUBLEA4
                DW      OneMinus,ZBranch,DOUBLEA3
DOUBLEA2:       DW      DoLIT,-13,THROW
DOUBLEA3:       DW      CFetch,DoLIT,'.',Equals,ZBranch,DOUBLEA2
                DW      RFrom,ZBranch,DOUBLEA5
                DW      DNEGATE
DOUBLEA5:       DW      DoLIT,2,EXIT		; result is a double
DOUBLEA4:       DW      TwoDROP,RFrom,ZBranch,DOUBLEA6
                DW      NEGATE
DOUBLEA6:       DW      One,EXIT		; result is a single

;   doubleAlso  ( c-addr u -- x | x x )
;               Handle the word not found in the search-order. If the string
;               is legal, leave a single or double cell number in
;               interpretation state.
;
;   : doubleAlso
;               (doubleAlso) DROP ;

                $COLON  10,'doubleAlso',DoubleAlso,_SLINK
                DW      ParenDoubleAlso,DROP,EXIT

;   doubleAlso, ( c-addr u -- )
;               Handle the word not found in the search-order. If the string
;               is legal, compile a single or double cell number in
;               compilation state.
;
;   : doubleAlso,
;               (doubleAlso) 1- IF SWAP LITERAL THEN LITERAL ;

                $COLON  11,'doubleAlso,',DoubleAlsoComma,_SLINK
                DW      ParenDoubleAlso,OneMinus,ZBranch,DOUBC1
                DW      SWAP,LITERAL
DOUBC1:         DW      LITERAL,EXIT

;   -.          ( -- )
;               You don't need this word unless you care that '-.' returns
;               double cell number 0. Catching illegal number '-.' in this way
;               is easier than make 'interpret' catch this exeption.
;
;   : -.        -13 THROW ; IMMEDIATE   \ undefined word

                $COLON  IMMED+2,'-.',MinusDot,_SLINK
                DW      DoLIT,-13,THROW

;   lastName    ( -- c-addr )
;               Return the address of the last definition name.
;
;   : lastName  npVar @ CELL+ CELL+ ;

                $COLON  8,'lastName',LastName,_SLINK
                DW      NPVar,Fetch,CELLPlus,CELLPlus,EXIT

;   linkLast    ( -- )
;               Link the word being defined to the current wordlist.
;               Do nothing if the last definition is made by :NONAME .
;
;   : linkLast  lastXT lastName name>xt =
;               IF lastName GET-CURRENT !  0 TO lastXT THEN ;

                $COLON  8,'linkLast',LinkLast,_SLINK
                DW      LastXT,LastName,NameToXT,Equals,ZBranch,LINKL1
                DW      LastName,GET_CURRENT,Store,Zero,DoTO,AddrLastXT
LINKL1:         DW      EXIT

;   name>xt     ( c-addr -- xt )
;               Return execution token using counted string at c-addr.
;
;   : name>xt   cell- cell- @ ;

                $COLON  7,'name>xt',NameToXT,_SLINK
                DW      CellMinus,CellMinus,Fetch,EXIT

;   pack"       ( c-addr u a-addr -- a-addr2 )
;               Place a string c-addr u at a-addr and gives the next
;               cell-aligned address. Fill the rest of the last cell with
;               null character.
;
;   : pack"     2DUP SWAP CHARS + CHAR+ ALIGNED DUP >R  \ ca u aa aa+u+1
;               cell- 0 SWAP !                  \ fill 0 at the end of string
;               2DUP C! CHAR+ SWAP              \ c-addr a-addr+1 u
;               CHARS MOVE R> ; COMPILE-ONLY

                $COLON  5,'pack"',PackQuote,_SLINK
                DW      TwoDUP,SWAP,CHARS,Plus,CHARPlus,ALIGNED,DUPP,ToR
                DW      CellMinus,Zero,SWAP,Store
                DW      TwoDUP,CStore,CHARPlus,SWAP
                DW      CHARS,MOVE,RFrom,EXIT

;   PARSE-WORD  ( "<spaces>ccc<space>" -- c-addr u )
;               Skip leading spaces and parse a word. Return the name.
;
;   : PARSE-WORD   BL skipPARSE ;

                $COLON  10,'PARSE-WORD',PARSE_WORD,_SLINK
                DW      BLank,SkipPARSE,EXIT

;   pipe        ( -- ) ( R: xt -- )
;               Connect most recently defined word to code following DOES>.
;               Structure of CREATEd word:
;                       | call-doCREATE | 0 or DOES> code addr | a-addr |
;
;   : pipe      lastName name>xt ?call DUP IF   \ code-addr xt2
;                   ['] doCREATE = IF
;                   R> SWAP !           \ change DOES> code of CREATEd word
;                   EXIT
;               THEN THEN
;               -32 THROW       \ invalid name argument, no-CREATEd last name
;               ; COMPILE-ONLY

                $COLON  COMPO+4,'pipe',Pipe,_SLINK
                DW      LastName,NameToXT,QCall,DUPP,ZBranch,PIPE2
                DW      DoLIT,DoCREATE,Equals,ZBranch,PIPE2
                DW      RFrom,SWAP,Store,EXIT
PIPE2:          DW      DoLIT,-32,THROW

;   skipPARSE   ( char "<chars>ccc<char>" -- c-addr u )
;               Skip leading chars and parse a word using char as a
;               delimeter. Return the name.
;
;   : skipPARSE
;               >R SOURCE >IN @ /STRING    \ c_addr u  R: char
;               DUP IF
;                  BEGIN  OVER C@ R@ =
;                  WHILE  1- SWAP CHAR+ SWAP DUP 0=
;                  UNTIL  R> DROP EXIT
;                  ELSE THEN
;                  DROP SOURCE DROP - 1chars/ >IN ! R> PARSE EXIT
;               THEN R> DROP ;

                $COLON  9,'skipPARSE',SkipPARSE,_SLINK
                DW      ToR,SOURCE,ToIN,Fetch,SlashSTRING
                DW      DUPP,ZBranch,SKPAR1
SKPAR2:         DW      OVER,CFetch,RFetch,Equals,ZBranch,SKPAR3
                DW      OneMinus,SWAP,CHARPlus,SWAP
                DW      DUPP,ZeroEquals,ZBranch,SKPAR2
                DW      RFrom,DROP,EXIT
SKPAR3:         DW      DROP,SOURCE,DROP,Minus,OneCharsSlash
                DW      ToIN,Store,RFrom,PARSE,EXIT
SKPAR1:         DW      RFrom,DROP,EXIT

;   rake        ( C: do-sys -- )
;               Gathers LEAVEs.
;
;   : rake      DUP code, rakeVar @
;               BEGIN  2DUP U<
;               WHILE  DUP @ xhere ROT !
;               REPEAT rakeVar ! DROP
;               ?DUP IF POSTPONE THEN THEN ;    \ check for ?DO

                $COLON  COMPO+4,'rake',rake,_SLINK
                DW      DUPP,CodeComma,RakeVar,Fetch
rake1:          DW      TwoDUP,ULess,ZBranch,rake2
                DW      DUPP,Fetch,XHere,ROT,Store,Branch,rake1
rake2:          DW      RakeVar,Store,DROP
                DW      QuestionDUP,ZBranch,rake3
                DW      THENN
rake3:          DW      EXIT

;   rp0         ( -- a-addr )
;               Pointer to bottom of the return stack.
;
;   : rp0       userP @ CELL+ CELL+ @ ;

                $COLON  3,'rp0',RPZero,_SLINK
                DW      UserP,Fetch,CELLPlus,CELLPlus,Fetch,EXIT

;   search      ( c-addr u -- c-addr u 0 | xt f 1 | xt f -1)
;               Search dictionary for a match with the given name. Return
;               execution token, not-compile-only flag and -1 or 1
;               ( IMMEDIATE) if found; c-addr u 0 if not.
;
;   : search    #order @ DUP                    \ not found if #order is 0
;               IF 0
;                  DO 2DUP                      \ ca u ca u
;                     I CELLS #order CELL+ + @  \ ca u ca u wid
;                     (search-wordlist)         \ ca u; 0 | w f 1 | w f -1
;                     ?DUP IF                   \ ca u; 0 | w f 1 | w f -1
;                        >R 2SWAP 2DROP R> UNLOOP EXIT \ xt f 1 | xt f -1
;                     THEN                      \ ca u
;                  LOOP 0                       \ ca u 0
;               THEN ;

                $COLON  6,'search',Search,_SLINK
                DW      NumberOrder,Fetch,DUPP,ZBranch,SEARCH1
                DW      Zero,DoDO
SEARCH2:        DW      TwoDUP,I,CELLS,NumberOrder,CELLPlus,Plus,Fetch
                DW      ParenSearch_Wordlist,QuestionDUP,ZBranch,SEARCH3
                DW      ToR,TwoSWAP,TwoDROP,RFrom,UNLOOP,EXIT
SEARCH3:        DW      DoLOOP,SEARCH2
                DW      Zero
SEARCH1:        DW      EXIT

;   sourceVar   ( -- a-addr )
;               Hold the current count and address of the terminal input buffer.

                $VAR   9,'sourceVar',SourceVar,_SLINK
_VAR 		SETA _VAR +CELLL

;   sp0         ( -- a-addr )
;               Pointer to bottom of the data stack.
;
;   : sp0       userP @ CELL+ @ ;

                $COLON  3,'sp0',SPZero,_SLINK
                DW      UserP,Fetch,CELLPlus,Fetch,EXIT

;   TOxhere     ( a-addr -- )
;               Set the next available code space address as a-addr.
;
;   : TOxhere   cpVar ! ;

                $COLON  7,'TOxhere',TOXHere,_SLINK
                DW      CPVar,Store,EXIT

;   xhere       ( -- a-addr )
;               Return next available code space address.
;
;   : xhere     cpVar @ ;

                $COLON  5,'xhere',XHere,_SLINK
                DW      CPVar,Fetch,EXIT

;   code,       ( x -- )
;               Reserve one cell in code space and store x in it.
;
;   : code,     xhere DUP CELL+ TOxhere ! ;

                $COLON  5,'code,',CodeComma,_SLINK
                DW      XHere,DUPP,CELLPlus,TOXHere,Store,EXIT

;
; Words for multitasking
;

;   PAUSE       ( -- )
;               Stop current task and transfer control to the task of which
;               'status' USER variable is stored in 'follower' USER variable
;               of current task.
;
;   : PAUSE     rp@ sp@ stackTop !  follower @ >R ; COMPILE-ONLY

                $COLON  COMPO+5,'PAUSE',PAUSE,_SLINK
                DW      RPFetch,SPFetch,StackTop,Store,Follower,Fetch,ToR,EXIT

;   wake        ( -- )
;               Wake current task.
;
;   : wake      R> userP !      \ userP points 'follower' of current task
;               stackTop @ sp!          \ set data stack
;               rp! ; COMPILE-ONLY      \ set return stack

                $COLON  COMPO+4,'wake',Wake,_SLINK
                DW      RFrom,UserP,Store,StackTop,Fetch,SPStore,RPStore,EXIT

;***************
; Essential Standard words - Colon definitions
;***************

;   #           ( ud1 -- ud2 )                  \ CORE
;               Extract one digit from ud1 and append the digit to
;               pictured numeric output string. ( ud2 = ud1 / BASE )
;
;   : #         0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP
;               9 OVER < [ CHAR A CHAR 9 1 + - ] LITERAL AND +
;               [ CHAR 0 ] LITERAL + HOLD R> ;

                $COLON  1,'#',NumberSign,_FLINK
                DW      Zero,BASE,Fetch,UMSlashMOD,ToR,BASE,Fetch,UMSlashMOD
                DW      SWAP,DoLIT,9,OVER,LessThan,DoLIT,'A'-'9'-1,ANDD,Plus
                DW      DoLIT,'0',Plus,HOLD,RFrom,EXIT

;   #>          ( xd -- c-addr u )              \ CORE
;               Prepare the output string to be TYPE'd.
;               ||xhere>WORD/#-work-area|
;
;   : #>        2DROP hld @ xhere size-of-PAD + OVER - 1chars/ ;

                $COLON  2,'#>',NumberSignGreater,_FLINK
                DW      TwoDROP,HLD,Fetch,XHere,DoLIT,PADSize*CHARR,Plus
                DW      OVER,Minus,OneCharsSlash,EXIT

;   #S          ( ud -- 0 0 )                   \ CORE
;               Convert ud until all digits are added to the output string.
;
;   : #S        BEGIN # 2DUP OR 0= UNTIL ;

                $COLON  2,'#S',NumberSignS,_FLINK
NUMSS1:         DW      NumberSign,TwoDUP,ORR
                DW      ZeroEquals,ZBranch,NUMSS1
                DW      EXIT

;   '           ( "<spaces>name" -- xt )        \ CORE
;               Parse a name, find it and return xt.
;
;   : '         (') DROP ;

                $COLON  1,"'",Tick,_FLINK
                DW      ParenTick,DROP,EXIT

;   +           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
;               Add top two items and gives the sum.
;
;   : +         um+ DROP ;

                $COLON  1,'+',Plus,_FLINK
                DW      UMPlus,DROP,EXIT

;   +!          ( n|u a-addr -- )               \ CORE
;               Add n|u to the contents at a-addr.
;
;   : +!        SWAP OVER @ + SWAP ! ;

                $COLON  2,'+!',PlusStore,_FLINK
                DW      SWAP,OVER,Fetch,Plus
                DW      SWAP,Store,EXIT

;   ,           ( x -- )                        \ CORE
;               Reserve one cell in RAM or ROM data space and store x in it.
;
;   : ,         HERE ! cell-size hereVar +! ;

                $COLON  1,',',Comma,_FLINK
                DW      HERE,Store
                DW      DoLIT,CELLL,HereVar,PlusStore,EXIT

;   -           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
;               Subtract n2|u2 from n1|u1, giving the difference n3|u3.
;
;   : -         NEGATE + ;

                $COLON  1,'-',Minus,_FLINK
                DW      NEGATE,Plus,EXIT

;   .           ( n -- )                        \ CORE
;               Display a signed number followed by a space.
;
;   : .         S>D D. ;

                $COLON  1,'.',Dot,_FLINK
                DW      SToD,DDot,EXIT

;   /           ( n1 n2 -- n3 )                 \ CORE
;               Divide n1 by n2, giving single-cell quotient n3.
;
;   : /         /MOD NIP ;

                $COLON  1,'/',Slash,_FLINK
                DW      SlashMOD,NIP,EXIT

;   /MOD        ( n1 n2 -- n3 n4 )              \ CORE
;               Divide n1 by n2, giving single-cell remainder n3 and
;               single-cell quotient n4.
;
;   : /MOD      >R S>D R> FM/MOD ;

                $COLON  4,'/MOD',SlashMOD,_FLINK
                DW      ToR,SToD,RFrom,FMSlashMOD,EXIT

;   /STRING     ( c-addr1 u1 n -- c-addr2 u2 )  \ STRING
;               Adjust the char string at c-addr1 by n chars.
;
;   : /STRING   CHARS DUP >R - SWAP R> CHARS + SWAP ;

                $COLON  7,'/STRING',SlashSTRING,_FLINK
                DW      CHARS,DUPP,ToR,Minus
                DW      SWAP,RFrom,CHARS,Plus,SWAP,EXIT

;   1+          ( n1|u1 -- n2|u2 )              \ CORE
;               Increase top of the stack item by 1.
;
;   : 1+        1 + ;

                $COLON  2,'1+',OnePlus,_FLINK
                DW      One,Plus,EXIT

;   1-          ( n1|u1 -- n2|u2 )              \ CORE
;               Decrease top of the stack item by 1.
;
;   : 1-        -1 + ;

                $COLON  2,'1-',OneMinus,_FLINK
                DW      MinusOne,Plus,EXIT

;   2!          ( x1 x2 a-addr -- )             \ CORE
;               Store the cell pare x1 x2 at a-addr, with x2 at a-addr and
;               x1 at the next consecutive cell.
;
;   : 2!        SWAP OVER ! CELL+ ! ;

                $COLON  2,'2!',TwoStore,_FLINK
                DW      SWAP,OVER,Store,CELLPlus,Store,EXIT

;   2@          ( a-addr -- x1 x2 )             \ CORE
;               Fetch the cell pair stored at a-addr. x2 is stored at a-addr
;               and x1 at the next consecutive cell.
;
;   : 2@        DUP CELL+ @ SWAP @ ;

                $COLON  2,'2@',TwoFetch,_FLINK
                DW      DUPP,CELLPlus,Fetch,SWAP,Fetch,EXIT

;   2DROP       ( x1 x2 -- )                    \ CORE
;               Drop cell pair x1 x2 from the stack.

                $COLON  5,'2DROP',TwoDROP,_FLINK
                DW      DROP,DROP,EXIT

;   2DUP        ( x1 x2 -- x1 x2 x1 x2 )        \ CORE
;               Duplicate cell pair x1 x2.

                $COLON  4,'2DUP',TwoDUP,_FLINK
                DW      OVER,OVER,EXIT

;   2SWAP       ( x1 x2 x3 x4 -- x3 x4 x1 x2 )  \ CORE
;               Exchange the top two cell pairs.
;
;   : 2SWAP     ROT >R ROT R> ;

                $COLON  5,'2SWAP',TwoSWAP,_FLINK
                DW      ROT,ToR,ROT,RFrom,EXIT

;   :           ( -- ; <string> )               \ CORE
;               Start a new colon definition using next word as its name.
;
;   : :         :NONAME head, ;

                $COLON  1,':',COLON,_FLINK
                DW      ColonNONAME,HeadComma,EXIT

;   :NONAME     ( -- xt )                       \ CORE EXT
;               Create an execution token xt, enter compilation state and
;               start the current definition.
;
;   : :NONAME   STATE @ IF -29 THROW THEN
;               ['] doLIST xt,  0 bal ! ] ;

                $COLON  7,':NONAME',ColonNONAME,_FLINK
                DW      STATE,Fetch,ZBranch,NONAME1
                DW      DoLIT,-29,THROW
NONAME1:        DW      DoLIT,DoLIST,xtComma,Zero,Balance,Store
                DW      RightBracket,EXIT

;   ;           ( -- )                          \ CORE
;               Terminate a colon definition.
;
;   : ;         bal @ IF -22 THROW THEN         \ control structure mismatch
;               POSTPONE EXIT linkLast          \ add EXIT at the end of the
;                               \ definition and link the word to wordlist
;               [ ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+1,';',Semicolon,_FLINK
                DW      Balance,Fetch,ZBranch,SEMI1
                DW      DoLIT,-22,THROW
SEMI1:          DW      DoLIT,EXIT,COMPILEComma
                DW      LinkLast,LeftBracket,EXIT

;   <           ( n1 n2 -- flag )               \ CORE
;               Returns true if n1 is less than n2.
;
;   : <         2DUP XOR 0<             \ same sign?
;               IF DROP 0< EXIT THEN    \ different signs, true if n1 <0
;               - 0< ;                  \ same signs, true if n1-n2 <0

                $COLON  1,'<',LessThan,_FLINK
                DW      TwoDUP,XORR,ZeroLess,ZBranch,LESS1
                DW      DROP,ZeroLess,EXIT
LESS1:          DW      Minus,ZeroLess,EXIT

;   <#          ( -- )                          \ CORE
;               Initiate the numeric output conversion process.
;               ||xhere>WORD/#-work-area|
;
;   : <#        xhere size-of-PAD + hld ! ;

                $COLON  2,'<#',LessNumberSign,_FLINK
                DW      XHere,DoLIT,PADSize*CHARR,Plus,HLD,Store,EXIT

;   =           ( x1 x2 -- flag )               \ CORE
;               Return true if top two are equal.
;
;   : =         XORR 0= ;

                $COLON  1,'=',Equals,_FLINK
                DW      XORR,ZeroEquals,EXIT

;   >           ( n1 n2 -- flag )               \ CORE
;               Returns true if n1 is greater than n2.
;
;   : >         SWAP < ;

                $COLON  1,'>',GreaterThan,_FLINK
                DW      SWAP,LessThan,EXIT

;   >IN         ( -- a-addr )
;               Hold the character pointer while parsing input stream.

                $VAR    3,'>IN',ToIN,_FLINK

;   >NUMBER     ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )    \ CORE
;               Add number string's value to ud1. Leaves string of any
;               unconverted chars.
;
;   : >NUMBER   BEGIN  DUP
;               WHILE  >R  DUP >R C@                    \ ud char  R: u c-addr
;                      DUP [ CHAR 9 1+ ] LITERAL [CHAR] A WITHIN
;                          IF DROP R> R> EXIT THEN
;                      [ CHAR 0 ] LITERAL - 9 OVER <
;                      [ CHAR A CHAR 9 1 + - ] LITERAL AND -
;                      DUP 0 BASE @ WITHIN
;               WHILE  SWAP BASE @ UM* DROP ROT BASE @ UM* D+ R> R> 1 /STRING
;               REPEAT DROP R> R>
;               THEN ;

                $COLON  7,'>NUMBER',ToNUMBER,_FLINK
TONUM1:         DW      DUPP,ZBranch,TONUM3
                DW      ToR,DUPP,ToR,CFetch,DUPP
                DW      DoLIT,'9'+1,DoLIT,'A',WITHIN,ZeroEquals,ZBranch,TONUM2
                DW      DoLIT,'0',Minus,DoLIT,9,OVER,LessThan
                DW      DoLIT,'A'-'9'-1,ANDD,Minus,DUPP
                DW      Zero,BASE,Fetch,WITHIN,ZBranch,TONUM2
                DW      SWAP,BASE,Fetch,UMStar,DROP,ROT,BASE,Fetch
                DW      UMStar,DPlus,RFrom,RFrom,One,SlashSTRING
                DW      Branch,TONUM1
TONUM2:         DW      DROP,RFrom,RFrom
TONUM3:         DW      EXIT

;   ?DUP        ( x -- x x | 0 )                \ CORE
;               Duplicate top of the stack if it is not zero.
;
;   : ?DUP      DUP IF DUP THEN ;

                $COLON  4,'?DUP',QuestionDUP,_FLINK
                DW      DUPP,ZBranch,QDUP1
                DW      DUPP
QDUP1:          DW      EXIT

;   ABORT       ( i*x -- ) ( R: j*x -- )        \ EXCEPTION EXT
;               Reset data stack and jump to QUIT.
;
;   : ABORT     -1 THROW ;

                $COLON  5,'ABORT',ABORT,_FLINK
                DW      MinusOne,THROW

;   ACCEPT      ( c-addr +n1 -- +n2 )           \ CORE
;               Accept a string of up to +n1 chars. Return with actual count.
;               Implementation-defined editing. Stops at EOL# .
;               Supports backspace and delete editing.
;
;   : ACCEPT    >R 0
;               BEGIN  DUP R@ <                 \ ca n2 f  R: n1
;               WHILE  EKEY max-char AND
;                      DUP BL <
;                      IF   DUP  cr# = IF ROT 2DROP R> DROP EXIT THEN
;                           DUP  tab# =
;                           IF   DROP 2DUP + BL DUP EMIT SWAP C! 1+
;                           ELSE DUP  bsp# =
;                                SWAP del# = OR
;                                IF DROP DUP
;                                       \ discard the last char if not 1st char
;                                IF 1- bsp# EMIT BL EMIT bsp# EMIT THEN THEN
;                           THEN
;                      ELSE >R 2DUP CHARS + R> DUP EMIT SWAP C! 1+  THEN
;                      THEN
;               REPEAT SWAP  R> 2DROP ;

                $COLON  6,'ACCEPT',ACCEPT,_FLINK
                DW      ToR,Zero
ACCPT1:         DW      DUPP,RFetch,LessThan,ZBranch,ACCPT5
                DW      EKEY,DoLIT,MaxChar,ANDD
                DW      DUPP,BLank,LessThan,ZBranch,ACCPT3
                DW      DUPP,DoLIT,CRR,Equals,ZBranch,ACCPT4
                DW      ROT,TwoDROP,RFrom,DROP,EXIT
ACCPT4:         DW      DUPP,DoLIT,TABB,Equals,ZBranch,ACCPT6
                DW      DROP,TwoDUP,Plus,BLank,DUPP,EMIT,SWAP,CStore,OnePlus
                DW      Branch,ACCPT1
ACCPT6:         DW      DUPP,DoLIT,BKSPP,Equals
                DW      SWAP,DoLIT,DEL,Equals,ORR,ZBranch,ACCPT1
                DW      DUPP,ZBranch,ACCPT1
                DW      OneMinus,DoLIT,BKSPP,EMIT,BLank,EMIT,DoLIT,BKSPP,EMIT
                DW      Branch,ACCPT1
ACCPT3:         DW      ToR,TwoDUP,CHARS,Plus,RFrom,DUPP,EMIT,SWAP,CStore
                DW      OnePlus,Branch,ACCPT1
ACCPT5:         DW      SWAP,RFrom,TwoDROP,EXIT

;   AGAIN       ( C: dest -- )                  \ CORE EXT
;               Resolve backward reference dest. Typically used as
;               BEGIN ... AGAIN . Move control to the location specified by
;               dest on execution.
;
;   : AGAIN     POSTPONE branch code, dest- ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+5,'AGAIN',AGAIN,_FLINK
                DW      DoLIT,Branch,COMPILEComma,CodeComma,DestMinus,EXIT

;   AHEAD       ( C: -- orig )                  \ TOOLS EXT
;               Put the location of a new unresolved forward reference onto
;               control-flow stack.
;
;   : AHEAD     POSTPONE branch xhere 0 code, orig+ ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+5,'AHEAD',AHEAD,_FLINK
                DW      DoLIT,Branch,COMPILEComma
                DW      XHere,Zero,CodeComma,OrigPlus,EXIT

;NAC: this was processed incorrectly by makesrc - fixed by defining SPC
;instead of using ' '
;   BL          ( -- char )                     \ CORE
;               Return the value of the blank character.
;
;   : BL        blank-char-value EXIT ;

                $CONST  2,'BL',BLank,SPC,_FLINK

;   CATCH       ( i*x xt -- j*x 0 | i*x n )     \ EXCEPTION
;               Push an exception frame on the exception stack and then execute
;               the execution token xt in such a way that control can be
;               transferred to a point just after CATCH if THROW is executed
;               during the execution of xt.
;
;   : CATCH     sp@ >R throwFrame @ >R          \ save error frame
;               rp@ throwFrame !  EXECUTE       \ execute
;               R> throwFrame !                 \ restore error frame
;               R> DROP  0 ;                    \ no error

                $COLON  5,'CATCH',CATCH,_FLINK
                DW      SPFetch,ToR,ThrowFrame,Fetch,ToR
                DW      RPFetch,ThrowFrame,Store,EXECUTE
                DW      RFrom,ThrowFrame,Store
                DW      RFrom,DROP,Zero,EXIT

;   CELL+       ( a-addr1 -- a-addr2 )          \ CORE
;               Return next aligned cell address.
;
;   : CELL+     cell-size + ;

                $COLON  5,'CELL+',CELLPlus,_FLINK
                DW      DoLIT,CELLL,Plus,EXIT

;   CHAR+       ( c-addr1 -- c-addr2 )          \ CORE
;               Returns next character-aligned address.
;
;   : CHAR+     char-size + ;

                $COLON  5,'CHAR+',CHARPlus,_FLINK
                DW      DoLIT,CHARR,Plus,EXIT

;   COMPILE,    ( xt -- )                       \ CORE EXT
;               Compile the execution token on data stack into current
;               colon definition.
;
;   : COMPILE,  code, ; COMPILE-ONLY

                $COLON  COMPO+8,'COMPILE,',COMPILEComma,_FLINK
                DW      CodeComma,EXIT

;   CONSTANT    ( x "<spaces>name" -- )         \ CORE
;               name Execution: ( -- x )
;               Create a definition for name which pushes x on the stack on
;               execution.
;
;   : CONSTANT  ['] doCONST xt, head, code, linkLast ;

                $COLON  8,'CONSTANT',CONSTANT,_FLINK
                DW      DoLIT,DoCONST,xtComma,HeadComma,CodeComma,LinkLast,EXIT

;   COUNT       ( c-addr1 -- c-addr2 u )        \ CORE
;               Convert counted string to string specification. c-addr2 is
;               the next char-aligned address after c-addr1 and u is the
;               contents at c-addr1.
;
;   : COUNT     DUP CHAR+ SWAP C@ ;

                $COLON  5,'COUNT',COUNT,_FLINK
                DW      DUPP,CHARPlus,SWAP,CFetch,EXIT

;   CREATE      ( "<spaces>name" -- )           \ CORE
;               name Execution: ( -- a-addr )
;               Create a data object in RAM/ROM data space, which return
;               data object address on execution
;
;   : CREATE    ['] doCREATE xt, head,
;               xhere DUP CELL+ CELL+ TOxhere   \ reserve two cells
;               0 OVER !                \ no DOES> code yet
;               ALIGN HERE SWAP CELL+ ! \ >BODY returns this address
;               linkLast ;              \ link CREATEd word to current wordlist

                $COLON  6,'CREATE',CREATE,_FLINK
                DW      DoLIT,DoCREATE,xtComma,HeadComma
                DW      XHere,DUPP,CELLPlus,CELLPlus,TOXHere
                DW      Zero,OVER,Store
                DW      ALIGNN,HERE,SWAP,CELLPlus,Store
                DW      LinkLast,EXIT

;   D+          ( d1|ud1 d2|ud2 -- d3|ud3 )     \ DOUBLE
;               Add double-cell numbers.
;
;   : D+        >R SWAP >R um+ R> R> + + ;

                $COLON  2,'D+',DPlus,_FLINK
                DW      ToR,SWAP,ToR,UMPlus
                DW      RFrom,RFrom,Plus,Plus,EXIT

;   D.          ( d -- )                        \ DOUBLE
;               Display d in free field format followed by a space.
;
;   : D.        (d.) TYPE SPACE ;

                $COLON  2,'D.',DDot,_FLINK
                DW      ParenDDot,TYPEE,SPACE,EXIT

;   DECIMAL     ( -- )                          \ CORE
;               Set the numeric conversion radix to decimal 10.
;
;   : DECIMAL   10 BASE ! ;

                $COLON  7,'DECIMAL',DECIMAL,_FLINK
                DW      DoLIT,10,BASE,Store,EXIT

;   DEPTH       ( -- +n )                       \ CORE
;               Return the depth of the data stack.
;
;   : DEPTH     sp@ sp0 SWAP - cell-size / ;

                $COLON  5,'DEPTH',DEPTH,_FLINK
                DW      SPFetch,SPZero,SWAP,Minus
                DW      DoLIT,CELLL,Slash,EXIT

;   DNEGATE     ( d1 -- d2 )                    \ DOUBLE
;               Two's complement of double-cell number.
;
;   : DNEGATE   INVERT >R INVERT 1 um+ R> + ;

                $COLON  7,'DNEGATE',DNEGATE,_FLINK
                DW      INVERT,ToR,INVERT
                DW      One,UMPlus
                DW      RFrom,Plus,EXIT

;   EKEY        ( -- u )                        \ FACILITY EXT
;               Receive one keyboard event u.
;
;   : EKEY      BEGIN PAUSE EKEY? UNTIL 'ekey EXECUTE ;

                $COLON  4,'EKEY',EKEY,_FLINK
EKEY1:          DW      PAUSE,EKEYQuestion,ZBranch,EKEY1
                DW      TickEKEY,EXECUTE,EXIT

;   EMIT        ( x -- )                        \ CORE
;               Send a character to the output device.
;
;   : EMIT      'emit EXECUTE ;

                $COLON  4,'EMIT',EMIT,_FLINK
                DW      TickEMIT,EXECUTE,EXIT

;   FIND        ( c-addr -- c-addr 0 | xt 1 | xt -1)     \ SEARCH
;               Search dictionary for a match with the given counted name.
;               Return execution token and -1 or 1 ( IMMEDIATE) if found;
;               c-addr 0 if not found.
;
;   : FIND      DUP COUNT search ?DUP IF NIP ROT DROP EXIT THEN
;               2DROP 0 ;

                $COLON  4,'FIND',FIND,_FLINK
                DW      DUPP,COUNT,Search,QuestionDUP,ZBranch,FIND1
                DW      NIP,ROT,DROP,EXIT
FIND1:          DW      TwoDROP,Zero,EXIT

;   FM/MOD      ( d n1 -- n2 n3 )               \ CORE
;               Signed floored divide of double by single. Return mod n2
;               and quotient n3.
;
;   : FM/MOD    DUP >R 2DUP XOR >R >R DUP 0< IF DNEGATE THEN
;               R@ ABS UM/MOD DUP 0<
;               IF DUP 08000h XOR IF -11 THROW THEN THEN \ result out of range
;               SWAP R> 0< IF NEGATE THEN
;               SWAP R> 0< IF NEGATE OVER IF R@ ROT - SWAP 1- THEN THEN
;               R> DROP ;

                $COLON  6,'FM/MOD',FMSlashMOD,_FLINK
                DW      DUPP,ToR,TwoDUP,XORR,ToR,ToR,DUPP,ZeroLess
                DW      ZBranch,FMMOD1
                DW      DNEGATE
FMMOD1:         DW      RFetch,ABSS,UMSlashMOD,DUPP,ZeroLess,ZBranch,FMMOD2
;NAC: Was architecture-dependent. Changed 08000h to MaxNegative
                DW      DUPP,DoLIT,MaxNegative,XORR,ZBranch,FMMOD2
                DW      DoLIT,-11,THROW
FMMOD2:         DW      SWAP,RFrom,ZeroLess,ZBranch,FMMOD3
                DW      NEGATE
FMMOD3:         DW      SWAP,RFrom,ZeroLess,ZBranch,FMMOD4
                DW      NEGATE,OVER,ZBranch,FMMOD4
                DW      RFetch,ROT,Minus,SWAP,OneMinus
FMMOD4:         DW      RFrom,DROP,EXIT

;   GET-CURRENT   ( -- wid )                    \ SEARCH
;               Return the indentifier of the compilation wordlist.
;
;   : GET-CURRENT   current @ ;

                $COLON  11,'GET-CURRENT',GET_CURRENT,_FLINK
                DW      Current,Fetch,EXIT

;   HERE        ( -- addr )                     \ CORE
;               Return data space pointer.
;
;   : HERE      hereVar @ ;

                $COLON  4,'HERE',HERE,_FLINK
                DW      HereVar,Fetch,EXIT

;   HOLD        ( char -- )                     \ CORE
;               Add char to the beginning of pictured numeric output string.
;
;   : HOLD      hld @  1 CHARS - DUP hld ! C! ;

                $COLON  4,'HOLD',HOLD,_FLINK
                DW      HLD,Fetch,DoLIT,0-CHARR,Plus
                DW      DUPP,HLD,Store,CStore,EXIT

;   I           ( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
;               Push the innermost loop index.
;
;   : I         rp@ [ 1 CELLS ] LITERAL + @
;               rp@ [ 2 CELLS ] LITERAL + @  +  ; COMPILE-ONLY

                $COLON  COMPO+1,'I',I,_FLINK
                DW      RPFetch,DoLIT,CELLL,Plus,Fetch
                DW      RPFetch,DoLIT,2*CELLL,Plus,Fetch,Plus,EXIT

;   IF          Compilation: ( C: -- orig )             \ CORE
;               Run-time: ( x -- )
;               Put the location of a new unresolved forward reference orig
;               onto the control flow stack. On execution jump to location
;               specified by the resolution of orig if x is zero.
;
;   : IF        POSTPONE 0branch xhere 0 code, orig+
;               ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+2,'IF',IFF,_FLINK
                DW      DoLIT,ZBranch,COMPILEComma,XHere
                DW      Zero,CodeComma,OrigPlus,EXIT

;   INVERT      ( x1 -- x2 )                    \ CORE
;               Return one's complement of x1.
;
;   : INVERT    -1 XOR ;

                $COLON  6,'INVERT',INVERT,_FLINK
                DW      MinusOne,XORR,EXIT

;   KEY         ( -- char )                     \ CORE
;               Receive a character. Do not display char.
;
;   : KEY       EKEY max-char AND ;

                $COLON  3,'KEY',KEY,_FLINK
                DW      EKEY,DoLIT,MaxChar,ANDD,EXIT

;   LITERAL     Compilation: ( x -- )           \ CORE
;               Run-time: ( -- x )
;               Append following run-time semantics. Put x on the stack on
;               execution
;
;   : LITERAL   POSTPONE doLIT code, ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+7,'LITERAL',LITERAL,_FLINK
                DW      DoLIT,DoLIT,COMPILEComma,CodeComma,EXIT

;   NEGATE      ( n1 -- n2 )                    \ CORE
;               Return two's complement of n1.
;
;   : NEGATE    INVERT 1+ ;

                $COLON  6,'NEGATE',NEGATE,_FLINK
                DW      INVERT,OnePlus,EXIT

;   NIP         ( n1 n2 -- n2 )                 \ CORE EXT
;               Discard the second stack item.
;
;   : NIP       SWAP DROP ;

                $COLON  3,'NIP',NIP,_FLINK
                DW      SWAP,DROP,EXIT

;   PARSE       ( char "ccc<char>"-- c-addr u )         \ CORE EXT
;               Scan input stream and return counted string delimited by char.
;
;   : PARSE     >R  SOURCE >IN @ /STRING        \ c-addr u  R: char
;               DUP IF
;                  OVER CHARS + OVER       \ c-addr c-addr+u c-addr  R: char
;                  BEGIN  DUP C@ R@ XOR
;                  WHILE  CHAR+ 2DUP =
;                  UNTIL  DROP OVER - 1chars/ DUP
;                  ELSE   NIP  OVER - 1chars/ DUP CHAR+
;                  THEN   >IN +!
;               THEN   R> DROP EXIT ;

                $COLON  5,'PARSE',PARSE,_FLINK
                DW      ToR,SOURCE,ToIN,Fetch,SlashSTRING
                DW      DUPP,ZBranch,PARSE4
                DW      OVER,CHARS,Plus,OVER
PARSE1:         DW      DUPP,CFetch,RFetch,XORR,ZBranch,PARSE3
                DW      CHARPlus,TwoDUP,Equals,ZBranch,PARSE1
PARSE2:         DW      DROP,OVER,Minus,DUPP,OneCharsSlash,Branch,PARSE5
PARSE3:         DW      NIP,OVER,Minus,DUPP,OneCharsSlash,CHARPlus
PARSE5:         DW      ToIN,PlusStore
PARSE4:         DW      RFrom,DROP,EXIT

;   QUIT        ( -- ) ( R: i*x -- )            \ CORE
;               Empty the return stack, store zero in SOURCE-ID, make the user
;               input device the input source, and start text interpreter.
;
;   : QUIT      BEGIN
;                 rp0 rp!  0 TO SOURCE-ID  POSTPONE [
;                 BEGIN CR REFILL DROP SPACE    \ REFILL returns always true
;                       ['] interpret CATCH ?DUP 0=
;                 WHILE STATE @ 0= IF .prompt THEN
;                 REPEAT
;                 DUP -1 XOR IF                                 \ ABORT
;                 DUP -2 = IF SPACE abort"msg 2@ TYPE    ELSE   \ ABORT"
;                 SPACE errWord 2@ TYPE
;                 SPACE [CHAR] ? EMIT SPACE
;                 DUP -1 -NumTHROWMsgs WITHIN IF ." Exception # " . ELSE \ undefined exeption
;                 CELLS THROWMsgTbl + @ COUNT TYPE       THEN THEN THEN
;                 sp0 sp!
;               AGAIN ;

                $COLON  4,'QUIT',QUIT,_FLINK
QUIT1:          DW      RPZero,RPStore,Zero,DoTO,AddrSOURCE_ID,LeftBracket
QUIT2:          DW      CR,REFILL,DROP,SPACE
                DW      DoLIT,Interpret,CATCH,QuestionDUP,ZeroEquals
                DW      ZBranch,QUIT3
                DW      STATE,Fetch,ZeroEquals,ZBranch,QUIT2
                DW      DotPrompt,Branch,QUIT2
QUIT3:          DW      DUPP,MinusOne,XORR,ZBranch,QUIT5
                DW      DUPP,DoLIT,-2,Equals,ZBranch,QUIT4
                DW      SPACE,AbortQMsg,TwoFetch,TYPEE,Branch,QUIT5
QUIT4:          DW      SPACE,ErrWord,TwoFetch,TYPEE
                DW      SPACE,DoLIT,'?',EMIT,SPACE
                DW      DUPP,MinusOne,DoLIT,-1*NumTHROWMsgs,WITHIN,ZBranch,QUIT7
                D$      DoDotQuote,' Exception # '
                DW      Dot,Branch,QUIT5
QUIT7:          DW      CELLS,THROWMsgTbl,Plus,Fetch,COUNT,TYPEE
QUIT5:          DW      SPZero,SPStore,Branch,QUIT1

;   REFILL      ( -- flag )                     \ CORE EXT
;               Attempt to fill the input buffer from the input source. Make
;               the result the input buffer, set >IN to zero, and return true
;               if successful. Return false if the input source is a string
;               from EVALUATE.
;
;   : REFILL    SOURCE-ID IF 0 EXIT THEN
;               npVar @ [ size-of-PAD CHARS 2* ] LITERAL - DUP
;               size-of-PAD ACCEPT sourceVar 2!
;               0 >IN ! -1 ;

                $COLON  6,'REFILL',REFILL,_FLINK
                DW      SOURCE_ID,ZBranch,REFIL1
                DW      Zero,EXIT
REFIL1:         DW      NPVar,DoLIT,0-PADSize*CHARR*2,Plus,DUPP
                DW      DoLIT,PADSize*CHARR,ACCEPT,SourceVar,TwoStore
                DW      Zero,ToIN,Store,MinusOne,EXIT

;   ROT         ( x1 x2 x3 -- x2 x3 x1 )        \ CORE
;               Rotate the top three data stack items.
;
;   : ROT       >R SWAP R> SWAP ;

                $COLON  3,'ROT',ROT,_FLINK
                DW      ToR,SWAP,RFrom,SWAP,EXIT

;   S>D         ( n -- d )                      \ CORE
;               Convert a single-cell number n to double-cell number.
;
;   : S>D       DUP 0< ;

                $COLON  3,'S>D',SToD,_FLINK
                DW      DUPP,ZeroLess,EXIT

;   SEARCH-WORDLIST     ( c-addr u wid -- 0 | xt 1 | xt -1)     \ SEARCH
;               Search word list for a match with the given name.
;               Return execution token and -1 or 1 ( IMMEDIATE) if found.
;               Return 0 if not found.
;
;   : SEARCH-WORDLIST
;               (search-wordlist) DUP IF NIP THEN ;

                $COLON  15,'SEARCH-WORDLIST',SEARCH_WORDLIST,_FLINK
                DW      ParenSearch_Wordlist,DUPP,ZBranch,SRCHW1
                DW      NIP
SRCHW1:         DW      EXIT

;   SIGN        ( n -- )                        \ CORE
;               Add a minus sign to the numeric output string if n is negative.
;
;   : SIGN      0< IF [CHAR] - HOLD THEN ;

                $COLON  4,'SIGN',SIGN,_FLINK
                DW      ZeroLess,ZBranch,SIGN1
                DW      DoLIT,'-',HOLD
SIGN1:          DW      EXIT

;   SOURCE      ( -- c-addr u )                 \ CORE
;               Return input buffer string.
;
;   : SOURCE    sourceVar 2@ ;

                $COLON  6,'SOURCE',SOURCE,_FLINK
                DW      SourceVar,TwoFetch,EXIT

;   SPACE       ( -- )                          \ CORE
;               Send the blank character to the output device.
;
;   : SPACE     32 EMIT ;

                $COLON  5,'SPACE',SPACE,_FLINK
                DW      BLank,EMIT,EXIT

;   STATE       ( -- a-addr )                   \ CORE
;               Return the address of a cell containing compilation-state flag
;               which is true in compilation state or false otherwise.

                $VAR    5,'STATE',STATE,_FLINK

;   THEN        Compilation: ( C: orig -- )     \ CORE
;               Run-time: ( -- )
;               Resolve the forward reference orig.
;
;   : THEN      xhere SWAP ! orig- ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+4,'THEN',THENN,_FLINK
                DW      XHere,SWAP,Store,OrigMinus,EXIT

;   THROW       ( k*x n -- k*x | i*x n )        \ EXCEPTION
;               If n is not zero, pop the topmost exception frame from the
;               exception stack, along with everything on the return stack
;               above the frame. Then restore the condition before CATCH and
;               transfer control just after the CATCH that pushed that
;               exception frame.
;
;   : THROW     ?DUP
;               IF   throwFrame @ rp!   \ restore return stack
;                    R> throwFrame !    \ restore THROW frame
;                    R> SWAP >R sp!     \ restore data stack
;                    DROP R>
;                    'init-i/o EXECUTE
;               THEN ;

                $COLON  5,'THROW',THROW,_FLINK
                DW      QuestionDUP,ZBranch,THROW1
                DW      ThrowFrame,Fetch,RPStore,RFrom,ThrowFrame,Store
                DW      RFrom,SWAP,ToR,SPStore,DROP,RFrom
                DW      TickINIT_IO,EXECUTE
THROW1:         DW      EXIT

;   TYPE        ( c-addr u -- )                 \ CORE
;               Display the character string if u is greater than zero.
;
;   : TYPE      ?DUP IF 0 DO DUP C@ EMIT CHAR+ LOOP THEN DROP ;

                $COLON  4,'TYPE',TYPEE,_FLINK
                DW      QuestionDUP,ZBranch,TYPE2
                DW      Zero,DoDO
TYPE1:          DW      DUPP,CFetch,EMIT,CHARPlus,DoLOOP,TYPE1
TYPE2:          DW      DROP,EXIT

;   U<          ( u1 u2 -- flag )               \ CORE
;               Unsigned compare of top two items. True if u1 < u2.
;
;   : U<        2DUP XOR 0< IF NIP 0< EXIT THEN - 0< ;

                $COLON  2,'U<',ULess,_FLINK
                DW      TwoDUP,XORR,ZeroLess
                DW      ZBranch,ULES1
                DW      NIP,ZeroLess,EXIT
ULES1:          DW      Minus,ZeroLess,EXIT

;   UM*         ( u1 u2 -- ud )                 \ CORE
;               Unsigned multiply. Return double-cell product.
;
;   : UM*       0 SWAP cell-size-in-bits 0 DO
;                  DUP um+ >R >R DUP um+ R> +
;                  R> IF >R OVER um+ R> + THEN     \ if carry
;               LOOP ROT DROP ;

                $COLON  3,'UM*',UMStar,_FLINK
                DW      Zero,SWAP,DoLIT,CELLL*8,Zero,DoDO
UMST1:          DW      DUPP,UMPlus,ToR,ToR
                DW      DUPP,UMPlus,RFrom,Plus,RFrom
                DW      ZBranch,UMST2
                DW      ToR,OVER,UMPlus,RFrom,Plus
UMST2:          DW      DoLOOP,UMST1
                DW      ROT,DROP,EXIT

;   UM/MOD      ( ud u1 -- u2 u3 )              \ CORE
;               Unsigned division of a double-cell number ud by a single-cell
;               number u1. Return remainder u2 and quotient u3.
;
;   : UM/MOD    DUP 0= IF -10 THROW THEN        \ divide by zero
;               2DUP U< IF
;                  NEGATE cell-size-in-bits 0
;                  DO   >R DUP um+ >R >R DUP um+ R> + DUP
;                       R> R@ SWAP >R um+ R> OR
;                       IF >R DROP 1+ R> THEN
;                       ELSE DROP THEN
;                       R>
;                  LOOP DROP SWAP EXIT
;               ELSE -11 THROW          \ result out of range
;               THEN ;

                $COLON  6,'UM/MOD',UMSlashMOD,_FLINK
                DW      DUPP,ZBranch,UMM5
                DW      TwoDUP,ULess,ZBranch,UMM4
                DW      NEGATE,DoLIT,CELLL*8,Zero,DoDO
UMM1:           DW      ToR,DUPP,UMPlus,ToR,ToR,DUPP,UMPlus,RFrom,Plus,DUPP
                DW      RFrom,RFetch,SWAP,ToR,UMPlus,RFrom,ORR,ZBranch,UMM2
                DW      ToR,DROP,OnePlus,RFrom,Branch,UMM3
UMM2:           DW      DROP
UMM3:           DW      RFrom,DoLOOP,UMM1
                DW      DROP,SWAP,EXIT
UMM5:           DW      DoLIT,-10,THROW
UMM4:           DW      DoLIT,-11,THROW

;   UNLOOP      ( -- ) ( R: loop-sys -- )       \ CORE
;               Discard loop-control parameters for the current nesting level.
;               An UNLOOP is required for each nesting level before the
;               definition may be EXITed.
;
;   : UNLOOP    R> R> R> 2DROP >R EXIT ;

                $COLON  COMPO+6,'UNLOOP',UNLOOP,_FLINK
                DW      RFrom,RFrom,RFrom,TwoDROP,ToR,EXIT

;   WITHIN      ( n1|u1 n2|n2 n3|u3 -- flag )   \ CORE EXT
;               Return true if (n2|u2<=n1|u1 and n1|u1<n3|u3) or
;               (n2|u2>n3|u3 and (n2|u2<=n1|u1 or n1|u1<n3|u3)).
;
;   : WITHIN    OVER - >R - R> U< ;

                $COLON  6,'WITHIN',WITHIN,_FLINK
                DW      OVER,Minus,ToR                  ;ul <= u < uh
                DW      Minus,RFrom,ULess,EXIT

;   [           ( -- )                          \ CORE
;               Enter interpretation state.
;
;   : [         0 STATE ! ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+1,'[',LeftBracket,_FLINK
                DW      Zero,STATE,Store,EXIT

;   ]           ( -- )                          \ CORE
;               Enter compilation state.
;
;   : ]         -1 STATE ! ;

                $COLON  1,']',RightBracket,_FLINK
                DW      MinusOne,STATE,Store,EXIT

;***************
; Rest of CORE words and two facility words, EKEY? and EMIT?
;***************
;       Following definitions can be removed from assembler source and
;       can be colon-defined later.

;   (           ( "ccc<)>" -- )                 \ CORE
;               Ignore following string up to next ) . A comment.
;
;   : (         [CHAR] ) PARSE 2DROP ;

                $COLON  IMMED+1,'(',Paren,_FLINK
                DW      DoLIT,')',PARSE,TwoDROP,EXIT

;   *           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
;               Multiply n1|u1 by n2|u2 giving a single product.
;
;   : *         UM* DROP ;

                $COLON  1,'*',Star,_FLINK
                DW      UMStar,DROP,EXIT

;   */          ( n1 n2 n3 -- n4 )              \ CORE
;               Multiply n1 by n2 producing double-cell intermediate,
;               then divide it by n3. Return single-cell quotient.
;
;   : */        */MOD NIP ;

                $COLON  2,'*/',StarSlash,_FLINK
                DW      StarSlashMOD,NIP,EXIT

;   */MOD       ( n1 n2 n3 -- n4 n5 )           \ CORE
;               Multiply n1 by n2 producing double-cell intermediate,
;               then divide it by n3. Return single-cell remainder and
;               single-cell quotient.
;
;   : */MOD     >R M* R> FM/MOD ;

                $COLON  5,'*/MOD',StarSlashMOD,_FLINK
                DW      ToR,MStar,RFrom,FMSlashMOD,EXIT

;   +LOOP       Compilation: ( C: do-sys -- )   \ CORE
;               Run-time: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;               Terminate a DO-+LOOP structure. Resolve the destination of all
;               unresolved occurences of LEAVE.
;               On execution add n to the loop index. If loop index did not
;               cross the boundary between loop_limit-1 and loop_limit,
;               continue execution at the beginning of the loop. Otherwise,
;               finish the loop.
;
;   : +LOOP     POSTPONE do+LOOP rake dosys- ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+5,'+LOOP',PlusLOOP,_FLINK
                DW      DoLIT,DoPLOOP,COMPILEComma,rake,DoSysMinus,EXIT

;   ."          ( "ccc<">" -- )                 \ CORE
;               Run-time ( -- )
;               Compile an inline string literal to be typed out at run time.
;
;   : ."        POSTPONE do." [CHAR] " PARSE
;               xhere pack" TOxhere ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+2,'."',DotQuote,_FLINK
                DW      DoLIT,DoDotQuote,COMPILEComma
                DW      DoLIT,'"',PARSE,XHere,PackQuote,TOXHere,EXIT

;   2OVER       ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )      \ CORE
;               Copy cell pair x1 x2 to the top of the stack.
;
;   : 2OVER     >R >R 2DUP R> R> 2SWAP ;

                $COLON  5,'2OVER',TwoOVER,_FLINK
                DW      ToR,ToR,TwoDUP,RFrom,RFrom,TwoSWAP,EXIT

;   >BODY       ( xt -- a-addr )                \ CORE
;               Push data field address of CREATEd word.
;               Structure of CREATEd word:
;                       | call-doCREATE | 0 or DOES> code addr | a-addr |
;
;   : >BODY     ?call DUP IF                    \ code-addr xt2
;                   ['] doCREATE = IF           \ should be call-doCREATE
;                   CELL+ @ EXIT
;               THEN THEN
;               -31 THROW ;             \ >BODY used on non-CREATEd definition

                $COLON  5,'>BODY',ToBODY,_FLINK
                DW      QCall,DUPP,ZBranch,TBODY1
                DW      DoLIT,DoCREATE,Equals,ZBranch,TBODY1
                DW      CELLPlus,Fetch,EXIT
TBODY1:         DW      DoLIT,-31,THROW

;   ABORT"      ( "ccc<">" -- )                 \ EXCEPTION EXT
;               Run-time ( i*x x1 -- | i*x ) ( R: j*x -- | j*x )
;               Conditional abort with an error message.
;
;   : ABORT"    S" POSTPONE ROT
;               POSTPONE IF POSTPONE abort"msg POSTPONE 2!
;               -2 POSTPONE LITERAL POSTPONE THROW
;               POSTPONE ELSE POSTPONE 2DROP POSTPONE THEN
;               ;  COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+6,'ABORT"',ABORTQuote,_FLINK
                DW      SQuote,DoLIT,ROT,COMPILEComma
                DW      IFF,DoLIT,AbortQMsg,COMPILEComma ; IF is immediate
                DW      DoLIT,TwoStore,COMPILEComma
                DW      DoLIT,-2,LITERAL                 ; LITERAL is immediate
                DW      DoLIT,THROW,COMPILEComma	 ; throw -2 .. abort
                DW      ELSEE,DoLIT,TwoDROP,COMPILEComma ; ELSE and THEN are
                DW      THENN,EXIT                       ; immediate

;   ABS         ( n -- u )                      \ CORE
;               Return the absolute value of n.
;
;   : ABS       DUP 0< IF NEGATE THEN ;

                $COLON  3,'ABS',ABSS,_FLINK
                DW      DUPP,ZeroLess,ZBranch,ABS1
                DW      NEGATE
ABS1:           DW      EXIT

;   ALLOT       ( n -- )                        \ CORE
;               Allocate n bytes in RAM or ROM data space.
;
;   : ALLOT     hereVar +! ;

                $COLON  5,'ALLOT',ALLOT,_FLINK
                DW      HereVar,PlusStore,EXIT

;   BEGIN       ( C: -- dest )                  \ CORE
;               Start an infinite or indefinite loop structure. Put the next
;               location for a transfer of control, dest, onto the data
;               control stack.
;
;   : BEGIN     xhere dest+ ; COMPILE-ONLY IMMDEDIATE

                $COLON  IMMED+COMPO+5,'BEGIN',BEGIN,_FLINK
                DW      XHere,DestPlus,EXIT

;   C,          ( char -- )                     \ CORE
;               Compile a character into data space.
;
;   : C,        HERE C! char-size hereVar +! ;

                $COLON  2,'C,',CComma,_FLINK
                DW      HERE,CStore,DoLIT,CHARR,HereVar,PlusStore,EXIT

;   CHAR        ( "<spaces>ccc" -- char )       \ CORE
;               Parse next word and return the value of first character.
;
;   : CHAR      PARSE-WORD DROP C@ ;

                $COLON  4,'CHAR',CHAR,_FLINK
                DW      PARSE_WORD,DROP,CFetch,EXIT

;   DO          Compilation: ( C: -- do-sys )   \ CORE
;               Run-time: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
;               Start a DO-LOOP structure in a colon definition. Place do-sys
;               on control-flow stack, which will be resolved by LOOP or +LOOP.
;
;   : DO        0  POSTPONE doDO xhere dosys+   \ 0 for rake
;               ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+2,'DO',DO,_FLINK
                DW      Zero,DoLIT,DoDO,COMPILEComma,XHere,DoSysPlus,EXIT

;   DOES>       ( C: colon-sys1 -- colon-sys2 ) \ CORE
;               Build run time code of the data object CREATEd.
;
;   : DOES>     bal @ IF -22 THROW THEN         \ control structure mismatch
;               0 bal !
;               POSTPONE pipe ['] doLIST xt, DROP ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+5,'DOES>',DOESGreater,_FLINK
                DW      Balance,Fetch,ZBranch,DOES1
                DW      DoLIT,-22,THROW
DOES1:          DW      Zero,Balance,Store
                DW      DoLIT,Pipe,COMPILEComma
                DW      DoLIT,DoLIST,xtComma,DROP,EXIT

;   ELSE        Compilation: ( C: orig1 -- orig2 )      \ CORE
;               Run-time: ( -- )
;               Start the false clause in an IF-ELSE-THEN structure.
;               Put the location of new unresolved forward reference orig2
;               onto control-flow stack.
;
;   : ELSE      POSTPONE AHEAD SWAP POSTPONE THEN ; COMPILE-ONLY IMMDEDIATE

                $COLON  IMMED+COMPO+4,'ELSE',ELSEE,_FLINK
                DW      AHEAD,SWAP,THENN,EXIT

;   ENVIRONMENT?   ( c-addr u -- false | i*x true )     \ CORE
;               Environment query.
;
;   : ENVIRONMENT?
;               envQList SEARCH-WORDLIST
;               DUP >R IF EXECUTE THEN R> ;

                $COLON  12,'ENVIRONMENT?',ENVIRONMENTQuery,_FLINK
                DW      EnvQList,SEARCH_WORDLIST
                DW      DUPP,ToR,ZBranch,ENVRN1
                DW      EXECUTE
ENVRN1:         DW      RFrom,EXIT

;   EVALUATE    ( i*x c-addr u -- j*x )         \ CORE
;               Evaluate the string. Save the input source specification.
;               Store -1 in SOURCE-ID.
;
;   : EVALUATE  SOURCE >R >R >IN @ >R  SOURCE-ID >R
;               -1 TO SOURCE-ID
;               sourceVar 2!  0 >IN !  interpret
;               R> TO SOURCE-ID
;               R> >IN ! R> R> sourceVar 2! ;

                $COLON  8,'EVALUATE',EVALUATE,_FLINK
                DW      SOURCE,ToR,ToR,ToIN,Fetch,ToR,SOURCE_ID,ToR
                DW      MinusOne,DoTO,AddrSOURCE_ID
                DW      SourceVar,TwoStore,Zero,ToIN,Store,Interpret
                DW      RFrom,DoTO,AddrSOURCE_ID
                DW      RFrom,ToIN,Store,RFrom,RFrom,SourceVar,TwoStore,EXIT

;   FILL        ( c-addr u char -- )            \ CORE
;               Store char in each of u consecutive characters of memory
;               beginning at c-addr.
;
;   : FILL      ROT ROT ?DUP IF 0 DO 2DUP C! CHAR+ LOOP THEN 2DROP ;

                $COLON  4,'FILL',FILL,_FLINK
                DW      ROT,ROT,QuestionDUP,ZBranch,FILL2
                DW      Zero,DoDO
FILL1:          DW      TwoDUP,CStore,CHARPlus,DoLOOP,FILL1
FILL2:          DW      TwoDROP,EXIT

;   IMMEDIATE   ( -- )                          \ CORE
;               Make the most recent definition an immediate word.
;
;   : IMMEDIATE   lastName [ =imed ] LITERAL OVER @ OR SWAP ! ;

                $COLON  9,'IMMEDIATE',IMMEDIATE,_FLINK
                DW      LastName,DoLIT,IMMED,OVER,Fetch,ORR,SWAP,Store,EXIT

;   J           ( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
;               Push the index of next outer loop.
;
;   : J         rp@ [ 3 CELLS ] LITERAL + @
;               rp@ [ 4 CELLS ] LITERAL + @  +  ; COMPILE-ONLY

                $COLON  COMPO+1,'J',J,_FLINK
                DW      RPFetch,DoLIT,3*CELLL,Plus,Fetch
                DW      RPFetch,DoLIT,4*CELLL,Plus,Fetch,Plus,EXIT

;   LEAVE       ( -- ) ( R: loop-sys -- )       \ CORE
;               Terminate definite loop, DO|?DO  ... LOOP|+LOOP, immediately.
;
;   : LEAVE     POSTPONE UNLOOP POSTPONE branch
;               xhere rakeVar DUP @ code, ! ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+5,'LEAVE',LEAVEE,_FLINK
                DW      DoLIT,UNLOOP,COMPILEComma,DoLIT,Branch,COMPILEComma
                DW      XHere,RakeVar,DUPP,Fetch,CodeComma,Store,EXIT

;   LOOP        Compilation: ( C: do-sys -- )   \ CORE
;               Run-time: ( -- ) ( R: loop-sys1 -- loop-sys2 )
;               Terminate a DO|?DO ... LOOP structure. Resolve the destination
;               of all unresolved occurences of LEAVE.
;
;   : LOOP      POSTPONE doLOOP rake dosys- ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+4,'LOOP',LOOPP,_FLINK
                DW      DoLIT,DoLOOP,COMPILEComma,rake,DoSysMinus,EXIT

;   LSHIFT      ( x1 u -- x2 )                  \ CORE
;               Perform a logical left shift of u bit-places on x1, giving x2.
;               Put 0 into the least significant bits vacated by the shift.
;
;   : LSHIFT    ?DUP IF 0 DO 2* LOOP THEN ;

                $COLON  6,'LSHIFT',LSHIFT,_FLINK
                DW      QuestionDUP,ZBranch,LSHIFT2
                DW      Zero,DoDO
LSHIFT1:        DW      TwoStar,DoLOOP,LSHIFT1
LSHIFT2:        DW      EXIT

;   M*          ( n1 n2 -- d )                  \ CORE
;               Signed multiply. Return double product.
;
;   : M*        2DUP XOR 0< >R ABS SWAP ABS UM* R> IF DNEGATE THEN ;

                $COLON  2,'M*',MStar,_FLINK
                DW      TwoDUP,XORR,ZeroLess,ToR,ABSS,SWAP,ABSS
                DW      UMStar,RFrom,ZBranch,MSTAR1
                DW      DNEGATE
MSTAR1:         DW      EXIT

;   MAX         ( n1 n2 -- n3 )                 \ CORE
;               Return the greater of two top stack items.
;
;   : MAX       2DUP < IF SWAP THEN DROP ;

                $COLON  3,'MAX',MAX,_FLINK
                DW      TwoDUP,LessThan,ZBranch,MAX1
                DW      SWAP
MAX1:           DW      DROP,EXIT

;   MIN         ( n1 n2 -- n3 )                 \ CORE
;               Return the smaller of top two stack items.
;
;   : MIN       2DUP > IF SWAP THEN DROP ;

                $COLON  3,'MIN',MIN,_FLINK
                DW      TwoDUP,GreaterThan,ZBranch,MIN1
                DW      SWAP
MIN1:           DW      DROP,EXIT

;   MOD         ( n1 n2 -- n3 )                 \ CORE
;               Divide n1 by n2, giving the single cell remainder n3.
;               Returns modulo of floored division in this implementation.
;
;   : MOD       /MOD DROP ;

                $COLON  3,'MOD',MODD,_FLINK
                DW      SlashMOD,DROP,EXIT

;   POSTPONE    ( "<spaces>name" -- )           \ CORE
;               Parse name and find it. Append compilation semantics of name
;               to current definition.
;
;   : POSTPONE  (') 0< IF POSTPONE LITERAL
;                         POSTPONE COMPILE, EXIT THEN   \ non-IMMEDIATE
;               COMPILE, ; COMPILE-ONLY IMMEDIATE       \ IMMEDIATE

                $COLON  IMMED+COMPO+8,'POSTPONE',POSTPONE,_FLINK
                DW      ParenTick,ZeroLess,ZBranch,POSTP1
                DW      LITERAL,DoLIT,COMPILEComma
POSTP1:         DW      COMPILEComma,EXIT

;   RECURSE     ( -- )                          \ CORE
;               Append the execution semactics of the current definition to
;               the current definition.
;
;   : RECURSE   lastXT COMPILE, ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+7,'RECURSE',RECURSE,_FLINK
                DW      LastXT,COMPILEComma,EXIT

;   REPEAT      ( C: orig dest -- )             \ CORE
;               Terminate a BEGIN-WHILE-REPEAT indefinite loop. Resolve
;               backward reference dest and forward reference orig.
;
;   : REPEAT    AGAIN THEN ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+6,'REPEAT',REPEAT,_FLINK
                DW      AGAIN,THENN,EXIT

;   RSHIFT      ( x1 u -- x2 )                  \ CORE
;               Perform a logical right shift of u bit-places on x1, giving x2.
;               Put 0 into the most significant bits vacated by the shift.
;
;   : RSHIFT    ?DUP IF
;                       0 SWAP  cell-size-in-bits SWAP -
;                       0 DO  2DUP D+  LOOP
;                       NIP
;                    THEN ;

                $COLON  6,'RSHIFT',RSHIFT,_FLINK
                DW      QuestionDUP,ZBranch,RSHIFT2
                DW      Zero,SWAP,DoLIT,CELLL*8,SWAP,Minus,Zero,DoDO
RSHIFT1:        DW      TwoDUP,DPlus,DoLOOP,RSHIFT1
                DW      NIP
RSHIFT2:        DW      EXIT

;   SLITERAL    ( c-addr1 u -- )                \ STRING
;               Run-time ( -- c-addr2 u )
;               Compile a string literal. Return the string on execution.
;
;   : SLITERAL   POSTPONE doS" xhere pack" TOxhere ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+8,'SLITERAL',SLITERAL,_FLINK
                DW      DoLIT,DoSQuote,COMPILEComma
                DW      XHere,PackQuote,TOXHere,EXIT

;   S"          Compilation: ( "ccc<">" -- )    \ CORE
;               Run-time: ( -- c-addr u )
;               Parse ccc delimetered by " . Return the string specification
;               c-addr u on execution.
;
;   : S"        [CHAR] " PARSE
;               STATE @ IF POSTPONE SLITERAL EXIT THEN
;               CR ." Use of S" in interpretation state is non-portable."
;               CR ." Use  CHAR " PARSE string" or BL PARSE word  instead."
;               ; IMMEDIATE

                $COLON  IMMED+2,'S"',SQuote,_FLINK
                DW      DoLIT,'"',PARSE,STATE,Fetch,ZBranch,SQUOT1
                DW      SLITERAL,EXIT
SQUOT1:         DW      CR
                D$      DoDotQuote,'Use of S" in interpretation state is non-portable.'
                DW      CR
                D$      DoDotQuote,'Use instead  CHAR " PARSE word" or BL PARSE word .'
                DW      EXIT

;   SM/REM      ( d n1 -- n2 n3 )               \ CORE
;               Symmetric divide of double by single. Return remainder n2
;               and quotient n3.
;
;   : SM/REM    OVER >R >R DUP 0< IF DNEGATE THEN
;               R@ ABS UM/MOD DUP 0<
;               IF DUP 08000h XOR IF -11 THROW THEN THEN \ result out of range
;               R> R@ XOR 0< IF NEGATE THEN
;               R> 0< IF SWAP NEGATE SWAP THEN ;

                $COLON  6,'SM/REM',SMSlashREM,_FLINK
                DW      OVER,ToR,ToR,DUPP,ZeroLess,ZBranch,SMREM1
                DW      DNEGATE
SMREM1:         DW      RFetch,ABSS,UMSlashMOD,DUPP,ZeroLess,ZBranch,SMREM4
;NAC: Was architecture-dependent. Changed 08000h to MaxNegative
                DW      DUPP,DoLIT,MaxNegative,XORR,ZBranch,SMREM4
                DW      DoLIT,-11,THROW
SMREM4:         DW      RFrom,RFetch,XORR,ZeroLess,ZBranch,SMREM2
                DW      NEGATE
SMREM2:         DW      RFrom,ZeroLess,ZBranch,SMREM3
                DW      SWAP,NEGATE,SWAP
SMREM3:         DW      EXIT

;   SPACES      ( n -- )                        \ CORE
;               Send n spaces to the output device if n is greater than zero.
;
;   : SPACES    ?DUP IF 0 DO SPACE LOOP THEN ;

                $COLON  6,'SPACES',SPACES,_FLINK
                DW      QuestionDUP,ZBranch,SPACES2
                DW      Zero,DoDO
SPACES1:        DW      SPACE,DoLOOP,SPACES1
SPACES2:        DW      EXIT

;   TO          Interpretation: ( x "<spaces>name" -- ) \ CORE EXT
;               Compilation:    ( "<spaces>name" -- )
;               Run-time:       ( x -- )
;               Store x in name.
;
;   : TO        ' ?call DUP IF          \ should be call-doVALUE
;                 ['] doVALUE =         \ verify VALUE marker
;                 IF @ STATE @
;                    IF POSTPONE doTO code, EXIT THEN
;                    ! EXIT
;                    THEN THEN
;               -32 THROW ; IMMEDIATE   \ invalid name argument (e.g. TO xxx)

                $COLON  IMMED+2,'TO',TO,_FLINK
                DW      Tick,QCall,DUPP,ZBranch,TO1
                DW      DoLIT,DoVALUE,Equals,ZBranch,TO1
                DW      Fetch,STATE,Fetch,ZBranch,TO2
                DW      DoLIT,DoTO,COMPILEComma,CodeComma,EXIT
TO2:            DW      Store,EXIT
TO1:            DW      DoLIT,-32,THROW

;   U.          ( u -- )                        \ CORE
;               Display u in free field format followed by space.
;
;   : U.        0 D. ;

                $COLON  2,'U.',UDot,_FLINK
                DW      Zero,DDot,EXIT

;   UNTIL       ( C: dest -- )                  \ CORE
;               Terminate a BEGIN-UNTIL indefinite loop structure.
;
;   : UNTIL     POSTPONE 0branch code, dest- ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+5,'UNTIL',UNTIL,_FLINK
                DW      DoLIT,ZBranch,COMPILEComma,CodeComma,DestMinus,EXIT

;   VALUE       ( x "<spaces>name" -- )         \ CORE EXT
;               name Execution: ( -- x )
;               Create a value object with initial value x.
;
;   : VALUE     ['] doVALUE xt, head,
;               xhere DUP CELL+ TOxhere
;               RAMB @ SWAP !
;               , linkLast ; \ store x and link VALUE word to current wordlist

                $COLON  5,'VALUE',VALUE,_FLINK
                DW      DoLIT,DoVALUE,xtComma,HeadComma
                DW      XHere,DUPP,CELLPlus,TOXHere,RAMB,Fetch,SWAP,Store
                DW      Comma,LinkLast,EXIT

;   VARIABLE    ( "<spaces>name" -- )           \ CORE
;               name Execution: ( -- a-addr )
;               Parse a name and create a variable with the name.
;               Resolve one cell of data space at an aligned address.
;               Return the address on execution.
;
;   : VARIABLE  ['] doCONST xt, head,
;               xhere DUP CELL+ TOxhere
;               RAMB @ DUP CELL+ RAMB ! \ allocate one cell in RAM area
;               SWAP ! linkLast ;

                $COLON  8,'VARIABLE',VARIABLE,_FLINK
                DW      DoLIT,DoCONST,xtComma,HeadComma
                DW      XHere,DUPP,CELLPlus,TOXHere
                DW      RAMB,Fetch,DUPP,CELLPlus,RAMB,Store
                DW      SWAP,Store,LinkLast,EXIT

;   WHILE       ( C: dest -- orig dest )        \ CORE
;               Put the location of a new unresolved forward reference orig
;               onto the control flow stack under the existing dest. Typically
;               used in BEGIN ... WHILE ... REPEAT structure.
;
;   : WHILE     POSTPONE IF SWAP ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+5,'WHILE',WHILE,_FLINK
                DW      IFF,SWAP,EXIT

;   WORD        ( char "<chars>ccc<char>" -- c-addr )   \ CORE
;               Skip leading delimeters and parse a word. Return the address
;               of a transient region containing the word as counted string.
;
;   : WORD      skipPARSE xhere pack" DROP xhere ;

                $COLON  4,'WORD',WORDD,_FLINK
                DW      SkipPARSE,XHere,PackQuote,DROP,XHere,EXIT

;   [']         Compilation: ( "<spaces>name" -- )      \ CORE
;               Run-time: ( -- xt )
;               Parse name. Return the execution token of name on execution.
;
;   : [']       ' POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+3,"[']",BracketTick,_FLINK
                DW      Tick,LITERAL,EXIT

;   [CHAR]      Compilation: ( "<spaces>name" -- )      \ CORE
;               Run-time: ( -- char )
;               Parse name. Return the value of the first character of name
;               on execution.
;
;   : [CHAR]    CHAR POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

                $COLON  IMMED+COMPO+6,'[CHAR]',BracketCHAR,_FLINK
                DW      CHAR,LITERAL,EXIT

;   \           ( "ccc<eol>" -- )               \ CORE EXT
;               Parse and discard the remainder of the parse area.
;
;   : \         SOURCE >IN ! DROP ; IMMEDIATE

                $COLON  IMMED+1,'\',Backslash,_FLINK
                DW      SOURCE,ToIN,Store,DROP,EXIT

; Optional Facility words

;   EKEY?       ( -- flag )                     \ FACILITY EXT
;               If a keyboard event is available, return true.
;
;   : EKEY?     'ekey? EXECUTE ;

                $COLON  5,'EKEY?',EKEYQuestion,_FLINK
                DW      TickEKEYQ,EXECUTE,EXIT

;   EMIT?       ( -- flag )                     \ FACILITY EXT
;               flag is true if the user output device is ready to accept data
;               and the execution of EMIT in place of EMIT? would not have
;               suffered an indefinite delay. If device state is indeterminate,
;               flag is true.
;
;   : EMIT?     'emit? EXECUTE ;

                $COLON  5,'EMIT?',EMITQuestion,_FLINK
                DW      TickEMITQ,EXECUTE,EXIT

;***************
; RAM/ROM System Only
;***************

;   RESET-SYSTEM   ( -- )
;               Reset the system. Restore initialization values of system
;               variables.
;
;   : RESET-SYSTEM
;               sysVar00 sysVar0 [ sysVar0End sysVar0 - ] LITERAL  MOVE COLD ;

                $COLON  12,'RESET-SYSTEM',RESET_SYSTEM,_SLINK
                DW      DoLIT,UZERO0,SysVar0,DoLIT,ULAST-UZERO
                DW      MOVE,COLD,EXIT

UZERO0:         DW      RXQ                     ;'ekey?
                DW      RXFetch                 ;'ekey
                DW      TXQ                     ;'emit?
                DW      TXStore                 ;'emit
                DW      Set_IO                  ;'init-i/o
                DW      DotOK                   ;'prompt
                DW      HI                      ;'boot
                DW      0                       ;SOURCE-ID
                DW      AddrROMB                ;CPVar
                DW      AddrROMT                ;NPVar
                DW      AddrRAMB                ;HereVar points RAM space.
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

;===============================================================
;NAC: these are put in by makesrc.awk -- CTOP and VTOP are OK because _VAR
;NAC: is SETA to the right value as the awk script emits new source
;LASTENV         EQU     _ENVLINK-0
;LASTSYSTEM      EQU     _SLINK-0        ;last SYSTEM word name address
;LASTFORTH       EQU     _FLINK-0        ;last FORTH word name address

;NTOP            EQU     _NAME-0         ;next available memory in name dictionary
CTOP            EQU     .-0             ;next available memory in code dictionary
VTOP            EQU     _VAR-0          ;next available memory in variable area

;nMAIN    ENDS
;nEND     ORIG

;===============================================================






