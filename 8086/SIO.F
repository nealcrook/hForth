\
\ SIO.F
\ Serial input/output words for IBM PC compatiables.
\
\ Adjust the value of CONSTANT words IRQ and COMBASE for your system.
\
\ SIO.F is a example of direct control of hardware.
\ You should not or can not do this in a respectable OS.
\ However, MS-DOS is not one of them :).
\
\ There are two input and output buffers. Serial port output as well as input
\ are driven by interrupts. High-level words simply take or put characters in
\ the buffers. Then the interrupt service routine takes outgoing characters
\ from the output buffer and puts incomming characters in the input buffer.
\
\ 1996. 2. 9.
\ Wonyong Koh
\
\ 1997. 6. 21
\       Align 'IrptServ' according to new EXE structure.

HEX

  3 CONSTANT IRQ	\ normally COM1 and COM3 use IRQ4,
			\	   COM2 and COM4 use IRQ3
02F8 CONSTANT COMBASE   \ base address, 3F8 for COM1, 2F8 for COM2
			\		3E8 for COM3, 2E8 for COM4

: BINARY   2 BASE ! ;

CODE ENABLE
    STI,
    NEXT,
END-CODE

CODE DISABLE
    CLI,
    NEXT,
END-CODE

20 CONSTANT CTRL8259_0		\ interrupt control register
21 CONSTANT CTRL8259_1		\ interrupt mask register
20 CONSTANT EOI 		\ end of interrupt

COMBASE     CONSTANT TXR	\ transmission register (WRITE)
COMBASE     CONSTANT RXR	\ receive register	(READ)
COMBASE 1 + CONSTANT IER	\ interrupt enable
COMBASE 2 + CONSTANT IIR	\ interrupt ID
COMBASE 3 + CONSTANT LCR	\ line contril
COMBASE 4 + CONSTANT MCR	\ modem control
COMBASE 5 + CONSTANT LSR	\ line status
COMBASE 6 + CONSTANT MSR	\ modem status
COMBASE     CONSTANT DLL	\ divisor latch low
COMBASE 1 + CONSTANT DLH	\ divisor latch high

00 CONSTANT NO
18 CONSTANT ODD
08 CONSTANT EVEN
: PARITY ( n -- )
    LCR PC@
    [ BINARY 00011000 INVERT HEX ] LITERAL AND
    OR LCR PC! ;

: BITS ( n -- ) 		\ n = 5, 6, 7, or 8
    5 -
    LCR PC@
    [ BINARY 00000011 INVERT HEX ] LITERAL AND
    OR LCR PC! ;

: STOPBIT ( n -- )		\ n = 0 or 1
    2 LSHIFT
    LCR PC@
    [ BINARY 00000100 INVERT HEX ] LITERAL AND
    OR LCR PC! ;

: BPS ( n -- )			\ set speed
    LCR PC@ SWAP
    0FF LCR PC! 		\ set Divisor-Latch Access-Bit
    [ DECIMAL ] 115200.
    [ HEX ] ROT UM/MOD NIP	\ calculate divisor
    DUP 0FF AND  DLL PC!
	8 RSHIFT DLH PC!
    LCR PC! ;			\ restore original LCR

: DROP-RTS
    1 MSR PC! ;

: DTR-RTS
    3 MSR PC! ;

DTR-RTS

BINARY
: CLEAR-UART
    BEGIN
       RXR PC@ DROP
       LSR PC@ DROP
       MSR PC@ DROP
       EOI CTRL8259_0 PC!
       IIR PC@ 00000001 AND
    UNTIL ;

: ENABLE-IRQ
    CTRL8259_1 PC@
    [ 1 IRQ LSHIFT INVERT ] LITERAL AND 	\ clear mask bit
    CTRL8259_1 PC!

    LCR PC@  01111111 AND  LCR PC!		\ clear divisor latch addr.

    00001111 IER PC!			\ interrupts when data received
    CLEAR-UART
    MCR PC@  00001000 OR  MCR PC!	\ allow modem to generate interrupts
    ENABLE ;

: DISABLE-IRQ
    CTRL8259_1 PC@
    [ 1 IRQ LSHIFT ] LITERAL OR 	\ set mask bit
    CTRL8259_1 PC!

    00000000 IER PC!			\ no interrupt allowed

    MCR PC@  11110111 AND  MCR PC! ;

DECIMAL

1 10 LSHIFT CONSTANT RxBufSize	\ receive buffer size = 2 ^ 10 (1024)
				\ The buffer size should be power of 2.
VARIABLE RxBuffer  RxBufSize ALLOT
VARIABLE #Rx
VARIABLE RxHead
VARIABLE RxTail
VARIABLE RxOverflow

1 8 LSHIFT CONSTANT TxBufSize   \ transmit buffer size = 2 ^ 8 (256)
				\ The buffer size should be power of 2.
CREATE TxBuffer  TxBufSize CHARS ALLOT
VARIABLE #Tx
VARIABLE TxHead
VARIABLE TxTail

VARIABLE LSR@
VARIABLE MSR@

HEX

: CLEAR-BUFFER
    DISABLE
    0 #Rx    !
    0 RxHead !
    0 RxTail !
    0 #Tx    !
    0 TxHead !
    0 TxTail !
    -1 LSR@ !
    -1 MSR@ !
    ENABLE ;

CODE ModemServ
    MSR # DX MOV,
    DX AL IN,
    AL MSR@ ) MOV,
    RET,
END-CODE

CODE TxServ
    0 # #Tx ) WORD CMP,
    1 L# JNZ,
    IER # DX MOV,
    01 # AL MOV,
    DX AL OUT,			\ Disable TXR empty irpt
    RET,
1 L:
    TxTail ) BX MOV,
    TxBuffer [BX] AL MOV,
    TXR # DX MOV,
    DX AL OUT,
    BX INC,
    TxBufSize 1- # BX AND,
    BX TxTail ) MOV,
    #Tx ) WORD DEC,
    RET,
END-CODE

CODE RxServ
    RXR # DX MOV,
    DX AL IN,
    RxBufSize # #Rx ) CMP,
    1 L# JNZ,
    -1 # RxOverflow ) MOV,
    RET,
1 L:
    RxHead ) BX MOV,
    AL RxBuffer [BX] MOV,
    BX INC,
    RxBufSize 1- # BX AND,
    BX RxHead ) MOV,
    #Rx ) WORD INC,
    RET,
END-CODE

CODE LineServ
    LSR # DX MOV,
    DX AL IN,
    AL LSR@ ) MOV,
    RET,
END-CODE

CREATE IrptTable
' ModemServ , ' TxServ , ' RxServ , ' LineServ ,

HEX

CODE IrptServ
    STI,			\ Enable irpt
    AX PUSH,
    BX PUSH,
    DX PUSH,
    DS PUSH,
    CHAR " PARSE model" ENVIRONMENT? DROP
    CHAR " PARSE ROM Model" COMPARE 0=
    CHAR " PARSE model" ENVIRONMENT? DROP
    CHAR " PARSE RAM Model" COMPARE 0= OR
    [IF]
       CS AX MOV,
       AX DS MOV,
    [THEN]
    CHAR " PARSE model" ENVIRONMENT? DROP
    CHAR " PARSE EXE Model" COMPARE 0=
    [IF]
      CS AX MOV,
      1000 # AX ADD,            \ data segment follows after 64KB code segment
      AX DS MOV,
    [THEN]
    IIR # DX MOV,		\ identify irpt
    DX AL IN,
    01 # AL TEST,
    1 L# JNZ,
    AX BX MOV,
    0006 # BX AND,
    IrptTable [BX] CALL,
1 L:				\ do end of interrupt
    EOI # AL MOV,
    CTRL8259_0 # AL OUT,
    IER # DX MOV,
    DX AL IN,
    AX PUSH,
    0 # AL MOV,
    DX AL OUT,
    AX POP,
    DX AL OUT,
    DS POP,
    DX POP,
    BX POP,
    AX POP,
    IRET,
END-CODE

CREATE OLD-VECTOR 2 CELLS ALLOT
CODE ATTACH-IRPT ( -- )
    BX PUSH,
    DS PUSH,
    IRQ 8 + 3500 OR # AX MOV,		\ AL = irpt number, AH = 35h
    21 INT,				\ DOS get interrupt vector service
    BX OLD-VECTOR ) MOV,		\ save old vector
    ES OLD-VECTOR CELL+ ) MOV,
    IRQ 8 + 2500 OR # AX MOV,		\ AL = irpt number, AH = 25h
    CS@ # DX MOV,
    DX DS MOV,				\ irpt service roution in CS:IrptServ
    ' IrptServ # DX MOV,
    21 INT,				\ DOS set irpt vector
    DS POP,
    BX POP,
    NEXT,
END-CODE

CODE DETACH-IRPT ( -- ) 		\ restore old vector
    BX PUSH,
    DS PUSH,
    IRQ 8 + 2500 OR # AX MOV,		\ AL = irpt number, AH = 25h
    OLD-VECTOR ) DX MOV,
    OLD-VECTOR CELL+ ) DS MOV,		\ DOS set irpt vector
    21 INT,
    DS POP,
    BX POP,
    NEXT,
END-CODE

: SER-IN? ( -- f )			\ true if char received
    #Rx @ 0<> ;

: SER-IN ( -- x )
    #Rx @ 0= IF    0 EXIT
	     ELSE  RxTail @ RxBuffer + C@
		   RxTail @ 1+ [ RxBufSize 1 - ] LITERAL AND RxTail !
		   -1 #Rx +!
	     THEN ;

VARIABLE TIMEOUT
: SER-OUT ( x -- error_code )
    #Tx @ TxBufSize <>
    IF TxHead @ TxBuffer + C!
       TxHead @ 1+ [ TxBufSize 1- ] LITERAL AND TxHead !
       1 #Tx +!
       [ HEX ] 0F IER PC!
    THEN ;

DECIMAL

: TERM
    19200 BPS  NO PARITY  8 BITS  0 STOPBIT
    CLEAR-BUFFER
    ATTACH-IRPT
    ENABLE-IRQ
    CLEAR-BUFFER
    BEGIN
       SER-IN? IF  SER-IN EMIT	THEN
       EKEY?   IF  KEY DUP 27 = IF DROP DISABLE-IRQ DETACH-IRPT EXIT THEN
		       SER-OUT
	       THEN
    AGAIN ;

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
