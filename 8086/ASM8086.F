\ 8086/8 Assembler for hForth
\
\ This 8088 Assembler has been rewritten by Sheen Lee for hForth
\
\ ----------------------------------------------------------------------------
\ This 8088 Assembler was originally written by Mike Perry and
\ Steve Pollack.  It has been rewritten by Martin Tracy
\ and rewritten again by Rick VanNorman (to adapt it to a
\ 32-bit environment).
\ Programmers who are familiar with the original F83 assembler
\ will find the following major differences:
\
\ 1. the mode  #) is now simply )
\ 2. the mode S#) has disappeared.
\ 3. conditional macros have been replaced by local labels.
\ 4. REPZ and REPNZ are now REPE and REPNE.
\ 5. JZ  JNZ  JC  JNC  and more error checks have been added.
\ 6. the JMP and CALL instructions now have an indirect mode:
\
\    MYLABEL  # JMP  means  JMP to this label, but
\    MYVECTOR ) JMP  means  JMP indirectly through this address.
\ ----------------------------------------------------------------------------

\ Further modifications by Wonyong Koh
\
\ 1996. 11. 29.
\       Revise ';CODE' for control-flow stack.
\ 1996. 4. 15.
\       ';CODE' is fixed. END-CODE is changed.
\ 1995. 11. 27.
\       ';CODE' is redefined following the change of 'DOES>' and 'doCREATE'.
\
\ o 'MOV', 'JMP', etc are renamed to 'MOV,', 'JMP,', etc. You can
\   use Standard Forth words 'AND', 'OR', 'XOR' between 'CODE' and
\   'END-CODE' with no confliction.
\ o ANS Standard word ';CODE' is added.
\ o The definition of '1MI' for hForth 8086 ROM Model is better to be
\       : 1MI   RAM/ROM@ ROM CREATE C, RAM/ROM!  DOES> C@ xb, ;
\   rather than
\       : 1MI   CREATE  C,  DOES> C@ xb, ;
\   However, I did not bother and simply put 'ROM' and 'RAM' in
\   'ASM8086.F' since '1MI' won't be used in any other places.

CHAR " PARSE CPU" ENVIRONMENT? DROP
CHAR " PARSE 8086" COMPARE
[IF] CR .( This assembler is for 8086 only.) ABORT [THEN]

BASE @
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF] RAM/ROM@ [THEN]
GET-ORDER  GET-CURRENT

WORDLIST WORDLIST-NAME ASSEMBLER-WORDLIST

: ASSEMBLER
    GET-ORDER NIP ASSEMBLER-WORDLIST SWAP SET-ORDER ;
ALSO ASSEMBLER DEFINITIONS

HEX

\ ----------------------------------------------------- System dependant words

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : codeB!      C! ;
  : codeB,      xhere DUP 1+ TOxhere C! ;
  : code2B,     xhere DUP CELL+ TOxhere ! ;
  : code4B,     SWAP code2B, code2B, ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : codeB!      C! ;
  : codeB,      HERE DUP 1+    TO HERE C! ;
  : code2B,     HERE DUP CELL+ TO HERE ! ;
  : code4B,     SWAP code2B, code2B, ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : codeB,       xhere DUP 1+    TO xhere codeB! ;
  : code2B,      xhere DUP CELL+ TO xhere code! ;
  : code4B,      SWAP code2B, code2B, ;
[THEN]

\ ----------------------------------------------------------------- Predicates

\ true if offset requires 2 bytes.
: BIG? ( o - f)
    0080 +  FF00 AND  0= INVERT ;

\ Error action of several words.
: huh? ( w)
   INVERT IF ." ? " SOURCE TYPE ABORT THEN ;

\ aborts if relative distance is too far.
: ?FAR ( o )
    BIG? INVERT huh? ;

\ --------------------------------------------------------------- Local labels

DECIMAL 16  CONSTANT  MXL#  HEX

\ unresolved fwd reference associative stack.  Emptied by INIT.
\ Associate stacks can be "popped" from the middle, or wherever
\ the key is found.

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  RAM
[THEN]

CREATE FWDS
    2 CELLS ALLOT           ( pointers)
    MXL# 2 * CELLS ALLOT    ( pairs)

\ resolved label value array.  Cleared by INIT.
CREATE BWDS
    MXL# CELLS ALLOT

\ pushes unresolved reference.
: LPUSH ( value=here' key=label#)
    FWDS 2@ = 0= huh? ( full?)  FWDS  @ 2!  2 CELLS FWDS +! ;

\ pops any unresolved references.
: LPOP  ( key=label# - value=addr true | key 0)
    >R  FWDS @  FWDS 2 CELLS +
    BEGIN  2DUP = 0= ( end start)  WHILE
	DUP @  R@ =  IF ( found!)
	    DUP CELL+ @ ( addr) >R
	    SWAP 2 CELLS -  DUP FWDS !  2@ ROT 2!  \ promote last pair
	    R> R> ( addr key)  -1 OR  ( addr true)
	    EXIT
	THEN
	2 CELLS +
    REPEAT
    2DROP R> 0 ;

\ returns the address of the label n or 0 if unresolved.
: L? ( n - a | 0)
    DUP MXL# U< huh?  CELLS  BWDS + @ ;

\ assigns HERE to label n-1.  Resolves any forward references.
\ Assumes 8-bit relative displacements.
: L: ( n - a)
    DUP L? 0= huh? ( should be unknown)
    xhere  OVER CELLS BWDS + ! ( now known)
    BEGIN  DUP LPOP ( a -1 | n 0)  WHILE
	xhere OVER - 1-  SWAP OVER  ?FAR  codeB!  ( resolve ref)
    REPEAT
    2DROP ;

: L# ( n - a )   \ retrieves the value of label n-1.
   DUP L?
   ?DUP 0=  IF  xhere 1+ 2DUP SWAP LPUSH  THEN
   NIP ;

\ ------------------------------------------------------------------ Variables

VARIABLE WORD=  \ WORD/BYTE switch  -- normally WORD.
VARIABLE FAR=   \ NEAR/FAR  switch  -- normally NEAR.
VARIABLE LOG=   \ holds op mask for logical opcodes.  See B/L?

: WORD  TRUE  WORD= ! ;
: BYTE  FALSE WORD= ! ;
: FAR   TRUE  FAR=  ! ;

\ ------------------------------------------------ base switches to octal here

: OCTAL  [ DECIMAL ] 8 BASE ! ;

OCTAL

\ ------------------------------------------------------------------ Registers

\ defines n register-id-modes used for building opcodes.
: REGS ( n id )
    SWAP 0  DO
	DUP I  11 * SWAP 1000 * OR CONSTANT
    LOOP
    DROP ;

10  1 REGS AL CL DL BL AH CH DH BH
10  2 REGS AX CX DX BX SP BP SI DI
10  4 REGS [BX+SI] [BX+DI] [BP+SI] [BP+DI] [SI] [DI] [BP] [BX]
 4  4 REGS [SI+BX] [DI+BX] [SI+BP] [DI+BP]
 4 10 REGS ES CS SS DS
 2 20 REGS  )  #

\ ----------------------------------------------------------------- Mode tests

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  ROM
[THEN]

: MD   \ determines if a mode is a member of the given class.
    CREATE ( mode -  )  1000 * ,
    DOES>  ( mode - f)  @ AND 0= INVERT ;

 1 MD R8?  ( mode -    8-bit-register?)
 2 MD R16? ( mode -   16-bit-register?)
 3 MD REG? ( mode - 8/16-bit-register?)
 4 MD [x]? ( mode -  indirect/indexed?)
10 MD SEG? ( mode -  segment-register?)

: RLOW ( register-mode - r/m-mask )  07 AND ;
: RMID ( register-mode - reg-mask )  70 AND ;

\ --------------------------------------------------------- Special mode tests

\ true if n takes two bytes or sign-extend is not permitted.
: B/L? ( n - f)
    BIG?  LOG= @  OR ;

\ true if mem -> acc
: >ACC? ( mode reg - f)
    RLOW 0=  SWAP ) = AND ;

: ?MAD ( f )     IF ." Mode? " SOURCE TYPE ABORT THEN ;
: ?ACC ( mode )  DUP AX =  SWAP AL = OR INVERT ?MAD ;

\ ----------------------------------------------------------- Opcode compilers

:   OP, ( opcode mask | mask opcode ) OR codeB, ;

:    W, ( opcode mode )  R16?    NEGATE  OP, ;
: WORD, ( opcode      )  WORD= @ NEGATE  OP, ;

: RR, ( register-mode1 register-mode2 )
    RMID  SWAP RLOW OR  300 OP, ;

: ,/C, ( n 16-bit? )
    IF  code2B,  ELSE  codeB,  THEN ;

\ ---------------------------------------------------------- Address compilers

\ compiles memory->register operand.
: MEM, ( a/o mode register-mode)
    RMID  OVER ) =  IF
	6 OP,  DROP  code2B,                    ( direct )
    ELSE
	OVER RLOW OR  ( reg:r/m field) ROT ROT  ( field addr mode)
	( mode) [BP] =  OVER 0= AND  IF         ( 0 [BP] exception..)
	    SWAP  100 OP, codeB,                ( ...requires offset)
	ELSE  SWAP OVER BIG?  IF
	    200 OP,  ( 2-byte offset) code2B,
	ELSE  OVER  IF
	    100 OP,  ( 1-byte offset) codeB,
	ELSE
	    OP,  ( zero offset)
	THEN THEN THEN
    THEN  ;

\ register-mode selects BYTE/WORD w-field.
: WMEM, ( a/o mode register-mode opcode )
    OVER W,  MEM, ;

\ selects between register->register and memory->register.
: R/M,  ( [operand] mode register-mode )
    OVER REG?  IF  RR,  ELSE  MEM,  THEN ;

\  R/M, but modifies opcode with BYTE/WORD.
: WR/M, ( [operand] mode register-mode opcode )
    2 PICK  DUP REG?  IF
	W, RR,                  ( register->register)
    ELSE
	DROP  WORD, MEM,  WORD  ( memory  ->register)
    THEN ;

\ ---------------------------------------------------------- Opcode generators

\ one-byte opcodes with implied operands.
: 1MI
    CREATE  C,
    DOES>   C@ codeB, ;

\ two-byte opcodes with implied operands.
: 2MI
    CREATE  C,
    DOES>   C@ codeB,  12 codeB, ;

\ jump to a one-byte displacement.
: 3MI
    CREATE  C,
    DOES>   C@  codeB, ( a ) xhere - 1-  DUP ?FAR  codeB, ;

\ LDS LEA LES opcodes.
: 4MI
    CREATE  C,
    DOES>   C@  codeB, ( mem reg) OVER REG? ?MAD  MEM, ;

\ string opcodes.
: 5MI
    CREATE  C,
    DOES>   C@ WORD,  WORD ;

\ one-byte opcodes with single operands.
: 7MI
    CREATE  C,
    DOES>   C@  366 WR/M, ;

\ IN and OUT.  Syntax for both: port/DX AL/AX IN/OUT
: 8MI
    CREATE  C,
    DOES>   C@  OVER ?ACC  ROT
	    DUP # =  OVER DX =  OR INVERT ?MAD
	    # =  IF
		SWAP W,  codeB,
	    ELSE
		10 OR  SWAP W,
	    THEN ;

\ INC and DEC.  Syntax is: r/mem opcode.
: 9MI
   CREATE  C,
   DOES>   C@  OVER SEG? ?MAD
	   OVER R16?  IF
	       100 OR  SWAP RLOW OP,
	   ELSE
	       376 WR/M,
	   THEN ;

\ shift and rotate group.  Syntax is: r/mem [ CL | 1 ] opcode.
: 10MI
    CREATE  C,
    DOES>   C@  OVER CL =  IF
		NIP 322
	    ELSE
		OVER 1 =  IF  NIP  THEN
		320
	    THEN
	    WR/M, ;

\ CALL and JMP.
: 11MI
    CREATE  C, C,
    DOES>   >R  ( ... mode)  DUP REG? FAR= @ AND ?MAD  R>
	    OVER  # =  ( [d]addr # ^opcode) IF
		NIP  FAR= @ IF
		    1+ C@  codeB,  code4B,
		ELSE
		    C@ SWAP xhere - 2 - SWAP OVER
		    BIG? INVERT OVER 1 AND ( JMP?)  AND  IF
			2 OP, codeB,
		    ELSE
			codeB, 1- code2B,
		    THEN
		THEN
	    ELSE ( r/mem ^opcode)
		377 codeB,
		1+ C@  FAR= @ INVERT 10 AND  XOR  R/M,
	    THEN
	    0 FAR= ! ;

\ POP and PUSH.
: 12MI
    CREATE  C, C, C,
    DOES>   OVER REG?  IF
		C@  OVER  R8? ?MAD   SWAP RLOW  OP,
	    ELSE  1+  OVER SEG?  IF
		C@  OVER CS =  OVER 1 AND ( POP) AND  ?MAD
		RLOW SWAP  RMID OP,
	    ELSE
		COUNT SWAP C@  codeB,  MEM,
	    THEN THEN ;

\ Note: BIG # AL is not detected as an error.
: 13MA  ( operand reg opcode )
    >R  OVER REG?  IF
	R> OVER W, SWAP RR,                             ( reg->reg)
    ELSE  OVER  DUP [x]? SWAP  ) = OR  IF
	R> 2 OR WMEM,                                   ( mem->reg)
    ELSE
	SWAP # - ?MAD                                   ( #  ->reg)
	DUP RLOW 0= ( AL/AX?)  IF
	    R> 4 OR  OVER W,  R16? ,/C,                 ( #  ->acc)
	ELSE                                            ( data reg)
	    OVER B/L?  OVER R16?  2DUP AND  ROT ROT     ( data reg m m f)
	    NEGATE  SWAP INVERT 2 AND  OR  200 OP,      ( data reg m)
	    SWAP  RLOW 300  OR   R>  OP,  ,/C,
	THEN
    THEN THEN ;

: 13MB  ( operand opcode )
    >R  ROT DUP REG?  IF
	R> WMEM,                                ( reg->mem)
    ELSE
	# - ?MAD                                ( #  ->mem) ( data mem)
	2 PICK B/L?  DUP INVERT 2 AND  200 OR  WORD,
	ROT ROT R> MEM,  WORD= @ AND ,/C,  WORD
    THEN ;

\ adds, subtracts and logicals.
: 13MI
    CREATE  C, C,
    DOES>   COUNT SWAP C@ LOG= !
	    OVER REG?  IF  13MA  ELSE  13MB  THEN ;

\ RET.
: 14MI
    CREATE  C,
    DOES>   C@  FAR= @ 10 AND  OR  0 FAR= !     ( [offset] opcode)
	    DUP  codeB,
	    1 AND 0= IF  code2B,  THEN ;        (  offset  +RET  )


\ Segment override prefices.

: SEG ( seg )   RMID 46 OP, ;

: CS:   CS SEG ;
: DS:   DS SEG ;
: ES:   ES SEG ;
: SS:   SS SEG ;

\ ------------------------------------------------------- Special opcode  TEST
: TEST, ( source dest )
    DUP REG? IF
	OVER REG? IF
	    204 OVER W,  SWAP RR,       ( reg->reg)
	ELSE
	    SWAP  # - ?MAD              ( #  ->reg)
	    DUP RLOW 0= ( AL/AX?) IF
		250 OVER W,             ( #  ->acc)
	    ELSE
		366 OVER W,  DUP RLOW 300 OP,
	    THEN
	    R16? ,/C,
	THEN
    ELSE ( [offset] mode mem)
	ROT  DUP REG? IF
	    204 WMEM,                   ( reg->mem)
	ELSE
	    # - ?MAD                    ( #  ->mem)
	    366 WORD,  0 MEM,  WORD= @ ,/C,  WORD
	THEN
    THEN ;

\ -------------------------------------------------- base switches to hex here

HEX

\ --------------------------------------------------------- Special opcode MOV

: MOV, ( source destination )
    DUP SEG? IF
	8E codeB, R/M,                                          ( mem->seg)
    ELSE DUP REG? IF
	2DUP >ACC? IF
	    A0 SWAP W, DROP  code2B,                            ( mem->acc)
	ELSE OVER SEG? IF
	    SWAP 8C codeB, RR,                                  ( seg->reg)
	ELSE OVER # = IF
	    NIP DUP R16? SWAP RLOW OVER 8 AND OR B0 OP, ,/C,    ( #  ->reg)
	ELSE
	    8A OVER W, R/M,                                     ( mem->reg)
	THEN THEN THEN
    ELSE ROT DUP SEG? IF
	8C codeB, MEM,                                          ( seg->mem)
    ELSE DUP # = IF
	DROP C6 WORD, 0 MEM,  WORD= @ ,/C,                      ( #  ->mem)
    ELSE 2DUP >ACC? IF
	A2 SWAP W,  DROP  code2B,                               ( acc->mem)
    ELSE
	88 OVER W,  R/M,                                        ( reg->mem)
    THEN THEN THEN THEN THEN
    WORD ;

\ ----------------------------------------------- Special opcodes INT and XCHG

: INT,  ( n )
    DUP 3 =  IF  DROP  CC codeB,  EXIT  THEN
    CD codeB,  codeB, ;

: XCHG, ( mem reg)
    DUP REG? IF
	OVER REG? OVER AX = AND IF
	    DROP RLOW 90 OP,    ( reg->AX )
	ELSE OVER AX =  IF
	    NIP  RLOW 90 OP,    ( AX- >reg)
	ELSE
	    86 WR/M,            ( mem->reg)
	THEN THEN
    ELSE
	ROT 86 WR/M,            ( reg->mem)
    THEN ;

\ -------------------------------------------------------------------- Opcodes

   37  1MI AAA,      D5  2MI AAD,      D4  2MI AAM,      3F  1MI AAS,
00 10 13MI ADC,   00 00 13MI ADD,   02 20 13MI AND,   9A E8 11MI CALL,
   98  1MI CBW,      F8  1MI CLC,      FC  1MI CLD,      FA  1MI CLI,
   F5  1MI CMC,   00 38 13MI CMP,      A6  5MI CMPS,     99  1MI CWD,
   27  1MI DAA,      2F  1MI DAS,      08  9MI DEC,      30  7MI DIV,
	 ( ESC )     F4  1MI HLT,      38  7MI IDIV,     28  7MI IMUL,
   E4  8MI IN,       00  9MI INC,            ( INT )     CE  1MI INTO,
   CF  1MI IRET,

   9F  1MI LAHF,
   C5  4MI LDS,      8D  4MI LEA,      C4  4MI LES,      F0  1MI LOCK,
   AC  5MI LODS,     E2  3MI LOOP,     E1  3MI LOOPE,    E0  3MI LOOPNE,
	 ( MOV, )    A4  5MI MOVS,     20  7MI MUL,      18  7MI NEG,
   90  1MI NOP,      10  7MI NOT,   02 08 13MI OR,       E6  8MI OUT,
	       8F 07 58 12MI POP,      9D  1MI POPF,
	       FF 36 50 12MI PUSH,     9C  1MI PUSHF,
   10 10MI RCL,      18 10MI RCR,
   F3  1MI REP,      F2  1MI REPNE,    F3  1MI REPE,
   C3 14MI RET,      00 10MI ROL,       8 10MI ROR,      9E  1MI SAHF,
   38 10MI SAR,   00 18 13MI SBB,      AE  5MI SCAS,           ( SEG )
   20 10MI SHL,      28 10MI SHR,      F9  1MI STC,      FD  1MI STD,
   FB  1MI STI,      AA  5MI STOS,  00 28 13MI SUB,            ( TEST, )
   9B  1MI WAIT,           ( XCHG )    D7  1MI XLAT,  02 30 13MI XOR,
   C2 14MI +RET,
EA E9 11MI JMP,

   70  3MI JO,
   71  3MI JNO,
   72  3MI JB,       72  3MI JC,
   73  3MI JAE,      73  3MI JNC,
   74  3MI JE,       74  3MI JZ,
   75  3MI JNE,      75  3MI JNZ,
   76  3MI JBE,
   77  3MI JA,       77  3MI JNBE,
   78  3MI JS,
   79  3MI JNS,
   7A  3MI JPE,
   7B  3MI JPO,
   7C  3MI JL,       7C  3MI JNGE,
   7D  3MI JGE,      7D  3MI JNL,
   7E  3MI JLE,      7E  3MI JNG,
   7F  3MI JG,       7F  3MI JNLE,
   E3  3MI JCXZ,
   EB  3MI JU,

\ ----------------------------------------------------------------------------

: INIT-ASM   \ initializes local labels and switches.
    FWDS  2 CELLS +  DUP FWDS !
    MXL# 2* CELLS +  FWDS CELL+ !
    BWDS  MXL# CELLS  0 FILL
    0 FAR= !  WORD ;

: END-CODE
    PREVIOUS  notNONAME? IF  linkLast  0 TO notNONAME?  THEN ;

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : NEXT, ( a macro)
      AD    codeB,      \ LODSW
      E0FF  code2B, ;   \ JMP AX
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : NEXT, ( a macro)
      2E    codeB,      \ CS:
      AD    codeB,      \ LODSW
      E0FF  code2B, ;   \ JMP AX
[THEN]

\ ----------------------------------------------------------------------------

FORTH-WORDLIST SET-CURRENT      \ add the following word in FORTH-WORDLIST

\   CODE        ( '<spaces>name' -- )           \ TOOLS EXT
\               Skip leading space delimiters.  Parse name delimited by a
\               space. Create a definition for name, called a
\               'code definition,' with the execution semantics defined below.
\               Process subsequent characters in the parse area in an
\               implementation-defined manner, thus generating corresponding
\               machine code. Those characters typically represent source code
\               in a programming language, usually some form of assembly
\               language.  The process continues, refilling the input buffer
\               as needed, until an implementation-defined ending sequence is
\               processed.
\
\               name Execution:( i*x --- j*x )
\               Execute the machine code sequence that was generated
\               following CODE.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : CODE ( "<spaces>name" -- )    \ TOOLS EXT
      -1 TO notNONAME?
      xhere ALIGNED DUP TOxhere   \ align code address
      head,                       \ register a word in dictionary
      ALSO ASSEMBLER
      INIT-ASM ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : CODE ( "<spaces>name" -- )    \ TOOLS EXT
      -1 TO notNONAME?
      ALIGN  head,                \ register a word in dictionary
      ALSO ASSEMBLER
      INIT-ASM ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : CODE ( "<spaces>name" -- )
      -1 TO notNONAME?
      xhere ALIGNED               \ align code address and reserve
      CELL+ DUP TO xhere          \ one cell for 'xt>name' pointer
      head,                       \ register a word in dictionary
      ALSO ASSEMBLER
      INIT-ASM ;
[THEN]

\   ;CODE       Compilation: ( C: colon-sys -- )        \ TOOLS EXT
\               Interpretation: Interpretation semantics for this word
\                               are undefined.
\               Append the run-time semantics below to the current definition.
\               End the current definition, allow it to be found in the
\               dictionary, and enter interpretation state, consuming
\               colon-sys. Process subsequent characters in the parse area in
\               an implementation-defined manner, thus generating corresponding
\               machine code. Those characters typically represent source code
\               in a programming language, usually some form of assembly
\               language.  The process continues, refilling the input buffer as
\               needed, until an implementation-defined ending sequence is
\               processed.
\
\               Run-time:( -- ) ( R: nest-sys -- )
\               Replace the execution semantics of the most recent definition
\               with the name execution semantics given below. Return control
\               to the calling definition specified by nest-sys. An ambiguous
\               condition exists if the most recen definition was not defined
\               with CREATE or a user-defined word that calls CREATE.
\
\               name Execution:( i*x --- j*x )
\               Perform the machine code sequence that was generated
\               following ;CODE.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
: ;CODE
    bal 1- IF -22 THROW THEN        \ control structure mismatch
    NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
    bal- POSTPONE [
    xhere 2 CELLS - TOxhere
    ALSO ASSEMBLER INIT-ASM
    ; COMPILE-ONLY IMMEDIATE
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
: ;CODE
    bal 1- IF -22 THROW THEN        \ control structure mismatch
    NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
    bal- POSTPONE [
    HERE 2 CELLS - TO HERE
    ALSO ASSEMBLER INIT-ASM
    ; COMPILE-ONLY IMMEDIATE
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
: ;CODE
    bal 1- IF -22 THROW THEN        \ control structure mismatch
    NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
    bal- POSTPONE [
    xhere 2 CELLS - TO xhere
    ALSO ASSEMBLER INIT-ASM
    ; COMPILE-ONLY IMMEDIATE
[THEN]

\ Define some useful non-Standard CODE definitions
NONSTANDARD-WORDLIST SET-CURRENT

CODE PC@  ( portAddr -- char )
    BX DX MOV,          \ MOV   DX,BX
    DX AL IN,           \ IN    AL,DX
    BX BX XOR,          \ XOR   BX,BX
    AL BL MOV,          \ MOV   BL,AL
    NEXT,
END-CODE

CODE PC!  ( char portAddr -- )
    BX DX MOV,         \ MOV   DX,BX
    AX POP,            \ POP   AX
    DX AL OUT,         \ OUT   DX,AL
    BX POP,            \ POP   BX
    NEXT,
END-CODE

CODE L@  ( segment offset -- x )
   DS DX MOV,           \ MOV   DX,DS
   DS POP,              \ POP   DS
   0 [BX] BX MOV,       \ MOV   BX,[BX]
   DX DS MOV,           \ MOV   DS,DX
   NEXT,
END-CODE

CODE LC@  ( segment offset -- char )
   DS DX MOV,           \ MOV   DX,DS
   DS POP,              \ POP   DS
   0 [BX] BL MOV,       \ MOV   BL,[BX]
   BH BH XOR,           \ XOR   BH,BH
   DX DS MOV,           \ MOV   DS,DX
   NEXT,
END-CODE

CODE L!  ( x segment offset -- )
   DS DX MOV,           \ MOV   DX,DS
   DS POP,              \ POP   DS
   0 [BX] POP,          \ POP   [BX]
   DX DS MOV,           \ MOV   DS,DX
   BX POP,              \ POP   BX
   NEXT,
END-CODE

CODE LC!  ( char segment offset -- )
   DS DX MOV,           \ MOV   DX,DS
   DS POP,              \ POP   DS
   AX POP,              \ POP   AX
   AL 0 [BX] MOV,       \ MOV   [BX],AL
   DX DS MOV,           \ MOV   DS,DX
   BX POP,              \ POP   BX
   NEXT,
END-CODE

DECIMAL
: LDUMP  ( segment offset u -- )
    ?DUP
    IF   BASE @ >R HEX          \ segment offset u  R: BASE@
	 1- 16 / 1+
	 0 DO CR OVER 4 U.R [CHAR] : EMIT DUP 4 U.R SPACE 2DUP
	      16 0 DO 2DUP LC@ 3 U.R CHAR+ LOOP
	      2SWAP SPACE SPACE
	      16 0 DO   2DUP LC@ 127 AND DUP 0 BL WITHIN
			OVER 127 = OR
			IF DROP [CHAR] _ THEN
			EMIT CHAR+
		   LOOP 2DROP
	      enough? IF LEAVE THEN
	 LOOP
	 R> BASE !
    THEN 2DROP ;

CODE DS@  ( -- data_segment_addr )
  BX PUSH,
  DS BX MOV,
  NEXT,
END-CODE

CODE CS@  ( -- code_segment_addr )
  BX PUSH,
  CS BX MOV,
  NEXT,
END-CODE

envQList SET-CURRENT
-1 CONSTANT ASM8086

SET-CURRENT  SET-ORDER

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF] RAM/ROM! [THEN]
BASE !

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
