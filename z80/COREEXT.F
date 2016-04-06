\
\ COREEXT.F
\ More Core Extention wordset words for hForth - code definitions
\
\ COREEXT.F can be loaded as following order:
\
\	<< OPTIONAL.F
\	<< ASM8086.F
\	<< COREEXT.F
\
\ 1996. 2. 9.
\ Wonyong Koh
\
\ 1997. 6. 5.
\	Fix colon definition of do?DO.
\ 1997. 2. 28.
\	Facelift to be used with other CPUs.
\ 1996. 11. 29.
\	Provide CODE definition of ROLL.
\	Revise '?DO' for control-flow stack.
\	Revise 'C"' to catch exception -24 'parsed string overflow'.

BASE @
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF] RAM/ROM@ [THEN]
GET-ORDER  GET-CURRENT

FORTH-WORDLIST SET-CURRENT

\   <>		( x1 x2 -- flag )		\ CORE EXT
\		Return false if x1 is the same as x2.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : <>	 = 0= ;
[ELSE] DROP
  CODE <>
      AX POP,
      AX BX CMP,
      -1 # BX MOV,
      1 L# JNE,
      BX INC,
  1 L:
      NEXT,
  END-CODE
[THEN]

\   0<> 	( x -- flag )			\ CORE EXT
\		flag is true if and only if x is not equal to zero.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : 0<>   0 <> ;
[ELSE] DROP
  CODE 0<>
      BX BX OR,
      -1 # BX MOV,
      1 L# JNZ,
      BX INC,
  1 L:
      NEXT,
  END-CODE
[THEN]

\   0>		( n -- flag )			\ CORE EXT
\		flag is true if and only if n is greater than zero.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : 0>	 0 > ;
[ELSE] DROP
  CODE 0>
      BX AX MOV,
      AX DEC,
      CWD,
      DX NOT,
      DX BX MOV,
      NEXT,
  END-CODE
[THEN]

\   2>R 	( x1 x2 -- ) ( R: -- x1 x2 )	\ CORE EXT
\		Transfer cell pair to the return stack.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : 2>R   SWAP R> SWAP >R SWAP >R >R ;
[ELSE] DROP
  CODE 2>R
      AX POP,
      2 CELLS # BP SUB,
      AX 1 CELLS [BP] MOV,
      BX 0 [BP] MOV,
      BX POP,
      NEXT,
  END-CODE COMPILE-ONLY
[THEN]

\   2R> 	( -- x1 x2 ) ( R: x1 x2 -- )	\ CORE EXT
\		Transfer cell pair from the return stack.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : 2R>   R> R> SWAP R> SWAP >R SWAP ;
[ELSE] DROP
  CODE 2R>
      BX PUSH,
      1 CELLS [BP] AX MOV,
      0 [BP] BX MOV,
      AX PUSH,
      2 CELLS # BP ADD,
      NEXT,
  END-CODE COMPILE-ONLY
[THEN]

\   2R@ 	( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )	\ CORE EXT
\		Copy cell pair from the return stack.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : 2R@   R> R> R> 2DUP >R >R SWAP ROT >R ;
[ELSE] DROP
  CODE 2R@
      BX PUSH,
      1 CELLS [BP] AX MOV,
      0 [BP] BX MOV,
      AX PUSH,
      NEXT,
  END-CODE COMPILE-ONLY
[THEN]

HEX
NONSTANDARD-WORDLIST SET-CURRENT

\   do?DO	( n1|u1 n2|u2 -- ) ( R: -- n1 n2-n1-max_negative )
\		Run-time funtion of ?DO.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  CHAR " PARSE model" ENVIRONMENT? DROP
  CHAR " PARSE ROM Model" COMPARE 0=
  CHAR " PARSE model" ENVIRONMENT? DROP
  CHAR " PARSE RAM Model" COMPARE 0= OR
  [IF]
    : do?DO
	2DUP = IF 2DROP R> @ >R EXIT THEN
	>R
	\ get max-negative
	[ -1 BL PARSE MAX-N ENVIRONMENT? DROP - ] LITERAL
	+ R> OVER - SWAP R> SWAP >R SWAP >R CELL+ >R ; COMPILE-ONLY
  [THEN]
  CHAR " PARSE model" ENVIRONMENT? DROP
  CHAR " PARSE EXE Model" COMPARE 0=
  [IF]
    : do?DO
	2DUP = IF 2DROP R> code@ >R EXIT THEN
	>R
	\ get max-negative
	[ -1 BL PARSE MAX-N ENVIRONMENT? DROP - ] LITERAL
	+ R> OVER - SWAP R> SWAP >R SWAP >R CELL+ >R ; COMPILE-ONLY
  [THEN]
[ELSE] DROP
  CODE do?DO
      AX POP,
      AX BX CMP,
      1 L# JE,
      1 CELLS # SI ADD,
      2 CELLS # BP SUB,
      8000 # AX ADD,
      AX 1 CELLS [BP] MOV,
      AX BX SUB,
      BX 0 [BP] MOV,
      BX POP,
      NEXT,
  1 L:
      BX POP,
      CS:
      0 [SI] SI MOV,
      NEXT,
  END-CODE COMPILE-ONLY
[THEN]

FORTH-WORDLIST SET-CURRENT

\   ?DO 	( C: -- do-sys )		\ CORE EXT
\		Run-time: ( n1|u1 n2|u2 -- ) ( R: -- | loop-sys )
\		Start a ?DO ... LOOP structure in a colon definition.
\		On execution do as DO only if n1|u1 is not equal to n2|u2.
: ?DO
    0 rakeVar !
    POSTPONE do?DO xhere 0 code,	\ leave ?DO-orig
    xhere  bal+ 			\ leave DO-dest
    ; COMPILE-ONLY IMMEDIATE

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  NONSTANDARD-WORDLIST SET-CURRENT
  : doC" ( -- c-addr )   R> DUP COUNT + ALIGNED >R ; COMPILE-ONLY
  FORTH-WORDLIST SET-CURRENT
[THEN]

\   C"          ( "ccc<">" -- )
\		Run-time: ( -- c-addr )
\		Parse ccc delimetered by " . Return the counted string, c-addr.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : C"   [CHAR] " PARSE
	 DUP [ BL PARSE /COUNTED-STRING ENVIRONMENT? DROP ] LITERAL
	 > IF -18 THROW THEN	\ parsed string overflow
	 POSTPONE doC" xhere pack" TOxhere ; COMPILE-ONLY IMMEDIATE
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : C"   [CHAR] " PARSE
	 DUP [ BL PARSE /COUNTED-STRING ENVIRONMENT? DROP ] LITERAL
	 > IF -18 THROW THEN	\ parsed string overflow
	 POSTPONE doC" HERE pack" TO HERE ; COMPILE-ONLY IMMEDIATE
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : C"   [CHAR] " PARSE
	 DUP [ BL PARSE /COUNTED-STRING ENVIRONMENT? DROP ] LITERAL
	 > IF -18 THROW THEN	\ parsed string overflow
	 ALIGN HERE DUP POSTPONE LITERAL
	 pack" ALIGNED TO HERE ; COMPILE-ONLY IMMEDIATE
[THEN]

\   ERASE	( addr u -- )			\ CORE EXT
\		If u is greater than zero, clear all bits in each of u
\		consecutive address units of memory beginning at addr .
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  CR .( ERASE need to be CODE defined.) CR
  .( No ANS Standard word except MOVE and ERASE can access address unit directly.)
\  ABORT
[ELSE] DROP
  CODE ERASE
      DI POP,
      BX BX OR,
      1 L# JZ,
      DS AX MOV,
      AX ES MOV,	  \ set ES same as DS
      SI DX MOV,	  \ save SI
      DI SI MOV,
      AL AL XOR,
      AL 0 [SI] MOV,
      BX CX MOV,
      DI INC,
      CX DEC,
      REP, BYTE MOVS,
      DX SI MOV,
  1 L:
      BX POP,
      NEXT,
  END-CODE
[THEN]

\   ROLL	( xu xu-1 ... x0 u -- xu-1 ... x0 xu )		\ CORE EXT
\		Remove u.  Rotate u+1 items on the top of the stack.  An
\		ambiguous condition exists if there are less than u+2 items
\		on the stack before ROLL is executed.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : ROLL
      DEPTH DUP 2 < IF -4 THROW THEN	\ stack underflow
      2 - OVER U< IF -4 THROW THEN
      DUP 1+ PICK >R >R 	  \ x_u ... x1 x0  R: x_u u
      sp@ DUP CELL+ R> CELLS MOVE DROP R> ;
[ELSE] DROP
  CODE ROLL
      userP ) DI MOV,
      1 CELLS [DI] DI MOV,	  \ sp0
      SP DI SUB,
      DI 1 SAR, 		  \ depth-1 in DI
      DI DEC,
      1 L# JS,
      BX DI CMP,
      1 L# JB,
      SI DX MOV,
      BX CX MOV,
      BX 1 SHL,
      SP BX ADD,
      BX DI MOV,
      DI SI MOV,
      1 CELLS # SI SUB,
      0 [BX] BX MOV,
      STD,
      REP, WORD MOVS,
      CLD,
      AX POP,
      DX SI MOV,
      NEXT,
  1 L:
      -4 # BX MOV,
      ' THROW # JMP,
  END-CODE
[THEN]

\   TUCK	( x1 x2 -- x2 x1 x2 )		\ CORE EXT
\		Copy the first (top) stack item below the second stack item.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : TUCK   SWAP OVER ;
[ELSE] DROP
  CODE TUCK
      AX POP,
      BX PUSH,
      AX PUSH,
      NEXT,
  END-CODE
[THEN]

\   U>		( u1 u2 -- flag )		\ CORE EXT
\		flag is true if and only if u1 is greater than u2.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : U>	 SWAP U< ;
[ELSE] DROP
CODE U>
      AX POP,
      AX BX CMP,
      -1 # BX MOV,
      1 L# JB,
      BX INC,
  1 L:
      NEXT,
  END-CODE
[THEN]

\   CS-PICK					\ TOOLS EXT
\    Execution: ( C: destu ... orig0|dest0 -- destu ... orig0|dest0 destu )
\		( S: u -- )
\  Interpretation: Interpretation semantics for this word are undefined.
\
\		Remove u.  Copy destu to the top of the control-flow
\		stack.	An ambiguous condition exists if there are
\		less than u+1 items, each of which shall be an orig
\		or dest, on the control-flow stack before CS-PICK is
\		executed.
\
\		If the control-flow stack is implemented using the
\		data stack, u shall be the topmost item on the data
\		stack.
: CS-PICK  ( destu ... orig0|dest0 u -- destu ... orig0|dest0 destu )
    DUP 2* 1+ PICK		\ check destu; dest type is 0
    IF -22 THROW THEN		\ control structure mismatch
    DUP >R  0 SWAP		\ destu ... orig0|dest0 0 u  R: u
    1+ 0 DO I 2* 1+ PICK OR LOOP	\ dest type is 0; orig type is 1
    1 INVERT AND IF -22 THROW THEN	\ ORed types should be 0
    R> 2* 1+ PICK 0
    bal 1+ TO bal ; COMPILE-ONLY

\   CS-ROLL					\ TOOLS EXT
\    Execution: ( C: origu|destu origu-1|destu-1 ... orig0|dest0 --
\				origu-1|destu-1 ... orig0|dest0 origu|destu )
\		( S: u -- )
\  Interpretation: Interpretation semantics for this word are undefined.
\
\		Remove u.  Rotate u+1 elements on top of the
\		control-flow stack so that origu|destu is on top of
\		the control-flow stack.  An ambiguous condition
\		exists if there are less than u+1 items, each of
\		which shall be an orig or dest, on the control-flow
\		stack before CS-ROLL is executed.
\
\		If the control-flow stack is implemented using the
\		data stack, u shall be the topmost item on the data
\		stack.
: CS-ROLL  ( origu|destu origu-1|destu-1 ... orig0|dest0 u --
		\	origu-1|destu-1 ... orig0|dest0 origu|destu )
    DUP >R  0 SWAP		\ destu ... orig0|dest0 0 u  R: u
    1+ 0 DO I 2* 1+ PICK OR LOOP	\ dest type is 0; orig type is 1
    1 INVERT AND IF -22 THROW THEN	\ ORed types should be 0
    R@ 2* 1+ ROLL
    R> 2* 1+ ROLL ; COMPILE-ONLY

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
