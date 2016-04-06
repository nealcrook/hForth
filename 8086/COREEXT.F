\
\ COREEXT.F
\ More Core Extention wordset words for hForth - code definitions
\
\ COREEXT.F can be loaded as following order:
\
\	<< OPTIONAL.F
\	<< ASM8086.F
\       << COREEXT.F
\
\ 1996. 2. 9.
\ Wonyong Koh

BASE @
CHAR " PARSE systemID" ENVIRONMENT? DROP
CHAR " PARSE hForth 8086 ROM Model" COMPARE 0=
[IF] RAM/ROM@ [THEN]
GET-CURRENT  GET-ORDER

FORTH-WORDLIST SET-CURRENT

\   0<> 	( x -- flag )			\ CORE EXT
\		flag is true if and only if x is not equal to zero.
\ : 0<>   0 <> ;
CODE 0<>
    BX BX OR,
    -1 # BX MOV,
    1 L# JNZ,
    BX INC,
1 L:
    NEXT,
END-CODE

\   0>		( n -- flag )			\ CORE EXT
\		flag is true if and only if n is greater than zero.
\ : 0>	 0 > ;
CODE 0>
    BX AX MOV,
    AX DEC,
    CWD,
    DX NOT,
    DX BX MOV,
    NEXT,
END-CODE

\   2>R         ( x1 x2 -- ) ( R: -- x1 x2 )    \ CORE EXT
\               Transfer cell pair to the return stack.
\ : 2>R   SWAP R> SWAP >R SWAP >R >R ;
CODE 2>R
    AX POP,
    2 CELLS # BP SUB,
    AX 1 CELLS [BP] MOV,
    BX 0 [BP] MOV,
    BX POP,
    NEXT,
END-CODE COMPILE-ONLY

\   2R>         ( -- x1 x2 ) ( R: x1 x2 -- )    \ CORE EXT
\               Transfer cell pair from the return stack.
\ : 2R>   R> R> SWAP R> SWAP >R SWAP ;
CODE 2R>
    BX PUSH,
    1 CELLS [BP] AX MOV,
    0 [BP] BX MOV,
    AX PUSH,
    2 CELLS # BP ADD,
    NEXT,
END-CODE COMPILE-ONLY

\   2R@         ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )      \ CORE EXT
\               Copy cell pair from the return stack.
\ : 2R@   R> R> R> 2DUP >R >R SWAP ROT >R ;
CODE 2R@
    BX PUSH,
    1 CELLS [BP] AX MOV,
    0 [BP] BX MOV,
    AX PUSH,
    NEXT,
END-CODE COMPILE-ONLY

\   <>          ( x1 x2 -- flag )               \ CORE EXT
\		Return false if x1 is the same as x2.
\ : <>	 = 0= ;
CODE <>
    AX POP,
    AX BX CMP,
    -1 # BX MOV,
    1 L# JNE,
    BX INC,
1 L:
    NEXT,
END-CODE

HEX
NONSTANDARD-WORDLIST SET-CURRENT
CODE do?DO ( x1 x2 -- ) ( R: -- loop-sys )
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
FORTH-WORDLIST SET-CURRENT

\   ?DO 	( C: -- do-sys )		\ CORE EXT
\		Run-time: ( n1|u1 n2|u2 -- ) ( R: -- | loop-sys )
\		Start a ?DO ... LOOP structure in a colon definition.
\		On execution do as DO only if n1|u1 is not equal to n2|u2.
\ : ?DO   POSTPONE 2DUP POSTPONE =
\         POSTPONE IF POSTPONE 2DROP POSTPONE ELSE
\         POSTPONE doDO xhere dosys+ ; COMPILE-ONLY IMMEDIATE
: ?DO
    POSTPONE do?DO xhere 0 code, orig+ xhere dosys+ ; COMPILE-ONLY IMMEDIATE

CHAR " PARSE systemID" ENVIRONMENT? DROP
CHAR " PARSE hForth 8086 ROM Model" COMPARE 0=
CHAR " PARSE systemID" ENVIRONMENT? DROP
CHAR " PARSE hForth 8086 RAM Model" COMPARE 0= OR
[IF]
  NONSTANDARD-WORDLIST SET-CURRENT
  : doC" ( -- c-addr )   R> DUP COUNT + ALIGNED >R ; COMPILE-ONLY
  FORTH-WORDLIST SET-CURRENT
[THEN]

\   C"          ( "ccc<">" -- )
\               Run-time: ( -- c-addr )
\               Parse ccc delimetered by " . Return the counted string, c-addr.
CHAR " PARSE systemID" ENVIRONMENT? DROP
CHAR " PARSE hForth 8086 ROM Model" COMPARE 0=
[IF]
  : C"   [CHAR] " PARSE
         POSTPONE doC" xhere pack" TOxhere ; COMPILE-ONLY IMMEDIATE
[THEN]
CHAR " PARSE systemID" ENVIRONMENT? DROP
CHAR " PARSE hForth 8086 RAM Model" COMPARE 0=
[IF]
  : C"   [CHAR] " PARSE
         POSTPONE doC" HERE pack" TO HERE ; COMPILE-ONLY IMMEDIATE
[THEN]
CHAR " PARSE systemID" ENVIRONMENT? DROP
CHAR " PARSE hForth 8086 EXE Model" COMPARE 0=
[IF]
  : C"   [CHAR] " PARSE
         ALIGN HERE DUP POSTPONE LITERAL
         pack" ALIGNED TO HERE ; COMPILE-ONLY IMMEDIATE
[THEN]

\   ERASE	( addr u -- )			\ CORE EXT
\		If u is greater than zero, clear all bits in each of u
\		consecutive address units of memory beginning at addr .
CODE ERASE
    DI POP,
    BX BX OR,
    1 L# JZ,
    DS AX MOV,
    AX ES MOV,		\ set ES same as DS
    SI DX MOV,		\ save SI
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

\   PICK	( x_u ... x1 x0 u -- x_u ... x1 x0 x_u )	\ CORE EXT
\		Remove u and copy the uth stack item to top of the stack.
\ : PICK   1+ CELLS sp@ + @ ;
CODE PICK
    BX 1 SHL,
    SP BX ADD,
    0 [BX] BX MOV,
    NEXT,
END-CODE

\   ROLL	( xu xu-1 ... x0 u -- xu-1 ... x0 xu )		\ CORE EXT
\		Remove u.  Rotate u+1 items on the top of the stack.  An
\		ambiguous condition exists if there are less than u+2 items
\		on the stack before ROLL is executed.
: ROLL
    DEPTH OVER - 2 < IF -4 THROW THEN	\ stack underflow
    DUP 1+ PICK >R >R		\ x_u ... x1 x0  R: x_u u
    sp@ DUP CELL+ R> CELLS MOVE DROP R> ;

\   TUCK	( x1 x2 -- x2 x1 x2 )		\ CORE EXT
\		Copy the first (top) stack item below the second stack item.
\ : TUCK   SWAP OVER ;
CODE TUCK
    AX POP,
    BX PUSH,
    AX PUSH,
    NEXT,
END-CODE

\   U>          ( u1 u2 -- flag )               \ CORE  EXT
\               flag is true if and only if u1 is greater than u2.
\ : U>   SWAP U< ;
CODE U>
    AX POP,
    AX BX CMP,
    -1 # BX MOV,
    1 L# JB,
    BX INC,
1 L:
    NEXT,
END-CODE

SET-ORDER  SET-CURRENT

CHAR " PARSE systemID" ENVIRONMENT? DROP
CHAR " PARSE hForth 8086 ROM Model" COMPARE 0=
[IF] RAM/ROM! [THEN]
BASE !

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
