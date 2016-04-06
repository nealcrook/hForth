\
\ DOUBLE.F
\ Double wordset words for hForth
\
\ Worning: Not fully tested yet. Maybe contains some bugs.
\
\ 1997. 2. 28.
\	Facelift to be used with other CPUs.
\ 1996. 7. 19.
\	Fix 'M+'. Thanks M. Edward Borasky.

BASE @
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF] RAM/ROM@ [THEN]
GET-ORDER  GET-CURRENT

FORTH-WORDLIST SET-CURRENT

\   2LITERAL	Compilation: ( x1 x2 -- )	\ DOUBLE
\		Run-time: ( -- x1 x2 )
\		Append the run-time semantics below to the current definition.
\		On run-time, place cell pair x1 x2 on the stack.
: 2LITERAL   SWAP POSTPONE LITERAL POSTPONE LITERAL ; IMMEDIATE COMPILE-ONLY

\   2CONSTANT	( x1 x2 '<spaces>name' -- )     \ DOUBLE
\		name Execution: ( -- x1 x2 )
\		Create a definition for name with the execution semantics.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  CHAR " PARSE ASM8086" ENVIRONMENT? 0=
  [IF]
    : 2CONSTANT   CREATE SWAP , , DOES> DUP @ SWAP CELL+ @ ;
  [ELSE] DROP
    : 2CONSTANT   CREATE SWAP , , DOES>
		  ;CODE
		    0 [BX] PUSH,
		    1 CELLS [BX] BX MOV,
		    NEXT,
		  END-CODE
  [THEN]
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  :NONAME   EXECUTE POSTPONE 2LITERAL ;
  CHAR " PARSE ASM8086" ENVIRONMENT? 0=
  [IF]
    : 2CONSTANT   CREATE SWAP , , compiles> DOES> DUP @ SWAP CELL+ @ ;
  [ELSE] DROP
    : 2CONSTANT   CREATE SWAP , , compiles> DOES>
		  ;CODE
		    0 [BX] PUSH,
		    1 CELLS [BX] BX MOV,
		    NEXT,
		  END-CODE
  [THEN]
[THEN]

\   2VARIABLE	( '<spaces>name' -- )           \ DOUBLE
\		name Execution: ( -- a_addr )
\		Create a definition for name with the execution semantics.
: 2VARIABLE   CREATE 2 CELLS ALLOT ;

\   D+		( d1|ud1 d2|ud2 -- d3|ud3 )	\ DOUBLE
\		Add two double numbers, giving the double sum.
\
\ Already defined in .ASM source.

\   D-		( d1|ud1 d2|ud2 -- d3|ud3 )	\ DOUBLE
\		Subtract d2|ud2 from d1|ud1, giving the difference d3|ud3.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D-	 DNEGATE D+ ;
[ELSE] DROP
  CODE D-
      BX DX MOV,
      AX POP,
      BX POP,
      CX POP,
      AX CX SUB,
      CX PUSH,
      DX BX SBB,
      NEXT,
  END-CODE
[THEN]

\   D.		( d -- )			\ DOUBLE
\		Display d in free field format followed by a space.
\
\ Already defined in .ASM source.

\   D.R 	( d n -- )			\ DOUBLE
\		Display d right-justified in field of width n.
\
\ Already defined in OPTIONAL.F .

\   D0< 	( d -- flag )			\ DOUBLE
\		flag is true if and only if d is less than 0.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D0<   NIP 0< ;
[ELSE] DROP
  CODE D0<
      CX POP,
      BX AX MOV,
      CWD,
      DX BX MOV,
      NEXT,
  END-CODE
[THEN]

\   D0= 	( xd -- flag )			\ DOUBLE
\		flag is true if and only if d is 0.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D0=   OR 0= ;
[ELSE] DROP
  CODE D0=
      CX POP,
      CX BX OR,
      -1 # BX MOV,
      1 L# JZ,
      BX INC,
  1 L:
      NEXT,
  END-CODE
[THEN]

\   D2* 	( xd1 -- xd2 )			\ DOUBLE
\		xd2 is the result of shifting xd1 one bit toward the
\		most-significant bit, filling the vacated least-significant
\		bit with zero.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D2*   2DUP D+ ;
[ELSE] DROP
  CODE D2*
      AX POP,
      AX 1 SHL,
      BX 1 RCL,
      AX PUSH,
      NEXT,
  END-CODE
[THEN]

\   D2/ 	( xd1 -- xd2 )			\ DOUBLE
\		xd2 is the result of shifting xd1 one bit toward the least-
\		significant bit, leaving the most-significant bit unchanged.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D2/   >R 1 RSHIFT R@ 1 AND IF TRUE 1 RSHIFT INVERT OR THEN R> 2/ ; \ by W. Baden
[ELSE] DROP
  CODE D2/
      AX POP,
      BX 1 SAR,
      AX 1 RCR,
      AX PUSH,
      NEXT,
  END-CODE
[THEN]

\   D<		( d1 d2 --- flag )		\ DOUBLE
\		flag is true if and only if d1 is less than d2.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D<	 ROT  2DUP = IF  2DROP U<  EXIT THEN
	 2SWAP 2DROP  > ;
[ELSE] DROP
  CODE D<
      CX POP,
      DX POP,
      AX POP,
      BX DX CMP,
      0 # BX MOV,
      1 L# JZ,
      2 L# JGE,
      BX DEC,
      NEXT,
  1 L:
      CX AX CMP,
      2 L# JAE,
      BX DEC,
  2 L:
      NEXT,
  END-CODE
[THEN]

\   D=		( xd1 xd2 --- flag )		\ DOUBLE
\		flag is true if and only if xd1 is bit-for-bit the same as xd2.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D=	 D- OR 0= ;
[ELSE] DROP
  CODE D=
      CX POP,
      DX POP,
      AX POP,
      BX DX CMP,
      0 # BX MOV,
      1 L# JNZ,
      CX AX CMP,
      1 L# JNZ,
      BX DEC,
  1 L:
      NEXT,
  END-CODE
[THEN]

\   D>S 	( d -- n )			\ DOUBLE
\		n is the equivalent of d.  An ambiguous condition exists if
\		d lies outside the range of a signed single-cell number.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : D>S   OVER S>D NIP <> IF -11 THROW THEN ;	  \ result out of range
[ELSE] DROP
  CODE D>S
      AX POP,
      CWD,
      BX DX CMP,
      1 L# JNE,
      AX BX MOV,
      NEXT,
  1 L:
      -11 # BX MOV,	  \ result out of range
      ' THROW # JMP,
  END-CODE
[THEN]

\   DABS	( d --- ud )			\ DOUBLE
\		ud is the absolute value of d.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : DABS   DUP 0< IF  DNEGATE  THEN ;
[ELSE] DROP
  CODE DABS
      BX BX OR,
      1 L# JNS,
      AX POP,
      AX NEG,
      0 # BX ADC,
      BX NEG,
      AX PUSH,
  1 L:
      NEXT,
  END-CODE
[THEN]

\   DMAX	( d1 d2 --- d3 )		\ DOUBLE
\		d3 is the greater of d1 and d2.
: DMAX	 2OVER 2OVER D< IF  2SWAP  THEN  2DROP ;

\   DMIN       ( d1 d2 --- d3 ) 		\ DOUBLE
\	       d3 is the lesser of d1 and d2.
: DMIN	 2OVER 2OVER D< 0= IF  2SWAP  THEN  2DROP ;

\   DNEGATE	  ( d1 --- d2 ) 		\ DOUBLE
\		  d2 is the negation of d1.
\ Already defined in .ASM source.

\   M*/ 	  ( d1 n1 +n2 --- d2 )		\ DOUBLE
\		  Multiply d1 by n1 producing the triple-cell intermediate
\		  result t. Divide t by +n2 giving the double-cell quotient d2.
\		  An ambiguous condition exists if +n2 is zero or negative, or
\		  the quotient lies outside of the range of a double-precision
\		  signed integer.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  \ by Wil Baden
  : T*	  TUCK UM* 2SWAP UM* SWAP >R 0 D+ R> ROT ROT ;	( u . u -- u . . )
  : T/	  DUP >R UM/MOD ROT ROT R> UM/MOD NIP SWAP ;	( u . . u -- u . )
  : M*/   >R T*  R> T/ ;				( u . u u -- u . )
[ELSE] DROP
  CODE M*/
      BX BX OR,
      1 L# JZ,
      2 L# JS,
      CX POP,	  \ n1
      DI POP,	  \ high significant part of d1
      AX POP,	  \ low significant part of d1
      DI DX MOV,
      CX DX XOR,
      DX PUSH,	  \ save sign of the result
      BX PUSH,	  \ save n2
      CX CX OR,
      3 L# JNS,
      CX NEG,	  \ ABS(n1)
  3 L:
      DI DI OR,
      4 L# JNS,
      AX NEG,	  \ DABS(d1)
      0 # DI ADC,
      DI NEG,
  4 L:
      CX MUL,	  \ lower partial product DX:AX
      AX DI XCHG, \ lower partial product DX:DI
      DX BX MOV,  \ lower partial product BX:DI
      CX MUL,	  \ lower partial product BX:DI, upper partial product DX:AX
      BX AX ADD,
      0 # DX ADC, \ intermediate product DX:AX:DI
      BX POP,	  \ restore n2
      BX DX CMP,
      5 L# JAE,
      BX DIV,
      AX DI XCHG, \ upper part of the quotient in DI
      BX DIV,	  \ quotient DI:AX
      DI DI OR,
      5 L# JS,	  \ DI:AX does not fit in double signed integer
      CX POP,	  \ restore sign of the result
      CX CX OR,
      6 L# JNS,
      AX NEG,	  \ DNEGATE
      0 # DI ADC,
      DI NEG,
  6 L:
      AX PUSH,
      DI BX MOV,
      NEXT,
  5 L:
      -11 # BX MOV,	  \ result out of range
      ' THROW # JMP,
  2 L:
      -12 # BX MOV,	  \ argument type mismatch
      ' THROW # JMP,
  1 L:
      -10 # BX MOV,	  \ divide by zero
      ' THROW # JMP,
  END-CODE
[THEN]

\   M+		  ( d1|ud1 n --- d2|ud2 )	\ DOUBLE
\		  Add n to d1|ud1, giving the sum d2|ud2.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : M+	 S>D D+ ;
[ELSE] DROP
  CODE M+
      BX AX MOV,	\ move stack top to AX for sign-extend
      CWD,		\ DX:AX now has 32-bit value
      BX POP,		\ upper half of second argument
      CX POP,		\ lower half of second argument
      AX CX ADD,	\ add lower halves
      DX BX ADC,	\ add upper halves with carry - sum now in BX:CX
      CX PUSH,		\ push lower half of result to stack
      NEXT,		\ finished
  END-CODE
[THEN]

\   2ROT	( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )	\ DOUBLE EXT
\		Rotate the top three cell pairs on the stack bringing cell
\		pair x1 x2 to the top of the stack.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : 2ROT   2>R 2SWAP 2R> 2SWAP ;
[ELSE] DROP
  CODE 2ROT
      CX POP,
      DX POP,
      AX POP,
      SP DI MOV,
      1 CELLS [DI] AX XCHG,
      0 [DI] DX XCHG,
      CX PUSH,
      BX PUSH,
      AX PUSH,
      DX BX MOV,
      NEXT,
  END-CODE
[THEN]

\   DU< 	( ud1 ud2 --- flag )				\ DOUBLE EXT
\		flag is true if and only if ud1 is less than ud2.
CHAR " PARSE ASM8086" ENVIRONMENT? 0=
[IF]
  : DU<   ROT  2DUP = IF  2DROP U<  EXIT THEN
	  2SWAP 2DROP  U> ;
[ELSE] DROP
  CODE DU<
      CX POP,
      DX POP,
      AX POP,
      BX DX CMP,
      0 # BX MOV,
      1 L# JZ,
      2 L# JAE,
      BX DEC,
      NEXT,
  1 L:
      CX AX CMP,
      2 L# JAE,
      BX DEC,
  2 L:
      NEXT,
  END-CODE
[THEN]

envQList SET-CURRENT
-1 CONSTANT DOUBLE
-1 CONSTANT DOUBLE-EXT

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
