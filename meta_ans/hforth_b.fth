CR .( *** Slightly less trivial high-level words )
\ The distinction is that the previous set of words never used any IMMEDIATE
\ words in their definitions.
F-DEF

META-HI [IF]
: DECIMAL	( -- )				\ CORE
\		Set the numeric conversion radix to decimal 10.
		H# 0A BASE ! ;
[ELSE]
		CODE DECIMAL
		GetVaxAdr BASE R0 =,
		0A # R1 MOV,
		[ R0 ] R1 STR,
		END-CODE
[THEN]

META-HI [IF]
: ?DUP		( x -- x x | 0 )		 \ CORE
\		Duplicate top of the stack if it is not zero.
		DUP IF DUP THEN ;
[ELSE]
		CODE ?DUP
		0 # tos tos ORRS,		\ set flags
		![ CELLL NEGATE # dsp ] tos NE STR,	\ pushD if ne
		END-CODE
[THEN]

: <		( n1 n2 -- flag )		\ CORE
\		Return true if n1 is less than n2.
		2DUP XOR 0<             \ same sign?
		IF DROP 0< EXIT THEN    \ different signs, true if n1 <0
		- 0< ;			\ same signs, true if n1-n2 <0

: >		( n1 n2 -- flag )		\ CORE
\		Return true if n1 is greater than n2.
		SWAP < ;

META-HI [IF]
: LSHIFT	( x1 u -- x2 )		   \ CORE
\		Perform a logical left shift of u bit-places on x1, giving x2.
\		Put 0 into the least significant bits vacated by the shift.
		?DUP IF 0 DO 2* LOOP THEN ;
[ELSE]
		CODE LSHIFT
		R0 popD,
		tos LSL R0 tos MOV,
		END-CODE
[THEN]

META-HI [IF]
: RSHIFT	( x1 u -- x2 )		   \ CORE
\		Perform a logical right shift of u bit-places on x1, giving x2.
\		Put 0 into the most significant bits vacated by the shift.
		?DUP IF
			0 SWAP [ CELLL 8 * ] LITERAL SWAP -
			0 DO  2DUP D+  LOOP
			NIP
		THEN ;
[ELSE]
		CODE RSHIFT
		R0 popD,
		tos LSR R0 tos MOV,
		END-CODE
[THEN]

: THROW		( k*x n -- k*x | i*x n )        \ EXCEPTION
\		If n is not zero, pop the topmost exception frame from the
\		exception stack, along with everything on the return stack
\		above the frame. Then restore the condition before CATCH and
\		transfer control just after the CATCH that pushed that
\		exception frame.
		?DUP
		IF   throwFrame @ rp!   \ restore return stack
		     R> throwFrame !    \ restore THROW frame
		     R> SWAP >R sp!     \ restore data stack
		     DROP R>
		     'init-i/o EXECUTE
		THEN ;

META-HI [IF]
: ABORT		( i*x -- ) ( R: j*x -- )	\ EXCEPTION EXT
\		Reset data stack and jump to QUIT.
		-1 THROW ;
[ELSE]
		CODE ABORT
		-1 tos =,	\ overwrite the top stack item - going to reset
				\ the data stack, so who cares..
		' THROW B,
		END-CODE-FLOW	\ tidy up
[THEN]

META-HI [IF]
: I		( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
\		Push the innermost loop index.
		rp@ [ 1 CELLS ] LITERAL + @
		rp@ [ 2 CELLS ] LITERAL + @ + ; COMPILE-ONLY
[ELSE]
		CODE I
		tos pushD,
		[ rsp ] R0 LDR,		\ note that we're one CELLL closer to
					\ the tos in CODE
		[ CELLL # rsp ] tos LDR,
		R0 tos tos ADD,
		END-CODE COMPILE-ONLY
[THEN]

META-HI [IF]
: J		( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
\		Push the index of next outer loop.
		rp@ [ 3 CELLS ] LITERAL + @
		rp@ [ 4 CELLS ] LITERAL + @  +  ; COMPILE-ONLY
[ELSE]
		CODE J
		tos pushD,
		\ note that we're one CELLL closer to the tos in CODE
		[ CELLL 2 * # rsp ] R0 LDR,
		[ CELLL 3 * # rsp ] tos LDR,
		R0 tos tos ADD,
		END-CODE COMPILE-ONLY
[THEN]

META-HI [IF]
: CELLS		( n1 -- n2 )			\ CORE
\		Calculate number of address units for n1 cells.
		[ CELLL ] LITERAL * ;   \ slow, very portable
[ELSE]
		CODE CELLS
		2 # LSL tos tos MOV,		\ multiply by 4
		END-CODE
[THEN]

S-DEF
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
  : ?call DUP @ H# 0FF000000 AND [ CALLL ] LITERAL =
	\ that call-detection is crude - not an exact check..
	IF DUP DUP @ H# 00FFFFFF AND     \ it's a branch.. get offset
		DUP H# 007FFFFF > IF
			H# 00FF000000 OR \ sign extend the offset
	        THEN
		D# 2 LSHIFT              \ convert to byte offset
		+ CELL+ CELL+            \ fix up for pipeline prefetch
	SWAP CELL+ SWAP EXIT THEN 0 ;

\   same?	( c-addr1 c-addr2 u -- -1|0|1 )
\		Return 0 if two strings, ca1 u and ca2 u, are same; -1 if
\		string, ca1 u is smaller than ca2 u; 1 otherwise. Used by
\		'(search-wordlist)'. Code definition is preferred to speed up
\		interpretation. Colon definition is shown below.
META-HI [IF]
  : same?	?DUP IF         \ null strings are always same
                0 DO OVER C@ OVER C@ XOR \ 0 (false) if match.. so continue
                       IF UNLOOP C@ SWAP C@ > 2* 1+ EXIT THEN
                       CHAR+ SWAP CHAR+ SWAP
                  LOOP
		THEN 2DROP 0 ;
[ELSE]
		CODE same?
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
		END-CODE-EARLY		\ could simply fall through the movCOND
01 L.		1 # tos GT MOV,
		-1 tos LT =,
		END-CODE
[THEN]

META-HI [IF]
: (search-wordlist)	( c-addr u wid -- 0 | xt f 1 | xt f -1)
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
		ROT >R SWAP DUP 0= IF D# -16 THROW THEN
				\ attempt to use zero-length string as a name
		>R		\ wid  R: ca1 u
		BEGIN @		\ ca2  R: ca1 u
		  DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
		  DUP COUNT [ =MASK ]  LITERAL AND R@ = \ ca2 ca2+char f
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
		CODE (search-wordlist)
		R0 popD,		\ u
		R1 popD,		\ c-addr
		R0 R0 R0 ORRS,
		10 # tos EQ MVN,	\ message -16 decimal
		' THROW EQ B,		\ can't use 0-length string as a name
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
		END-CODE
[THEN]

META-HI [IF]
: bal+		( -- )
\		Increase bal by 1.
		bal 1+ TO bal ;
[ELSE]
		CODE bal+
		GetVaxAdr bal R0 =,
		[ R0 ] R1 LDR,
		1 # R1 R1 ADD,
		[ R0 ] R1 STR,
		END-CODE
[THEN]

META-HI [IF]
: bal-		( -- )
\		Decrease bal by 1.
		bal 1- TO bal ;
[ELSE]
		CODE bal-
		GetVaxAdr bal R0 =,
		[ R0 ] R1 LDR,
		1 # R1 R1 SUB,
		[ R0 ] R1 STR,
		END-CODE
[THEN]

: search-word	( c-addr u -- c-addr u 0 | xt f 1 | xt f -1)
\		Search dictionary for a match with the given name. Return
\		execution token, not-compile-only flag and -1 or 1
\		( IMMEDIATE) if found; c-addr u 0 if not.
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

CR .( *** Standard words - Colon definitions)
F-DEF

: SEARCH-WORDLIST ( c-addr u wid -- 0 | xt 1 | xt -1)     \ SEARCH
\		Search word list for a match with the given name.
\		Return execution token and -1 or 1 ( IMMEDIATE) if found.
\		Return 0 if not found.
		(search-wordlist) DUP IF NIP THEN ;


META-HI [IF]
: UM*		( u1 u2 -- ud )			\ CORE
\		Unsigned multiply. Return double-cell product.
		0 SWAP [ CELLL 8 * ] LITERAL 0 DO
		   DUP um+ >R >R DUP um+ R> +
		   R> IF >R OVER um+ R> + THEN	\ if carry
		LOOP ROT DROP ;
[ELSE]
		CODE UM*
		R0 popD,
		tos R0 tos R1 UMULL,
		R1 pushD,		\ ms 32-bits are at top of stack
		END-CODE
[THEN]

META-HI [IF]
: *		( n1|u1 n2|u2 -- n3|u3 )	\ CORE
\		Multiply n1|u1 by n2|u2 giving a single product.
		UM* DROP ;
[ELSE]
		CODE *
		R0 popD,
		tos R0 tos MUL,
		END-CODE
[THEN]

META-HI [IF]
: CHARS		( n1 -- n2 )			\ CORE
\		Calculate number of address units for n1 characters.
		[ CHARR ] LITERAL * ;   \ slow, very portable
[ELSE]
		CODE CHARS	\ no-op for ARM
		END-CODE
[THEN]


META-HI [IF]
: /STRING	( c-addr1 u1 n -- c-addr2 u2 )  \ STRING
\		Adjust the char string at c-addr1 by n chars.
		DUP >R - SWAP R> CHARS + SWAP ;
[ELSE]
		CODE /STRING
		R0 popD,		\ u1 (tos is n)
		R1 popD,		\ c-addr1
		tos R1 R1 ADD,
		R1 pushD,		\ c-addr2 <= c-addr1 + n
		tos R0 tos SUB,		\ new count u2 <= u1 - n
		END-CODE
[THEN]


\ TODO the error was rather nasty/unhelpful when this puddle wasn't present
\ it was "fatal internal error"
\ due to the TODO highlighted in line 1074 of asmarm.fth
\ basically, it means that the current literal pool is unreachable 

		ALSO ASSEMBLER
		10 LTORG \ create the 2nd puddle in the literal pool
		PREVIOUS

S-DEF
META-HI [IF]
: COMPILE-ONLY	( -- )
\		Make the most recent definition an compile-only word.
		lastName [ =COMP ] LITERAL OVER @ OR SWAP ! ;
[ELSE]
		CODE COMPILE-ONLY
		GetVaxAdr npVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL 2 * # R2 R2 ADD,	\ Lastname
		[ R2 ] R3 LDR,		\ Get length
		=COMP # R3 R3 ORR,	\ Set flag
		[ R2 ] R3 STR,
		END-CODE
[THEN]

F-DEF
META-HI [IF]
: IMMEDIATE	( -- )				\ CORE
\		Make the most recent definition an immediate word.
		lastName [ =IMED ] LITERAL OVER @ OR SWAP ! ;
[ELSE]
		CODE IMMEDIATE
		GetVaxAdr npVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL 2 * # R2 R2 ADD,		\ Lastname
		[ R2 ] R3 LDR,			\ get length
		=IMED # R3 R3 ORR,		\ set flag
		[ R2 ] R3 STR,
		END-CODE
[THEN]

S-DEF
: doDO		( n1|u1 n2|u2 -- ) ( R: -- n1 n2-n1-max_negative )
\		Run-time funtion of DO.
		>R [ MaxNegative ] LITERAL + R>
		OVER - SWAP R> SWAP >R SWAP >R >R ;

F-DEF
META-HI [IF]
: [		( -- )				\ CORE
\		Enter interpretation state.
		0 STATE ! ; COMPILE-ONLY IMMEDIATE
[ELSE]
		CODE [
		GetVaxAdr STATE R0 =,
		0 # R1 MOV,
		[ R0 ] R1 STR,
		END-CODE COMPILE-ONLY IMMEDIATE
[THEN]

META-HI [IF]
: ]		( -- )				\ CORE
\		Enter compilation state.
		-1 STATE ! ;
[ELSE]
		CODE ]
		GetVaxAdr STATE R0 =,
		-1 R1 =,
		[ R0 ] R1 STR,
		END-CODE
[THEN]

META-HI [IF]
: DNEGATE	( d1 -- d2 )			\ DOUBLE
\		Two's complement of double-cell number.
		INVERT >R INVERT 1 um+ R> + ;
[ELSE]
		CODE DNEGATE
		R0 popD,
		tos tos MVN,			\ 1's complement of high part
		R0 R0 MVN,			\ 1's complement of lo part
		1 # R0 R0 ADDS,
		0 # tos tos ADC,
		R0 pushD,
		END-CODE
[THEN]

META-HI [IF]
: U<		( u1 u2 -- flag )		\ CORE
\		Unsigned compare of top two items. True if u1 < u2.
		2DUP XOR 0< IF NIP 0< EXIT THEN - 0< ;
[ELSE]
		CODE U<
		R0 popD,			\ u1
		R0 tos CMP,			\ u2-u1
		TRUEE tos =,
		FALSEE tos LS =,		\ C or Z
		END-CODE
[THEN]

: WITHIN	( n1|u1 n2|n2 n3|u3 -- flag )   \ CORE EXT
\		Return true if (n2|u2<=n1|u1 and n1|u1<n3|u3) or
\		(n2|u2>n3|u3 and (n2|u2<=n1|u1 or n1|u1<n3|u3)).
		OVER - >R - R> U< ;

: UM/MOD	( ud u1 -- u2 u3 )		\ CORE
\		Unsigned division of a double-cell number ud by a single-cell
\		number u1. Return remainder u2 and quotient u3.
	DUP 0= IF D# -10 THROW THEN	\ divide by zero
	2DUP U< IF
		NEGATE [ CELLL 8 * ] LITERAL 0 DO
			>R DUP um+ >R >R DUP um+ R> + DUP
			R> R@ SWAP >R um+ R> OR
			IF >R DROP 1+ R> ELSE DROP THEN
			R>
		LOOP DROP SWAP EXIT
	ELSE D# -11 THROW          \ result out of range
	THEN ;

META-HI [IF]
: ABS		( n -- u )			\ CORE
\		Return the absolute value of n.
		DUP 0< IF NEGATE THEN ;
[ELSE]
		CODE ABS
		0 # tos tos ORRS,		\ set flags
		0 R0 =,
		tos R0 tos MI SUB,
		END-CODE
[THEN]

META-HI [IF]
: M*		( n1 n2 -- d )			\ CORE
\		Signed multiply. Return double product.
		2DUP XOR 0< >R ABS SWAP ABS UM* R> IF DNEGATE THEN ;
[ELSE]
		CODE M*
		R0 popD,
		tos R0 tos R1 SMULL,
		R1 pushD,		\ ms 32-bits are at top of stack
		END-CODE
[THEN]

: FM/MOD	( d n1 -- n2 n3 )		\ CORE
\		Signed floored divide of double by single. Return mod n2
\		and quotient n3.
		DUP >R 2DUP XOR >R >R DUP 0< IF DNEGATE THEN
		R@ ABS UM/MOD
		R> 0< IF SWAP NEGATE SWAP THEN
		R> 0< IF NEGATE         \ negative quotient
		    OVER IF R@ ROT - SWAP 1- THEN
		    R> DROP
		    0 OVER < IF D# -11 THROW THEN   \ result out of range
		    EXIT
		THEN
		R> DROP DUP 0< IF D# -11 THROW THEN ; \ result out of range

: SM/REM	( d n1 -- n2 n3 )		\ CORE
\		Symmetric divide of double by single. Return remainder n2
\		and quotient n3.
		2DUP XOR >R OVER >R >R DUP 0< IF DNEGATE THEN
		R> ABS UM/MOD
		R> 0< IF SWAP NEGATE SWAP THEN
		R> 0< IF        \ negative quotient
		    NEGATE 0 OVER < 0= IF EXIT THEN
		    D# -11 THROW THEN          \ result out of range
		DUP 0< IF D# -11 THROW THEN ;  \ result out of range

: */MOD		( n1 n2 n3 -- n4 n5 )		\ CORE
\		Multiply n1 by n2 producing double-cell intermediate,
\		then divide it by n3. Return single-cell remainder and
\		single-cell quotient.
		>R M* R> FM/MOD ;

: */		( n1 n2 n3 -- n4 )              \ CORE
\		Multiply n1 by n2 producing double-cell intermediate,
\		then divide it by n3. Return single-cell quotient.
		*/MOD NIP ;

: /MOD		( n1 n2 -- n3 n4 )		\ CORE
\		Divide n1 by n2, giving single-cell remainder n3 and
\		single-cell quotient n4.
		>R S>D R> FM/MOD ;

: MOD		( n1 n2 -- n3 )		  \ CORE
\		Divide n1 by n2, giving the single cell remainder n3.
\		Returns modulo of floored division in this implementation.
		/MOD DROP ;

: /		( n1 n2 -- n3 )		  \ CORE
\		Divide n1 by n2, giving single-cell quotient n3.
		/MOD NIP ;

S-DEF
META-HI [IF]
: 1chars/	( n1 -- n2 )
\		Calculate number of chars for n1 address units.
		1 CHARS / ;     \ slow, very portable
\   : 1chars/   ;               \ fast, must be redefined for each system
[ELSE]
		CODE 1chars/
		END-CODE
[THEN]

F-DEF
: >NUMBER	( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )    \ CORE
\		Add number string's value to ud1. Leaves string of any
\		unconverted chars.
  BEGIN  DUP
  WHILE  >R  DUP >R C@		     \ ud char  R: u c-addr
    DUP [ CHAR 9 1+ ] LITERAL [CHAR] A WITHIN
    IF DROP R> R> EXIT THEN
    [ CHAR 0 ] LITERAL - D# 9 OVER <
    [ CHAR A CHAR 9 1 + - ] LITERAL AND -
    DUP 0 BASE @ WITHIN
  WHILE  SWAP BASE @ UM* DROP ROT BASE @ UM* D+ R> R> 1 /STRING
  REPEAT DROP R> R> THEN ;

: LITERAL	Compilation: ( x -- )           \ CORE
\		Run-time: ( -- x )
\		Append following run-time semantics. Put x on the stack on
\		execution
		POSTPONE doLIT code, ; COMPILE-ONLY IMMEDIATE

META-HI [IF]
: COMPILE,	( xt -- )		        \ CORE EXT
\		Compile the execution token on data stack into current
\		colon definition.
		code, ; COMPILE-ONLY
[ELSE]
		CODE COMPILE,
		' code, B,
		END-CODE-FLOW COMPILE-ONLY
[THEN]

: GET-CURRENT	( -- wid )			\ SEARCH
\		Return the indentifier of the compilation wordlist.
		current @ ;

S-DEF
: singleOnly	( c-addr u -- x )
\		Handle the word not found in the search-order. If the string
\		is legal, leave a single cell number in interpretation state.
		0 DUP 2SWAP OVER C@ [CHAR] -
		= DUP >R IF 1 /STRING THEN
		>NUMBER IF D# -13 THROW THEN       \ undefined word
		2DROP R> IF NEGATE THEN ;

: singleOnly,	( c-addr u -- )
\		Handle the word not found in the search-order. Compile a
\		single cell number in compilation state.
		singleOnly POSTPONE LITERAL ;

: (doubleAlso)	( c-addr u -- x 1 | x x 2 )
\		If the string is legal, leave a single or double cell number
\		and size of the number.
		0 DUP 2SWAP OVER C@ [CHAR] -
		= DUP >R IF 1 /STRING THEN
		>NUMBER ?DUP
		IF 1- IF D# -13 THROW THEN    \ more than one char is remained
		     DUP C@ [CHAR] . XOR      \ last char is not '.'
		     IF D# -13 THROW THEN        \ undefined word
		     R> IF DNEGATE THEN
		     D# 2 EXIT THEN
		2DROP R> IF NEGATE THEN       \ single number
		1 ;

: doubleAlso	( c-addr u -- x | x x )
\		Handle the word not found in the search-order. If the string
\		is legal, leave a single or double cell number in
\		interpretation state.
		(doubleAlso) DROP ;

: doubleAlso,	( c-addr u -- )
\		Handle the word not found in the search-order. If the string
\		is legal, compile a single or double cell number in
\		compilation state.
		(doubleAlso) 1- IF SWAP POSTPONE LITERAL THEN
		POSTPONE LITERAL ;

: -.		( -- )
\		You don't need this word unless you care that '-.' returns
\		double cell number 0. Catching illegal number '-.' in this way
\		is easier than make 'interpret' catch this exception.
		D# -13 THROW ; IMMEDIATE   \ undefined word

: optiCOMPILE,	( xt -- )
\		Optimized COMPILE, . Reduce doLIST ... EXIT sequence if
\		xt is COLON definition which contains less than two words.
  DUP ?call ['] doLIST = IF
    DUP @ ['] EXIT = IF         \ if first word is EXIT
      2DROP EXIT THEN
    DUP CELL+ @ ['] EXIT = IF   \ if second word is EXIT
      @ DUP ['] doLIT XOR  \ make sure it is not literal value
      IF SWAP THEN
    THEN
  THEN DROP COMPILE, ;

META-HI [IF]
: linkLast	( -- )
\		Link the word being defined to the current wordlist.
		lastName GET-CURRENT ! ;
[ELSE]
		CODE linkLast
		GetVaxAdr npVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,
		CELLL 2 * # R2 R2 ADD,	\ address of last definition name
		GetVaxAdr current R0 =,
		[ R0 ] R1 LDR,		\ wid of compilation wordlist
		[ R1 ] R2 STR,		\ gets bumped up by new addition
		END-CODE
[THEN]

\ ------------------- got to here in rearranging words

: pipe		( -- ) ( R: xt -- )
\		Connect most recently defined word to code following DOES>.
\		Structure of CREATEd word:
\		        | call-doCREATE | 0 or DOES> code addr | a-addr |
  lastName name>xt ?call DUP IF   \ code-addr xt2
    ['] doCREATE = IF
      R> SWAP ! EXIT           \ change DOES> code of CREATEd word
    THEN
  THEN
  D# -32 THROW     \ invalid name argument, no-CREATEd last name
  ; COMPILE-ONLY

F-DEF
: PARSE		( char "ccc<char>"-- c-addr u )		\ CORE EXT
\		Scan input stream and return counted string delimited by char.
		>R  SOURCE >IN @ /STRING        \ c-addr u  R: char
		DUP IF
		   CHARS OVER + OVER       \ c-addr c-addr+u c-addr  R: char
		   BEGIN  DUP C@ R@ XOR
		   WHILE  CHAR+ 2DUP =
		   UNTIL  DROP OVER - 1chars/ DUP
		   ELSE   NIP  OVER - 1chars/ DUP CHAR+
		   THEN   >IN +!
		THEN   R> DROP EXIT ;

S-DEF
: skipPARSE	( char "<chars>ccc<char>" -- c-addr u )
\		Skip leading chars and parse a word using char as a
\		delimeter. Return the name.
		>R SOURCE >IN @ /STRING    \ c_addr u  R: char
		DUP IF
		   BEGIN  OVER C@ R@ =
		   WHILE 1- SWAP CHAR+ SWAP DUP 0=
		   UNTIL  R> DROP EXIT
		   ELSE THEN
		   DROP SOURCE DROP - 1chars/ >IN ! R> PARSE EXIT
		THEN R> DROP ;

META-HI [IF]
: PARSE-WORD	( "<spaces>ccc<space>" -- c-addr u )
\		Skip leading spaces and parse a word. Return the name.
		BL skipPARSE ;
[ELSE]
		CODE PARSE-WORD
		tos pushD,
		SPC # tos MOV,
		' skipPARSE B,
		END-CODE-FLOW			\ tidy up
[THEN]

: (')		( "<spaces>name" -- xt 1 | xt -1 )
\		Parse a name, find it and return execution token and
\		-1 or 1 ( IMMEDIATE) if found
		PARSE-WORD search-word ?DUP IF NIP EXIT THEN
		errWord 2!      \ if not found error
		D# -13 THROW ;     \ undefined word

F-DEF
: '		( "<spaces>name" -- xt )        \ CORE
\		Parse a name, find it and return xt.
		(') DROP ;

S-DEF
: set-i/ov	( -- )
\		Set input/output vectors, according to IOBYTES. Easiest way to
\		do this is to assume that the default vectors will be used
\		then check IOBYTES; if DEMON vectors are required, overwrite
\		them
    sysVar0 var0 [ 4 CELLS ] LITERAL MOVE       \ set i/o vectors
    IOBYTES H# FF AND H# FF = IF
      ['] TRUE TO 'ekey?
      ['] DRX@ TO 'ekey
      ['] TRUE TO 'emit?
      ['] DTX! TO 'emit
    THEN ;

: set-i/o	( -- )
\		Set input/output device. (Includes initialising any hardware)
		set-i/ov !IO ;

F-DEF
: THEN		Compilation: ( C: orig -- )     \ CORE
\		Run-time: ( -- )
\		Resolve the forward reference orig.
		1- IF D# -22 THROW THEN    \ control structure mismatch
				        \ orig type is 1
		xhere SWAP ! bal- ; COMPILE-ONLY IMMEDIATE

S-DEF
: rake		( C: do-sys -- )
\		Gathers LEAVEs.
  DUP code, rakeVar @
  BEGIN  2DUP U<
  WHILE  DUP @ xhere ROT !
  REPEAT  rakeVar ! DROP
  ?DUP IF		  \ check for ?DO
    1 bal+ POSTPONE THEN \ orig type is 1
  THEN bal- ; COMPILE-ONLY


CR .( *** Words for multitasking)
S-DEF

: PAUSE		( -- )
\		Stop current task and transfer control to the task of which
\		'status' USER variable is stored in 'follower' USER variable
\		of current task.
		rp@ sp@ stackTop !  follower @ >R ; COMPILE-ONLY

: wake		( -- )
\		Wake current task.
		R> userP !      \ userP points 'follower' of current task
		stackTop @ sp!          \ set data stack
		rp! ; COMPILE-ONLY      \ set return stack

CR .( *** Essential Standard words - Colon definitions)
F-DEF

META-HI [IF]
: HOLD		( char -- )			\ CORE
\		Add char to the beginning of pictured numeric output string.
		hld @  1 CHARS - DUP hld ! C! ;
[ELSE]
		CODE HOLD
		GetVaxAdr hld R0 =,
		[ R0 ] R1 LDR,
		CHARR # R1 R1 SUB,
		[ R1 ] tos STRB,
		[ R0 ] R1 STR,
		tos popD,
		END-CODE
[THEN]

: #		( ud1 -- ud2 )			\ CORE
\		Extract one digit from ud1 and append the digit to
\		pictured numeric output string. ( ud2 = ud1 / BASE )
		0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP
		D# 9 OVER < [ CHAR A CHAR 9 1 + - ] LITERAL AND +
		[ CHAR 0 ] LITERAL + HOLD R> ;

: #>		( xd -- c-addr u )              \ CORE
\		Prepare the output string to be TYPE'd.
\		||xhere>WORD/#-work-area|
		2DROP hld @ xhere [ PADSize ] LITERAL + OVER - 1chars/ ;

: #S		( ud -- 0 0 )			\ CORE
\		Convert ud until all digits are added to the output string.
		BEGIN # 2DUP OR 0= UNTIL ;

: SIGN		( n -- )			\ CORE
\		Add a minus sign to the numeric output string if n is negative.
		0< IF [CHAR] - HOLD THEN ;

META-HI [IF]
: <#		( -- )				\ CORE
\		Initiate the numeric output conversion process.
\		||xhere>WORD/#-work-area|
		xhere [ PADSize ] LITERAL + hld ! ;
[ELSE]
		CODE <#
		GetVaxAdr cpVar R0 =,
		[ R0 ] R1 LDR,
		[ R1 ] R2 LDR,		\ xhere address
		PADSize CHARR * # R2 R2 ADD,
		GetVaxAdr hld R3 =,
		[ R3 ] R2 STR,
		END-CODE
[THEN]

S-DEF
: (d.)		( d -- c-addr u )
\		Convert a double number to a string.
		SWAP OVER  DUP 0< IF  DNEGATE  THEN
		<#  #S ROT SIGN  #> ;

F-DEF
META-HI [IF]
: ,		( x -- )		         \ CORE
\		Reserve one cell in RAM or ROM data space and store x in it.
		HERE ! [ CELLL ] LITERAL hereVar +! ;
[ELSE]
		CODE ,
		GetVaxAdr hereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		CELLL # [ R2 ] tos STR,
		tos popD,
		[ R1 ] R2 STR,
		END-CODE
[THEN]

: DEPTH		( -- +n )		        \ CORE
\		Return the depth of the data stack.
		sp@ sp0 SWAP - [ CELLL ] LITERAL / ;

META-HI [IF]
: ALIGNED	( addr -- a-addr )		\ CORE
\		Align address to the cell boundary by rounding UP if necessary
		DUP 0 [ CELLL ] LITERAL UM/MOD DROP DUP
		IF [ CELLL ] LITERAL SWAP - THEN + ;    \ slow, very portable
[ELSE]
		CODE ALIGNED
		3 # tos tos ADD,
		3 # tos tos BIC,		\ round it up
		END-CODE
[THEN]

META-HI [IF]
: ALIGN		( -- )				\ CORE
\		Align the data space pointer.
		hereVar DUP @ ALIGNED SWAP ! ;
[ELSE]
		CODE ALIGN
		GetVaxAdr hereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		3 # R2 R2 ADD,
		3 # R2 R2 BIC,			\ round it up
		[ R1 ] R2 STR,
		END-CODE
[THEN]

S-DEF
: xt,		( xt1 -- xt2 )
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
\ nac added []literal twice
\ TODO: need a check to see that the destination is reachable..
	xhere ALIGNED DUP TOxhere SWAP
	xhere - cell- cell- D# 2 RSHIFT \ get signed offset
	H# 00FFFFFF AND                 \ mask off high-order sign bits
	[ CALLL ] LITERAL OR            \ make the opcode 
	xhere SWAP                      \ remember where it will go
	code, IDflushline ;             \ emit it and purge the block

F-DEF
: TYPE		( c-addr u -- )		  \ CORE
\		Display the character string if u is greater than zero.
		?DUP IF 0 DO DUP C@ EMIT CHAR+ LOOP THEN DROP ;

META-HI [IF]
: SPACE		( -- )				\ CORE
\		Send the blank character to the output device.
		D# 32 EMIT ;
[ELSE]
		CODE SPACE
		tos pushD,
		SPC # tos MOV,
		' EMIT B,
		END-CODE-FLOW
[THEN]

S-DEF
META-HI [IF]
: pack"		( c-addr u a-addr -- a-addr2 )
\		Place a string c-addr u at a-addr and gives the next
\		cell-aligned address. Fill the rest of the last cell with
\		null character.
\
        OVER [ MaxCountedString ] LITERAL SWAP U< 
	IF D# -18 THROW THEN		\ parsed string overflow
	2DUP SWAP CHARS + CHAR+ DUP >R  \ ca u aa aa+u+1
	ALIGNED cell- 0 SWAP !		\ fill 0 at the end of string
	2DUP C! CHAR+ SWAP		\ c-addr a-addr+1 u
	CHARS MOVE R> ALIGNED ; COMPILE-ONLY
[ELSE]
\ TODO this code version doesn't do the length check and THROW
		CODE pack"
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
		END-CODE COMPILE-ONLY
[THEN]

: head,		( xt "<spaces>name" -- )
\		Parse a word and build a dictionary entry using xt and name.
		PARSE-WORD DUP 0=
		IF errWord 2! D# -16 THROW THEN
		               \ attempt to use zero-length string as a name
		DUP [ =MASK ] LITERAL > IF D# -19 THROW THEN   \ definition name too long
		2DUP GET-CURRENT SEARCH-WORDLIST  \ name exist?
		IF DROP ." redefine " 2DUP TYPE SPACE THEN \ warn if redefined
		npVar @ OVER CHARS CHAR+ -
		DUP ALIGNED SWAP OVER XOR IF cell- THEN \ aligned to lower address
		DUP >R pack" DROP R>            \ pack the name in dictionary
		cell- GET-CURRENT @ OVER !      \ build wordlist link
		cell- DUP npVar !  ! ;          \ adjust name space pointer
						\ and store xt at code field

F-DEF
: :NONAME	( -- xt colon-sys )		\ CORE EXT
\		Create an execution token xt, enter compilation state and
\		start the current definition.
		bal IF D# -29 THROW THEN	\ compiler nesting
		['] doLIST xt, DUP D# -1
		0 TO notNONAME? 1 TO bal  ] ;

: :		( "<spaces>name" -- colon-sys ) \ CORE
\		Start a new colon definition using next word as its name.
		:NONAME ROT head, D# -1 TO notNONAME? ;

: ;		( colon-sys -- )		 \ CORE
\		Terminate a colon definition.
  bal 1- IF D# -22 THROW THEN        \ control structure mismatch
  NIP 1+ IF D# -22 THROW THEN        \ colon-sys type is -1
  notNONAME? IF   \ if the last definition is created by ':'
    linkLast 0 TO notNONAME?     \ link the word to wordlist
  THEN  POSTPONE EXIT     \ add EXIT at the end of the definition
  0 TO bal POSTPONE [ ; COMPILE-ONLY IMMEDIATE

		ALSO ASSEMBLER
		8 LTORG \ create the 3rd puddle in the literal pool
		PREVIOUS

S-DEF
: FILE		( -- )
\		Set FILE bit in IOBYTES to disable local echo and impose
\		XON/XOFF flow control for host file download
		IOBYTES H# 10000000 OR TO IOBYTES ;

: HAND		( -- )
\		Clear FILE bit in IOBYTES to turn off XON/XOFF handshake
\		and turn on local echo
		IOBYTES H# EFFFFFFF AND TO IOBYTES ;

: FLOW-ON	( -- )
\		Send an XON character for the file downloading process.
		[ XON ] LITERAL EMIT ;

: FLOW-OFF	( -- )
\		Send an XOFF character for the file downloading process; ONLY
\		if FILE bit is set
		IOBYTES H# 10000000 AND IF [ XOFF ] LITERAL EMIT THEN ;

F-DEF
: CR		( -- )				\ CORE
\		Carriage return and linefeed.
		[ CRR ] LITERAL EMIT [ LFF ] LITERAL EMIT ;

: EKEY		( -- u )		         \ FACILITY EXT
\		Receive one keyboard event u.
		BEGIN PAUSE EKEY? UNTIL 'ekey EXECUTE ;

: KEY		( -- char )		      \ CORE
\		Receive a character. Do not display char.
		EKEY [ MaxChar ] LITERAL AND ;

S-DEF
META-HI [IF]
: EMITE		( x -- )
\		Send a character to the output device unless FILE bit
\		is set in IOBYTES
		IOBYTES H# 10000000 AND 
		IF DROP EXIT THEN 'emit EXECUTE ;
[ELSE]
		CODE EMITE
		GetVaxAdr IOBYTES R0 =,		\ find out whether to emit it
		[ R0 ] R1 LDR,
		10000000 # R1 R1 ANDS,
		01 G# EQ B,			\ yes, do it
		tos popD,
		END-CODE
[THEN]

F-DEF
: ACCEPT	( c-addr +n1 -- +n2 )		\ CORE
\		Accept a string of up to +n1 chars. Return with actual count.
\		Implementation-defined editing. Stops at EOL# .
\		Supports backspace and delete editing.
		FLOW-ON >R 0
		BEGIN  DUP R@ < 		\ ca n2 f  R: n1
		WHILE  KEY DUP BL <
		       IF   DUP  [ CRR ] LITERAL = IF ROT 2DROP R> DROP FLOW-OFF EXIT THEN
			    DUP  [ TABB ] LITERAL =
			    IF	 DROP 2DUP + BL DUP EMITE SWAP C! 1+
			    ELSE DUP  [ BKSPP ] LITERAL =
				 SWAP [ DEL ] LITERAL = OR
				 IF DUP
					\ discard the last char if not 1st char
				 	IF 1- [ BKSPP ] LITERAL EMITE BL EMITE [ BKSPP ] LITERAL EMITE THEN
				 THEN
			    THEN
		       ELSE >R 2DUP CHARS + R> DUP EMITE SWAP C! 1+ THEN
		REPEAT SWAP  R> 2DROP FLOW-OFF ;

: AGAIN		( C: dest -- )			\ CORE EXT
\		Resolve backward reference dest. Typically used as
\		BEGIN ... AGAIN . Move control to the location specified by
\		dest on execution.
		IF D# -22 THROW THEN	\ control structure mismatch;
					\ dest type is 0
		POSTPONE branch code, bal- ; COMPILE-ONLY IMMEDIATE

: AHEAD		( C: -- orig )			\ TOOLS EXT
\		Put the location of a new unresolved forward reference onto
\		control-flow stack.
		POSTPONE branch xhere 0 code,
		1 bal+          \ orig type is 1
		; COMPILE-ONLY IMMEDIATE

: CATCH		( i*x xt -- j*x 0 | i*x n )     \ EXCEPTION
\		Push an exception frame on the exception stack and then execute
\		the execution token xt in such a way that control can be
\		transferred to a point just after CATCH if THROW is executed
\		during the execution of xt.
		sp@ >R throwFrame @ >R          \ save error frame
		rp@ throwFrame !  EXECUTE       \ execute
		R> throwFrame !		  \ restore error frame
		R> DROP  0 ;		     \ no error

: CONSTANT	( x "<spaces>name" -- )         \ CORE
\		name Execution: ( -- x )
\		Create a definition for name which pushes x on the stack on
\		execution.
		bal IF D# -29 THROW THEN	\ compiler nesting
		['] doCONST xt, head, code, linkLast ;

: CREATE	( "<spaces>name" -- )           \ CORE
\		name Execution: ( -- a-addr )
\		Create a data object in RAM/ROM data space, which return
\		data object address on execution
		bal IF D# -29 THROW THEN	\ compiler nesting
		['] doCREATE xt, head,
		xhere DUP CELL+ CELL+ TOxhere   \ reserve two cells
		0 OVER !		 \ no DOES> code yet
		ALIGN HERE SWAP CELL+ ! \ >BODY returns this address
		linkLast ;              \ link CREATEd word to current wordlist

: IF		Compilation: ( C: -- orig )	\ CORE
\		Run-time: ( x -- )
\		Put the location of a new unresolved forward reference orig
\		onto the control flow stack. On execution jump to location
\		specified by the resolution of orig if x is zero.
		POSTPONE 0branch xhere 0 code,
		1 bal+          \ orig type is 1
		; COMPILE-ONLY IMMEDIATE

: ELSE		Compilation: ( C: orig1 -- orig2 )      \ CORE
\		Run-time: ( -- )
\		Start the false clause in an IF-ELSE-THEN structure.
\		Put the location of new unresolved forward reference orig2
\		onto control-flow stack.
		POSTPONE AHEAD 2SWAP POSTPONE THEN ; COMPILE-ONLY IMMEDIATE

S-DEF
: interpret	( i*x -- j*x )
\		Interpret input string.
		BEGIN  DEPTH 0< IF D# -4 THROW THEN        \ stack underflow
		       PARSE-WORD DUP
		WHILE  2DUP errWord 2!
		       search-word          \ ca u 0 | xt f -1 | xt f 1
		       DUP IF
		         SWAP STATE @ OR 0= \ compile-only in interpretation
		         IF D# -14 THROW THEN  \ interpreting a compile-only word
		       THEN
		       1+ 2* STATE @ 1+ + CELLS 'doWord + @ EXECUTE
		REPEAT 2DROP ;

F-DEF
: REFILL	( -- flag )		      \ CORE EXT
\		Attempt to fill the input buffer from the input source. Make
\		the result the input buffer, set >IN to zero, and return true
\		if successful. Return false if the input source is a string
\		from EVALUATE.
		SOURCE-ID IF 0 EXIT THEN
		npVar @ [ PADSize CHARS 2* ] LITERAL - DUP
		[ PADSize ] LITERAL ACCEPT sourceVar 2!
		0 >IN ! -1 ;

S-DEF
: .ok		( -- )
\		Display 'ok'.
		S" ok" TYPE ;

F-DEF
: D.		( d -- )		         \ DOUBLE
\		Display d in free field format followed by a space.
		(d.) TYPE SPACE ;

META-HI [IF]
: .		( n -- )		         \ CORE
\		Display a signed number followed by a space.
		S>D D. ;
[ELSE]
		CODE .
		tos pushD,
		0 # R1 MOV,
		\ sign-extend tos into tos to form ms of double
		20 # ASR tos R1 tos ADD, \ 20H = 32 decimal
		' D. B,
		END-CODE-FLOW			\ tidy up
[THEN]

META-HI [IF]
: U.		( u -- )			\ CORE
\		Display u in free field format followed by space.
		0 D. ;
[ELSE]
		CODE U.
		tos pushD,
		0 # tos MOV,
		' D. B,
		END-CODE-FLOW
[THEN]

: ENVIRONMENT?	( c-addr u -- false | i*x true )     \ CORE
\		Environment query.
		envQList SEARCH-WORDLIST
		DUP >R IF EXECUTE THEN R> ;

S-DEF
: hi		( -- )
\		By default, this is the application started through 'boot
		CR ." hForth "
		S" CPU" ENVIRONMENT? DROP TYPE SPACE
		S" model" ENVIRONMENT? DROP TYPE SPACE [CHAR] v EMIT
		S" version"  ENVIRONMENT? DROP TYPE
		."  by Wonyong Koh, 1997" CR
		." ARM Arch 4/StrongARM port by nac@forth.org" CR
		." ALL noncommercial and commercial uses are granted." CR
		." Please send comment, bug report and suggestions to:" CR
		."   wykoh@pado.krict.re.kr or wykoh@hitel.kol.co.kr" CR ;

F-DEF
: QUIT		( -- ) ( R: i*x -- )            \ CORE
\		Empty the return stack, store zero in SOURCE-ID, make the user
\		input device the input source, and start text interpreter.
  BEGIN
    rp0 rp! 0 TO SOURCE-ID  0 TO bal  POSTPONE [
    BEGIN CR REFILL DROP SPACE    \ REFILL returns always true
      ['] interpret CATCH ?DUP 0=
    WHILE STATE @ 0= IF .prompt THEN
    REPEAT
    DUP D# -1 XOR IF				   \ ABORT
      DUP D# -2 = IF SPACE abort"msg 2@ TYPE ELSE   \ ABORT"
	SPACE errWord 2@ TYPE
	SPACE [CHAR] ? EMIT SPACE
	DUP D# -1 [ 0 NumTHROWMsgs - ] LITERAL WITHIN
        IF ." Exception # " . ELSE \ undefined exception
	  CELLS THROWMsgTbl + @ COUNT TYPE
        THEN
      THEN
    THEN sp0 sp!
  AGAIN ;

S-DEF
TAR_EBSA285 [IF]
: INIT-BUS	( -- )
\               For the 21285, need to init some PCI stuff to avoid hanging
\               the host system (if any)
    H# 40012000 @ H# 40 AND 0= IF
    \ stand-alone bit is clear so must init the PCI bus to avoid hanging the
    \ host system - see EBSA-285 reference manual for explanation of these
    H#       0C H# 42000034 !    H#       00 H# 42000150 !
    H#       00 H# 42000154 !    H#       00 H# 42000140 !
    H#      200 H# 4200013C !    H#   7C0000 H# 42000100 !
    H# 40000000 H# 42000010 !    H#     F000 H# 42000018 !
    H#       17 H# 42000004 !    H#        1 H# 4200013C !
  THEN ;
[ELSE]
: INIT-BUS ;
[THEN]

ALSO ASSEMBLER 02 G. PREVIOUS
: COLD		( -- )
\		The cold start sequence execution word.

\ TODO the first defn would be more efficient but requires us to be able
\ to find the values of target VALUEs at meta-compile time.
\  sysVar0 var0 [ sysVar0End sysVar0 - ] LITERAL MOVE \ init system variables
  sysVar0 var0 sysVar0End sysVar0 - MOVE \ init system variables
  xhere DUP @			\ free-ROM [free-ROM]
  INVERT SWAP 2DUP ! @ XOR	\ writable ROM?
  IF RAMB TO cpVar RAMT TO npVar THEN
  sp0 sp! rp0 rp!		\ initialize stack
  'init-i/o EXECUTE 'boot EXECUTE
  INIT-BUS QUIT ;		\ start interpretation

CR .( *** Rest of CORE words and two facility words, EKEY? and EMIT?)
F-DEF

: (		( "ccc<)>" -- )			\ CORE
\		Ignore following string up to next ) . A comment.
		[CHAR] ) PARSE 2DROP ; IMMEDIATE

S-DEF
META-HI [IF]
: doS"		( u -- c-addr u )
\		Run-time function of S" .
		R> SWAP 2DUP + ALIGNED >R ; COMPILE-ONLY
[ELSE]
		CODE doS"
		\ in the colon case the 'next address' is on the
		\ return stack. In the code case it is still in fpc
		fpc pushD,		\ start of string
		tos fpc fpc ADD,	\ point to end of string
		3 # fpc fpc ADD,	\ and align
		3 # fpc fpc BIC,	\ by rounding it up
		END-CODE COMPILE-ONLY
[THEN]

F-DEF
: SLITERAL	( c-addr1 u -- )		\ STRING
\		Run-time ( -- c-addr2 u )
\		Compile a string literal. Return the string on execution.
		DUP POSTPONE LITERAL POSTPONE doS"
		CHARS xhere 2DUP + ALIGNED TOxhere
		SWAP MOVE ; COMPILE-ONLY IMMEDIATE

: S"		Compilation: ( "ccc<">" -- )	\ CORE
\		Run-time: ( -- c-addr u )
\		Parse ccc delimetered by " . Return the string specification
\		c-addr u on execution.
		[CHAR] " PARSE POSTPONE SLITERAL ; COMPILE-ONLY IMMEDIATE

: ."		( "ccc<">" -- )			\ CORE
\		Run-time ( -- )
\		Compile an inline string literal to be typed out at run time.
		POSTPONE S" POSTPONE TYPE ; COMPILE-ONLY IMMEDIATE

\   BYE		( -- )				\ TOOLS EXT
\		Return control to the host operation system, if any.
\
MM_DEMON [IF]
		CODE BYE
		11 SWI,				\ SWI_Exit - halt execution and
						\ return to debugger
		END-CODE-FLOW
[ELSE]
  : BYE		." Sorry - nowhere to go" CR ;
[THEN]

: >BODY		( xt -- a-addr )		 \ CORE
\		Push data field address of CREATEd word.
\		Structure of CREATEd word:
\		        | call-doCREATE | 0 or DOES> code addr | a-addr |
\
  ?call DUP IF		     \ code-addr xt2
    ['] doCREATE = IF        \ should be call-doCREATE
      CELL+ @ EXIT
    THEN
  THEN D# -31 THROW ;        \ >BODY used on non-CREATEd definition

: ABORT"	( "ccc<">" -- )			\ EXCEPTION EXT
\		Run-time ( i*x x1 -- | i*x ) ( R: j*x -- | j*x )
\		Conditional abort with an error message.
		POSTPONE S" POSTPONE ROT
		POSTPONE IF POSTPONE abort"msg POSTPONE 2!
		D# -2 POSTPONE LITERAL POSTPONE THROW
		POSTPONE ELSE POSTPONE 2DROP POSTPONE THEN
		;  COMPILE-ONLY IMMEDIATE


: BEGIN		( C: -- dest )			\ CORE
\		Start an infinite or indefinite loop structure. Put the next
\		location for a transfer of control, dest, onto the data
\		control stack.
		xhere 0 bal+            \ dest type is 0
		; COMPILE-ONLY IMMEDIATE

META-HI [IF]
: C,		( char -- )			\ CORE
\		Compile a character into data space.
		HERE C! [ CHARR ] LITERAL hereVar +! ;
[ELSE]
		CODE C,
		GetVaxAdr hereVar R0 =,
		[ R0 ] R1 LDR,			\ point to ROM or RAM pointer
		[ R1 ] R2 LDR,			\ get its value
		CHARR # [ R2 ] tos STRB,
		[ R1 ] R2 STR,
		tos popD,
		END-CODE
[THEN]

: CHAR		( "<spaces>ccc" -- char )       \ CORE
\		Parse next word and return the value of first character.
		PARSE-WORD DROP C@ ;

: DO		Compilation: ( C: -- do-sys )   \ CORE
\		Run-time: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
\		Start a DO-LOOP structure in a colon definition. Place do-sys
\		on control-flow stack, which will be resolved by LOOP or +LOOP.
		0 rakeVar !  0		   \ ?DO-orig is 0 for DO
		POSTPONE doDO xhere  bal+       \ DO-dest
		; COMPILE-ONLY IMMEDIATE

: DOES>		( C: colon-sys1 -- colon-sys2 ) \ CORE
\		Build run time code of the data object CREATEd.
  bal 1- IF D# -22 THROW THEN        \ control structure mismatch
  NIP 1+ IF D# -22 THROW THEN        \ colon-sys type is -1
  POSTPONE pipe ['] doLIST xt, -1 ; COMPILE-ONLY IMMEDIATE

: EVALUATE	( i*x c-addr u -- j*x )         \ CORE
\		Evaluate the string. Save the input source specification.
\		Store -1 in SOURCE-ID.
		SOURCE >R >R >IN @ >R  SOURCE-ID >R
		-1 TO SOURCE-ID
		sourceVar 2!  0 >IN !  interpret
		R> TO SOURCE-ID
		R> >IN ! R> R> sourceVar 2! ;

META-HI [IF]
: FILL		( c-addr u char -- )		\ CORE
\		Store char in each of u consecutive characters of memory
\		beginning at c-addr.
		ROT ROT ?DUP IF 0 DO 2DUP C! CHAR+ LOOP THEN 2DROP ;
[ELSE]
		CODE FILL
		\ tos holds char to fill with
		R0 popD,		\ count
		R1 popD,	      	\ dest
		R0 R0 R0 ORRS,		\ drop straight through if length is 0
01 L.		CHARR # [ R1 ] tos NE STRB,
		CHARR # R0 R0 NE SUBS,
		01 L# NE B,
		tos popD,
		END-CODE
[THEN]

: FIND		( c-addr -- c-addr 0 | xt 1 | xt -1)     \ SEARCH
\		Search dictionary for a match with the given counted name.
\		Return execution token and -1 or 1 ( IMMEDIATE) if found\
\		c-addr 0 if not found.
		DUP COUNT search-word ?DUP IF NIP ROT DROP EXIT THEN
		2DROP 0 ;

: LEAVE		( -- ) ( R: loop-sys -- )       \ CORE
\		Terminate definite loop, DO|?DO  ... LOOP|+LOOP, immediately.
		POSTPONE UNLOOP POSTPONE branch
		xhere rakeVar DUP @ code, ! ; COMPILE-ONLY IMMEDIATE

: LOOP		Compilation: ( C: do-sys -- )   \ CORE
\		Run-time: ( -- ) ( R: loop-sys1 -- loop-sys2 )
\		Terminate a DO|?DO ... LOOP structure. Resolve the destination
\		of all unresolved occurences of LEAVE.
		POSTPONE doLOOP  rake ; COMPILE-ONLY IMMEDIATE

: +LOOP		Compilation: ( C: do-sys -- )   \ CORE
\		Run-time: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Terminate a DO-+LOOP structure. Resolve the destination of all
\		unresolved occurences of LEAVE.
\		On execution add n to the loop index. If loop index did not
\		cross the boundary between loop_limit-1 and loop_limit,
\		continue execution at the beginning of the loop. Otherwise,
\		finish the loop.
		POSTPONE do+LOOP  rake ; COMPILE-ONLY IMMEDIATE

META-HI [IF]
: MAX		( n1 n2 -- n3 )			\ CORE
\		Return the greater of two top stack items.
		2DUP < IF SWAP THEN DROP ;
[ELSE]
		CODE MAX
		R0 popD,
		tos R0 CMP,
		R0 tos GT MOV,
		END-CODE
[THEN]

META-HI [IF]
: MIN		( n1 n2 -- n3 )			\ CORE
\		Return the smaller of top two stack items.
		2DUP > IF SWAP THEN DROP ;
[ELSE]
		CODE MIN
		R0 popD,
		tos R0 CMP,
		R0 tos LT MOV,
		END-CODE
[THEN]

: PICK		( x_u ... x1 x0 u -- x_u ... x1 x0 x_u )        \ CORE EXT
\		Remove u and copy the uth stack item to top of the stack. An
\		ambiguous condition exists if there are less than u+2 items
\		on the stack before PICK is executed.
		DEPTH DUP D# 2 < IF D# -4 THROW THEN    \ stack underflow
		D# 2 - OVER U< IF D# -4 THROW THEN
		1+ CELLS sp@ + @ ;

: POSTPONE	( "<spaces>name" -- )           \ CORE
\		Parse name and find it. Append compilation semantics of name
\		to current definition.
\ TODO -- this defn is recursive. Will it work if I flip in RECURSE? No;
\ because RECURSE will not run as an immediate word. Should code POSTPONE
\ like the version in hmeta_colon.
  (') 0< IF POSTPONE LITERAL
    POSTPONE COMPILE, EXIT THEN           \ non-IMMEDIATE
  COMPILE, ; COMPILE-ONLY IMMEDIATE       \ IMMEDIATE

: RECURSE	( -- )				\ CORE
\		Append the execution semactics of the current definition to
\		the current definition.
		bal 1- 2* PICK 1+ IF D# -22 THROW THEN
		        \ control structure mismatch; colon-sys type is -1
		bal 1- 2* 1+ PICK       \ xt of current definition
		COMPILE, ; COMPILE-ONLY IMMEDIATE

: REPEAT	( C: orig dest -- )		\ CORE
\		Terminate a BEGIN-WHILE-REPEAT indefinite loop. Resolve
\		backward reference dest and forward reference orig.
		POSTPONE AGAIN POSTPONE THEN ; COMPILE-ONLY IMMEDIATE

: SPACES	( n -- )			\ CORE
\		Send n spaces to the output device if n is greater than zero.
		DUP 0 > IF 0 DO SPACE LOOP EXIT THEN  DROP ;

: TO	\	Interpretation: ( x "<spaces>name" -- ) \ CORE EXT
\		Compilation:    ( "<spaces>name" -- )
\		Run-time:       ( x -- )
\		Store x in name.
  ' ?call DUP IF          \ should be call-doVALUE
    ['] doVALUE =         \ verify VALUE marker
    IF @ STATE @
      IF POSTPONE doTO code, EXIT THEN
      ! EXIT
    THEN
  THEN
  D# -32 THROW ; IMMEDIATE   \ invalid name argument (e.g. TO xxx)

: UNTIL		( C: dest -- )			\ CORE
\		Terminate a BEGIN-UNTIL indefinite loop structure.
		IF D# -22 THROW THEN  \ control structure mismatch; dest type is 0
		POSTPONE 0branch code, bal- ; COMPILE-ONLY IMMEDIATE

: VALUE		( x "<spaces>name" -- )         \ CORE EXT
\		name Execution: ( -- x )
\		Create a value object with initial value x.
		bal IF D# -29 THROW THEN	\ compiler nesting
		['] doVALUE xt, head,
		xhere DUP CELL+ TOxhere
		RAMB @ SWAP !
		, linkLast ; \ store x and link VALUE word to current wordlist

: VARIABLE	( "<spaces>name" -- )           \ CORE
\		name Execution: ( -- a-addr )
\		Parse a name and create a variable with the name.
\		Resolve one cell of data space at an aligned address.
\		Return the address on execution.
		bal IF D# -29 THROW THEN	\ compiler nesting
		['] doCONST xt, head,
		xhere DUP CELL+ TOxhere
		RAMB @ DUP CELL+ RAMB ! \ allocate one cell in RAM area
		SWAP ! linkLast ;

: WHILE		( C: dest -- orig dest )        \ CORE
\		Put the location of a new unresolved forward reference orig
\		onto the control flow stack under the existing dest. Typically
\		used in BEGIN ... WHILE ... REPEAT structure.
		POSTPONE IF 2SWAP ; COMPILE-ONLY IMMEDIATE

: WORD		( char "<chars>ccc<char>" -- c-addr )   \ CORE
\		Skip leading delimeters and parse a word. Return the address
\		of a transient region containing the word as counted string.
		skipPARSE xhere pack" DROP xhere ;

: [']		Compilation: ( "<spaces>name" -- )      \ CORE
\		Run-time: ( -- xt )
\		Parse name. Return the execution token of name on execution.
		' POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

: [CHAR]	Compilation: ( "<spaces>name" -- )      \ CORE
\		Run-time: ( -- char )
\		Parse name. Return the value of the first character of name
\		on execution.
		CHAR POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE


META-HI [IF]
: \		( "ccc<eol>" -- )		\ CORE EXT
\		Parse and discard the remainder of the parse area.
		SOURCE >IN ! DROP ; IMMEDIATE
[ELSE]
		CODE \
		GetVaxAdr sourceVar R0 =,
		[ R0 ] R1 LDR,
		GetVaxAdr >IN R3 =,
		[ R3 ] R1 STR,
		END-CODE IMMEDIATE
[THEN]


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 14: Target-specific definitions (?optional)
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ TODO word lists, express as CODE definitions



CODE W!	( c a -- )      "w store"
\		Store word (16-bit) value at word address.
		R1 popD,			\ pop data
						\ only interested in bits15:0
		[ tos ] R1 STRH,		\ store word at address
		tos popD,
		END-CODE

CODE W@	( a -- c )      "w fetch"
\		Fetch word value from word address.
		[ tos ] R1 LDRH,		\ get the word
		R1 tos MOV,			\ bung back on the stack
		END-CODE


CR .( *** StrongARM-specific definitions for coprocessor access..)

\   MMUPTAB	( -- a-addr )
\		Return the start address of the MMU page table
\
		MMU0 CONSTANT MMUPTAB

1000 hCONSTANT conm
FFFFEFFF hCONSTANT coffm

CODE CHIPID	( -- n )
\		Read chip ID
		tos pushD,
\ TODO		mrc     p15,0,tos,c0,c1,0
0 tCOMPILE,
		END-CODE

CODE ION	( -- )
\		Turn on Icache
		conm R1 =,		\ mask Icache ON
\ TODO		mrc     p15,0,r2,c1,c1,0
0 tCOMPILE,
		R1 R2 R2 ORR,		\ Icache ON
\ TODO		mcr     p15,0,r2,c1,c1,0
0 tCOMPILE,
		END-CODE

CODE IOFF	( -- )
\		Turn off Icache
		coffm R1 =,		\ mask Icache OFF
\ TODO		mrc     p15,0,r2,c1,c1,0
0 tCOMPILE,
		R1 R2 R2 AND,		\ Icache OFF
\ TODO		mcr     p15,0,r2,c1,c1,0
0 tCOMPILE,
		END-CODE

CODE IFLUSH	( -- )
\		Flush the Icache
\ TODO		mcr     p15,0,r2,c7,c5,0
0 tCOMPILE,
		END-CODE

CODE DFLUSH	( -- )
\		Flush the Dcache by forcing data out to memory
\		This algorithm comes from the SA-110 spec. Could optimise
\		it to half the loopcount by selecting an unused
\		address range and adding an MCR.
		0 # R0 MOV,
		8000 # R0 R1 ADD,	\ 32768 decimal entries
00 L.		20 # [ R0 ] R2 LDR,
		R0 R1 TEQ,
		00 L# NE B,
		END-CODE

CODE TLBFLUSH	( -- )
\		Flush the I and D TLBs
\ TODO		mcr     p15,0,r2,c8,c6,0
0 tCOMPILE,
		END-CODE

CODE MMUON	( -- )
\		Turn on the MMU, WB, Icache, Dcache
		100D R1 =,
\ TODO		mrc     p15,0,r2,c1,c1,0
0 tCOMPILE,
		R1 R2 R2 ORR,
\ TODO		mcr     p15,0,r2,c1,c1,0
0 tCOMPILE,
		END-CODE

CODE MMUOFF	( -- )
\		Turn off the MMU, WB, Icache, Dcache
		EFF2 R1 =,
\ TODO		mrc     p15,0,r2,c1,c1,0
0 tCOMPILE,
		R1 R2 R3 AND,
\ TODO		mcr     p15,0,r2,c1,c1,0
0 tCOMPILE,
		END-CODE

CODE WBDRAIN	( -- )
\		Drain the write buffer
\ TODO		mcr     p15,0,r2,c7,c10,4
0 tCOMPILE,
		END-CODE

CODE CKSWON	( -- )
\		clock switching ON
\ TODO		mcr     p15,0,r2,c15,c1,2
0 tCOMPILE,
		END-CODE

CODE CKSWOFF	( -- )
\		clock switching OFF
\ TODO		mcr     p15,0,r2,c15,c2,2
0 tCOMPILE,
		END-CODE

CODE WAI	( -- )
\		Wait for interrupt
\ TODO		mcr     p15,0,r2,c15,c8,2
0 tCOMPILE,
		END-CODE

CODE TTB!	( n -- )
\		Store n as the translation table base address
\ TODO		mcr     p15,0,tos,c2,c1,0
0 tCOMPILE,
		tos popD,
		END-CODE

CODE DAC@	( -- n )
\		Read the domain access control register
		tos pushD,
\ TODO		mrc     p15,0,tos,c3,c1,0
0 tCOMPILE,
		END-CODE

CODE DAC!	( n -- )
\		Store n in the domain access control register
\ TODO		mcr     p15,0,tos,c3,c1,0
0 tCOMPILE,
		tos popD,
		END-CODE

CODE IDflushline ( a-addr -- )
\		Ensure cache coherency after generating a new op-code
\		at a-addr. For machines with unified caches this is a DROP but
\		for machines with separate I and D caches you must flush the
\		Dcache (in case the code word is in a dirty block there)
\		and then flush the Icache (in case it has a copy of the old
\		value at that location).
\		Currently, the only word that uses this is xt,
\                       p15,??, arm_reg, co-proc_register, CRm, OPC_2
\ TODO		mcr     p15,0,tos,c7,c10,1      ;clean Dcache entry 
\ 		mcr     p15,0,tos,c7,c6,1       ;flush Dcache entry 
\		mcr     p15,0,tos,c7,c10,4      ;drain write buffer
\		mcr     p15,0,tos,c7,c5,0       ;flush Icache
0 tCOMPILE,
0 tCOMPILE,
0 tCOMPILE,
0 tCOMPILE,
		tos popD,
		END-CODE


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION 15: Image generation
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ TODO build images of initialisation of variables


CR .( *** RAM/ROM System Only)
S-DEF

: RESET-SYSTEM	( -- )
\		Reset the system. Restore initialization values of system
\		variables.

\ TODO the first defn would be more efficient but requires us to be able
\ to find the values of target VALUEs at meta-compile time.
\		sysVar00 sysVar0 [ sysVar0End sysVar0 - ]  LITERAL MOVE COLD ;
		sysVar00 sysVar0 sysVar0End sysVar0 - MOVE COLD ;


\ TODO: the problem with the current initialisation scheme is that it
\ does not allow additional files to be submitted to the meta-compiler;
\ since they would not be able to adjust CTOP, NTOP etc. The long-term
\ solution to this is to implement target variables as aliases on the
\ host, in a region of data space that is then "grabbed" to form the
\ initialisation table. "SAVE-SYSTEM" in optional.f "knows" how to do
\ this.

\ Set values for cold-start table.
\ ... first three are defined as VALUEs, last three as VARIABLEs
_CODE hCONSTANT CTOP		\ next available memory in code dictionary
_NAME hCONSTANT NTOP		\ next available memory in name dictionary
_VAR hCONSTANT  VTOP		\ next available memory in variable area
_ENVLINK @ hCONSTANT LASTENV
_SLINK @ hCONSTANT LASTSYSTEM	\ last SYSTEM word name address
_FLINK @ hCONSTANT LASTFORTH	\ last FORTH word name address


CR .( *** Build initialisation tables for system variables/values)

\ Use $INIT to build tables of initialisation values for system variables.
\ COLD and RESET-SYSTEM restore variables from these tables.
\ The space for the tables has already been reserved.
\ ENTRIES/OFFSETS MUST BE IN SAME ORDER AS SYSTEM VARIABLES.

HEX
ALSO its-words \ find XTs for these words in the target wordlist on the host
' RX?			4 $INIT	\ 'ekey?
' RX@			4 $INIT	\ 'ekey
' TX?			4 $INIT	\ 'emit?
' TX!			4 $INIT	\ 'emit
' set-i/o		4 $INIT	\ 'init-i/o
' .ok			4 $INIT	\ 'prompt
' hi			4 $INIT	\ 'boot
InitIOBYTES		4 $INIT	\ IOBYTES
0			4 $INIT	\ SOURCE-ID
10			4 $INIT	\ BASE
GetVaxAdr ROMB		4 $INIT	\ cpVar
GetVaxAdr ROMT		4 $INIT	\ cpVar
GetVaxAdr RAMB		4 $INIT	\ hereVar points RAM space
				\ execution vectors for 'doWord
' optiCOMPILE,		4 $INIT	\ nonimmediate word - compilation
' EXECUTE		4 $INIT	\ nonimmediate word - interpretation
' doubleAlso,		4 $INIT	\ not found word - compilation
' doubleAlso		4 $INIT	\ not found word - interpretation
' EXECUTE		4 $INIT	\ immediate word - compilation
' EXECUTE		4 $INIT	\ immediate word - interpretation
CTOP			4 $INIT	\ ROMB
NTOP			4 $INIT	\ ROMT
VTOP			4 $INIT	\ RAMB
RAMT0			4 $INIT	\ RAMT
0			4 $INIT	\ bal
0			4 $INIT	\ notNONAME?
0			4 $INIT	\ rakevar
\ search order stack - number of orders, followed by the stack and space for
\ a defined number of further entries
2			4 $INIT	\ #order
GetVaxAdr FORTH-WORDLIST	4 $INIT
GetVaxAdr NONSTANDARD-WORDLIST 4 OrderDepth 2 - CELLL * + $INIT

GetVaxAdr FORTH-WORDLIST 	4 $INIT	\ current pointer
LASTFORTH		4 $INIT	\ FORTH-WORDLIST
GetVaxAdr NONSTANDARD-WORDLIST 4 $INIT \ wordlist link
GetNameAdr FORTH-WORDLIST	4 $INIT \ name of the WORDLIST
LASTSYSTEM		4 $INIT \ NONSTANDARD-WORDLIST
0			4 $INIT \ wordlist link
GetNameAdr NONSTANDARD-WORDLIST \ name of the WORDLIST
	  4 MaxWLISTS 2 - 3 * CELLL * + $INIT \ room for further wordlists

LASTENV			4 $INIT	\ envQList
SysUserP		4 $INIT	\ user pointer
SysUserP		4 $INIT	\ system tesk's tid
0			4 $INIT	\ user1
GetNameAdr SystemTask	4 $INIT	\ taskName
0			4 $INIT	\ throwFrame
0			4 $INIT	\ stackTop
' wake			4 $INIT	\ status
SysStatus		4 $INIT	\ follower
SPP			4 $INIT	\ system task's sp0
RPP			4 $INIT	\ system task's rp0

PREVIOUS


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


\ This set of variables is used in the assembler source to track where
\ to emit stuff. The final values are not exposed in the target Forth.
\ Addresses are in target space.
\ The LINK addresses have to be variables rather than values since
\ accesses are vectored.
0	VALUE	 _THROW \ current throw string
RAM0	VALUE	 _VAR   \ initial data space pointer
\ ROM0	VALUE	 _CODE  \ initial code space pointer
ROMEnd	VALUE	 _NAME  \ initial name space pointer.. decrement before use
	VARIABLE _SLINK		0 _SLINK ! \ force a null link
	VARIABLE _FLINK		0 _FLINK ! \ force a null link
	VARIABLE _ENVLINK	0 _ENVLINK ! \ force a null link


\ Ready to start definitions for the system. Until now, all of the facilities
\ of the host Forth compiler have been available. After we load the
\ meta-compiler, some of the facilities of the host Forth compiler become
\ hidden; replace by facilites of the traget compiler. Specifically, these
\ words all refer to the target system:
\ : ; ] COMPILE-ONLY IMMEDIATE 

DEPTH S" hmeta_colon.fth" INCLUDED CR DEPTH 1- -
[IF] .( hmeta_colon affected DEPTH) ABORT [THEN] CR .( ..hmeta_colon loaded OK)

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
	DUP 1- TO _NAME \ TODO should that be CHAR- ?
	SWAP tCMOVE \ move string to name space
	R> _NAME tC! \ store length 
	_NAME CONSTANT ;

\ (had to move this up to avoid foward reference in $THROWMSG')
_NAME CONSTANT AddrTHROWMsgTbl

\ Add a THROW message in name space. THROW messages won't be
\ needed if target system does not need names of Forth words.
: $THROWMSG' \ $THROWMSG' string'
	[CHAR] ' PARSE \ addr len
	\ hForth originally used aligned strings but I pack them on any
	\ boundary because ARM allows it and it saves space
	DUP >R _NAME OVER - \ save length and calculate destination addr
	DUP 1- TO _NAME \ TODO should that be CHAR-
	SWAP tCMOVE \ move string to name space
	R> _NAME tC! \ store length 
	_THROW CELLL + TO _THROW
	_NAME AddrTHROWMsgTbl _THROW - t! ;

\ TODO add similar variable for code defns in ASMARM
VARIABLE META-DEPTH


\ TODO as well as META-DEPTH, should have a flag that is set by CODE and
\ cleared by END-CODE-FLOW etc. that way, CODE can tell whether the previous defn
\ was terminated correctly, to avoid those nasty wordlist errors that occur
\ due to this mistake.

\ General-purpose routine for producing a dictionary entry. n is the address
\ of the variable used as the link address.
\ CODE leaves the assembler in the search path. For real code words,
\ the search order will be restored by an eventual END-CODE. For other
\ words, the search order must be restored before the end of the word.
\ The dstack depth is saved at the start; it can be checked at the end
\ if desired.
\ Words to set flags in the dictionary (eg immediate, compile-only bits)
\ can be used at any time before a new definition is started, but a
\ consistent approach is recommended.

: CODE ( -- )
	tGET-CURRENT >R
	DEPTH META-DEPTH ! \ should finish with 1 fewer than we start
	_CODE $ALIGN TO _CODE
	TDEF PARSE-WORD \ addr len
	DUP 1+ $ALIGN \ name + length takes an integral number of CELLs
	CELLL 2* + \ total size of dictionary entry
	_NAME SWAP - TO _NAME
	_CODE _NAME t!
	R@ @ _NAME CELLL + t!
	_NAME CELLL 2* + DUP R> ! \ from len to
	\ TODO -- CHAR+ may not be portable for some host/target pairs
	2DUP tC! CHAR+ SWAP tCMOVE \ store length then string
	ALSO ASSEMBLER ; \ leave assembler in the search order

PREVIOUS \ remove its-words


\ End a code definition. Use END-CODE-EARLY if there are multiple exit points,
\ or END-CODE-FLOW if the code doesn't need the fpc updating for some reason
ALSO ASSEMBLER

: END-CODE-FLOW
	LOCAL-RESOLVED PREVIOUS DEPTH META-DEPTH @ <> IF
		ABORT" END-CODE terminated definition with unbalanced stack"
	THEN ;

MICRO-DEBUG [IF]
: END-CODE
	00 G# B, END-CODE-FLOW ; \ udebug
: END-CODE-EARLY
	00 G# B, ; \ udebug
[ELSE]
: END-CODE
	CELLL # [ fpc ] PC LDR,	END-CODE-FLOW ;
: END-CODE-EARLY
	CELLL # [ fpc ] PC LDR, ;
[THEN]


\ $VALUE string - compile a VALUE header in the current wordlist
: $VALUE
	CODE PREVIOUS
	doVALUE BL, \ source and dest are target space addresses.
	_VAR tCOMPILE,
	_VAR CELLL + TO _VAR ;

\ TODO this (and $VAR, $VAL) should really be expressed using the target
\ versions of CONSTANT, VARIABLE, VALUE.
\ <value> $CONST string - compile a CONSTANT header in the current wordlist
: $CONST
	CODE PREVIOUS
	doCONST BL, tCOMPILE, ; \ emit the value of the constant (at tos) inline

\ $VAR string - compile a VARIABLE header in the current wordlist
\ (uses doCONST just as CONST does)
: $VAR
	CODE PREVIOUS
	doCONST BL,
	_VAR tCOMPILE, \ emit the address of the variable inline
	_VAR CELLL + TO _VAR ;


\ <offset> $USER string <value> - compile a USER header in the current w/list
: $USER
	CODE PREVIOUS
	doUSER BL,
	tCOMPILE, ; \ emit the value of the offset (at tos) inline

PREVIOUS \ take ASSEMBLER off search order

\ $ENVSTR" name" <constant-name>
\ Used for building a named string in name space for environment queries
\ TODO this routine does not align the string to any boundary. Any
\ subsequent name will align so will definitely be OK. ARM can handle
\ unaligned strings but some other processors (68K) cannot, so should
\ add an option to align by decrementing _NAME by a multiple of the cell
\ size and leaving space at the end of the string.
: $ENVSTR"
	[CHAR] " PARSE
	_NAME OVER - 1- \ leave room for counted string
	DUP CONSTANT \ create a constant that points to the named string
	DUP TO _NAME
	\ TODO -- CHAR+ may not be portable for some host/target pairs
	2DUP tC! CHAR+ SWAP tCMOVE ; \ store length then string


\ <value> <post-increment> $INIT
\ adds an entry to the variable/value intialisation tables by setting
\ memory at the current offset into the UZERO table and the same offset into
\ the UZERO0 table to the value <value>. After the store, the offset is
\ updated by the value <post-increment>.
0 hVALUE INIT_OS
0 hVALUE UZERO
0 hVALUE UZERO0
1B4 hCONSTANT INIT_SIZE
: $INIT
	INIT_OS SWAP OVER + TO INIT_OS DUP INIT_SIZE > IF
		ABORT" Initialise more data than allowed by INIT_SIZE"
	THEN
	2DUP UZERO + t! UZERO0 + t!
;








CR .( *** Load immediate definitions for target to run on host )
\ TODO put the h: stuff into hmeta_imm.fth
DEPTH S" hmeta_imm.fth" INCLUDED CR DEPTH 1- -
[IF] .( hmeta_imm affected DEPTH) ABORT [THEN] CR .( ..hmeta_imm loaded OK)

CR .( *** Make : invoke t: and ; invoke t; )
\ compilation wordlist is FORTH

ALSO it-words \ find these in it-words and put them into FORTH
: h: : ;
: h; hPOSTPONE ; ; hIMMEDIATE
: : t: ;

ALSO it-words DEFINITIONS \ put this into it-words
h: ; nit-; h; hIMMEDIATE

PREVIOUS PREVIOUS DEFINITIONS \ compilation wordlist back to FORTH


CR .( Check search order -> ) ORDER
CR .( *** Ready for colon defns..)


\ doesn't really matter what the compilation wordlist is at this point
\ As long as these words are the the search order:
\ : IMMEDIATE COMPILE-ONLY ]  
\ the target definitions can (and do) change the compilation and search
\ wordlists as they see fit.



CR .( *** Non-Standard words - Processor-dependent definitions)
CR .( ***        32-bit Forth for ARM RISC)
S-DEF

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
\ All definitions end with END-CODE -- either directly (code definitions) or
\ indirectly (colon definitions terminating in EXIT, which is itself a code
\ definition). The action of END-CODE is to use the fpc for the next word to
\ fetch the xt and jumps to it.
\
\ To use the udebug routine, replace the END-CODE expansion with a jump (not a
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

ALSO ASSEMBLER
\ note that the udebug trap cannot be in the table of initialised variables
\ because we want to use the debugger *before* that table is initialised.
00 G. \ udebug
		GetVaxAdr trapfpc R0 =,
		[ R0 ] R1 LDR,
		fpc R1 CMP,		\ compare the stored address with
					\ the address we're about to get the
					\ next xt from
		CELLL # [ fpc ] pc NE LDR, \ <> trap address, so we're done
		CELLL # FPC R1 ADD,	\ next time trap on the next xt
		[ R0 ] R1 STR,
		CELLL # [ fpc ] pc LDR,	\ make debugger TRAP at this address
PREVIOUS

