\
\ OPTIONAL.F
\ Optional wordset words for 8086 hForth
\
\ by Wonyong Koh
\
\ 1997. 2. 28.
\	Facelift to be used with other CPUs.
\ 1996. 12. 6.
\	Fix 'compiles>' for colon-sys.
\ 1996. 11. 29.
\	Remove PICK which was added in assembly source.
\	Revise CASE, ENDCASE, OF, ENDOF, RETRY for control-flow stack.
\	Revise '.xt' due to the removal of 'do."' and change of 'doS"'.
\ 1995. 12. 26.
\	Revise xt>name.
\ 1995. 11. 25.
\	Add RETRY described by Dr. Astle
\		in Forth Dimensions 17(4), 19-21 (1995).
\ 1995. 11. 7
\	Fix ?DO.
\ 1995. 10. 30.
\	Check validity of xt in 'xt>name'. '-1 @' generates exception.
\ 1995. 10. 17.
\	Replace < with U< in the definition of MARKER for RAM and EXE
\		models. U< should be used to compare addresses.
\	Fix < to U< in the definition of xtSEE
\ 1995. 10. 9.
\	Rename WORDLIST-NAME which more consistant along VARIABLE, CONSTANT
\		than NAME-WORDLIST
\ 1995. 7. 21.
\	Make HERE VALUE type and remove 'hereP'. Revise 'xhere'
\		and remove 'TOxhere'.
\	Make SOURCE-ID VALUE type, replace TOsource-id with
\		"TO SOURCE-ID" and remove TOsource-id .
\ 1995. 6. 11.
\	Fix 'enough?'.
\ 1995. 6. 3.
\	Fix 'xtSEE' for RAM and EXE model.
\
\ Dictionary structures of hForth ROM, RAM and EXE models are all
\ different.
\  o  WORDLIST allocate empty wordlist dynamically in RAM and EXE model. Thus,
\     there is no limit on maximum number of wordlist for RAM and EXE model.
\     Maximum number os wordlists are limited to 10 in ROM model.
\  o  -1 SET-ORDER is hard coded to put NONSTANDARD-WORDLIST and
\     FORTH-WORDLIST into the search order stack for RAM model.
\  o  MARKER is revised for combined code and name space for RAM model.
\  o  'xt>name' are different
\  o  PAD, xtSEE, xDUMP are different.

BASE @
DECIMAL

\ **********************
\ Optional String wordset
\ **********************

\   COMPARE	( c-addr1 u1 c-addr2 u2 -- -1|0|1 )	\ STRING
\		Compare the two strings. Return 0 if two strings are identical;
\		-1 if ca1 u1 is smaller than ca2 u2; 1 otherwise.
: COMPARE
    ROT 2DUP SWAP - >R		\ ca1 ca2 u2 u1  R: u1-u2
    MIN same? ?DUP
    IF R> DROP EXIT THEN
    R> DUP IF 0< 2* 1+ THEN ;

\ **********************
\ Optional Prgramming-Tools wordset
\ **********************

\   [ELSE]	( *<spaces>name...* - ) 	\ TOOLS EXT
\		Skipping leading spaces, parse and discard words from the
\		parse area, including nested [IF] ... [THEN] and [IF] ...
\		[ELSE] ... [THEN], until the word [THEN] has been parsed
\		and discared.
: [ELSE]  ( -- )
   1 BEGIN					\ level
     BEGIN  PARSE-WORD	DUP  WHILE		\ level c-addr len
       2DUP  S" [IF]"  COMPARE 0= IF            \ level c-addr len
	 2DROP 1+				\ level'
       ELSE					\ level c-addr len
	 2DUP  S" [ELSE]"  COMPARE 0= IF        \ level c-addr len
	   2DROP 1- DUP IF 1+ THEN		\ level'
	 ELSE					\ level c-addr len
	   S" [THEN]"  COMPARE 0= IF            \ level
	     1- 				\ level'
	   THEN
	 THEN
       THEN ?DUP 0=  IF EXIT THEN		\ level'
     REPEAT  2DROP				\ level
   REFILL 0= UNTIL				\ level
   DROP ;  IMMEDIATE

\   [IF]	( flag | flag *<spaces>name...* -- )	\ TOOLS EXT
\		If flag is true, do nothing. Otherwise, Skipping leading
\		spaces, parse and discard words from the parse area,
\		including nested [IF] ... [THEN] and [IF] ... [ELSE] ...
\		[THEN], until either the word [ELSE] or [THEN] has been
\		parsed and discared.
: [IF]	( flag -- )				\ TOOLS EXT
   0= IF POSTPONE [ELSE] THEN ;  IMMEDIATE

\   [THEN]	( -- )
\		Do nothing.
: [THEN]  ( -- )  ;  IMMEDIATE

\ **********************
\ Optional Search-Order wordset -- complete
\ **********************

\   SET-CURRENT   ( wid -- )			\ SEARCH
\		Set the compilation wordlist to the wordlist identified by wid.
: SET-CURRENT	current ! ;

\   DEFINITIONS   ( -- )			\ SEARCH
\		Make the compilation wordlist the same as the first wordlist
\		in the search order.
: DEFINITIONS	#order CELL+ @ SET-CURRENT ;

\   GET-ORDER	( -- widn .. wid1 n )		\ SEARCH
\		Return the number of wordlist in the search order and the
\		wordlist identifiers widn ... wid1 .
: GET-ORDER
    #order @ DUP
    IF 1- 0 SWAP DO I CELLS #order CELL+ + @ -1 +LOOP
       #order @
    THEN ;

\   SET-ORDER	( widn .. wid1 n -- )		\ SEARCH
\		Set the search order to the wordlist identified by widn ...
\		wid1. Later wordlist wid1 will be searched first, with wordlist
\		widn searched last. If n is 0, empty the search order. If n
\		is -1, set the search order to the implementation-defined
\		minimum search order.

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : SET-ORDER
      DUP -1 =
      IF  DROP [ #order var0 - sysVar0 + ] LITERAL  #order
	  [ BL PARSE WORDLISTS ENVIRONMENT? DROP 1+ ] LITERAL CELLS
	  MOVE EXIT						     THEN
      DUP [ BL PARSE WORDLISTS ENVIRONMENT? DROP ] LITERAL >
	  IF -49 THROW THEN
      DUP #order !
      ?DUP IF 0 DO I CELLS #order CELL+ + ! LOOP THEN ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0= OR
[IF]
  : SET-ORDER
      DUP -1 =
      IF DROP			\ restore default # of search order
	 #order0 DUP @ #order SWAP 1+ CELLS MOVE EXIT THEN
      DUP [ BL PARSE WORDLISTS ENVIRONMENT? DROP ] LITERAL >
	  IF -49 THROW THEN
      DUP #order !
      ?DUP IF 0 DO #order I CELLS + CELL+ ! LOOP THEN ;
[THEN]

\   WORDLIST	( -- wid )			\ SEARCH
\		Create a new empty wordlist and return its identifier wid.
\		The new wordlist is returned from a preallocated pool for
\		RAM/ROM system in this implementation since they need to be
\		initialized after SAVE-SYSTEM. It may be dynamically allocated
\		in RAM only system.
\
\		structure of a wordlist
\		//lastWord/next_wordlist/wordlist_name//

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : WORDLIST
      FORTH-WORDLIST			\ the first wordlist
      BEGIN CELL+ DUP @ WHILE @ REPEAT
      DUP CELL+ CELL+ DUP @ IF		\ pre-allocated wordlist is available?
	  -49 THROW THEN		\ search-order overflow
      DUP ROT ! ;			\ attach a wordlist to wordlist link
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0= OR
[IF]
  : WORDLIST
      FORTH-WORDLIST			\ the first wordlist
      BEGIN CELL+ DUP @ WHILE @ REPEAT	\ find end of wordlist link
      HERE SWAP !			\ attach a wordlist to wordlist link
      HERE 0 ,				\ no word in this wordlist yet
      0 ,				\ this is end of wordlist link
      0 , ;			\ no name is assigned to this wordlist yet.
[THEN]

\   ALSO	( -- )				\ SEARCH EXT
\		Transform the search order widn ... wid2, wid1 into widn ...
\		wid2, wid1, wid1.
: ALSO	 GET-ORDER OVER SWAP 1+ SET-ORDER ;

\   FORTH	( -- )
\		Transform the search order widn ... wid2, wid1 into widn ...
\		wid2, wid_FORTH-WORDLIST.
: FORTH   GET-ORDER NIP FORTH-WORDLIST SWAP SET-ORDER ;

\   ONLY	( -- )
\		Set the search order to the implementation-defined minimum
\		search order.
: ONLY	 -1 SET-ORDER ;

\   PREVIOUS	( -- )
\		Transform the search order widn ... wid2, wid1 into widn ...
\		wid2.
: PREVIOUS   GET-ORDER NIP 1- SET-ORDER ;

NONSTANDARD-WORDLIST SET-CURRENT

\   .name	( c-addr -- )
\		Display name of a word.
: .name   COUNT 31 AND TYPE SPACE ;

\   WORDLIST-NAME   ( wid -- )
\		Name a wordlist. Used to attach a name to a new wordlist
\		returned by WORDLIST to be displayed by ORDER.
: WORDLIST-NAME   DUP CONSTANT	lastName SWAP CELL+ CELL+ ! ;

\   .wordlist	( c-addr -- )
\		Display name of a wordlist.
: .wordlist
    8 SPACES DUP CELL+ CELL+ @ ?DUP
    IF .name DROP CR EXIT THEN . CR ;

FORTH-WORDLIST SET-CURRENT

\   ORDER	( -- )				\ SEARCH EXT
\		Display the wordlists in the search order from the first
\		to the last. Also display the wordlist into which new
\		definitions will be placed.
: ORDER
    CR ." Search-Order:" CR
    GET-ORDER 0 DO .wordlist LOOP
    ." Current:" CR
    GET-CURRENT .wordlist ;

envQList SET-CURRENT
-1 CONSTANT SEARCH-ORDER
-1 CONSTANT SEARCH-ORDER-EXT
FORTH-WORDLIST SET-CURRENT

\ **********************
\ Optional Core Extention wordset
\ **********************

NONSTANDARD-WORDLIST SET-CURRENT
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
    : xhere   HERE ;
    : code,   , ;
[THEN]

FORTH-WORDLIST SET-CURRENT

\   .(		( "ccc<)>" -- )                 \ CORE EXT
\		Output following string up to next ) .
: .(   [CHAR] ) PARSE TYPE ; IMMEDIATE

\   D.R 	( d n -- )			\ DOUBLE
\		Display d right-justified in field of width n.
: D.R	>R (d.) R> OVER - 0 MAX SPACES	TYPE ;

\   .R		( n1 n2 -- )			\ CORE EXT
\		Display n right-justified in field of width n2.
: .R   >R S>D R> D.R ;

\   FALSE	( -- false )			\ CORE EXT
\		Return a false flag.
0 CONSTANT FALSE

\   HEX 	( -- )				\ CORE EXT
\		Set contents of BASE to sixteen.
: HEX	16 BASE ! ;

\   U>		( u1 u2 -- flag )		\ CORE	EXT
\		flag is true if and only if u1 is greater than u2.
: U>   SWAP U< ;

\   MARKER	( "<spaces>name" -- )           \ CORE EXT
\		Create a definition with name. The new definition will
\		restore on execution all dictionary allocations and search
\		order pointers to the state they had just prior to the
\		definition of name.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : MARKER
      ROMB @ ROMT @ RAMB @ RAMT @
      CREATE , , , , GET-CURRENT ,
	     FORTH-WORDLIST			  \ start of wordlist link
	     BEGIN CELL+ DUP @ WHILE @ REPEAT	  \ find end of wordlist link
	     , GET-ORDER DUP , 0 DO , LOOP
      DOES>	  DUP @ RAMT ! CELL+ DUP @ RAMB !
	    CELL+ DUP @ ROMT ! CELL+ DUP @ ROMB !
	    CELL+ DUP @ SET-CURRENT
	    CELL+ DUP @ 0 SWAP !		  \ restore end of wordlist link
	    CELL+ DUP @ DUP >R CELLS + R@ 0 DO DUP @ SWAP cell- LOOP
		       DROP R> SET-ORDER	  \ restore search order
	    FORTH-WORDLIST    \ start of wordlist link
	    BEGIN DUP @       \ last word name field of wordlist
		  BEGIN  DUP npVar @ @ U<
		  WHILE  cell- @
		  REPEAT OVER !       \ restore search order pointer
		  CELL+ @ ?DUP 0=     \ repeat to next wordlist
	    UNTIL ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : MARKER
      HERE
      CREATE , GET-CURRENT ,
	     FORTH-WORDLIST			  \ start of wordlist link
	     BEGIN CELL+ DUP @ WHILE @ REPEAT	  \ find end of wordlist link
	     , GET-ORDER DUP , 0 DO , LOOP
      DOES>	  DUP @ TO HERE
	    CELL+ DUP @ SET-CURRENT
	    CELL+ DUP @ 0 SWAP !		  \ restore end of wordlist link
	    CELL+ DUP @ DUP >R CELLS + R@ 0 DO DUP @ SWAP cell- LOOP
		       DROP R> SET-ORDER	  \ restore search order
	    FORTH-WORDLIST    \ start of wordlist link
	    BEGIN DUP @       \ last word name field of wordlist
		  BEGIN  DUP HERE U>
		  WHILE  cell- @
		  REPEAT OVER !       \ restore search order pointer
		  CELL+ @ ?DUP 0=     \ repeat to next wordlist
	    UNTIL ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : MARKER
      HERE xhere
      CREATE , , GET-CURRENT ,
	     FORTH-WORDLIST			  \ start of wordlist link
	     BEGIN CELL+ DUP @ WHILE @ REPEAT	  \ find end of wordlist link
	     , GET-ORDER DUP , 0 DO , LOOP
      DOES>	  DUP @ TO xhere  CELL+ DUP @ TO HERE
	    CELL+ DUP @ SET-CURRENT
	    CELL+ DUP @ 0 SWAP !		  \ restore end of wordlist link
	    CELL+ DUP @ DUP >R CELLS + R@ 0 DO DUP @ SWAP cell- LOOP
		       DROP R> SET-ORDER	  \ restore search order
	    FORTH-WORDLIST    \ start of wordlist link
	    BEGIN DUP @       \ last word name field of wordlist
		  BEGIN  DUP HERE U>
		  WHILE  cell- @
		  REPEAT OVER !       \ restore search order pointer
		  CELL+ @ ?DUP 0=     \ repeat to next wordlist
	    UNTIL ;
[THEN]

\   PAD 	( -- a-addr )			\ CORE EXT
\		Return the address of a temporary buffer. See REFILL
\		|PAD|TIB|RAMTop
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : PAD   npVar @ [ BL PARSE /PAD ENVIRONMENT? DROP CHARS 3 * NEGATE ] LITERAL + ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : PAD   memTop [ BL PARSE /PAD ENVIRONMENT? DROP CHARS 2* NEGATE ] LITERAL + ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : PAD   memTop [ BL PARSE /PAD ENVIRONMENT? DROP CHARS 2* NEGATE ] LITERAL + ;
[THEN]

\   TRUE	( -- true )			\ CORE EXT
\		Return a true flag.
-1 CONSTANT TRUE

\   U.R 	( u n -- )			\ CORE EXT
\		Display u right-justified in field of width n.
: U.R	0 SWAP D.R ;

HEX

\   CASE	( C: -- case-sys )		\ CORE EXT
\		Run-time: ( -- )
\		Mark the start of CASE ... OF ... ENDOF ... ENDCASE structure.
\		On run-time, continue execution.
: CASE ( C: -- case-mark )
    0 3 	\ case type is 3
    bal+ ; COMPILE-ONLY IMMEDIATE

\   ENDCASE	( C: case-sys -- )		\ CORE EXT
\		Run-time: ( x -- )
\		Mark the end of CASE ... OF ... ENDOF ... ENDCASE structure.
\		On run-time, discard the case selector x and continue execution.
: ENDCASE ( C: case-mark of-orig ... of-orig -- )
    POSTPONE DROP
    BEGIN  DUP 2 =	\ of-orig type is 2
    WHILE  1- POSTPONE THEN
    REPEAT
    3 - IF -22 THROW THEN	\ control structure mismatch
    DROP bal- ; COMPILE-ONLY IMMEDIATE

\   OF		( C: -- of-sys )		\ CORE EXT
\		Run-time: ( x1 x2 -- |x1 )
\		Mark the start of OF ... ENDOF part of CASE structure.
\		On run-time if two values on the stack are not equal, discard
\		the top value and continue execution following the next ENDOF.
\		Otherwise discard both values and continue execution in line.
: OF ( C: -- of-orig )
    POSTPONE OVER  POSTPONE =  POSTPONE IF  POSTPONE DROP
    1+		\ change orig type 1 to of-sys type 2
    ; COMPILE-ONLY IMMEDIATE

\   ENDOF	( C: case-sys1 of-sys -- case-sys2 )	\ CORE EXT
\		Run-time: ( -- )
\		Mark the end of OF ... ENDOF part of CASE structre.
\		On run-time, continue execution following ENDCASE .
: ENDOF ( C: of-orig1 -- of-orig2 )
    1-	POSTPONE ELSE  1+	\ of-orig type is 2; orig type is 1
    ; COMPILE-ONLY IMMEDIATE

\   UNUSED	( -- u )				\ CORE EXT
\		Return available data space in address units.
: UNUSED    PAD HERE - ;    \ Available data space is HERE to PAD

\ **********************
\ Optional Prgramming-Tools wordset -- complete
\ **********************

DECIMAL

\   .S		( -- )				\ TOOLS
\		Display the values on the data stack.
: .S   CR DEPTH ?DUP
       IF   1- 0 SWAP	       \ 0 depth-1
	    DO	I PICK
		BASE @ 10 = IF . ELSE U. THEN
	    -1 +LOOP
       THEN ." <sp " ;

\   ?		( a-addr -- )			\ TOOLS
\		Display the contents at a-addr.
\
: ?    @ BASE @ 10 = IF . EXIT THEN U. ;

NONSTANDARD-WORDLIST SET-CURRENT

\   enough?	( -- flag )
\		Return false if no input, else pause and if CR return true.
: enough?   EKEY? DUP IF EKEY 2DROP EKEY 13 ( CR) = THEN ;

FORTH-WORDLIST SET-CURRENT

\   DUMP	( addr u -- )			\ TOOLS
\		Display the contents of u consecutive address units starting
\		at addr.
\
: DUMP	?DUP
	IF   BASE @ >R HEX
	     1- 16 / 1+
	     0 DO CR DUP DUP 0 <# # # # # #> TYPE SPACE SPACE
		  16 0 DO DUP C@ 0 <# # # #> TYPE SPACE CHAR+ LOOP
		  SPACE SWAP
		  16 0 DO   DUP C@ 127 AND DUP 0 BL WITHIN
			    OVER 127 = OR
			    IF DROP [CHAR] . THEN
			    EMIT CHAR+
		       LOOP DROP
		  enough? IF LEAVE THEN
	     LOOP
	     R> BASE !
	THEN DROP ;

NONSTANDARD-WORDLIST SET-CURRENT

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  \   xDUMP	  ( code-addr u -- )
  \		  Display the contents of u consecutive address units
  \		  starting at the code addr.
  \
  : xDUMP ?DUP
	  IF   BASE @ >R HEX
	       1- 16 / 1+
	       0 DO CR DUP 0 <# # # # # #> TYPE SPACE SPACE
		    8 0 DO DUP code@ 0 <# # # # # #> TYPE SPACE  CELL+ LOOP
		    enough? IF LEAVE THEN
	       LOOP
	       R> BASE !
	  THEN DROP ;
[THEN]

\   xt>name	( xt -- c-addr | 0 )
\		Remove xt from the stack and return the name address if xt
\		is execution token of valid word; otherwise return 0.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : xt>name
      DUP ALIGNED OVER XOR IF DROP 0 EXIT THEN	\ xt should be aligned
      >R			\ save xt
      FORTH-WORDLIST		\ Start of wordlist link
      BEGIN DUP @		\ last word name field of wordlist
	    BEGIN DUP name>xt R@ XOR
	    WHILE cell- @ ?DUP 0=
	    UNTIL		\ continue until the end of wordlist
	    ELSE  SWAP R> 2DROP EXIT	\ found
	    THEN  CELL+ @ ?DUP 0=
      UNTIL			\ continue to next wordlist
      R> DROP 0 ;		\ not found
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : xt>name
      DUP ALIGNED OVER XOR IF DROP 0 EXIT THEN	\ xt should be aligned
      DUP cell- @		\ xt c-addr
      DUP ALIGNED OVER XOR IF 2DROP 0 EXIT THEN
      SWAP OVER name>xt = AND ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : xt>name
      DUP ALIGNED OVER XOR IF DROP 0 EXIT THEN	\ xt should be aligned
      DUP cell- code@	\ xt c-addr
      DUP ALIGNED OVER XOR IF 2DROP 0 EXIT THEN
      SWAP OVER name>xt = AND ;
[THEN]

\   .xt 	( a-addr1 xt -- a-addr2 )
\		Display name of a xt if xt is valid and display string
\		constant and adjust a-addr1 to the end of string if xt is
\		not POSTPONEd 'doS"' ; otherwise display the xt as a number.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : .xt
      DUP
      IF DUP xt>name ?DUP
	  IF .name DUP >R ['] doS" =
	     IF DUP cell- @ ['] doLIT XOR
		IF DUP CELL+ SWAP cell- @ 2DUP TYPE + ALIGNED
		   cell- [CHAR] " EMIT SPACE       THEN THEN
	     R> DUP  ['] branch = OVER ['] 0branch = OR
		OVER ['] doLOOP = OR SWAP ['] do+LOOP = OR
	     IF DUP cell- @ ['] doLIT XOR
		IF CELL+ DUP @ OVER CELL+ - .	   THEN THEN
	     EXIT
	  THEN
      THEN U. ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : .xt
      DUP
      IF DUP xt>name ?DUP
	  IF .name
	     DUP  ['] branch = OVER ['] 0branch = OR
	     OVER ['] doLOOP = OR SWAP ['] do+LOOP = OR
	     IF DUP cell- code@ ['] doLIT XOR
		IF CELL+ DUP code@ OVER CELL+ - .  THEN THEN
	     EXIT
	  THEN
      THEN U. ;
[THEN]

\   xtSEE	( xt -- )
\		Display human-readable representation of xt.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : xtSEE   >R lastName
	    BEGIN  DUP COUNT 31 AND + ALIGNED CELL+ CELL+ \ na na'
		   DUP name>xt R@ U>
	    WHILE  NIP
	    REPEAT DROP name>xt R>
	    2DUP U> 0= IF NIP xhere SWAP THEN	\ end-of-code xt
	    CR BASE @ >R HEX
	    BEGIN ?call ?DUP
		  IF ." call-" .xt THEN
		  DUP @ .xt enough? 0=
	    WHILE CELL+ 2DUP U> 0=
	    UNTIL THEN 2DROP  R> BASE ! ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  \ Following definition is less dependent on dictionary structure although
  \ slower. It only assumes xt of a word is larger than xt of previously
  \ defined words. This works for ROM model also.
  : xtSEE   >R xhere >R 	  \ Search all wordlist to find end of xt.
	    FORTH-WORDLIST	  \ Find smallest link pointer larger than xt.
	    BEGIN DUP
		  BEGIN @ ?DUP
		  WHILE DUP name>xt R@ U<
			IF R> OVER name>xt R@ U>
			   IF DROP DUP name>xt THEN
			   >R
			THEN
			cell-
		  REPEAT
		  CELL+ @ ?DUP 0=		\ continue to next wordlist
	    UNTIL R> R> 			\ end-of-code xt
	    CR BASE @ >R HEX
	    BEGIN ?call ?DUP
		  IF ." call-" .xt THEN
		  DUP @ .xt enough? 0=
	    WHILE CELL+ 2DUP U> 0=
	    UNTIL THEN 2DROP  R> BASE ! ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  \ Following definition is less dependent on dictionary structure although
  \ slower. It only assumes xt of a word is larger than xt of previously
  \ defined words. This works for ROM model also.
  : xtSEE   >R xhere >R 	  \ Search all wordlist to find end of xt.
	    FORTH-WORDLIST	  \ Find smallest link pointer larger than xt.
	    BEGIN DUP
		  BEGIN @ ?DUP
		  WHILE DUP name>xt R@ U<
			IF R> OVER name>xt R@ U>
			   IF DROP DUP name>xt THEN
			   >R
			THEN
			cell-
		  REPEAT
		  CELL+ @ ?DUP 0=		\ continue to next wordlist
	    UNTIL R> R> 			\ end-of-code xt
	    CR BASE @ >R HEX
	    BEGIN ?call ?DUP
		  IF ." call-" .xt THEN
		  DUP code@ .xt enough? 0=
	    WHILE CELL+ 2DUP U> 0=
	    UNTIL THEN 2DROP  R> BASE ! ;
[THEN]

FORTH-WORDLIST SET-CURRENT

\   SEE 	( "<spaces>name" -- )           \ TOOLS
\		Display human-readable representation of the name's definition.
: SEE	(') 1+ IF ." IMMEDIATE-word" THEN  xtSEE ;

NONSTANDARD-WORDLIST SET-CURRENT

\   WORDLIST-WORDS    ( wid -- )
\		List the definition names in wordlist identified by wid.
: WORDLIST-WORDS
    CR 0 >R
    BEGIN @ ?DUP
    WHILE DUP .name  R> 1+ >R
	  cell- 		\ pointer to next word
    enough? UNTIL
    THEN  SPACE R> . ." words " ;

\   NONSTANDARD-WORDS	( -- )
\		List the definition names in NONSTANDARD-WORDLIST.
: NONSTANDARD-WORDS   NONSTANDARD-WORDLIST WORDLIST-WORDS ;

FORTH-WORDLIST SET-CURRENT

\   WORDS	( -- )				\ TOOLS
\		List the definition names in the first wordlist of the
\		search order.
: WORDS   #order CELL+ @ WORDLIST-WORDS ;

envQList SET-CURRENT
-1 CONSTANT TOOLS
FORTH-WORDLIST SET-CURRENT

\ **********************
\ Nonstandard system utility word
\ **********************

NONSTANDARD-WORDLIST SET-CURRENT

\   SAVE-SYSTEM   ( -- )
\		Save current state of the system. There must be a way
\		to preserve the memory image. Use non-volatile RAM or
\		DEBUG.EXE to store the image in MS-DOS.

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : SAVE-SYSTEM
      var0 sysVar0 [ sysVar0End sysVar0 - ] LITERAL MOVE ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : SAVE-SYSTEM
      #order DUP @ #order0 SWAP 1+ CELLS MOVE ;
[THEN]

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]

  \ **********************
  \ RAM/ROM System Only
  \ **********************

  \   RAM	  ( -- )
  \		  Set data space in RAM area.
  : RAM   RAMB TO hereVar ;

  \   ROM	  ( -- )
  \		  Set data space in ROM area.
  : ROM   ROMB TO hereVar ;

  \   RAM/ROM@	  ( -- ram/rom-id )
  \		  Return RAM/ROM identifier which will be consumed by RAM/ROM!
  : RAM/ROM@   hereVar ;

  \ RAM/ROM!	  ( ram/rom-id -- )
  \		  Set HERE according to RAM/ROM identifier on the stack.
  : RAM/ROM!   TO hereVar ;
[THEN]

\   RETRY	( -- )
\		Compile unconditional jump to the start of the current
\		definition. Described by Dr. Astle in Forth Dimensions
\		17(4), 19-21 (1995).
: RETRY   bal 1- 2* PICK 1+ IF -22 THROW THEN
		     \ control structure mismatch; colon-sys type is -1
	  bal 1- 2* 1+ PICK	  \ xt of current definition
	  ?call DROP POSTPONE branch COMPILE, ; IMMEDIATE COMPILE-ONLY

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  DECIMAL
  \ Structure of CREATEd word:
  \   |compile_xt|name_ptr| call-doCREATE | 0 or DOES>_xt | a-addr |
  : doCompiles>
      lastName DUP C@ 128 ( =seman) <
      IF -12 THROW THEN 	\ argument type mismatch
      name>xt cell- cell- code! ;
  \  compiles>	( xt colon-sys -- colon-sys )
  \		Assign xt as special compilation action of the last CREATEd
  \		word. It is the user's responsibility to match the special
  \		compilation action and the execution action.
  \	    Example: '2CONSTANT' can be defined as following:
  \	    :NONAME   EXECUTE POSTPONE 2LITERAL ;
  \	    : 2CONSTANT   CREATE SWAP , , compiles> DOES> DUP @ SWAP CELL+ @ ;
  : compiles>	ROT POSTPONE LITERAL POSTPONE doCompiles> ; IMMEDIATE
[THEN]

FORTH-WORDLIST SET-CURRENT

BASE !

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
