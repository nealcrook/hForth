\ define.fs - defining words.
\
\ This file defines the platform-dependent words of the cross and cross-ext
\ word sets, ie these words:
\
\ CELL+ CELLS CHAR+ CHARS ALIGN EQU
\ VARIABLE 2VARIABLE CVARIABLE VALUE
\ ' ['] CREATE DOES> POSTPONE COMPILE,
\ IMMEDIATE
\
\ It also defines other defining words for the target, specifically,
\ these words:
\
\ TO CONSTANT : CODE END-CODE ;CODE >BODY USER :NONAME
\ COMPILE-ONLY
\ ??WORDLIST-CONTROL-WORDS
\
\ It also defines these words that are not strictly defining words:
\
\ x-]
\
\ All but the tXXXX words need to be executable in TARGET scope and
\ so they are defined in INTERPRETER scope. Sub-factors of these words
\ are defined in HOST scope, where possible, in order to minimise the
\ number of words defined in INTERPRETER scope.

\ Policy on sections: when in target scope, you must choose the active
\ section based on where you want data objects to be built. It is
\ implementation dependent where things like code definitions get built.
\ For hForth (ie this code) code definitions explicitly use the CData
\ section, and so does the dictionary structure.

\ TODO?? Change ROM/RAM in hForth to Udata/Idata?

\ TODO highlight all sections here that are target-processor dependent

base @ decimal


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Declare data structures in HOST space for the defining words. Most of
\ these exist on the target, too, but it's too hard to use the target
\ versions (especially just now, when they haven't even been defined)


\ PORT: these are coded for a target that is a 32-bit byte-addressed machine
4 constant target-cell-size
1 constant target-char-size

\ emulate word lists on the target; initialise to empty wordlist (null link)
VARIABLE tFORTH-WORDLIST            0 tFORTH-WORDLIST !
VARIABLE tENVIRONMENT-WORDLIST      0 tENVIRONMENT-WORDLIST !
VARIABLE tNONSTANDARD-WORDLIST      0 tNONSTANDARD-WORDLIST !

VARIABLE tSTATE		        FALSE tSTATE !
VARIABLE rakeVar	            0 rakeVar !

VARIABLE tcurrent
CREATE #order 2 , tFORTH-WORDLIST , tNONSTANDARD-WORDLIST ,
\ TODO reserve space for the rest of the wordlists.. in practice, ought
\ to map this to areas in the real target space instead
0 VALUE bal
0 VALUE notNONAME?
CREATE errWord 2 CELLS ALLOT	\ last found word. Used for error reporting


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Declare vectors for xts used by defining/immediate words. These get
\ filled in in hforth.fs, once the primitives have been defined.

0 VALUE xt-doVALUE
0 VALUE xt-doCONST
0 VALUE xt-doUSER
0 VALUE xt-doLIT
0 VALUE xt-doCREATE
0 VALUE xt-doTO
0 VALUE xt-doLIST
0 VALUE xt-doLOOP
0 VALUE xt-do+LOOP
0 VALUE xt-branch
0 VALUE xt-0branch
0 VALUE xt-EXIT

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Primitives used by standard words

\ TODO subtle bug: the 3 XOR is suppost to generate a mask to lop off the
\ bottom 2 bits, but it is coded for the host cell size and needs to work
\ for the *target*
: align-p ( sect-off -- )
    \ Align the HERE-pointer for the current section specified by sect-off
    sect-table + @ \ sect-a
    [ 5 cells ] literal + \ address of HERE
    dup @ target-cell-size + 1 - -1 3 XOR AND swap !
;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Easy standard words

: x-ALIGN ( -- )
    \ CELL-align HERE-pointer for current section of current section type
    sect-table @ align-p
;

: x-CELL+ ( a-addr1 -- a-addr2) target-cell-size + ;
: x-CELLS ( n1 -- n2 ) target-cell-size * ;
: x-CHAR+ ( c-addr1 -- c-addr2 ) target-char-size + ;
: x-CHARS ( n1 -- n2 ) target-char-size * ;

: EQU ( "name" x -- ) 
    \ Create a word on the host whose run-time effect is
    \ to build a literal in the target definition
    create ,
does> @
    tSTATE @ IF
	\ compiling for the target; spit out a literal
	CDATA? xt-doLIT x-, x-, \ TODO or code-, or x-COMPILE,
    THEN \ if interpreting for the target, just leave x on stack
;


\ TODO -- to make it compile
base @ hex
1f CONSTANT =MASK
40 CONSTANT =COMP
20 CONSTANT =IMED
0eb000000 CONSTANT CALLL
0ff CONSTANT MaxCountedString
base !


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Trivial supporting definitions

: set-lex ( n -- )
    \ Set lexical bits for most recent word in target dictionary, which is
    \ built in the CData section
    asa@ swap cdata there x-cell+ x-cell+ swap \ x taddr n
    over c@c or swap c!c asa!
;

: x-COMPILE-ONLY =COMP set-lex ;
: x-IMMEDIATE    =IMED set-lex ;

: bal+ bal 1+ TO bal ;
: bal- bal 1- TO bal ;
\ TODO xhere should disappear??
: xhere ( -- c-taddr ) asa@ CDATA x-HERE swap asa! ; \ get the current value of the target PC
: TOxhere ( x -- )     asa@ swap CDATA org asa! ; \ update the target PC
: 1chars/ ;
: x-cell- target-cell-size - ;
: name>xt  x-cell- x-cell- x-@ ;
: lastName asa@ CDATA THERE x-CELL+ x-CELL+ swap asa! ;
: IDflushline DROP ;

: code,         ( x -- )
\               Reserve one cell in code space and store x in it.
		asa@ swap CDATA x-, asa! ;

\ TODO the distinction here is that code, is used for literal values and COMPILE, is
\ used for xts. If we moved from a DTC to subroutine threading, this distinction would be
\ important.
: x-COMPILE,    ( xt -- )
\               Append the xt to the current definition.
		asa@ swap CDATA x-, asa! ;

: x-count ( c-taddr -- c-taddr' n )
    DUP x-CHAR+ SWAP x-C@ ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Factors used by target wordlist stuff

: tGET-CURRENT tcurrent @ ;

: same?		( c-taddr1 c-haddr2 u -- -1|0|1 )
\		Return 0 if two strings, ca1 u and ca2 u, are same, -1 if
\		string, ca1 u is smaller than ca2 u, 1 otherwise. Used by
\		'(search-wordlist)'. Code definition is preferred to speed up
\		interpretation. Colon definition is shown below.
\               taddr is target address, haddr is host address.
    ?DUP IF         \ null strings are always same
	0 DO OVER x-C@ OVER C@ XOR \ 0 (false) if match.. so continue
	    IF UNLOOP C@ SWAP x-C@ > 2* 1+ EXIT THEN
	    CHAR+ SWAP x-CHAR+ SWAP
	LOOP
    THEN 2DROP 0
;

: (search-wordlist)   ( c-addr u wid -- 0 | xt f 1 | xt f -1)
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
    ROT >R SWAP DUP 0= IF -16 THROW THEN
    \ attempt to use zero-length string as a name
    >R		\ wid  R: ca1 u
    \ Moved the access outside the BEGIN loop. The wid addresses a cell in
    \ HOST space so that must be a host @. The rest of the addresses are
    \ on the target.
    @ BEGIN  \ ca2  R: ca1 u
	DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
	DUP x-COUNT [ =MASK ] LITERAL AND R@ = \ ca2 ca2+char f
	IF R> R@ SWAP DUP >R           \ ca2 ca2+char ca1 u
	    same?                        \ ca2 flag
	    \ ELSE DROP -1      \ unnecessary since ca2+char is not 0.
	THEN
    WHILE
	x-cell- x-@             \ pointer to next word in wordlist
    REPEAT
    R> R> 2DROP DUP name>xt SWAP            \ xt ca2
    x-C@ DUP [ =COMP ] LITERAL AND 0= SWAP
    [ =IMED ] LITERAL AND 0= 2* 1+
;

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
	      (search-wordlist)         \ ca u\ 0 | w f 1 | w f -1
	      ?DUP IF		    \ ca u; 0 | w f 1 | w f -1
	         >R 2SWAP 2DROP R> UNLOOP EXIT \ xt f 1 | xt f -1
	      THEN		       \ ca u
	   LOOP 0		        \ ca u 0
	THEN ;

\   skipPARSE	( char "<chars>ccc<char>" -- c-addr u )
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

: PARSE-WORD	( "<spaces>ccc<space>" -- c-addr u )
\		Skip leading spaces and parse a word. Return the name.
		BL skipPARSE ;

: tSEARCH-WORDLIST	( c-addr u wid -- 0 | xt 1 | xt -1)     \ SEARCH
\		Search word list for a match with the given name.
\		Return execution token and -1 or 1 ( IMMEDIATE) if found.
\		Return 0 if not found.
		(search-wordlist) DUP IF NIP THEN ;


\   (')		( "<spaces>name" -- xt 1 | xt -1 )
\		Parse a name, find it and return execution token and
\		-1 or 1 ( IMMEDIATE) if found
\
BASE @ HEX
: (')	PARSE-WORD search-word ?DUP IF NIP EXIT THEN
    errWord 2!      \ if not found error
    -13 THROW ;     \ undefined word
\ TODO may need to add separate printing routine here to show the
\ problem rather than relying on the host's THROW
BASE !


\ Preserve/restore HOST search order during target definitions.    
\ NOTE: This does not nest! You can only use it once.
CREATE order-! 5 CELLS ALLOT

: save-order
	GET-ORDER DUP 4 > IF
		ABORT" Too many wids for save-order storage"
	THEN DUP order-! !
	0 DO I 1+ CELLS order-! + ! LOOP ;

: restore-order
	order-! @ DUP 4 > SWAP 0= OR IF
		ABORT" wid count in save-order storage is illegal"
	THEN
	order-! DUP DUP @ CELLS + \ last-a first-a to be stacked
	DO I @ 1 CELLS NEGATE +LOOP SET-ORDER ;

\ search order for use within target definitions. Search *comp first. 
: tc-order save-order only also *hict also *target also *comp ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Target wordlist stuff
\ ' ['] F-DEF S-DEF ENV-DEF

\ words to switch the compilation wordlist for target definitions
: F-DEF   tFORTH-WORDLIST tcurrent ! ;
: S-DEF   tNONSTANDARD-WORDLIST tcurrent ! ;
: ENV-DEF tENVIRONMENT-WORDLIST tcurrent ! ;

: x-'            ( "<spaces>name" -- xt )        \ CORE
\		Parse a name, find it and return xt.
    (') DROP
;
    
\ TODO this is an immediate word and therefore shouldn't
\ be here.. but I think it's needed.
\ TODO not sure about the host part.. is it literal or compile, or sommat?
: x-[']     \    Compilation: ( "<spaces>name" -- )      \ CORE
\		Run-time: ( -- xt )
\		Parse name. Return the execution token of name on execution.
    x-' tSTATE @ IF
	\ compiling for the target
	xt-doLIT code, code, \ TODO or x-, ??
    ELSE
	\ interpret or compile a target xt for the *host*
	POSTPONE LITERAL
    THEN ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Factors used by defining words

\ TODO need to make sure that any word that needs these gets the
\ right ones; the target or host versions. For example, xt, above calls
\ ALIGNED and it needs the target version, but it gets the host version.
\ TODO may need to have t versions of these to avoid name-space clashes
\ - hard to detect because host and target are the same in this case.
\ might be worth creating a dummy host version where the alignment is
\ coarser, in order to check for bugs.
\ TODO .. make x-ALIGNED and alias it?
\ TODO align should be a factor of aligned or vice-versa

: x-ALIGNED
    DUP 0 target-cell-size UM/MOD DROP DUP
    IF target-cell-size SWAP - THEN + ;    \ slow, very portable

BASE @ HEX
\ TODO That comment's not clear..
: xt,		( xt1 -- xt2 )
\		Take a run-time word xt1 for :NONAME , CONSTANT , VARIABLE and
\		CREATE . Return xt2 of current definition.
\
\ TODO: need a check to see that the destination is reachable..
    xhere x-ALIGNED DUP TOxhere SWAP
    xhere - x-CELL- x-CELL- 2 RSHIFT  \ get signed offset
    00FFFFFF AND                      \ mask off high-order sign bits
    CALLL OR                          \ make the opcode 
    xhere swap                        \ remember where it will go
    code, IDflushline                 \ emit it and purge the block
; BASE !

\   ?call	( xt1 -- xt1 0 | a-addr xt2 )
\		If xt1 starts with a machine CALL instruction then leave
\		xt of the CALLed routine and the address of the cell following
\		the machine call at xt1. Otherwise leave xt1 and 0. See
\		optiCOMPILE for an example of usage
\
\ 8086 version:
\   : ?call	DUP x-@ call-code =
\		IF   x-CELL+ DUP x-@ SWAP x-CELL+ DUP ROT + EXIT THEN
\		        \ Direct Threaded Code 8086 relative call
\		0 ;
\
\ ARM version: call is one cell and contains a signed 24-bit relative
\ offset. The offset is fixed up for pipeline prefetch:
base @ HEX
: ?call
    DUP x-@ 0FF000000 AND CALLL =
    \ that call-detection is crude - not an exact check..
    IF DUP DUP x-@ 00FFFFFF AND    \ it's a branch.. get offset
	DUP 007FFFFF > IF
	    00FF000000 OR THEN     \ sign extend the offset
	2 LSHIFT                   \ convert to byte offset
	+ x-CELL+ x-CELL+          \ fix up for pipeline prefetch
	SWAP CELL+ SWAP EXIT
    THEN 0
; BASE !


: pack"         ( c-addr u a-addr -- a-addr2 )
    \ Copy a string c-addr u from host space to a-addr in target space and
    \ give the next cell-aligned address. Fill the rest of the last cell with
    \ null character.
    OVER MaxCountedString SWAP U<
    IF -18 THROW THEN		    \ parsed string overflow
    2DUP SWAP CHARS + CHAR+ DUP >R  \ ca u aa aa+u+1
    x-ALIGNED x-cell- 0 SWAP x-!    \ null-fill at end of string
    2DUP x-C! x-CHAR+ SWAP          \ c-addr a-addr+1 u
    CHARS x-MOVE R>
; COMPILE-ONLY


: head,		( xt "<spaces>name" -- )
    \ Parse a word and build a dictionary entry in the current CDATA section
    \ for the target image using xt and name.
    asa@ CDATA SWAP PARSE-WORD DUP 0=
    IF errWord 2! -16 THROW THEN \ attempt to use zero-length string as a name
    DUP =MASK > IF -19 THROW THEN \ definition name too long
    2DUP tGET-CURRENT tSEARCH-WORDLIST  \ name exists?
    IF DROP ." cross: Redefine " 2DUP TYPE SPACE THEN \ warn if redefined
    \ at this point the stack looks like this: xt c-addr u    
    DUP x-CHARS x-CHAR+      \ convert, leave room for length
    \ need to round up to xple of cells
    3 + 2 rshift 2 lshift \ TODO.. crude but effective.. use x-ALIGNED
\ \\\\    DUP x-ALIGNED SWAP OVER XOR IF x-cell- THEN \ aligned to low address
    TALLOT THERE pack" DROP         \ pack the name in dictionary
    [ 1 x-CELLS ] LITERAL TALLOT
    THERE tGET-CURRENT @ SWAP x-!   \ build wordlist link
    [ 1 x-CELLS ] LITERAL TALLOT
    THERE x-! asa!                  \ store xt at the code field
;

: linkLast	( -- )
    \ Link the word being defined to the current wordlist.
    lastName tGET-CURRENT ! ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Defining words for the target
\
\ Each defining word performs two functions:
\ Firstly, it builds an entry in the host dictionary (in the host's *target
\ wordlist)
\ Secondly, it builds an entry in the target dictionary, in the current
\ CDATA section, in the target's current compilation wordlist.
\
\ The two entries have the same name, and the data stored with the host
\ entry includes the definition name as a counted string. This means that
\ the name in the input stream gets parsed three times.
\
\ The existence of the host definition is the key thing that allows the
\ target definition to be "executed" on the host; without it, the name
\ (in the target dictionary) could not be found by the host's outer
\ interpreter. What is actually executed is a piece of DOES> code that
\ mimics part of the target word's functionality on the host.


\ name2tcs and s2txt are common factors in the define and DOES> parts
\ of all the defining words.

: name2tcs ( ccc -- exec: ccc --- c-haddr )
    \ parse ccc non-destructively, build a definition in the host
    \ dictionary with this name, and store the name in the data area
    \ of the host definition so that executing the name ccc will return
    \ the address of the name as a counted string
    GET-CURRENT ALSO *target DEFINITIONS PREVIOUS
    >IN @ >R CREATE R@ >IN ! BL PARSE R> >IN ! \ parse non-destructively
    \ copy string from input buffer to data area as counted string
    DUP HERE C! HERE CHAR+ SWAP DUP >R CMOVE R> 1+ ALLOT ALIGN
    SET-CURRENT
;    

: s2txt ( c-haddr -- c-haddr u 0 | xt f 1 | xt f -1)
    \ string-to-target-xt
    \ search for counted string in target dictionary
    \ return target xt and lexical information
    count search-word ?dup
    IF
	\ target words cannot be executed on the host, so we should
	\ NEVER encounter an immediate word; the function of all
	\ immediate words must be provided by equivalents that run
	\ on the host, in the TODO word list.
	1 = abort" cross: attempt to reference immediate word from target"
	DROP \ don't care about compile-only, since we always are.
    ELSE
	\ all definitions must have an equivalent on the host, so
	\ should never come here
	." cross: Fatal internal error: failed to find" TYPE
    THEN
;

: x-CONSTANT	( x "<spaces>name" -- )         \ CORE
\		name Execution: ( -- x )
\		Create a definition for name which pushes x on the stack on
\		execution.
   bal IF -29 THROW THEN	\ compiler nesting
   name2tcs
   xt-doCONST xt, head, code, linkLast
DOES>
    s2txt tSTATE @ IF
	code, \ compiling for the target - compile the xt
    ELSE
	x-cell+ x-@ \ interpreting - just return value
    THEN
;

: x-VALUE	( x "<spaces>name" -- )         \ CORE EXT
    \		name Execution: ( -- x )
    \ Create a value object with initial value x.
    bal IF -29 THROW THEN	\ compiler nesting
    name2tcs
    xt-doVALUE xt, head,
    \ reserve space in IDATA, leave address
    target-cell-size
    [ 3 cells ] literal dup align-p rez
    dup code, \ store address within definition
    x-! \ store x in reserved IDATA location
    linkLast  \ link VALUE word to current wordlist
DOES>
    s2txt tSTATE @ IF
	x-COMPILE, \ compiling for the target
    ELSE
	x-cell+ x-@ x-@ \ return the value
    THEN
;

: x-VARIABLE	( "<spaces>name" -- )           \ CORE
\		name Execution: ( -- a-addr )
\		Parse a name and create a variable with the name.
\		Resolve one cell of data space at an aligned address.
\		Return the address on execution.
    bal IF -29 THROW THEN	\ compiler nesting
    name2tcs
    xt-doCONST xt, head,
    \ reserve space in the section specified by VARIABLES
    target-cell-size govs dup align-p rez
    code, \ store address within definition
    linkLast
DOES>
    s2txt tSTATE @ IF
	x-COMPILE, \ compiling for the target
    ELSE
	x-cell+ x-@ \ return the address
    THEN
;
    

: x-2VARIABLE	( "<spaces>name" -- )           \ CORE
    bal IF -29 THROW THEN	\ compiler nesting
    name2tcs
    xt-doCONST xt, head,
    \ reserve space in the section specified by VARIABLES
    [ target-cell-size 2 * ] LITERAL govs dup align-p rez
    code, \ store address within definition
    linkLast
DOES>
    s2txt tSTATE @ IF
	x-COMPILE, \ compiling for the target
    ELSE
	x-cell+ x-@ \ return the address
    THEN
;

: CVARIABLE	( "<spaces>name" -- )           \ CORE
    bal IF -29 THROW THEN	\ compiler nesting
    name2tcs
    xt-doCONST xt, head,
    \ reserve space in the section specified by VARIABLES
    target-char-size govs rez
    code, \ store address within definition
    linkLast
DOES>
    s2txt tSTATE @ IF
	x-COMPILE, \ compiling for the target
    ELSE
	x-cell+ x-@ \ return the address
    THEN
;


: x->BODY		( xt -- a-addr )		 \ CORE
\		Push data field address of CREATEd word.
\		Structure of CREATEd word:
\		        | call-doCREATE | 0 or DOES> code addr | a-addr |
\
  ?call DUP IF		     \ code-addr xt2
    xt-doCREATE = IF        \ should be call-doCREATE
      CELL+ @ EXIT
    THEN
  THEN -31 THROW ;        \ >BODY used on non-CREATEd definition

\ TODO host actions
: x-CREATE	( "<spaces>name" -- )           \ CORE
\		name Execution: ( -- a-addr )
\		Create a data object in RAM/ROM data space, which return
\		data object address on execution
		bal IF -29 THROW THEN	\ compiler nesting
		xt-doCREATE xt, head,
		0 code,                 \ indicate there's no DOES> code yet
		\ TODO BUG: the next piece of code acts on the active section, as per the spec
		\ for CREATE. This will fail if the active section is a code section.
		x-ALIGN x-HERE		\ next (aligned) address in data space
		code,                   \ address of data: >BODY gets this address
		linkLast ;              \ link CREATEd word to current wordlist


\ Start target compilation by toggling the target STATE flag and switching
\ word lists for searching. The compilation word list is set by the TARGET
\ scope-selector word.
: x-]          -1 tSTATE ! tc-order ;


: x-:NONAME     ( -- xt colon-sys )             \ CORE EXT
    \ Create an execution token xt, enter compilation state and
    \ start the current definition.
    bal IF -29 THROW THEN           \ compiler nesting
    xt-doLIST xt, DUP -1
    0 TO notNONAME?  1 TO bal x-] ;

: x-:           ( "<spaces>name" -- colon-sys ) \ CORE
    \ Start a new colon definition using next word as its name.
    x-:NONAME ROT \ :NONAME sets the compilation order
    name2tcs
    head, -1 TO notNONAME?
DOES>
    s2txt tSTATE @ IF
	x-COMPILE, \ compiling for the target
    ELSE
	." cross: cannot interpret this target word on the host" ABORT
    THEN
;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ More defining words for the target:
\ CODE END-CODE END-CODE-EARLY ;CODE

\ TODO change these words to do the Right Stuff on the host and
\ target dictionaries etc

ALSO ASSEMBLER \ TODO .. don't really want this here..

: CODE		( '<spaces>name' -- )           \ TOOLS EXT
\		Skip leading space delimiters.	Parse name delimited by a
\		space. Create a definition for name, called a
\		'code definition,' with the execution semantics defined below.
\		Process subsequent characters in the parse area in an
\		implementation-defined manner, thus generating corresponding
\		machine code. Those characters typically represent source code
\		in a programming language, usually some form of assembly
\		language.  The process continues, refilling the input buffer
\		as needed, until an implementation-defined ending sequence is
\		processed.
\
\		name Execution:( i*x --- j*x )
\		Execute the machine code sequence that was generated
\		following CODE.
    -1 TO notNONAME?
    name2tcs
    asa@ CDATA x-ALIGN x-HERE SWAP asa! \ align code address
    head,				\ register a word in dictionary
    ALSO ASSEMBLER init-asm
DOES>
    s2txt tSTATE @ IF
	\ TODO compiling for the target.. spit out the xt
    ELSE
	." cross: cannot interpret this target word on the host" ABORT
    THEN
;


MICRO-DEBUG [IF]
: END-CODE-EARLY 00 G# B, ;
[ELSE]
: END-CODE-EARLY target-cell-size # [ fpc ] PC LDR, ;
[THEN]

: END-CODE
    LOCAL-RESOLVED \ ensure all forward references are resolved
    PREVIOUS notNONAME? IF linklast 0 to notNONAME? THEN
    END-CODE-EARLY
;


: ;CODE		\ Compilation: ( C: colon-sys -- )	\ TOOLS EXT
\		Interpretation: Interpretation semantics for this word
\				are undefined.
\		Append the run-time semantics below to the current definition.
\		End the current definition, allow it to be found in the
\		dictionary, and enter interpretation state, consuming
\		colon-sys. Process subsequent characters in the parse area in
\		an implementation-defined manner, thus generating corresponding
\		machine code. Those characters typically represent source code
\		in a programming language, usually some form of assembly
\		language.  The process continues, refilling the input buffer as
\		needed, until an implementation-defined ending sequence is
\		processed.
\
\		Run-time:( -- ) ( R: nest-sys -- )
\		Replace the execution semantics of the most recent definition
\		with the name execution semantics given below. Return control
\		to the calling definition specified by nest-sys. An ambiguous
\		condition exists if the most recen definition was not defined
\		with CREATE or a user-defined word that calls CREATE.
\
\		name Execution:( i*x --- j*x )
\		Perform the machine code sequence that was generated
\		following ;CODE.
	bal 1- IF -22 THROW THEN        \ control structure mismatch
	NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
\ TODO POSTPONE stuff needs reviewing...
	bal- POSTPONE [
\ TODO remove use of xhere?.. see DOES> ??
	xhere 2 CELLS - TOxhere
	ALSO ASSEMBLER init-asm ;

PREVIOUS

\ TODO temportary,, should be in immediate.fs

: x-[
\ Within a target definition, switch from compiling for the target to
\ interpreting for the target.. which really means interpreting for the host.
    0 tSTATE ! restore-order ; 


: x-;           ( colon-sys -- )		 \ CORE
\		Terminate a colon definition.
    bal 1- IF -22 THROW THEN        \ control structure mismatch
    NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
    notNONAME? IF   \ if the last definition is not created by ':'
	linkLast 0 TO notNONAME?     \ link the word to wordlist
    THEN xt-EXIT x-COMPILE,     \ add EXIT at the end of the definition
    0 TO bal x-[  ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Alias standard words into the appropriate wordlists

\ these words can be used whilst interpreting for the target
also *interp definitions previous
' x-ALIGN ALIAS ALIGN
' x-CELL+ ALIAS CELL+
' x-CELLS ALIAS CELLS
' x-CHAR+ ALIAS CHAR
' x-CHARS ALIAS CHARS
\ TODO don't need to do this since host words will be found anyway..
\ go back through cross.fs and remove them there and here.
' EQU ALIAS EQU
' F-DEF ALIAS F-DEF
' S-DEF ALIAS S-DEF
' ENV-DEF ALIAS ENV-DEF
' x-COMPILE, ALIAS COMPILE,
' x-' ALIAS '
' x-CONSTANT ALIAS CONSTANT
' x-VALUE ALIAS VALUE
' x-VARIABLE ALIAS VARIABLE
' x-2VARIABLE ALIAS 2VARIABLE
' CVARIABLE ALIAS CVARIABLE
' BUFFER: ALIAS BUFFER:
' x->BODY ALIAS x->BODY
' CREATE ALIAS CREATE
' x-] ALIAS ]
' x-:NONAME ALIAS :NONAME
' x-: ALIAS :
' CODE ALIAS CODE
' END-CODE ALIAS END-CODE
' END-CODE-EARLY ALIAS END-CODE-EARLY
' ;CODE ALIAS ;CODE                          COMPILE-ONLY IMMEDIATE
' x-COMPILE-ONLY ALIAS COMPILE-ONLY
' x-IMMEDIATE ALIAS IMMEDIATE
\ Standard words that we want to be available..
\ TODO need these for target compile as well
' [IF] ALIAS [IF]                              IMMEDIATE
' [ELSE] ALIAS [ELSE]                          IMMEDIATE
' [THEN] ALIAS [THEN]                          IMMEDIATE
also Forth definitions previous

\ these words can be used whilst compiling for the target
also *comp definitions previous
' x-['] ALIAS [']
' x-; ALIAS ; \ TODO temp.. whilst it exists in this file.
also Forth definitions previous


\ TODO temp hack
\ used in startup code. Need ASSEMBLER to find basic opcode stuff
\ and need FORTH so that assembler variables like LTORG-HEAD can be
\ initialised using the host's version of ! .. Maybe the MK-SYM-TABLE
\ and other init stuff should be in meta.fth
: +asm ALSO FORTH ALSO ASSEMBLER ;
: -asm PREVIOUS PREVIOUS ;
also *interp definitions previous
' +asm ALIAS +asm
' -asm ALIAS -asm
also Forth definitions previous



\ Leave politely
base !