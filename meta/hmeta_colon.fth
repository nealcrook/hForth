\ $Id$
\ $Log$
\ Revision 1.1  1998/06/07 22:51:02  crook
\ Initial revision
\

\ colon compiler and immediate words for hForth
MARKER *hmc*
HEX


: COMPILE-ONLY ; \ TODO this needs to set the bits in the TARGET. Need
\ a similar word for IMMEDIATE but must define them in the right place
\ so that they do not affect any of the host defns.

\ Host postpone. TODO may not need this
: hPOSTPONE POSTPONE POSTPONE ; IMMEDIATE
: POSTPONE ." Error: use hPOSTPONE or tPOSTPONE" QUIT ;

: g ." got here" .S ;
: ig ." (" DEPTH ." depth=" . ." )" ; IMMEDIATE

\ There are three definition of : (colon) :-
\ v1 - the host system's definition (runs on the host, generates definitions
\      on the host)
\ v2 - a meta-definition (runs on the host, generates code on the target)
\ v3 - the target system's definition (runs on the target, generates code on
\      the target)
\ 
\ Original plan was that v2 : would search the wordlists in the target
\ dictionary in order to find and emit the xt. However, I realised that this
\ would require me to implement an interpreter as part of ":", which is
\ tricky and not how ":" normally works. A paper by Jeff Fox showed me a far
\ better solution: each time a word is defined for the target, it creates
\ two dictionary entries. The first is in the target dictionary, the second
\ is in the host dictionary. The run-time effect of the word in the host
\ dictionary is simply to emit its (target) xt in the target code space.
\ The definitions in the host space are in a special wordlist called t-words.
\ The only special thing that has to happen is for the target's IMMEDIATE
\ words. For each IMMEDIATE word on the target, there must be a corresponding
\ definition on the host - in a special wordlist called it-words. During
\ compilation, the search order is set to it-words t-words (the search order
\ is restored when ; is invoked ?and toggled during [ ] ?).
\ When a target IMMEDIATE word is used, it will be found in it-words (in
\ preference to any definition that may exist in it-words) and its host
\ immediate actions (that affect the target) will be performed.
\ When a target IMMEDIATE word is defined, it will be defined in t-words.
\ The target IMMEDIATE words that execute on the host must be carefully
\ defined. For example: ' (tick) and POSTPONE must search the target dictionary
\ in order to find the correct xt (!! no - they must search t-words in
\ the host dictionary - otherwise they will not find the FORTDEF words)
\
\ This search-order trick means that we can continue to use the host's
\ interpreter for the target compilation process. Thanks, Jeff.
\
\ v3 never gets used in the meta environment, and will not ever get found
\ by accident unless you try to use : within a definition.
\
\ When the target immediate word IMMEDIATE is used, it has two effects; first,
\ it sets the appropriate lexicon bits in the target dictionary. Second, it
\ searches the it-words list to check that there is an equivalent definition
\ to run on the host on behalf of the target; it there's not, it can print
\ a helpful error message as a warning (since any subsequent use of that
\ immediate word will fail).
\
\ Finally, a third wordlist its-words, is used to define words that
\ act as support words ( for the immediate definitions and some others). These
\ words are only in the search order whilst the dependent words are being
\ defined.
\ 
\ The colon definitions that are used to define ":" itself are also hidden in
\ its-words
\ All definitions that will clash with definitions in the host are in special
\ wordlists or have special names that prevent them from clashing for the
\ time being.
\
\ If hForth was arranged entirely hierarchically, there would be no need for
\ forward definitions. However, that is not the case so some method is
\ needed for providing the xt of forward definitions (I can extract the
\ xt from the previously compiled code since the goal of the first 
\ implementation is to generate an identical binary). This is achieved
\ with the word FORTDEF.
\ <value> FORTDEF "name"
\ creates a definition in t-words, with an xt of <value>. When words
\ are to be defined in t-words, a search is made for an existing word
\ of the same name (duplicate definition). If a duplicate is found, no
\ new definition is added but the target xt of the existing definition is
\ checked against the target xt of the word that was going to be defined.
\ If they match, all is well. If they don't match, a flag controls whether
\ an informational or fatal error is generated.
\
\ Definitions in t-words should reserve an extra cell for holding
\ flags and counts. Flags might include:
\ - compile-only
\ - whether predefined
\ Counts might include
\ - how many times referenced
\ At the end of the run a bit of post-processing can work out how many
\ forward references remain. This will be a useful metric when re-arranging
\ the order of definitions.
\
\ TODO: Might be useful to mimic the behavior of the COMPILE-ONLY flag
\ by storing it in the host definition... worry about that later
\
\ TODO: An additional problem is words that are used interactively (ie
\ outside of definitions). Any of these that have compile-time behaviour
\ must also be defined in it-words. The only ones I can think of are
\ IMMEDIATE and COMPILE-ONLY.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Preserve search order
GET-CURRENT  \ wid of compilation word list
GET-ORDER \ list of wids in the search order

\ In hForth, WORDLIST-NAME associates the name with the wid such that ORDER
\ can display it.
hForth INVERT [IF] : WORDLIST-NAME CONSTANT ; [THEN]


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Create wordlists and associated manipulation words for later use
WORDLIST WORDLIST-NAME t-words-WORDLIST
WORDLIST WORDLIST-NAME it-words-WORDLIST
WORDLIST WORDLIST-NAME its-words-WORDLIST
\ add a wordlist to the search order; use with ALSO
: t-words GET-ORDER NIP t-words-WORDLIST SWAP SET-ORDER ;
: it-words GET-ORDER NIP it-words-WORDLIST SWAP SET-ORDER ;
: its-words GET-ORDER NIP its-words-WORDLIST SWAP SET-ORDER ;

\ TODO - save and restore search order properly
\ TODO - ought to use this for CODE words, too?
: save-order ;
: restore-order ONLY ALSO EXTENSIONS ALSO FORTH ;
: tc-order t-words-WORDLIST it-words-WORDLIST 2 SET-ORDER ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Support words required by the immediate words for the target system.
\ also used for support words for t:
\ These go into a private wordlist that is ONLY available when the it-words
\ (and the real target : ???) are compiled
\ are compiled. That avoids name clash with the host system
\ (t-words is required so that POSTPONE will work)

40000A04 hCONSTANT xtdoVALUE \ TODO shouldn't need these now..
40000A90 hCONSTANT xtdoLIST
40000A14 hCONSTANT xtdoCREATE
400020A8 hCONSTANT xtLITERAL
40001DB0 hCONSTANT xtCOMPILE,
40000C4C hCONSTANT xtEXIT

ALSO t-words ALSO its-words DEFINITIONS

VARIABLE STATE		FALSE STATE !
VARIABLE rakeVar	0 rakeVar !
VARIABLE current
VARIABLE TUNRESOLVED 	0 TUNRESOLVED !
CREATE #order 3 CELLS ALLOT
2 #order !
_FLINK #order CELL+ !
_SLINK #order CELL+ CELL+ !
\ TODO reserve space for the rest of the wordlists.. in practice, ought
\ to map this to areas in the real target space instead
0 VALUE bal
0 VALUE notNONAME?
CREATE errWord 2 CELLS ALLOT	\ last found word. Used for error reporting

: bal+ bal 1+ TO bal ;
: bal- bal 1- TO bal ;
: COMPILE, meta-asm,32 ;
: xhere meta-asm. ;
: TOxhere meta-asm! ;
: 1chars/ ;
: cell- CELLL - ;
: name>xt  cell- cell- @ ;
: lastName _NAME CELL+ CELL+ ;
: IDflushline DROP ;
: call_code CALLL ;
: [']-doLIST xtdoLIST ; \ TODO could do these three properly now..
: [']-doVALUE xtdoVALUE ;
: [']-doCREATE xtdoCREATE ;

\   code,       ( x -- )
\               Reserve one cell in code space and store x in it.
\
: code,     xhere DUP CELL+ TOxhere t! ;


\ TODO may need to have t versions of these to avoid name-space clashes
\ - hard to detect because host and target are the same in this case.
\ might be worth creating a dummy host version where the alignment is
\ coarser, in order to check for bugs.
: CHARS     CHARR * ;   \ slow, very portable
: ALIGNED   DUP 0 CELLL UM/MOD DROP DUP
            IF CELLL SWAP - THEN + ;    \ slow, very portable

\ TODO - need to make this have effect on the target entry
: COMPILE-ONLY ; \ doesn't mean anything to pfe


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
: ?call	DUP @ 0FF000000 AND CALLL =
\ that call-detection is crude - not an exact check..
IF DUP DUP @ 00FFFFFF AND    \ it's a branch.. get offset
	DUP 007FFFFF > IF
		00FF000000 OR \ sign extend the offset
	THEN
	2 LSHIFT                      \ convert to byte offset
	+ CELL+ CELL+                 \ fix up for pipeline prefetch
	SWAP CELL+ SWAP EXIT
THEN 0 ;

\ Simple wordlist stuff.
\ TODO Could re-express $FCODE etc. as FORTH-DEFN $CODE
\ then the FORTH-DEFN could be outside the META-HI conditional.. sounds good.
\ if I get $IMMED and $COMPO sorted out I could do the same for them -
\ only have a single version that affects either the : or the code
\ definition.
\ TODO want to express the code definitions using the same mechanism as
\ hForth ultimately does, linking in the name at the *end* of the defn.
\ so that the $FCODE (etc) get far simpler
: mFORTH-WORDLIST _FLINK ;
: NONSTANDARD-WORDLIST _SLINK ;
: F-DEF mFORTH-WORDLIST current ! ;
: S-DEF NONSTANDARD-WORDLIST current ! ;
: mGET-CURRENT current @ ;
F-DEF


\   pipe        ( -- ) ( R: xt -- )
\               Connect most recently defined word to code following DOES>.
\               Structure of CREATEd word:
\                       | call-doCREATE | 0 or DOES> code addr | a-addr |
\
BASE @ DECIMAL \ THROW value
: pipe      lastName name>xt ?call DUP IF   \ code-addr xt2
                   [']-doCREATE = IF
                   R> SWAP !           \ change DOES> code of CREATEd word
                   EXIT
               THEN THEN
               -32 THROW       \ invalid name argument, no-CREATEd last name
               ; COMPILE-ONLY
BASE !

\   xt,		( xt1 -- xt2 )
\		Take a run-time word xt1 for :NONAME , CONSTANT , VARIABLE and
\		CREATE . Return xt2 of current definition.
\
: xt,       xhere ALIGNED DUP TOxhere SWAP
               xhere - CELL- CELL- 2 RSHIFT    \ get signed offset
               00FFFFFF AND                   \ mask off high-order sign bits
               call_code OR                    \ make the opcode 
               xhere swap                      \ remember where it will go
               code, IDflushline ;             \ emit it and purge the block


\   same?	( c-addr1 c-addr2 u -- -1|0|1 )
\		Return 0 if two strings, ca1 u and ca2 u, are same\ -1 if
\		string, ca1 u is smaller than ca2 u\ 1 otherwise. Used by
\		'(search-wordlist)'. Code definition is preferred to speed up
\		interpretation. Colon definition is shown below.

: same?      ?DUP IF         \ null strings are always same
                  0 DO OVER C@ OVER C@ XOR \ 0 (false) if match.. so continue
                       IF UNLOOP C@ SWAP C@ > 2* 1+ EXIT THEN
                       CHAR+ SWAP CHAR+ SWAP
                  LOOP
               THEN 2DROP 0 ;


\   (search-wordlist)   ( c-addr u wid -- 0 | xt f 1 | xt f -1)
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
BASE @ DECIMAL \ THROW value
: (search-wordlist)
		ROT >R SWAP DUP 0= IF -16 THROW THEN
				\ attempt to use zero-length string as a name
		>R		\ wid  R: ca1 u
\ each time we get a wid must convert it from a target address to a host
\ address. The only other times we access memory we never care about the
\ target address. Do the conversion *after* the check for the end link
		BEGIN  @ 	\ ca2  R: ca1 u
		  DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
		  t2h DUP COUNT [ =MASK ] LITERAL AND R@ = \ ca2 ca2+char f
		  IF R> R@ SWAP DUP >R           \ ca2 ca2+char ca1 u
		    same?                        \ ca2 flag
		\ ELSE DROP -1      \ unnecessary since ca2+char is not 0.
		  THEN
		WHILE cell-             \ pointer to next word in wordlist
		REPEAT
		R> R> 2DROP DUP name>xt SWAP            \ xt ca2
		C@ DUP [ =COMP ] LITERAL AND 0= SWAP
		[ =IMED ] LITERAL AND 0= 2* 1+ ;
BASE !

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


: skipPARSE
	>R SOURCE >IN @ /STRING    \ c_addr u  R: char
	DUP IF
	   BEGIN  OVER C@ R@ =
	   WHILE  1- SWAP CHAR+ SWAP DUP 0=
	   UNTIL  R> DROP EXIT
	   ELSE THEN
	   DROP SOURCE DROP - 1chars/ >IN ! R> PARSE EXIT
	THEN R> DROP ;

\   PARSE-WORD  ( "<spaces>ccc<space>" -- c-addr u )
\		Skip leading spaces and parse a word. Return the name.
\
: PARSE-WORD   BL skipPARSE ;


\   mSEARCH-WORDLIST     ( c-addr u wid -- 0 | xt 1 | xt -1)     \ SEARCH
\		Search word list for a match with the given name.
\		Return execution token and -1 or 1 ( IMMEDIATE) if found.
\		Return 0 if not found.
\
: mSEARCH-WORDLIST (search-wordlist) DUP IF NIP THEN ;


\   pack"       ( c-addr u a-addr -- a-addr2 )
\               Place a string c-addr u at a-addr and gives the next
\               cell-aligned address. Fill the rest of the last cell with
\               null character.
\
: pack"	2DUP SWAP CHARS + CHAR+ DUP >R  \ ca u aa aa+u+1
	1 CHARS - 0 SWAP t!             \ fill 0 at the end of string
	2DUP tC! CHAR+ SWAP             \ c-addr a-addr+1 u
	CHARS tMOVE R> ; \ COMPILE-ONLY


\   head,       ( xt "<spaces>name" -- )
\		Parse a word and build a dictionary entry using xt and name.
\
BASE @ DECIMAL \ THROW value
: head,	PARSE-WORD DUP 0=
	IF errWord 2! -16 THROW THEN
		\ attempt to use zero-length string as a name
	DUP =MASK > IF -19 THROW THEN   \ definition name too long
	2DUP mGET-CURRENT mSEARCH-WORDLIST  \ name exist?
	IF DROP ." redefine " 2DUP TYPE SPACE THEN \ warn if redefined
	_NAME OVER CELL+ - ALIGNED
	DUP >R pack" DROP R>              \ pack the name in dictionary
	cell- mGET-CURRENT @ OVER t!      \ build wordlist link
	cell- DUP TO _NAME t! ;          \ adjust name space pointer
			                  \ and store xt at code field
BASE !

\   (')		( "<spaces>name" -- xt 1 | xt -1 )
\		Parse a name, find it and return execution token and
\		-1 or 1 ( IMMEDIATE) if found
\
: (')	PARSE-WORD search-word ?DUP IF NIP EXIT THEN
\ not found so assumed to be a forward reference; leave dummy xt
	2DROP 1 TUNRESOLVED +! DEADBEEF -1 ;

\	errWord 2!      \ if not found error
\ 	-13 THROW ;     \ undefined word

\   linkLast	( -- )
\		Link the word being defined to the current wordlist.
\		Do nothing if the last definition is made by :NONAME .
\
: linkLast  lastName mGET-CURRENT ! ;


\   '           ( "<spaces>name" -- xt )        \ CORE
\		Parse a name, find it and return xt.
\
: '         (') DROP ;


\   [']         Compilation: ( "<spaces>name" -- )      \ CORE
\		Run-time: ( -- xt )
\		Parse name. Return the execution token of name on execution.
\
\   : [']       ' tPOSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE
\ simplification because we *know* than LITERAL is immediate
\ LITERAL needs to be from the *target* dictionary

: ['] ' [ xtLITERAL ] LITERAL COMPILE, ; COMPILE-ONLY IMMEDIATE


\ Description of POSTPONE:
\ " force the compilation of a word that would normally be executed"
\ " postpone the compile action of the word"

\ explanation of POSTPONE:
\ : fred ." hello" ; IMMEDIATE
\ : jim ." hello" ;
\ : bill POSTPONE fred ;	SEE bill	doList fred EXIT
\ : dave POSTPONE jim ;		SEE dave	doList doLit jim code, EXIT
\ : bert jim ;			SEE bert        doList jim EXIT
\
\ POSTPONE appends the compilation semantics of the subsequent word to the
\ current definition. The compilation semantics of fred are to execute
\ (because it is immediate) therefore, in the definition of bill, POSTPONE
\ emits the xt of fred so that the run-time behaviour of bill is to execute
\ fred (and print the text hello). The compilation semantics of jim In the definition of dave, the compilation
\ semantics of jim is to  is itself immediate; what it does
\ is parse the subsequent word and determine its xt. It then compiles code
\ (in the definition of bill) to make the run-time action of bill be to
\ execute the xt of fred. During the definition of dave it compiles code to
\ make the run-time action of dave be to 
\ compile code in the definition (in this example, of bob) whose run-time
\ action is to execute FRED. Therefore, bob will have the same run-time
\ behaviour as FRED.
\
\ : POSTPONE  (') 0< IF POSTPONE LITERAL
\	POSTPONE COMPILE, EXIT THEN   \ non-IMMEDIATE
\	COMPILE, ; COMPILE-ONLY IMMEDIATE       \ IMMEDIATE
: tPOSTPONE  CR ." tpostpone"
	(') 0< IF
	." non-imm" .S [ xtLITERAL ] LITERAL COMPILE, COMPILE,
	[ xtCOMPILE, ] LITERAL THEN   \ non-IMMEDIATE
	COMPILE, ; COMPILE-ONLY IMMEDIATE       \ IMMEDIATE

\   rake        ( C: do-sys -- )
\		Gathers LEAVEs.
\
: rake      DUP code, rakeVar @
            BEGIN  2DUP U<
            WHILE  DUP @ xhere ROT !
            REPEAT rakeVar ! DROP
            ?DUP IF                 \ check for ?DO
               1 bal+ tPOSTPONE THEN \ orig type is 1
            THEN bal- ; COMPILE-ONLY


\ (End of support words for immediate words and t:)
PREVIOUS PREVIOUS FORTH DEFINITIONS PREVIOUS \ back to FORTH


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Host Run-time action of words in the target dictionary.
\ Each word in the target dictionary has an associated entry in the host
\ dictionary, in the wordlist t-words. These definitions are either created
\ explicitly as forward references (using FORTDEF) or are created implicitly
\ (called by t: ) using TDEF.
\ Rather than simply return an xt, these words return the address of a
\ counted string and then use the target search order (simulated in the
\ meta-compile) to search the target dictionary for the xt, along with
\ immediate information.
\ A word defined by FORTDEF will not be found in the dictionary on the first
\ pass - it will return a dummy xt of DEADBEEF and increment the host variable
\ TUNRESOLVED. A second pass will resolve these references.
\ A word defined by TDEF will be found in the dictionary and will return its
\ xt. Failure to find a word defined by TDEF means, by definition, that the
\ word has been hidden by changing the wordlist. All of this is a lot more
\ work that simply having the definition return an xt, but it means that
\ we implement search order, which is nice. The cost is in compile speed
\ and the additional space that the strings take up in the host space.
ALSO its-words
\ FORTDEF consumes a name from the input stream
: FORTDEF ( ccc -- )
	>IN @ CREATE >IN ! BL PARSE \ parse non-destructively
	\ copy string from input buffer to data area as counted string
	DUP HERE C! HERE CHAR+ SWAP DUP >R CMOVE R> 1+ ALLOT ALIGN
	\ at run-time, form counted string on stack and search wordlist in
	\ the search-order for it
	DOES> DUP CHAR+ SWAP C@ search-word
	DUP IF
		1 = IF
			CR ." FATAL ERROR "
			CR ." Found immediate word in t-words"
			CR ." -- should have been defined in it-words"
			QUIT
		THEN
		\ found and not immediate. Drop the compile-only flag to
		\ leave the xt, then emit the xt
		DROP meta-asm,32 \ TODO == compile, .. which should I use?
	ELSE
		\ not found
		DROP 1 TUNRESOLVED +!
		2DROP DEADBEEF meta-asm,32 \ drop string and compile dummy xt
	THEN ;

\ TDEF leaves the input stream unchanged: the calling word (t:) will parse
\ again to create the dictionary entry in the target
\ Create a definition whose run-time behaviour is to emit its xt in the
\ target space.
: TDEF ( ccc -- )
	ALSO t-words DEFINITIONS 
	>IN @ >R CREATE R@ >IN ! BL PARSE R> >IN ! \ parse non-destructively
	\ copy string from input buffer to data area as counted string
	DUP HERE C! HERE CHAR+ SWAP DUP >R CMOVE R> 1+ ALLOT ALIGN
	\ at run-time, form counted string on stack and search wordlist in
	\ the search-order for it
	PREVIOUS DEFINITIONS
	DOES> DUP CHAR+ SWAP C@ search-word
	DUP IF
		1 = IF
			CR ." FATAL ERROR "
			CR ." Found immediate word in t-words"
			CR ." -- should have been defined in it-words"
			QUIT
		THEN
		\ found and not immediate. Drop the compile-only flag to
		\ leave the xt, then emit the xt
		DROP meta-asm,32
	ELSE
		CR ." FATAL ERROR"
		CR ." TDEF-ed word not found - target search order must be wrong"
		QUIT
	THEN ;
PREVIOUS

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Forward definitions needed by POSTPONED words in immediate definitions
\ non-postponed words need equivalents to run on the host, but postponed
\ words only need to return their xt.
\ Although this first set of words is only needed to compile the immediate
\ definitions, it makes sense to put them in t-words, because there may be
\ other forward references to them later in the forth source.
ALSO t-words DEFINITIONS

FORTDEF EXIT
FORTDEF LITERAL
FORTDEF COMPILE,
FORTDEF branch
FORTDEF 0branch
FORTDEF doLIT
FORTDEF doLOOP
FORTDEF do+LOOP
FORTDEF doDO
FORTDEF doTO
FORTDEF doS"
FORTDEF TYPE
FORTDEF pipe
FORTDEF UNLOOP
FORTDEF ROT
FORTDEF abort"msg
FORTDEF 2!
FORTDEF THROW \ TODO name space clash?
FORTDEF 2DROP
FORTDEF [
FORTDEF SLITERAL
FORTDEF S"
FORTDEF IF
FORTDEF AHEAD
FORTDEF THEN
FORTDEF ELSE
FORTDEF AGAIN
\ from hForth definitions
FORTDEF (doubleAlso)
FORTDEF EMIT
FORTDEF (')
FORTDEF /MOD
FORTDEF NIP
FORTDEF S>D
FORTDEF FM/MOD
FORTDEF :NONAME
FORTDEF head,
FORTDEF TO \ TODO -- this should need to be here, when I fix the immediates
FORTDEF [']
FORTDEF xt,
FORTDEF <

PREVIOUS DEFINITIONS


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Immediate words for the target system. For hForth, these are:
\ -. ; AGAIN AHEAD IF LITERAL THEN [ ( +LOOP ." ABORT" BEGIN
\ DO DOES> ELSE LEAVE LOOP POSTPONE RECURSE REPEAT SLITERAL S" TO UNTIL
\ WHILE ['] [CHAR] \
\
\ In addition to this, the following words are used in the interpreter
\ and affect the compilation in the target:
\ IMMEDIATE COMPILE-ONLY VALUE CONSTANT VARIABLE
\ The last three are handled as special cases in the main source but must
\ ultimately be supported to allow the supplementary files to be included.
\
\ These words are put into the it-words list, which is NOT in the search
\ order since any self-references should either by POSTPONED (fulfilled by
\ searching t-words) or fulfilled by host words.
ALSO it-words DEFINITIONS PREVIOUS
ALSO its-words

: hSEE SEE ;
: hGET-ORDER GET-ORDER ;
: hWORDS WORDS ;
: h. . ;


\ TODO avoid name clash
\ : IMMEDIATE  lastName [ =IMED ] LITERAL OVER @ OR SWAP ! ;

\ TODO - need to make this have effect on the target entry
\ .. but that version must be in HOST vocab. There are now 3 versions
\ of compile-only.. need to rationalise
: COMPILE-ONLY ; \ doesn't mean anything to pfe

BASE @ DECIMAL \ THROW value
: -. -13 THROW ; IMMEDIATE
BASE !

: [         0 STATE ! restore-order ; IMMEDIATE COMPILE-ONLY


\ ; is defined as t; for now to prevent it being found in these defns
\ TODO could define it at the end to avoid this problem..
\ TODO the two POSTPONEs want target xts. For now, I mimic the POSTPONE
\ TODO behaviour explicitly.
BASE @ DECIMAL \ THROW value

\   ;           ( colon-sys -- )		 \ CORE
\		Terminate a colon definition.
\
: nit;    ." <-hFdefn)" restore-order
	bal 1- IF -22 THROW THEN        \ control structure mismatch
	NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
	notNONAME? IF   \ if the last definition is not created by ':'
	   linkLast 0 TO notNONAME?     \ link the word to wordlist
\ TODO	THEN POSTPONE EXIT     \ add EXIT at the end of the definition
        THEN [ xtEXIT ] LITERAL ASM,32
\ TODO	0 TO bal POSTPONE [  ; COMPILE-ONLY IMMEDIATE
        0 TO bal 0 STATE ! ;

: t;    ." <-hFdefn)" restore-order
	bal 1- IF -22 THROW THEN        \ control structure mismatch
	NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
	notNONAME? IF   \ if the last definition is not created by ':'
	   linkLast 0 TO notNONAME?     \ link the word to wordlist
\ TODO	THEN POSTPONE EXIT     \ add EXIT at the end of the definition
        THEN [ xtEXIT ] LITERAL ASM,32
\ TODO	0 TO bal POSTPONE [  ; COMPILE-ONLY IMMEDIATE
        0 TO bal 0 STATE ! ; COMPILE-ONLY IMMEDIATE
BASE !

\ TODO mods to run on host but apply to target
\ For words like IF we run the host's version within these definitions.
\ for words that emit code, we must run the target's version on the host.
\ POSTPONE falls into both camps; it is both a support word for the immediate
\ definitions (find the version in its-words) and itself an immediate
\ definition
\ TODO this also applies to ['] and maybe to some others

BASE @ DECIMAL \ THROW value
: AGAIN     IF -22 THROW THEN  \ control structure mismatch; dest type is 0
	tPOSTPONE branch code, bal- ; COMPILE-ONLY IMMEDIATE
BASE !

: AHEAD     tPOSTPONE branch xhere 0 code,
	    1 bal+          \ orig type is 1
	    ; COMPILE-ONLY IMMEDIATE

: LITERAL   tPOSTPONE doLIT code, ; COMPILE-ONLY IMMEDIATE

: (         [CHAR] ) PARSE 2DROP ; IMMEDIATE

: LOOP      tPOSTPONE doLOOP  rake ; COMPILE-ONLY IMMEDIATE

: +LOOP     tPOSTPONE do+LOOP  rake ; COMPILE-ONLY IMMEDIATE

: BEGIN     xhere 0 bal+            \ dest type is 0
	    ; COMPILE-ONLY IMMEDIATE

: DO        0 rakeVar !  0		   \ ?DO-orig is 0 for DO
	    tPOSTPONE doDO xhere  bal+       \ DO-dest
	    ; COMPILE-ONLY IMMEDIATE

\ TODO what's the difference between POSTPONE doLIST and ['] doLIST xt, ??
BASE @ DECIMAL \ THROW value
: DOES>     bal 1- IF -22 THROW THEN        \ control structure mismatch
	    NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
	    tPOSTPONE pipe [']-doLIST xt, -1 ; COMPILE-ONLY IMMEDIATE
BASE !

: LEAVE     tPOSTPONE UNLOOP POSTPONE branch
	    xhere rakeVar DUP @ code, ! ; COMPILE-ONLY IMMEDIATE

\ TODO not sure if this needs modification..
BASE @ DECIMAL \ THROW value
: RECURSE   bal 1- 2* PICK 1+ IF -22 THROW THEN
	    \ control structure mismatch; colon-sys type is -1
	    bal 1- 2* 1+ PICK       \ xt of current definition
	    COMPILE, ; COMPILE-ONLY IMMEDIATE
BASE !


META-TODO [IF]
: SLITERAL  DUP LITERAL tPOSTPONE doS"
	    CHARS xhere 2DUP + ALIGNED TOxhere
	    SWAP MOVE ; COMPILE-ONLY IMMEDIATE

: S" [CHAR] " PARSE tPOSTPONE SLITERAL ; COMPILE-ONLY IMMEDIATE
[THEN]

: ."        tPOSTPONE S" tPOSTPONE TYPE ; COMPILE-ONLY IMMEDIATE

META-TODO [IF]
BASE @ DECIMAL \ THROW value
: TO        ' ?call DUP IF          \ should be call-doVALUE
		[']-doVALUE =         \ verify VALUE marker
	  	IF @ STATE @
		     IF tPOSTPONE doTO code, EXIT THEN
		     ! EXIT
		THEN
            THEN -32 THROW ; IMMEDIATE   \ invalid name argument (e.g. TO xxx)
BASE !

[THEN]


BASE @ DECIMAL \ THROW value
: UNTIL     IF -22 THROW THEN  \ control structure mismatch; dest type is 0
	    tPOSTPONE 0branch code, bal- ; COMPILE-ONLY IMMEDIATE
BASE !

: [']       ' tPOSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

: [CHAR]    CHAR tPOSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

CR .(  If)
: IF	." IF"    tPOSTPONE 0branch ." ..IF" xhere 0 code,
	    1 bal+          \ orig type is 1
	    ; COMPILE-ONLY IMMEDIATE

: WHILE     tPOSTPONE IF 2SWAP ; COMPILE-ONLY IMMEDIATE

BASE @ DECIMAL \ THROW value
: THEN      1- IF -22 THROW THEN	\ control structure mismatch
					\ orig type is 1
	    xhere SWAP t! bal- ; COMPILE-ONLY IMMEDIATE
BASE !


: ELSE      tPOSTPONE AHEAD 2SWAP tPOSTPONE THEN ; COMPILE-ONLY IMMEDIATE


META-TODO [IF] \ TODO needs to find the AGAIN and THEN here
: REPEAT    AGAIN THEN ; COMPILE-ONLY IMMEDIATE
[THEN]

: ABORT"    S" tPOSTPONE ROT
	    tPOSTPONE IF tPOSTPONE abort"msg tPOSTPONE 2!
	    -2 tPOSTPONE LITERAL tPOSTPONE THROW
	    tPOSTPONE ELSE tPOSTPONE 2DROP tPOSTPONE THEN
	    ;  COMPILE-ONLY IMMEDIATE


: \ SOURCE >IN ! DROP ; IMMEDIATE

\ (End of immediate words)
PREVIOUS DEFINITIONS \ back to FORTH

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ START of definitions that meta-compiler : depends upon,
\ and the associated variables
ALSO its-words

\ TODO what wordlist should this go into - also, think about how [ should
\ behave
: ]          -1 STATE ! tc-order ;


\   :NONAME     ( -- xt colon-sys )             \ CORE EXT
\		Create an execution token xt, enter compilation state and
\		start the current definition.
\
BASE @ DECIMAL \ THROW value
: :NONAME   bal IF -29 THROW THEN           \ compiler nesting
	[']-doLIST xt, DUP -1
	0 TO notNONAME?  1 TO bal ] ;
BASE !

\   :           ( "<spaces>name" -- colon-sys ) \ CORE
\		Start a new colon definition using next word as its name.
\
: t:	CR ." (hFdefn->"
	TDEF
	:NONAME ROT
	head, -1 TO notNONAME?
	tc-order ;

\ (END of meta-compiler : definition and its dependencies)
PREVIOUS \ back to FORTH

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Restore search order
PREVIOUS SET-ORDER SET-CURRENT

\ TODO needed to have these found during hforth .. give them separate names
\ to remind me I need to fix it.
ALSO its-words
: xF-DEF F-DEF ;
: xS-DEF S-DEF ;
PREVIOUS
