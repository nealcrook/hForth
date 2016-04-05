\ $Id: hmeta_colon.fth,v 1.16 1998/11/25 21:48:57 crook Exp crook $
\ $Log: hmeta_colon.fth,v $
\ Revision 1.16  1998/11/25 21:48:57  crook
\ check-point before starting on STC modifications.
\
\ Revision 1.15  1998/10/08 20:33:54  crook
\ minor tidy-ups; first working asm version
\
\ Revision 1.14  1998/10/03 18:18:25  crook
\ Updates to pack" head, from Wonyong's release of 05-Jan-1998
\
\ Revision 1.13  1998/10/01 00:18:00  crook
\ fixed forward references for ACCEPT bugfix.
\
\ Revision 1.12  1998/09/30 23:56:10  crook
\ first working binary.
\
\ Revision 1.11  1998/09/05 12:07:57  crook
\ move target immediates to a separate file
\
\ Revision 1.10  1998/09/02 20:41:13  crook
\ minor tweaks
\
\ Revision 1.9  1998/09/01 22:26:19  crook
\ fix assembler references to variables
\
\ Revision 1.8  1998/09/01 21:46:27  crook
\ tidy up organisation of immediate words
\
\ Revision 1.10  1998/08/25 00:59:13  crook
\ revamp immediate target words
\
\ Revision 1.9  1998/08/23 23:10:33  crook
\ fix RECURSE, plus many other small fixes
\
\ Revision 1.8  1998/08/22 17:23:05  crook
\ fix TO, other minor tidy-ups
\
\ Revision 1.7  1998/08/20 23:50:35  crook
\ minor tweaks
\
\ Revision 1.6  1998/08/20 22:33:10  crook
\ fixed tPOSTPONE. Many other tidy-ups
\
\ Revision 1.5  1998/08/18 22:14:18  crook
\ many small fixes. POSTPONE is still a mess
\ 0x42 unresolved forward references.
\
\ Revision 1.4  1998/08/17 23:59:47  crook
\ much closer to finishing
\
\ Revision 1.3  1998/06/30 20:35:34  crook
\ add ability to save and restore search order
\
\ Revision 1.2  1998/06/14 18:30:54  crook
\ meta compilation debug. tPOSTPONE still not working, literals not
\ implemented.
\
\ Revision 1.1  1998/06/07 22:51:02  crook
\ Initial revision
\

\ TODO list:
\ 1. rearrange words
\ 2. new "where am I" messages in source
\ 3. put all defn in correct wordlists
\ 4. consistent cpVar, xhere, etc.
\ 5. allow early low-level defn to inhibit later hi-level defn
\ 6. DONE
\ 7. fix TODO things in assembler
\ 8. fix forward defns
\ 9. ? order of immediate words in hforth
\ 10. When are immediate words first needed? Make it as late as possible
\ 11. Tidy up use of wordlists
\ 12. Review and document is when to use nit- and when/whether
\     I really need it- --- it's very confusing at the moment when each are
\     used.
\ 13. DONE
\ 14. Fix forward reference for udebug and test udebugger
\ 15. : dd CATCH -55 THROW ; works on fpe but causes an undefined
\     instruction exception on hForth.
\ 16. Add throw/catch support to assembler
\ 17. add state machine rigor to assembler
\ 18. allow more words to be used immediately eg to allow optional coreext
\     and asmarm to be meta-compiled. This probably means wordlists,
\     environment queries, variables, constants and values, which also
\     means that my initialisation code will need to change and I will
\     need to have a way to emulate target RAM on the host
\ 19. 

\ colon compiler and immediate words for hForth
MARKER *hmc*
HEX

\ hPOSTPONE postpones the compile-time behaviour of words that have been
\ defined on the host. It generates code (in host code space) whose
\ run-time function is dependent upon the function of the word being 
\ postponed.
: hPOSTPONE POSTPONE POSTPONE ; IMMEDIATE

: POSTPONE ." Error: use hPOSTPONE or tPOSTPONE" QUIT ;

: g ." got here" .S ;
: xg ." got here" .S 2DUP DUMP ;
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
\ TODO: is the : defn on the host smart enough to generate an error
\ when a COMPILE-ONLY target word is used interactively? that probably
\ shows up all the deficiencies in the target compiler..
\
\ Words that are used interactively (ie outside of definitions) must be
\ recognised by the interpreter and therefore must be defined in the
\ search order that is current at the prompt (FORTH, if possible). The
\ list is given in the section that describes immediate words for the target
\ system.
\
\ TODO: make target : search wordlist and only add *new* definitions
\ (no redefines) -- or have a flag that switches this behaviour. Then
\ can load all defn that must be low-level then load all optionally
\ low-level primitives then load full set of colon definitions. The
\ existence of a low-level defn will prevent the high-level one from
\ being defined.. might be able to just omit the linklast stage.. saves
\ parsing the comments to look for ";"


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Preserve search order
GET-CURRENT  \ wid of compilation word list
GET-ORDER \ list of wids in the search order

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Create wordlists and associated manipulation words for later use
CHAR " PARSE hforth" ENVIRONMENT?
[IF]
  \ in hForth WORDLIST-NAME associates a string with a WID such that ORDER
  \ can display it.
  WORDLIST WORDLIST-NAME t-words-WORDLIST
  WORDLIST WORDLIST-NAME it-words-WORDLIST
  WORDLIST WORDLIST-NAME its-words-WORDLIST
  WORDLIST WORDLIST-NAME tunresolved-words-WORDLIST
[ELSE]
  WORDLIST CONSTANT t-words-WORDLIST
  WORDLIST CONSTANT it-words-WORDLIST
  WORDLIST CONSTANT its-words-WORDLIST
  WORDLIST CONSTANT tunresolved-words-WORDLIST
[THEN]

\ add a wordlist to the search order; use with ALSO
: t-words GET-ORDER NIP t-words-WORDLIST SWAP SET-ORDER ;
: it-words GET-ORDER NIP it-words-WORDLIST SWAP SET-ORDER ;
: its-words GET-ORDER NIP its-words-WORDLIST SWAP SET-ORDER ;
: tunresolved-words GET-ORDER NIP tunresolved-words-WORDLIST SWAP SET-ORDER ;


\ TODO - ought to use this for CODE words, too?
\ .. code words don't currently change the search order but maybe they
\ should, so that I can find xts within assembler definitions.
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

: tc-order save-order t-words-WORDLIST it-words-WORDLIST 2 SET-ORDER ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Support words required by the immediate words for the target system.
\ also used for support words for t:
\ These go into a private wordlist that is ONLY available when the it-words
\ (and the real target : ???) are compiled
\ That avoids name clash with the host system
\ (t-words is required so that POSTPONE will work)


\ TODO shouldn't need these now.. could find then using t['] except..
\ used in hmeta_colon -- before the words are defined in the target dict.
\ used in "pipe"
: [']-doCREATE 400005B4 ;


ALSO t-words ALSO its-words DEFINITIONS

VARIABLE STATE		FALSE STATE !
VARIABLE rakeVar	0 rakeVar !
VARIABLE current
\ Count of references that are declared as forward referenced and are
\ resolved by FORTDEF's words in the host t-words wordlist
VARIABLE FUNRESOLVED 	0 FUNRESOLVED !
\ Count of references that are looked for in the target image's wordlist
\ but are not found (I *think* these must arise from POSTPONEd words in
\ target definitions).
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
: xhere _CODE ; \ get the current value of the target PC
: TOxhere TO _CODE ; \ used to update the target PC
: 1chars/ ;
: cell- CELLL - ;
: name>xt  cell- cell- @ ;
: lastName _NAME CELL+ CELL+ ;
: IDflushline DROP ;

\ IMMEDIATE and COMPILE-ONLY words defined here are immediate/compile-only
\ on the *host* and therefore use the host's definitions of them.
: COMPILE-ONLY ; \ COMPILE-ONLY doesn't exist on pfe so make a no-op

\   code,       ( x -- )
\               Reserve one cell in code space and store x in it.
\
: code,     
		xhere DUP CELL+ TOxhere t! ;


\ TODO may need to have t versions of these to avoid name-space clashes
\ - hard to detect because host and target are the same in this case.
\ might be worth creating a dummy host version where the alignment is
\ coarser, in order to check for bugs.
: CHARS     CHARR * ;   \ slow, very portable
: ALIGNED   DUP 0 CELLL UM/MOD DROP DUP
            IF CELLL SWAP - THEN + ;    \ slow, very portable


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
: ?call
  DUP t2h @ 0FF000000 AND CALLL =
  \ that call-detection is crude - not an exact check..
  IF DUP DUP t2h @ 00FFFFFF AND    \ it's a branch.. get offset
	DUP 007FFFFF > IF
		00FF000000 OR \ sign extend the offset
	THEN
	2 LSHIFT                      \ convert to byte offset
	+ CELL+ CELL+                 \ fix up for pipeline prefetch
	SWAP CELL+ SWAP EXIT
  THEN 0 ;

\ Simple wordlist stuff.
\ TODO Could re-express $FCODE etc. as FORTH-DEFN $CODE
\ then the FORTH-DEFN could be outside the META-HI conditional.. and apply
\ to both the low-level and the high-level definition.
\ TODO want to express the code definitions using the same mechanism as
\ hForth ultimately does, linking in the name at the *end* of the defn.
\ so that the $FCODE (etc) get far simpler
: mFORTH-WORDLIST _FLINK ;
: NONSTANDARD-WORDLIST _SLINK ;
: ENVIRONMENT-WORDLIST _ENVLINK ;
: mGET-CURRENT current @ ;


DECIMAL

\   pipe        ( -- ) ( R: xt -- )
\               Connect most recently defined word to code following DOES>.
\               Structure of CREATEd word:
\                       | call-doCREATE | 0 or DOES> code addr | a-addr |
\
: pipe      lastName name>xt ?call DUP IF   \ code-addr xt2
                   [']-doCREATE = IF
                   R> SWAP !           \ change DOES> code of CREATEd word
                   EXIT
               THEN THEN
               -32 THROW      \ invalid name argument, no-CREATEd last name
               ; COMPILE-ONLY

\   xt,		( xt1 -- xt2 )
\		Take a run-time word xt1 for :NONAME , CONSTANT , VARIABLE and
\		CREATE . Return xt2 of current definition.
\
\ TODO: need a check to see that the destination is reachable..
BASE @ HEX
: xt,       xhere ALIGNED DUP TOxhere SWAP
               xhere - CELL- CELL- 2 RSHIFT    \ get signed offset
               00FFFFFF AND                   \ mask off high-order sign bits
               CALLL OR                       \ make the opcode 
               xhere swap                      \ remember where it will go
               code, IDflushline ;             \ emit it and purge the block
BASE !

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
: (search-wordlist)
		ROT >R SWAP DUP 0= IF -16 THROW THEN
				\ attempt to use zero-length string as a name
		>R		\ wid  R: ca1 u
\ each time we get a wid must convert it from a target address to a host
\ address. The only other times we access memory we never care about the
\ target address. Do the conversion *after* the check for the end link
		BEGIN  @ 	\ ca2  R: ca1 u
		  DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
		  t2h DUP COUNT [ MASKK ] LITERAL AND R@ = \ ca2 ca2+char f
		  IF R> R@ SWAP DUP >R           \ ca2 ca2+char ca1 u
		    same?                        \ ca2 flag
		\ ELSE DROP -1      \ unnecessary since ca2+char is not 0.
		  THEN
		WHILE cell-             \ pointer to next word in wordlist
		REPEAT
		R> R> 2DROP DUP name>xt SWAP            \ xt ca2
		C@ DUP [ COMPO ] LITERAL AND 0= SWAP
		[ IMMED ] LITERAL AND 0= 2* 1+ ;

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

\ TODO looks like something in here is stopping gforth from working
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
: pack"	OVER MaxCountedString SWAP U<
	IF -18 THROW THEN		\ parsed string overflow
	2DUP SWAP CHARS + CHAR+ DUP >R  \ ca u aa aa+u+1
	ALIGNED cell- 0 SWAP t!		\ fill at end of string
	2DUP tC! CHAR+ SWAP             \ c-addr a-addr+1 u
	CHARS tMOVE R> ; COMPILE-ONLY


\   head,       ( xt "<spaces>name" -- )
\		Parse a word and build a dictionary entry using xt and name.
\
: head,	PARSE-WORD DUP 0=
	IF errWord 2! -16 THROW THEN
		\ attempt to use zero-length string as a name
	DUP MASKK > IF -19 THROW THEN   \ definition name too long
	2DUP mGET-CURRENT mSEARCH-WORDLIST  \ name exist?
	IF DROP ." t-redefine " 2DUP TYPE SPACE THEN \ warn if redefined
	_NAME OVER CHARS CHAR+ -
	DUP ALIGNED SWAP OVER XOR IF cell- THEN \ aligned to low address
	DUP >R pack" DROP R>           \ pack the name in dictionary
	cell- mGET-CURRENT @ OVER t!   \ build wordlist link
	cell- DUP TO _NAME t! ;        \ adjust name space pointer
			               \ and store xt at code field

\ todo temp..
BASE @ HEX
ALSO tunresolved-words DEFINITIONS
S" forward.fth" INCLUDED
PREVIOUS DEFINITIONS
BASE !



\   (')		( "<spaces>name" -- xt 1 | xt -1 )
\		Parse a name, find it and return execution token and
\		-1 or 1 ( IMMEDIATE) if found
\
BASE @ HEX
: (')	PARSE-WORD search-word ?DUP IF NIP EXIT THEN
  \ not found in target dictionary. May be FORTDEF's in host t-words
  \ dictionary, but we can't be sure. Leave a dummy xt.. This should be
  \ fixable by changing the order of definitions in the target source
  \ think this should only arise due to "parsing words" in the source;
  \ POSTPONE etc. since all other words are located by the host searching
  \ t-words automatically

  \ See if it's been defined in the list of target unresolved constants
  tunresolved-words-WORDLIST SEARCH-WORDLIST
  \ 0 or xt 1 or xt -1 .. will never be 1
  IF
    EXECUTE \ convert CONSTANT's xt into the target's xt
  ELSE
    \ still not found.. bump the count and leave a bogus xt
    CR ." >> Target unresolved:"
    SOURCE TYPE
    1 TUNRESOLVED +! DEADBEEF
  THEN -1 ; \ always flag found, non-immediate
BASE !
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
: t'         (') DROP ;


\ Description of POSTPONE:
\ " force the compilation of a word that would normally be executed"
\ " postpone the compile action of the word"
\
\ Consider these example definitions:
\ : 0branch ..... ;
\ : IF POSTPONE 0branch .... ; IMMEDIATE
\ : FRED IF ." true" THEN ;
\ : JANET POSTPONE IF ;
\
\ During the compilation of FRED, IF executes (since it is an immediate
\ word (immediacy as a term is frowned upon by ANS forth.. it is more
\ strictly accurate to say that its compilation semantics are its execution
\ semantics)). If the 0branch has not been POSTPONEd in the definition of IF,
\ the execution of IF would cause the execution of 0branch -- so that 0branch
\ was executed during the definition of FRED -- which is not what we want
\ to happen.
\
\ POSTPONEing 0branch stops it from executing when IF executes; instead,
\ it makes it *compile*. The result is that the code for IF might look
\ something like this:
\ XT-0branch XT-dotquote ...
\
\ That example is of a POSTPONEd non-immediate word. For a POSTPONEd
\ immediate word, consider the definition of JANET, above. If the IF
\ had not been POSTPONEd, it would have executed during the definition of
\ JANET, just as it did in the definition of FRED. By postponing it, we
\ make JANET behave in the same way as IF behaves; the code produced by
\ quoting IF in the definition of JANET is simple the XT of IF. The XT gets
\ compiled into the definition of JANET (as though it was a non-immediate
\ word).
\
\ The Standard specifies that POSTPONE is a parsing word; it parses at
\ compile time and must search the wordlists in effect at that time.
\
\ The current environment is really a cross-meta-compiler: it's running
\ on one system to generate an executable for another system. In this
\ environment, POSTPONEd immediate words run on the host and POSTPONEd
\ non-immediate words run on the host but emit a target XT on the target.
\ That means that we really only need a single POSTPONE in this system
\ but it has to be pretty clever; for an immediate word it must search
\ the host wordlist and for a non-immediate word it must search the
\ target wordlist.. but of course, we don't know whether the word is
\ immediate or non-immediate until we have located it in the wordlist..

: tPOSTPONE
   (') 0< IF \ search the wordlists in the target image
    \ non-immediate word. Generate this code on the host:
    \ [ XTword ] LITERAL tCOMPILE, to spit out this code on the target:
    \ XTword.
    hPOSTPONE LITERAL hPOSTPONE tCOMPILE,
  ELSE
    \ immediate word. Generate this code on the host:
    \ XTword - this must be the *host* XT .. so it's exactly the same
    \ as a normal postpone on the host. In this code, we should never
    \ reach this path, as we have two separate versions of POSTPONE
    \ (which defeats the object of having POSTPONE). Really, we need to
    \ search both the host and the target wordlists depedent upon whether
    \ the word is immediate or not.
    ABORT" tPOSTPONE doesn't know how to POSTPONE an immediate word.."
  THEN ; COMPILE-ONLY IMMEDIATE

\   t[']         Compilation: ( "<spaces>name" -- )      \ CORE
\		Run-time: ( -- xt )
\		Parse name. Return the execution token of name on execution.
\
\ Allow definitions in the its-words worlist to locate the
\ xt of a word from the target dictionary and compile it in a defn that
\ runs in the host dictionary. That's very different from it-['] which
\ spits out stuff in target space
:   t[']       t' hPOSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE


\   rake        ( C: do-sys -- )
\		Gathers LEAVEs.
\
: rake      DUP code, rakeVar @
            BEGIN  2DUP U<
            WHILE  DUP @ xhere ROT !
            REPEAT rakeVar ! DROP
            ?DUP IF                 \ check for ?DO
               1 bal+ hPOSTPONE THEN \ orig type is 1
            THEN bal- ; COMPILE-ONLY


\ (End of support words for immediate words and t:)
PREVIOUS FORTH DEFINITIONS PREVIOUS \ back to FORTH
CR .( Check search order -> ) ORDER

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
\ FUNRESOLVED. A second pass will resolve these references.
\ A word defined by TDEF will be found in the dictionary and will return its
\ xt. Failure to find a word defined by TDEF means, by definition, that the
\ word has been hidden by changing the wordlist. All of this is a lot more
\ work that simply having the definition return an xt, but it means that
\ we implement search order, which is nice. The cost is in compile speed
\ and the additional space that the strings take up in the host space.
ALSO its-words
\ FORTDEF consumes a name from the input stream
BASE @ HEX
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
		DROP tCOMPILE,
	ELSE
		\ not found
		." (Forward definition unresolved)"
		DROP 1 FUNRESOLVED +!
		2DROP DEADBEEF tCOMPILE, \ drop string and compile dummy xt
	THEN ;
BASE !

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
		DROP tCOMPILE,
	ELSE
		CR ." FATAL ERROR"
		CR ." TDEF-ed word not found - target search order must be wrong"
		QUIT
	THEN ;
PREVIOUS \ back to FORTH
CR .( Check search order -> ) ORDER

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Forward definitions needed by POSTPONED words in immediate definitions
\ non-postponed words need equivalents to run on the host, but postponed
\ words only need to return their xt.
\ Although this first set of words is only needed to compile the immediate
\ definitions, it makes sense to put them in t-words, because there may be
\ other forward references to them later in the forth source.
ALSO t-words DEFINITIONS

: h.S .S ;
: hSEE SEE ;
: hGET-ORDER GET-ORDER ;
: hWORDS WORDS ;
: h. . ;
: hxx ." xxxxx" CR ;

\ bug in PFE?? For some reason it does not detect the absence of the
\ definition of - but instead stacks the number 0.

\ markers to show awareness of unresolved forward definitions in hforth source
\ .. this allows the source to compile without errors by creating dummy
\ words in the hosts t-words list. usage: FORTDEF optiCOMPILE
\ ..
\ there are no longer any forward definitions..

PREVIOUS DEFINITIONS \ back to Forth
CR .( Check search order -> ) ORDER


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Restore search order, which has been sitting on the stack since the
\ start of this file.
PREVIOUS SET-ORDER SET-CURRENT
CR .( Check search order -> ) ORDER


\ words to switch the compilation wordlist for target definitions
ALSO its-words
: F-DEF mFORTH-WORDLIST current ! ;
: S-DEF NONSTANDARD-WORDLIST current ! ;
: ENV-DEF ENVIRONMENT-WORDLIST current ! ;
: this-link current @ ;

: ' t' ;


\ TODO not sure where these get compiled to..

\ For many variables or values, want to know the address in RAM at which the
\ data is stored. We know that the structures are:
\ VARIABLE: "bl doConst" followed by a cell containing the address in RAM
\ VALUE: "bl DoValue followed by a cell containing the address in RAM
\ so we can look up the xt in the target dictionary and use this information
\ to extract the address we're after.
STC_MODEL [IF]
: GetVaxAdr   t' ?call
	DUP t['] doCONST =
	SWAP t['] doVALUE = OR	  \ CONST or VALUE marker?
\ STC TODO fixup to make STC work..
DROP -1
	    IF t@ EXIT
	    THEN -32 THROW ; \ invalid name argument
[ELSE]
: GetVaxAdr   t' ?call
	DUP t['] doCONST =
	SWAP t['] doVALUE = OR	  \ CONST or VALUE marker?
	    IF t@ EXIT
	    THEN -32 THROW ; \ invalid name argument
[THEN]

\ (search-wordlist4name)   ( c-addr u wid -- 0 | xt-addr )
\ The xt-addr is a target address in name space
: (search-wordlist4name)
		ROT >R SWAP DUP 0= IF -16 THROW THEN
				\ attempt to use zero-length string as a name
		>R		\ wid  R: ca1 u
\ each time we get a wid must convert it from a target address to a host
\ address. The only other times we access memory we never care about the
\ target address. Do the conversion *after* the check for the end link
		BEGIN  @ 	\ ca2  R: ca1 u
		  DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
		  DUP \ preserve *target* version of the address
                  t2h DUP COUNT [ MASKK ] LITERAL AND R@ = \ ca2 ca2+char f
		  IF R> R@ SWAP DUP >R           \ ca2 ca2+char ca1 u
		    same?                        \ ca2 flag
		\ ELSE 2DROP -1      \ unnecessary since ca2+char is not 0.
		  THEN
		WHILE SWAP DROP \ get rid of the *target* version of address
			cell-     \ pointer to next word in wordlist
		REPEAT
		R> R> 2DROP
		DROP \ get rid of the *host* version of the address
		cell- cell- ;            \ a-xt

\ For some initialisation values we want to find the address of a defn's
\ entry in the name dictionary. The format of the entry is:
\ <xt> <link> <counted string>
\ and we want to return the address of the <xt>. This code is a cut-down
\ version of ' (tick).
\ TODO want to QUIT or THROW if we don't find the word.
: GetNameAdr ( "name" -- 0 | xt-addr )
	PARSE-WORD #order @ DUP		     \ not found if #order is 0
	IF 0
	   DO 2DUP		       \ ca u ca u
	      I CELLS #order CELL+ + @  \ ca u ca u wid
	      (search-wordlist4name)    \ ca u   0 | a-xt
	      ?DUP IF		    \ ca u   0 | a-xt
		 \ found
	         >R 2DROP R> UNLOOP EXIT \ a-xt
	      THEN		       \ ca u
	   LOOP 2DROP 0		        \ 0
	THEN ;

PREVIOUS


\ Set COMPILE bit of most recent definition
: hCOMPILE-ONLY ;
: COMPILE-ONLY _NAME CELLL 2* + DUP tC@ COMPO OR SWAP tC! ;
\ Set IMMEDIATE bit of most recent definition
: hIMMEDIATE IMMEDIATE ;
: IMMEDIATE    _NAME CELLL 2* + DUP tC@ IMMED OR SWAP tC! ;


DECIMAL
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ START of definitions that meta-compiler : depends upon,
\ and the associated variables
ALSO its-words

\ TODO what wordlist should this go into - also, think about how [ should
\ behave
: ]          -1 STATE ! tc-order ;


CR .( >> TODO UNRESOLVED --  temp to resolve forward defn. in hmeta_colon )
BASE @ HEX
\ TODO these are forward definitions of XTs for the $VAR $CONST $VALUE $USER
\ definitions..
\ .. could use the normal forward defn routine IFF t['] was findable at
\ the time
STC_MODEL [IF]
\ STC TODO -- these are accurate for the moment but any code changes could
\ change them and nit-TO is bodged to not detect the problem..
4000065C hCONSTANT doVALUE
40000650 hCONSTANT doCONST
40000690 hCONSTANT doUSER
[ELSE]
400005A4 hCONSTANT doVALUE
40000598 hCONSTANT doCONST
400005DC hCONSTANT doUSER
[THEN]
BASE !



\   :NONAME     ( -- xt colon-sys )             \ CORE EXT
\		Create an execution token xt, enter compilation state and
\		start the current definition.
\
STC_MODEL [IF]
\ for STC there should be no doLIST so won't need xt, here..
\ TODO need a nicer way of extracting xt than using _CODE.
: :NONAME   bal IF -29 THROW THEN           \ compiler nesting
	_CODE \ xt for new definition
	COLON-ENTER code, DUP -1
	0 TO notNONAME?  1 TO bal ] ;
[ELSE]
: :NONAME   bal IF -29 THROW THEN           \ compiler nesting
	t['] doLIST xt, DUP -1
	0 TO notNONAME?  1 TO bal ] ;
[THEN]

\   :           ( "<spaces>name" -- colon-sys ) \ CORE
\		Start a new colon definition using next word as its name.
\
: t:	TDEF
	:NONAME ROT \ :NONAME sets the compilation order
	head, -1 TO notNONAME? ;

\ (END of meta-compiler : definition and its dependencies)
PREVIOUS \ back to FORTH
CR .( Check search order -> ) ORDER

HEX
