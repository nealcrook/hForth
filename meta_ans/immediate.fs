\ immediate.fs - words that are used within target definitions
\
\ This file contains words that would normally be immediate
\ -- words that are used within definitions. For a cross-compiler,
\ these words execute to build data structures in the target
\ dictionary for looping constructs etc. These words all go
\ into the COMPILER wordlist. The following words are defined
\ herein, together with their factors
\
\ TODO this should be called compile.fs -- words that can be used
\ whilst compiling for the target (what would normally be immediate
\ words.. words that execute during target definitions.)
\

\ TODO revamp, update and put in order.

\ Immediate words for target system
\ For hForth, these are:
\ -. ; AGAIN AHEAD IF LITERAL THEN [ ( +LOOP ." ABORT" BEGIN
\ DO DOES> ELSE LEAVE LOOP POSTPONE RECURSE REPEAT SLITERAL S" TO UNTIL
\ WHILE ['] [CHAR] \
\
\ However, of these, some are not needed:
\ -. only exists to catch an error in hForth that is detected in some other
\    way in gForth)
\ \  the function of this can simply be left to the host's definition
\ (  the function of this can simply be left to the host's definition


base @ decimal


\ TODO hacks to make it compile


: tPOSTPONE
   (') 0< IF \ search the wordlists in the target image
    \ non-immediate word. Generate this code on the host:
    \ [ XTword ] LITERAL tCOMPILE, to spit out this code on the target:
    \ XTword.
    POSTPONE LITERAL POSTPONE tCOMPILE,
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



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Factors


\ TODO devamp
\ In addition to this, the following words are used in the interpreter
\ and affect the compilation in the target:
\ : <wordlist control> IMMEDIATE COMPILE-ONLY VALUE CONSTANT VARIABLE ]
\ Since they must be available in the interpreter, they are all defined
\ right at the end of this file, to avoid conflict with host definitions
\ by the same name (TODO the exceptions at the moment are the : definition,
\ and the last three; which are handled as special cases in the main source
\ but must ultimately be supported to allow the supplementary files to be
\ included.
\
\ You need to understand the difference between tPOSTPONE and hPOSTPONE to
\ understand why both are used in this subsequent section of code.


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

: x-BEGIN
    xhere 0 bal+            \ dest type is 0
;

: x-DO
    0 rakeVar !  0		   \ ?DO-orig is 0 for DO
    tPOSTPONE doDO xhere  bal+     \ DO-dest
;

: x-UNTIL
    IF -22 THROW THEN  \ control structure mismatch; dest type is 0
    xt-0branch tCOMPILE, code, bal- ;

: x-LITERAL
\ Generate a code sequence in target space that has the run-time effect
\ of putting a value on the stack. The code sequence is:
\ <dolit> <value from stack>
    xt-doLIT tCOMPILE, tCOMPILE, ;

\ TODO this also exists in the define.fs ...
\ TODO try the POSTPONE definition.
\ parse the next word, and look in the target dictionary to find its xt.
\ push the xt onto the stack then generate code to emit it in the target
\ code stream
: x-['] x-' x-LITERAL ;

: pipe          ( -- ) ( R: xt -- )
\               Connect most recently defined word to code following DOES>.
\               Structure of CREATEd word:
\                       | call-doCREATE | 0 or DOES> code addr | a-addr |
   lastName name>xt ?call DUP IF   \ code-addr xt2
       xt-doCREATE = IF
	   R> SWAP !           \ change DOES> code of CREATEd word
	   EXIT
       THEN THEN
   -32 THROW      \ invalid name argument, no-CREATEd last name
; COMPILE-ONLY

: x-DOES>
    bal 1- IF -22 THROW THEN        \ control structure mismatch
    NIP 1+ IF -22 THROW THEN        \ colon-sys type is -1
    \ TODO that was tPOSTPONE but it looked wrong.. and was never tested by
    \ old metacompiler..
    POSTPONE pipe xt-doLIST xt, -1 ;

: x-LEAVE
    tPOSTPONE UNLOOP tPOSTPONE branch
    xhere rakeVar DUP @ code, ! ;

: x-RECURSE
    bal 1- 2* PICK 1+ IF -22 THROW THEN
    \ control structure mismatch; colon-sys type is -1
    bal 1- 2* 1+ PICK       \ xt of current definition
    tCOMPILE,
;

: x-IF
    xt-0branch tCOMPILE, xhere 0 code,
    1 bal+          \ orig type is 1
;

: x-THEN
    1- IF -22 THROW THEN	\ control structure mismatch
    \ .. check that orig type was 1
    xhere SWAP t! bal-
;

: rake          ( C: do-sys -- )
\		Gathers LEAVEs.
   DUP code, rakeVar @
   BEGIN  2DUP U<
   WHILE  DUP @ xhere ROT !
   REPEAT rakeVar ! DROP
   ?DUP IF                 \ check for ?DO
       1 bal+ POSTPONE x-THEN \ orig type is 1
THEN bal-
;

: x-LOOP
    xt-doLOOP tCOMPILE, rake ;

: x-+LOOP
    xt-do+LOOP tCOMPILE, rake ;


: x-AHEAD
    xt-branch tCOMPILE, xhere 0 code,
    1 bal+          \ orig type is 1
;

: x-ELSE
    POSTPONE x-AHEAD 2SWAP POSTPONE x-THEN
;

: x-AGAIN
    IF -22 THROW THEN  \ control structure mismatch; dest type is 0
    xt-branch tCOMPILE, code, bal- ;

: x-REPEAT
    POSTPONE x-AGAIN POSTPONE x-THEN
;

: x-WHILE
    POSTPONE x-IF 2SWAP
;

\ Copy a string into target space along with the words that are needed
\ at run-time in order to print it.
: x-SLITERAL
    DUP x-LITERAL tPOSTPONE doS"
\ since LITERAL is immediate, tPOSTPONE LITERAL would emit LITERAL
\ so we short-circuit this process by using nit-LITERAL. Since doS" is non-imm,
\ tPOSTPONE doS" should emit doLIT doS" COMPILE,
    CHARS xhere 2DUP + ALIGNED TOxhere
    SWAP tMOVE
;

: x-S"
    [CHAR] " PARSE x-SLITERAL
;

: x-."
    x-S" tPOSTPONE TYPE
;

: x-TO
\ search target dictionary for name and examine the code that makes up
\ the definition to make sure that it is a VALUE.
    x-' ?call DUP IF          \ should be call-doVALUE
	xt-doVALUE =         \ verify VALUE marker
	IF x-@ tSTATE @
	    IF xt-doTO tCOMPILE, code, EXIT THEN
	    x-! EXIT
	THEN
    THEN -32 THROW \ invalid name argument (e.g. TO xxx)
;

: x-[CHAR]
\ parse the next word and push the character code for its first character
\ onto the stack, then emit the character code in the code stream
    CHAR x-LITERAL
;

: x-POSTPONE
\ " force the compilation of a word that would normally be executed"
\ " postpone the compile action of the word"
\ parse the next word in the input stream and look for it in the target
\ dictionary. It needs to be found, since a forward reference cannot save
\ us. When found, determine whether it is immediate or non-immediate and
\ based on this knowledge, emit the code sequence that will have the run-time
\ effect of executing the word.
    (') 0< IF \ search the wordlists in the target image
	\ non-immediate word. Make the target definition spit out the XT by
	\ generating this code in the target code space:
	\ <dolit> <xt> <compile,>
	xt-doLIT tCOMPILE, tCOMPILE,
	x-['] COMPILE,   \ non-IMMEDIATE
    THEN
    \ IMMEDIATE word - just spit out the xt in the target code space
    tCOMPILE, \ IMMEDIATE
;

\ TODO that looks dodgy.. (maybe OK now..)
: x-ABORT"
    x-S" tPOSTPONE ROT x-IF tPOSTPONE abort"msg
    tPOSTPONE 2! -2 x-LITERAL tPOSTPONE THROW x-ELSE
    tPOSTPONE 2DROP x-THEN
;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Misc. supporting definitions

: N#
    \ take a number off the stack and create an immediate word whose action is
    \ to parse the input buffer and extract a number. The word D# n has the
    \ same effect as [ n ] LITERAL - which is normally redundant, but is needed
    \ when using the interpreter to target compile.
    \ Additional twist: it is target-state dependant, allowing it to be used
    \ to push a number onto the stack OR to generate a literal
    \ TODO fix up those words...
    CREATE , IMMEDIATE
DOES>
    BASE @ >R @ BASE ! \ change BASE to value in defined word's para field
    0 0 \ >NUMBER accumulates a DOUBLE
    BL PARSE     \  TODO used to use   PARSE-WORD but cannot find it yet
    \ handle the possibility of -ve numbers..
    OVER C@ [CHAR] - = DUP >R
    IF
	1 - SWAP CHAR+ SWAP \ skip leading - but remember we had it
    THEN
    >NUMBER
    IF
	\ one or more characters were unconverted
	SOURCE TYPE ABORT" -- unable to convert the number"
    THEN
    2DROP \ string address and the high part of the double number
    \ (assumed to be 0..)
    R> IF NEGATE THEN R> BASE !
    \ if compiling for the target, do a literal otherwise just leave on stack
    tSTATE @ IF x-LITERAL THEN
;
16 N# H#
10 N# D#

    
\ TODO put them all in the correct wordlist..

\ Leave politely
base !


\ notes on POSTPONE
\ there are potentially 4 versions
\ POSTPONE
\ hPOSTPONE
\ tPOSTPONE
\ x-POSTPONE 
\
\ need to think about what any of them need to do and whether any
\ of them are (as I suspect) equivalent.
\
\ POSTPONE = hPOSTPONE
\
\ immediate, compile-only parsing word. Executes on the host. Used
\ in a definition in the host dictionary to search words in the host
\ dictionary.
\
\ : foo ... POSTPONE bar ;                 : blob .... foo ;
\
\ bar is defined on the host. POSTPONE compiles code into foo so
\ that the run-time effect of foo includes gluing the semantics
\ of bar into the current definition (blob).
\
\ tPOSTPONE
\
\ immediate, (target?-) compile-only on host. Executes on the host.
\ Used in a definition in the host dictionary, to search words in
\ the target dictionary.
\
\ : foo ... POSTPONE bar ;                : blob .... foo ;
\
\ bar is defined on the target. POSTPONE compiles code into foo
\ so that the run-time effects of foo includes gluing the semantics
\ of bar into the current definition (blob).
\
\
\ ?? is tPOSTPONE the same as x-POSTPONE?
\
\ The std says: 
\
\ Extend the semantics of POSTPONE when encountered in a COMPILER
\ definition to search for <name> only among COMPILER and TARGET
\ definitions and to append the compilation behaviour of <name>
\ to the current COMPILER definition.
\ An ambiguous condition exists if POSTPONE is used in a TARGET
\ definition.
\
\ ?? not clear what they mean by the "compilation behaviour"
\ ?? why does it need to search COMPILER definitions? I would
\ expect it only to search TARGET definitions
\
\ .. I think that I want/need to support POSTPONE in a TARGET
\ definition, so I will have a target-state-smart word, and
\ therefore, effectively, two behaviours.
\
\ Example:

TARGET
: foo IF blah THEN ; IMMEDIATE

needs to produce the code:

call doLIST
xt-0branch   ---+
destination     | if false
xt-blah         |
xt-exit      <--+

Therefore, IF will be defined thus:

COMPILER
: IF tPOSTPONE 0branch xhere 0 code, ;

which, if the host is a DTC Forth, would look something like this

call doLIST
xt-doLIT    
xt-0branch  \ target xt
xt-compile, \ the version to compile onto the target
xt-xhere    \ stack the target's value of HERE
xt-doLIT
0
xt-code,    \ to compile onto the target
xt-exit

\ TODO what would that all look like if the postponed word
\ was immediate.. or can that never occur? Think it indicates
\ an error.

\ That behaviour of tPOSTPONE is correct for x-POSTPONE in
\ tSTATE=interpret modes.
\ When POSTPONE is used in a target definition, it needs 
\ to search target definitions and lay down code in the
\ current target definition. Do a worked example of this..


