\ cross.fs - an attempt at implementing the proposed ANS Forth
\ cross-compiler wordset, using word-lists to implement scopes.
\
\ The wordset can be divided into two parts:
\ - words that are independent of the target
\ - words that are dependent on characteristics of the target
\
\ This file contains definitions for the former set, and therefore
\ excludes the following:
\
\ CELL+ CELLS CHAR+ CHARS ALIGN
\ VARIABLE 2VARIABLE CVARIABLE VALUE
\ EQU ' ['] CREATE DOES> POSTPONE COMPILE,
\ IMMEDIATE
\
\ This file also includes a few non-standard utility words:
\ SECTIONS ( -- ) \ list all sections
\ .SCOPE ( -- )    \ display the current scope
\ SECTION2DISK ( c-addr u -- ) \ save active section to disk as binary file
\
\ All the words that would have name-space clash with standard words are named
\ with an x- prefix (for "cross"). At the end they get aliased into the
\ correct wordlists with their correct names. This also allows us to retain
\ the host system's default search list and compilation order through the
\ whole process.

base @ decimal

\ TODO preserve search order?


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ SECTION-related words

\ sect-a is the address of a section. Up to 3 sections are current at any
\ time; one each of CDATA, IDATA, UDATA. There is a linked list of sections
\ of each section type. Adding a section adds it to the head of its list
\ (Addressed by sect-table). The oldest section is at the tail of its list
\ and has a link address of 0. The sect-a here is zero if no section of that
\ type is defined. The linked list is used by SECTIONS and for searching
\ sections of a given type in order to do a target address->host address
\ translation. The list could have been relinked in order to make the
\ current section the head of its list, but that makes it necessary to store
\ more information for SAVE-SECTIONS, so a separate offset is stored in
\ sect-table instead.

create sect-table 0 , \ 0 offset to current section of current section type
                  0 , \ 1 offset to section used by VARIABLEs
		      \   `4' in either of the above specifies CDATA
		      \   `8' specifies IDATA and `C' specifies UDATA
                  0 , \ 2 sect-a of current CDATA section
		  0 , \ 3 sect-a of current IDATA section
		  0 , \ 4 sect-a of current UDATA section
                  0 , \ 5 sect-a of CDATA linked list
		  0 , \ 6 sect-a of IDATA linked list
		  0 , \ 7 sect-a of UDATA linked list

\ TODO may not need all of these any more...
\ Assert that the current section is of a sertain type; ABORT if not
: *DATA? ( -- ) sect-table @ 0 =                           ABORT" cross: No active section" ;
: CDATA? ( -- ) *DATA? sect-table @ [ 2 cells ] literal <> ABORT" cross: Expected CData active section" ;
: IDATA? ( -- ) *DATA? sect-table @ [ 3 cells ] literal <> ABORT" cross: Expected IData active section" ;
: UDATA? ( -- ) *DATA? sect-table @ [ 4 cells ] literal <> ABORT" cross: Expected UData active section" ;
: ICDATA? ( -- ) *DATA? sect-table @ [ 4 cells ] literal = ABORT" cross: Expected IData or CData active section" ;
: IUDATA? ( -- ) *DATA? sect-table @ [ 2 cells ] literal = ABORT" cross: Expected IData or UData active section" ;

: CDATA ( -- ) [ 2 cells ] literal sect-table ! ;
: IDATA ( -- ) [ 3 cells ] literal sect-table ! ;
: UDATA ( -- ) [ 4 cells ] literal sect-table ! ;

: VARIABLES ( -- )
    \ set active section as the type that will be used to allocate variables from
    sect-table dup @ IUDATA?
    swap cell+ !
;

: SAVE-SECTIONS ( -- i*j )
    \ Save the section context on the stack. Want to be able
    \ to go SAVE-SECTIONS <define new section> RESTORE-SECTIONS
    \ therefore, save the section offsets but do *not* save the
    \ linked list headers
    sect-table dup [ 5 cells ] literal + swap do
	i @
    [ 1 cells ] literal +loop
;

: RESTORE-SECTIONS ( j*i -- )
    \ Restore the section context from the stack.
    \ note the non-symmetrical behaviour of do..+loop for -ve counts
    sect-table dup [ 4 cells ] literal + do
	i !
    [ 1 cells negate ] literal +loop
;

: asa@ ( -- x )
    \ active section address fetch - lightweight version of
    \ SAVE-SECTIONS; just preserves the active section
    sect-table @
;

: asa! ( x -- )
    sect-table !
;

: swap-head ( addr1 addr2 -- )
    \ addr1 - stores head of new section
    \ addr2 - stores head of old section
    \ tmp <- [addr2] ; [addr2] <- addr1 ; [addr1] <- tmp
    dup @ >r
    over swap !
    r> swap !
;

: gaas ( -- a-sect-a )
    \ get address of active section
    \ a-sect-a holds the sect-a of the active section
    sect-table dup @ +
;

: govs ( -- sect-off )
    \ get offset of VARIABLES section
    sect-table cell+ @
;

: SECTION ( addr1 addr2 "<spaces>name" -- )
    \ Create a section of the current type. CDATA and IDATA both allot
    \ corresponding space on the host. Storing section type in a section
    \ is not strictly necessary but means that all section types can
    \ share a single DOES> action.
    *DATA? swap 2dup - 1+
    dup 0< ABORT" cross: Attempt to create a section of negative size"
    CR ." Creating a section of " dup . ." bytes."
    sect-table @ [ 4 cells ] literal = IF
	drop 0 \ UData - dummy host address of 0
    ELSE
	align HERE swap allot
    THEN
    CREATE
    \ TODO following code is clumsy.. tidy it up
    HERE gaas ! \ make this the current section of this type
    HERE [ 1 cells ] literal allot \ reserve cell for link
    gaas [ 3 cells ] literal + swap-head \ link to list
                   \ v--- offset in CELLS
                   \ 0 link to next section of this type
    ,              \ 1 address of start of host image of this section
    sect-table @ , \ 2 section type                       
    2dup ,         \ 3 start target address
    ,              \ 4 end target address
    ,              \ 5 HERE pointer
    1+ ,           \ 6 THERE pointer (allocate from end of section)
DOES>
    \ make this the current section of the current section type
    DUP [ 2 CELLS ] LITERAL + @ \ sect-a, type-o/s
    dup sect-table ! \ set this type as the active type
    sect-table + ! \ set this section as the current section of this type
;

: x-HERE ( -- addr )
    \ Return the value of the next available target address in the current
    \ section of the current section type
    gaas @ [ 5 CELLS ] LITERAL + @
;

: THERE ( -- addr )
    \ Return the value of the next *un*available target address at the *top*
    \ of the current section of the current section type
    \ In contrast to HERE, where typical usage is: n HERE ALLOT
    \ typical usage of THERE is: n TALLOT THERE
    gaas @ [ 6 CELLS ] LITERAL + @
;

: .sect ( sect-a -- )
    \ Show the section type; as (foo) if the section is inactive, foo if
    \ it is active.
    \ an address uniquely identifies a section regardless of type
    dup IF
	dup [ 2 cells ] literal + @ \ sect-a type-os
	sect-table + @ over = >r
	[ 2 cells ] literal + @ \ TODO tidy that up..
	r@ IF ."  " ELSE ." (" THEN
	dup [ 2 cells ] literal = IF ." C" THEN
	dup [ 3 cells ] literal = IF ." I" THEN
	[ 4 cells ] literal = IF ." U" THEN
	r> IF ." Data " ELSE ." Data)" THEN
    ELSE
	."  ????? "
    THEN
;

: .sect-chain ( sect-a -- )
    \ go through each section in this list, displaying its details
    begin
	@ dup \ if 0 we reached the tail and want to fall through
    while
	\ addr of current section
	dup .sect
	cell+ dup @ 10 u.r \ host address
	cell+              \ step past section type
	cell+ dup @ 10 u.r \ start ta
	cell+ dup @ 10 u.r \ end ta
	cell+ dup @ 10 u.r \ here
	cell+ dup @ 10 u.r \ there
	[ 6 cells ] literal - 	\ finish with addr unchanged; chain thru it to the next section
	CR
    repeat drop
;

: SECTIONS ( -- )
    \ Show statistics on all defined sections.
    CR ."  Type        Base     Start       End      Here     There"
    CR ." ---------------------------------------------------------" CR
    sect-table [ 5 cells ] literal + \ address of CData list
    dup       .sect-chain \ Cdata sections
    cell+ dup .sect-chain \ Idata sections
    cell+     .sect-chain \ UData sections
    ." ---------------------------------------------------------"
    CR gaas @ .sect ." is the active section, VARIABLEs use"
    sect-table dup cell+ @ + @ .sect CR
;

: file-ok
    \ Check to see if a file operation completed successfully. If it didn't,
    \ fail ungracefully..
    DUP 0= IF DROP ELSE CR ." Cross: File operation failed with error code " .
	ABORT THEN ;

: SECTION2DISK ( addr u -- )
    \ Save the active section to disk as a binary image
    \ Usage: s" name.bin" section2disk  
    R/W BIN CREATE-FILE file-ok >R \ r: file handle
    gaas @ \ sect-a
    dup cell+ @ swap \ host-image-a sect-a
    [ 4 cells ] LITERAL + dup @ swap [ 1 cells ] LITERAL - @ - 1+ \ start-a size
    R@ WRITE-FILE file-ok
    R> CLOSE-FILE file-ok ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ memory-access words

: ta2ha-p ( c-taddr sect-a -- c-haddr TRUE | sect-a FALSE )
    \ convert target address in the section selected by sect-a
    \ into an address in host space.
    \ check it's >= start address and < end address
    \ then subtract the start address and add the host
    >r \ c-addr1 r: sect-a
    dup r@ [ 3 cells ] literal + @
    r@ [ 4 cells ] literal + @ 1+ within
    IF
	r@ cell+ @ r> [ 3 cells ] literal + @ - + -1
    ELSE
	DROP r> 0
    THEN
;

: sta2ha-p ( c-taddr a-sect-a -- c-haddr true | c-taddr false )
    \ searching target address to host address conversion primitive
    swap >r BEGIN
	@ dup IF
	    r@ swap ta2ha-p ?dup \ haddr true true | a-sect-a' false
	ELSE
	    \ end of the list of sections and not found
	    r@ swap -1 \ c-taddr false true
	THEN
    UNTIL r> drop
;

: ta2ha ( c-taddr sect-off -- c-haddr )
    \ convert target address in the section selected by sect-off
    \ into an address in host space. ABORT if address out of range
    sect-table + @ ta2ha-p
    invert ABORT" cross: Address out of range for that section"
;

: sta2hac ( c-taddr -- c-haddr )
    \ search all CData sections for a conversion from c-taddr to c-haddr
    \ ABORT if no translation found
    sect-table [ 5 cells ] literal + sta2ha-p
    invert ABORT" cross: Address not found in any CData section"
;

: sta2ha ( c-taddr -- c-haddr )
    \ search all IData then all CData sections for a conversion from c-taddr to
    \ c-haddr. ABORT if no translation found
    sect-table [ 6 cells ] literal + sta2ha-p \ search IData sections
    IF exit THEN
    sect-table [ 5 cells ] literal + sta2ha-p \ search CData sections
    invert ABORT" cross: Address not found in any IData or CData section"
;

: rez ( n1 sect-off -- a-addr )
    \ factor of ALLOT. Reserve n1 bytes in the section whose offset is
    \ sect-off (Offsets are in the form 0*cell 1*cell 2*cell)
    \ ABORT if insufficient space, otherwise allocate space and return
    \ the first target address, a-addr in the allocated space.
    \ Note: does not check whether n2 is legal or whether a section
    \ of that type is current.
    sect-table + @ \ n1 sect-a
    [ 5 cells ] literal + dup >r @ + \ new-here r: a-here
    dup r@ cell+ @ \ new-here new-here there r: a-here
    \ here -> address to be allocated, there -> unavailable address
    \ => exactly full iff (here = there), overflow iff (here > there)
    u> ABORT" cross: Insufficient space for allocation"
    r> dup @ >r
    ! r>
;

: trez ( n1 sect-off -- a-addr )
    \ factor of TALLOT. Reserve n1 bytes from the top downwards in the section
    \ whose offset is sect-off (Offsets are in the form 0*cell 1*cell 2*cell)
    \ ABORT if insufficient space, otherwise allocate space and return
    \ the first (smallest) target address, a-addr in the allocated space.
    \ Note: does not check whether n2 is legal or whether a section
    \ of that type is current.
    sect-table + @ \ n1 sect-a
    [ 6 cells ] literal + dup >r @ SWAP - \ new-there r: a-there
    dup r@ cell- @ \ new-there new-there here r: a-there
    \ here -> address to be allocated, there -> unavailable address
    \ => exactly full iff (here = there), overflow iff (here > there)
    swap u> ABORT" cross: Insufficient space for allocation"
    r> dup @ >r
    ! r>
;

: x-ALLOT ( n -- )
    \ allocate n bytes in current section of the current section type
    sect-table @ rez drop
;

: TALLOT ( n -- )
    \ allocate n bytes from the top downwards in current section of the
    \ current section type
    sect-table @ trez drop
;

: RESERVE ( n -- addr )
    \ like ALLOT but explicitly works on UDATA and returns the start
    \ address of the reserved region
    [ 4 cells ] literal rez
;

: BUFFER: ( n "<spaces> name" -- )
    RESERVE create ,
DOES> @
;

: ORG ( addr -- )
    \ set HERE to addr for the current section of the current
    \ section type
    dup
    sect-table @ ta2ha drop \ ABORT if addr not in section
    gaas @ [ 5 cells ] literal + !
;

\ TODO Spec. needs clarification
\ For FILL BLANK ERASE c-addr is a target address in the active
\ section, which must be either IDATA or CDATA.
\ For MOVE CMOVE CMOVE> addr1 is a host address and addr2 is a
\ target address in a defined IData or CData section.
\ For CMOVEC addr1 is a target address in a defined IData or CData section
\ and addr2 is a target address in a defined CData section.
\
\ Note: FILL, BLANK, ERASE all check that their start address is
\ legal but do NOT check for possible overrun.
: usta2ha ( c-taddr x -- c-haddr x)
    \ Attempt to convert a target address from a currently defined IData or
    \ CData section to a host address. Leave x unchanged
    SWAP sta2ha SWAP
;

: x-FILL   ( c-addr u char -- ) >R usta2ha R> FILL ;
: x-BLANK  ( c-addr u -- )      usta2ha BLANK ;
: x-ERASE  ( c-addr u -- )      usta2ha ERASE ;

: CMOVEC ( c-addr1 c-addr2 u -- )
    >r usta2ha >r \ look for c-addr1 in IData then CData
    r> sta2hac >r \ look for c-addr2 in CData
    CMOVE
;

: x-MOVE   ( addr1 addr2 u -- ) usta2ha MOVE ;
: x-CMOVE  ( addr1 addr2 u -- ) usta2ha CMOVE ;
: x-CMOVE> ( addr1 addr2 u -- ) usta2ha CMOVE> ;


: x-, ( x -- )
    \ Store x at the next available cell location in the active section
    ICDATA? 
    [ 1 cells ] literal sect-table @ rez
    sect-table @ ta2ha !
;

: x-C, ( c -- )
    \ Compile c at the next available char location in the active section
    ICDATA? 
    [ 1 chars ] literal sect-table @ rez
    sect-table @ ta2ha c!
;

\ These always access CData or ABORT in the attempt
\ The ! words are not in the standard but are useful..
: @C ( a-addr -- x )     sta2hac @ ;
: C@C ( c-addr -- char ) sta2hac C@ ;
: !C ( x a-addr -- )     sta2hac ! ;
: C!C ( char c-addr -- ) sta2hac C! ;

\ These always access IData or CData or ABORT in the attempt
: x-@ ( a-addr -- x )      sta2ha @ ;
: x-C@ ( c-addr -- char )  sta2ha C@ ;
: x-! ( x a-addr -- )      sta2ha ! ;
: x-C! ( x a-addr -- )     sta2ha C! ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Set up the search orders for the different scopes.

\ words common to all scopes - including the scope words themselves.
vocabulary *hict
\ words specific to each scope
vocabulary *interp
vocabulary *comp
vocabulary *target

-1 value cross-scope \ only used for informational/debug purposes.

also *hict definitions previous

: HOST
    0 to cross-scope only also forth also *hict
    also forth definitions previous ;

: INTERPRETER
    1 to cross-scope only also forth also *hict also *interp
    definitions ;

: COMPILER
    2 to cross-scope only also forth also *hict
    also *comp definitions previous ;

: TARGET
    3 to cross-scope only also *hict also *interp
    also *target definitions previous ;

: .SCOPE
    CR ." cross: Current scope is "
    cross-scope 0 = IF ." HOST " CR EXIT THEN
    cross-scope 1 = IF ." INTERPRETER" CR EXIT THEN
    cross-scope 2 = IF ." COMPILER" CR EXIT THEN
    cross-scope 3 = IF ." TARGET" CR EXIT THEN
    ." UNKNOWN scope"
;

\ Alias a few standard words into this wordset so that they
\ are always available. This might be  *slightly* dangerous
\ in the TARGET scope, but should be quite safe elsewhere.
' SECTIONS ALIAS SECTIONS
' ORDER ALIAS ORDER
' SEE ALIAS SEE
' .S ALIAS .S
' WORDS ALIAS WORDS
' dbg ALIAS dbg
' BYE ALIAS BYE
\ TODO move this and ( from immediate.. make sure the target can still tick the correct version of \ (
' \ ALIAS \                                    IMMEDIATE
' ( ALIAS (                                    IMMEDIATE
definitions \ back to Forth.

\ always need *hict to be ahead of forth-wordlist else might find a host
\ word called COMPILER etc.
also *hict


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Alias standard words into the appropriate wordlists

\ these words can be used whilst interpreting for the target
also *interp definitions previous
' CDATA ALIAS CDATA
' IDATA ALIAS IDATA
' UDATA ALIAS UDATA
' VARIABLES ALIAS VARIABLES \ TODO will this work with my home-made defn?
' SAVE-SECTIONS ALIAS SAVE-SECTIONS
' RESTORE-SECTIONS ALIAS RESTORE-SECTIONS
' SECTION ALIAS SECTION
' x-HERE ALIAS HERE
' THERE ALIAS THERE
' x-ALLOT ALIAS ALLOT
' TALLOT ALIAS TALLOT
' RESERVE ALIAS RESERVE
' ORG ALIAS ORG
' x-FILL ALIAS FILL
' x-BLANK ALIAS BLANK
' x-ERASE ALIAS ERASE
' CMOVEC ALIAS CMOVEC
' x-MOVE ALIAS MOVE
' x-CMOVE ALIAS CMOVE
' x-CMOVE> ALIAS CMOVE>
' x-, ALIAS ,
' x-C, ALIAS C,
' @C ALIAS @C
' C@C ALIAS C@C
' x-@ ALIAS @
' x-C@ ALIAS C@
' x-! ALIAS !
' x-C! ALIAS C!
also Forth definitions previous

\ Leave politely
base !
CR ." Depth=" depth . ." at end of cross.fs" CR
