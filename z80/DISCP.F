\ Listing in "Towards a Discipline of ANS Forth Programming"
\ Originally published in Forth Dimensions XVIII, No.4, pp5-14
\ Adapted to hForth v0.9.9 by Wonyong Koh
\   An ANS compliance problem in v0.9.7 is fixed now.
\   'dest+' should not be necessary.

\ Dijkstra Guarded Command Control Structures
\ M. Edward Borasky
\ 03-AUG-96
\
\ Environmental dependencies:
\
\ Requires AGAIN from the CORE EXT word set
\ Requires AHEAD from the TOOLS EXT word set
\ Requires CS-PICK from the TOOLS EXT word set
\ Requires CS-ROLL from the TOOLS EXT word set
\ Requires PICK from the CORE EXT word set
\ Requires ROLL from the CORE EXT word set
\ Requires THROW from the EXCEPTION word set
\ Requires hForth word COMPILE-ONLY or equivalent
\ Requires .( from CORE EXT word set (test sequence only)

\ hForth has the capability to flag a word COMPILE-ONLY.  On
\ other systems, COMPILE-ONLY can be ignored by defining it as
\ follows:

BL WORD COMPILE-ONLY FIND NIP 0= [IF]
	: COMPILE-ONLY ;
[THEN]

: {IF \ start a conditional
	( -- 0 )

	0 \ put counter on stack
; COMPILE-ONLY IMMEDIATE

: IF> \ right-arrow for {IF ... FI}
	( count -- count+1 )
	( C: -- orig1 )

	1+ >R \ increment and save count
	POSTPONE IF \ create orig1
	R> \ restore count
; COMPILE-ONLY IMMEDIATE

: |IF| \ bar for {IF ... FI}
	( count -- count )
	( C: orig ... orig1 -- orig ... orig2 )

	>R \ save count
	POSTPONE AHEAD \ new orig
	1 CS-ROLL \ old orig to top of CFStack
	POSTPONE THEN \ resolve old orig
	R> \ restore count
; COMPILE-ONLY IMMEDIATE

: BAD{IF...FI} \ abort if there is no TRUE condition
	( -- )

	CR ." {IF ... FI}: no TRUE condition" CR \ error message
	-22 THROW \ 'control structure mismatch'
;

: FI} \ end of conditional
	( count -- )
	( C: orig1 ... orign -- )

	>R \ save count
	POSTPONE AHEAD \ new orig
	1 CS-ROLL \ old orig
	POSTPONE THEN \ resolve old orig

	\ if we got here, none of the guards were TRUE
	\ so abort
	POSTPONE BAD{IF...FI} \ compile the abort
	R> \ restore count

	0 ?DO \ resolve all remaining origs
		POSTPONE THEN
	LOOP
; COMPILE-ONLY IMMEDIATE

: {DO \ start a loop
	( C: -- dest )

	POSTPONE BEGIN \ create dest
; COMPILE-ONLY IMMEDIATE

: DO> \ right arrow for {DO ... OD}
	( C: dest -- orig1 dest )

	POSTPONE IF \ create orig
	1 CS-ROLL \ bring dest back to top of CFStack
; COMPILE-ONLY IMMEDIATE

: |DO| \ bar for {DO ... OD}
	( C: orig1 dest -- dest )

	0 CS-PICK \ copy the dest
	POSTPONE AGAIN \ resolve the copy
	1 CS-ROLL \ old orig
	POSTPONE THEN \ resolve old orig
; COMPILE-ONLY IMMEDIATE

: OD} \ end of loop
	( C: orig dest -- )
	POSTPONE AGAIN \ resolve dest
	POSTPONE THEN \ resolve orig
; COMPILE-ONLY IMMEDIATE


\ Simple test words

: TEST1 \ print the relationship between 'x' and 'y'
	( x y -- )

	{IF
		2DUP = IF> CR ." = "
	|IF|
		2DUP > IF> CR ." > "
	|IF|
		2DUP < IF> CR ." < "
	FI}
	2DROP
;

\ execute TEST1 for all three combinations

CR .( 5 0 TEST1 )
5 0 TEST1

CR .( 5 5 TEST1 )
5 5 TEST1

CR .( 0 5 TEST1 )
0 5 TEST1

: TEST2 \ deliberately erroneous test case --
	\ 'equal' case left out!
	( x y -- )

	{IF
		2DUP < IF> CR ." < "
	|IF|
		2DUP > IF> CR ." > "
	FI}
	2DROP
;

CR .( Since TEST2 aborts if 'x' and 'y' are equal, we will )
CR .( test TEST2 later; first we will compile and test USEFUL )

\ define arguments
VARIABLE x  5 6553 * x !
VARIABLE y 6551 5 * y !

: USEFUL \ sets both 'x' and 'y' to GCD(x, y)
	( -- )

	{DO
		x @ y @ > DO> y @ NEGATE x +!
	|DO|
		y @ x @ > DO> x @ NEGATE y +!
	OD}
;

CR .( Before: x, y = ) x @ . y @ . CR
CR .( USEFUL ) USEFUL
CR .( After: x, y = ) x @ . y @ . CR

CR .( Now we'll test TEST2 )

CR .( 5 0 TEST2 )
5 0 TEST2

CR .( 0 5 TEST2 )
0 5 TEST2

CR .( 5 5 TEST2 )
5 5 TEST2
