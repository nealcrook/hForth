\
\ CLOCK.F
\ Displaying time on screen using hForth multitasker
\ HIOMULTI.F or HIOMULT2.F must be loaded first.
\
\ 1995. 11. 5.
\ Wonyong Koh

BASE @
GET-ORDER  GET-CURRENT

Ðe‹i·³Â‰b-WORDLIST GET-ORDER 1+ SET-ORDER
NONSTANDARD-WORDLIST SET-CURRENT

DECIMAL
0 60 CELLS 60 CELLS HAT CLOCK  CLOCK BUILD

:NONAME CLOCK ACTIVATE
	BEGIN
	  Œq¤b·± @ 0 DO PAUSE LOOP
	  GRAPHIC? SCREEN-UPDATED? AND			IF
	    BASE @ DECIMAL
	    MAX-X 20 - DUP >R
	    0 BL EFONT!
	    TIME&DATE DROP DROP DROP	\ second minute hour
	    12 MOD
	    S>D <# # # #>
	    R> 1+ DUP >R 0 2SWAP xySTR!
	    R> 2 + DUP >R 0 [CHAR] : EFONT!
	    S>D <# # # #>
	    R> 1+ DUP >R 0 2SWAP xySTR!
	    R> 2 + DUP >R 0 [CHAR] : EFONT!
	    S>D <# # # #>
	    R> 1+ DUP >R 0 2SWAP xySTR!
	    R> 2 + 0 BL EFONT!
	    BASE !					THEN
	AGAIN
; EXECUTE

SET-CURRENT  SET-ORDER
BASE !
