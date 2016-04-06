\
\ STACK.F
\ Displaying data stack on screen for Forth beginners.
\ HIOMULTI.F or HIOMULT2.F must be loaded first.
\
\ 1996. 2. 9.
\ Wonyong Koh

BASE @
GET-ORDER  GET-CURRENT
Ðe‹i·³Â‰b-WORDLIST GET-ORDER 1+ SET-ORDER

DECIMAL

Ðe‹i·³Â‰b-WORDLIST SET-CURRENT

7 CONSTANT SWIDTH
CREATE BLANKS SWIDTH CHARS ALLOT  BLANKS SWIDTH CHARS BL FILL

: #!R  ( column row x width )
    >R BASE @ 10 = IF S>D ELSE 0 THEN	\ col row d  R: width
    SWAP OVER DUP 0< IF DNEGATE THEN
    <#	#S ROT SIGN  #>
    2OVER 2OVER NIP R@ SWAP - BLANKS SWAP xySTR!
    DUP R> SWAP - >R 2SWAP SWAP R> + SWAP 2SWAP xySTR! ;

NONSTANDARD-WORDLIST SET-CURRENT

0 60 CELLS 60 CELLS HAT ”á£¡¥¡·¡ˆ  ”á£¡¥¡·¡ˆ BUILD
\ 0 60 CELLS 60 CELLS HAT ShowStack  ShowStack BUILD

: TEST
	BEGIN
          10 0 DO LOOP
          0 IF
            0 IF HEX THEN
\            10 0 ?DO LOOP
	    BASE !
	  THEN
	AGAIN
;

:NONAME ”á£¡¥¡·¡ˆ ACTIVATE
	BEGIN
	  Œq¤b·± @ 0 DO PAUSE LOOP
	  GRAPHIC? SCREEN-UPDATED? AND			IF
	    BASE @ DUP 10 <> IF HEX THEN
            MAX-X 8 - 0 S" ‹¼·¡:" xySTR!
	    SystemTask			\ system task's userP
	    @ CELL+ @
	    SystemTask stackTop 's @    \ sp0 sp@
	    4 CELLS +			\ PAUSE pushes 4
	    2DUP - 2/ ( 1 CELLS / ) DUP
	    MAX-X 3 - 0 ROT 3 #!R
            10 MIN
            DUP 1+ MAX-X 8 - SWAP S"         " xySTR!
            0 ?DO
		DUP @ MAX-X SWIDTH - I 1+ ROT SWIDTH #!R CELL+
	    LOOP 2DROP
	    BASE !
	  THEN
	AGAIN
; EXECUTE

SET-CURRENT  SET-ORDER
BASE !
