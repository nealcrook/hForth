\
\ LOG.F
\   Capture screen output in a textfile for hForth.
\
\ 1996. 2. 28.
\ Wonyong Koh.
\
\ Usage:
\   LOGON  ( -- )
\	ÑÁ¡e Â‰b·i HFORTH.LOGµA ˆi¢Ÿ¡Ðs“¡”a. Â‰b ˆaŸ¡Ç±¯©Ð—ˆt 'emit·¡
\	¤aŽå ÒµA“e (µŸi —i´á, HIOMULT?.F· TEXTa HGRAPHIC·i ¯¡Ç¥ Òa
\	Í¡¯a ¯¡¯aÉQ·¡ '?'·i ¥¡µa ¸i¡µ·i ´iŸ¥ ‰w¶) LOGON·¡œa‰¡ ”a¯¡ ¯¡Åa´¡
\	ˆi¢Ÿ¡ˆa ‰­¢–S“¡”a.
\	Start to save screen output in HFORTH.LOG.
\	Please reissue 'LOGON' after changing "'emit" vector.
\	 (for example, after TEXT or HGRAPHIC of HIOMULT?.F)
\   LOGOFF  ( -- )
\	HFORTH.LOGŸi ”h‰¡ ÑÁ¡e ˆi¢Ÿ¡Ÿi  ñÂ““¡”a.
\	Close HFORTH.LOG and stop saving screen output.

MARKER ~LOG

BASE @
GET-ORDER  GET-CURRENT

GET-ORDER DOS-WORDLIST SWAP 1+ SET-ORDER
DOS-WORDLIST SET-CURRENT

HEX
8000 CONSTANT invalid-fid

invalid-fid VALUE logfid
0 VALUE old'emit
CREATE LogBUFFER 1 CHARS ALLOT ALIGN

: LogEMIT  ( char -- )
    DUP LogBUFFER C! LogBUFFER 1 CHARS logfid WRITE-FILE THROW
    old'emit EXECUTE ;

NONSTANDARD-WORDLIST SET-CURRENT

: LOGON
    logfid CLOSE-FILE DROP
    S" HFORTH.LOG" W/O OPEN-FILE
    ?DUP IF
	DUP [ 2 iorOffset + ] LITERAL <>	\ file not found?
	IF THROW THEN
	2DROP S" HFORTH.LOG" W/O CREATE-FILE THROW TO logfid
    ELSE
	TO logfid
	logfid FILE-SIZE THROW logfid REPOSITION-FILE THROW
    THEN
    CR ." All characters on screen will be saved in HFORTH.LOG until 'emit is revectored."
    CR ." ÑÁ¡eµA ¥¡·¡“e ‹i¸a—i·e ¡¡– 'emit·¡ ¤aŽá‹¡ ¸åŒa»¡ HFORTH.LOGµA ˆi¢Ÿ¡–S“¡”a." CR
    'emit ['] LogEMIT <> IF
	'emit TO old'emit
	['] LogEMIT TO 'emit
    THEN ;

: LOGOFF
    logfid CLOSE-FILE
    invalid-fid TO logfid
    old'emit TO 'emit ;

LOGON

SET-CURRENT  SET-ORDER
BASE !

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
