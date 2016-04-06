\
\ DOSEXEC.F
\   call DOS executables in hForth
\
\ By 1994.12 Lee,Sheen
\
\ 1995. 10. 28.   Revised by Wonyong Koh.
\ 1996. 12. 14.   Revise (DOSEXEC).
\
\ Usage:
\   parseDOSEXEC  ( c_addr u -- )
\	Extract DOS program name from the string 'c_addr u', pass the
\	remaining string to the DOS program and empty the input source.
\   DOS>  ( "program_name command_line" -- )
\	Parse the following input source and transfer it to 'parseDOSEXEC'.
\	Program name should include full directory path.
\     Example:
\	DOS> C:\BIN\DOS\EDIT.COM TEST.TXT
\   Q  ( "command_line" -- )
\	Start Q editor. Current path is C:\BIN\Q.EXE.
\   U  ( "command_line" -- )
\	Start U editor. Current path is C:\BIN\UEDIT\U.EXE.

BASE @
GET-ORDER  GET-CURRENT

NONSTANDARD-WORDLIST SET-CURRENT

BL WORD DOS-WORDLIST FIND NIP [IF]
  GET-ORDER DOS-WORDLIST SWAP 1+ SET-ORDER
  DOS-WORDLIST SET-CURRENT
[THEN]

HEX
CREATE PARA-BLOCK   \ Parameter block for EXEC dos fuction
    0 , 	    \ environment block
    0 , 	    \ offset of CMD-LINE
    DS@ ,	    \ segment of CMD-LINE
    0 , 0 ,	    \ no data in PSP #1
    0 , 0 ,	    \ no data in PSP #2

CODE ((DOSEXEC)) ( asciiz-addr command-line-addr -- 0 | error-code )
    BX PARA-BLOCK CELL+ ) MOV,

    1000 # BX MOV,	\ reserve 64 KB
    CS AX MOV,
    AX ES MOV,

    CHAR " PARSE model" ENVIRONMENT? DROP
    CHAR " PARSE EXE Model" COMPARE 0=
    [IF]
	10 # AX SUB,	\ PSP//code-segment//data-segment/HERE
	AX ES MOV,	\ HERE 16 / 1+ CS DS - 10 + +
	DS CX MOV,	\ is program size in # paragraphs
	AX CX SUB,
	CX BX ADD,
    [THEN]

    4A # AH MOV,		\ function number for SET BLOCK function
    21 INT,			\ ES: memory area segment address

    DX POP,			\ name of the program
    DS AX MOV,
    AX ES MOV,
    PARA-BLOCK # BX MOV,	\ ES:BX points to parameter block
    4B00 # AX MOV,		\ function number for EXEC function
    21 INT,			\ call dos function

    0 # BX MOV,
    1 L# JNC,
    AX BX MOV,
    iorOffset # BX ADD,
1 L:
    NEXT,
END-CODE

\ Make all other tasks sleep except SystemTask before calling DOS program.
\ Call DOS program, then awake the tasks temperarily being slept.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : (DOSEXEC)
      0 >R			\ # of tasks to be awaken
		\ save active task list on return stack
      follower			\ current task's follower
      BEGIN
	@ CELL+ 		\ next task's follower
	DUP follower <>
      WHILE
	DUP cell- DUP @ ['] wake = IF  R> SWAP >R 1+ >R  ELSE DROP THEN
      REPEAT DROP

      ((DOSEXEC))
		\ restore active tasks
      BEGIN R> ?DUP
      WHILE R> ['] wake SWAP !  1- >R
      REPEAT ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : (DOSEXEC)
      0 >R			\ # of tasks to be awaken
		\ save active task list on return stack
      follower			\ current task's follower
      BEGIN
	@ code@ CELL+ CELL+ code@	\ next task's follower
	DUP follower <>
      WHILE
	DUP @ cell- DUP code@ ['] wake = IF  R> SWAP >R 1+ >R  ELSE DROP THEN
      REPEAT DROP

      ((DOSEXEC))
		\ restore active tasks
      BEGIN R> ?DUP
      WHILE R> ['] wake SWAP code!  1- >R
      REPEAT ;
[THEN]

\ Call DOS program identified with the name and pass remaining input source
\ to the program. This word modifies input source, which is against ANS
\ Forth Standard recommandation.
: parseDOSEXEC	( c_addr u -- )
    asciiz SOURCE >IN @ /STRING 	\ asciiz c_addr u
    2DUP + 0D SWAP C!
    SWAP 1- TUCK C!			\ asciiz c_addr-1
    (DOSEXEC) THROW
    SOURCE >IN ! DROP ;

NONSTANDARD-WORDLIST SET-CURRENT
BL WORD Ðe‹i·³Â‰b-WORDLIST FIND NIP [IF]
  GET-ORDER Ðe‹i·³Â‰b-WORDLIST SWAP 1+ SET-ORDER
[THEN]

: DOS>	( "program_name command_line" -- )
    BL PARSE  parseDOSEXEC ;

: Q  ( "command_line" -- )
    [ BL WORD Ðe‹i·³Â‰b-WORDLIST FIND NIP [IF]
      ] GRAPHIC?  DUP IF TEXT THEN
	S" C:\BIN\Q.EXE"  parseDOSEXEC  IF HGRAPHIC THEN  [  [ELSE]
      ] S" C:\BIN\Q.EXE"  parseDOSEXEC                    [  [THEN]  ]
    ;

: U  ( "command_line" -- )
    S" C:\BIN\UEDIT\U.EXE"  parseDOSEXEC ;

SET-CURRENT  SET-ORDER
BASE !

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
