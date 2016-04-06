\ IBM-PCµA¬á ³a“e hForth ¶w Ðe‹i ·³Â‰b Ïa¡‹aœ‘·³“¡”a. »¡‹q·e ¹¡ÐsÑw
\ Ðe‹i e ³i ® ·¶·s“¡”a.
\
\ ·¡ Ïa¡‹aœ‘·e VGA ‹aœÏ¢ Äa—aµÁ ÐáÇIA¯a ‹aœÏ¢ Äa—a e·i »¡¶¥Ðs“¡”a.
\
\ 'TEXT'œa‰¡ ¯¡Åa¡e ÉB¯aËa ÑÁ¡e·a¡ ¤aŽá‰¡ 'HGRAPHIC'·¡œa‰¡ ¯¡Ç¡¡e ‹aœÏ¢
\ ÑÁ¡e·a¡ ¤aŽá´á¬á Ðe‹i·i ÑÁ¡eµA Îa¯¡Ði ® ·¶·s“¡”a.
\
\ Àá·qµA“e –¤é¯¢ ¸aÌe·a¡ ¬é¸÷–A´á ·¶·s“¡”a. 3¤é¯¢ ¸aÌe·a¡ ¤aŽa¡e
\ '3BUL'·¡œa‰¡ ¯¡Ç¡‰¡ 2¤é¯¢ ¸aÌe·a¡ ¤aŽa¡e '2BUL'·¡œa‰¡ ¯¡Ç¡¯³¯¡µ¡.
\
\ ‹i©·i ¤aŽa¡e ´aœÀáœñ ENGFONT-LOADEDµÁ HANFONT-LOADEDŸi ³a¯³¯¡µ¡.
\
\     BL PARSE ENG.FNT ENGFONT-LOADED
\     BL PARSE HAN.FNT HANFONT-LOADED
\
\ Ça‹¡ˆa 11008·¡a 11520 ¤a·¡Ëa·¥ 8¤éX4¤éX4¤é ‹i©·i ³i ® ·¶·s“¡”a.
\
\ HF86EXE.EXEŸi ¯¡¸bÐe Ò ”a·q ®…¬á¡ ·¡ Ïa¡‹aœ‘·i µ©Ÿ© ® ·¶·s“¡”a.
\
\	<< OPTIONAL.F
\	<< ASM8086.F
\	<< COREEXT.F
\	<< MSDOS.F
\	BL PARSE MULTI.F    INCLUDED
\	BL PARSE HIOMULT2.F INCLUDED
\
\ 1996. 2. 9.
\ Wonyong Koh
\
\ Usage:
\   TEXT  ( -- )
\	Set text screen and redirect i/o vectors to DOS functions.
\   HGRAPHIC  ( -- )
\	Set graphics screen and redirect i/o vectors to handle Korean
\	character input and graphics screen output.
\   ENGFONT-LOADED  ( c-addr u -- )
\	Load English font file 'c-addr u' of which size is 4096 bytes.

CHAR " PARSE FILE" ENVIRONMENT? 0= [IF] 0 [THEN]
0= [IF]
    CR .( This program needs FILE wordset words.) ABORT
[THEN]

BASE @
GET-ORDER  GET-CURRENT

FORTH-WORDLIST SET-CURRENT
WORDLIST WORDLIST-NAME Ðe‹i·³Â‰b-WORDLIST
Ðe‹i·³Â‰b-WORDLIST SET-CURRENT
GET-ORDER Ðe‹i·³Â‰b-WORDLIST SWAP 1+ SET-ORDER

CR .( Loading character font data)
DECIMAL
CREATE ENGFONT	    \ 8x16, 128 ¸a
16 128 * ALLOT

CREATE HANFONT	    \ 16x16, 19 ¸a x 8 ¤é + 21 ¸a X 4 ¤é + 27 ¸a X 4 ¤é
11008 ALLOT
HANFONT CONSTANT Á¡¬÷‹i©
Á¡¬÷‹i© 32 19 * 8 * + CONSTANT º—¬÷‹i©
º—¬÷‹i© 32 21 * 4 * + CONSTANT ¤hÃ±‹i©

NONSTANDARD-WORDLIST SET-CURRENT
: ENGFONT-LOADED  ( c_addr u -- )
    R/O OPEN-FILE THROW 				    \ fileid
    DUP ENGFONT [ 16 128 * ] LITERAL ROT READ-FILE THROW    \ fileid n
    [ 16 128 * ] LITERAL <> IF
	." µw¢…‹i© Ìa·©µA ‹i©ˆt·¡ ¡¡¸aœs“¡”a." CR
	ABORT" Not enough font data in English font file." THEN
    CLOSE-FILE THROW ;

: HANFONT-LOADED ( c_addr u -- )
    R/O OPEN-FILE THROW 	\ fileid
    DUP FILE-SIZE THROW 	\ fileid ud
    IF	." ¡¡Ÿa“e Ðe‹i‹i© Ìa·©·³“¡”a - Ìa·©·¡ á¢ Çs“¡”a." CR
	ABORT" Unknown type of Korean font file - file too big." THEN
    \ fileid u
    CASE
      11008 OF DUP Á¡¬÷‹i© 11008 ROT READ-FILE THROW DROP ENDOF
      11520 OF
	8 0 DO DUP DUP FILE-POSITION THROW 32 S>D D+ ROT REPOSITION-FILE THROW
	       DUP Á¡¬÷‹i© [ 19 32 * ] LITERAL I * + [ 19 32 * ] LITERAL
	       ROT READ-FILE THROW DROP LOOP
	4 0 DO DUP DUP FILE-POSITION THROW 32 S>D D+ ROT REPOSITION-FILE THROW
	       DUP º—¬÷‹i© [ 21 32 * ] LITERAL I * + [ 21 32 * ] LITERAL
	       ROT READ-FILE THROW DROP LOOP
	4 0 DO DUP DUP FILE-POSITION THROW 32 S>D D+ ROT REPOSITION-FILE THROW
	       DUP ¤hÃ±‹i© [ 27 32 * ] LITERAL I * + [ 27 32 * ] LITERAL
	       ROT READ-FILE THROW DROP LOOP
	ENDOF
      DROP ." ¡¡Ÿa“e Ðe‹i‹i© Ìa·©·³“¡”a." CR
	   ABORT" Unknown type of Korean font file."
    ENDCASE
    CLOSE-FILE THROW ;

BL PARSE ENG.FNT ENGFONT-LOADED
BL PARSE HAN.FNT HANFONT-LOADED

Ðe‹i·³Â‰b-WORDLIST SET-CURRENT

\
\ Ðe‹i Â‰b { i
\
CR .( Loading character output words)

DECIMAL 80 CONSTANT MAX-X
VARIABLE VIR_X
VARIABLE VIR_Y

HEX
: BINARY   2 BASE ! ;
: 16* ( n -- 16*n )   2* 2* 2* 2* ;
: –õ®A= ( char -- 0|-1 )   DUP 08 = OVER 07F = OR SWAP 0FF = OR ;

CODE INT10  ( AX -- AX )
    BX AX MOV,		\ BXˆa ”á£¡·  … ¶á ˆt·³“¡”a.
    10 INT,		\ AH = 0
    AX BX MOV,
    NEXT,
END-CODE

: GET-MODE ( -- mode )
    0F00 INT10 0FF AND ;

3 VALUE OldMode#

: SET-MODE  ( mode -- )   INT10 DROP ;

: VGA?	( -- flag )   1A00 INT10 0FF AND 1A = ;

0 VALUE GRAPHIC?
3 VALUE textmode#

\ for VGA graphics card
DECIMAL 30 VALUE MAX-Y			\ 640X480 Ð¬w•¡; 480 / 16 = 30 º‰
HEX

: VGA-SET-GRAPHIC  ( -- )   11 SET-MODE  -1 TO GRAPHIC? ;

\ VGA §¡—¡µ¡ ¡A¡¡Ÿ¡· ˆb º‰µA ”Ðe ­A‹a åËa º­¡· ÎaŸi  e—k
\ Y ¹ÁÎa¡ ·¡ ­A‹a åËa ˆt·i ´è·a¡e X ¹ÁÎaŸi ¤a¡
\ µ¡Ïa­U º­¡¡ ³i ® ·¶·s“¡”a.
CREATE VGA-Y>SegTable MAX-Y 16* CELLS ALLOT

MARKER ~TEMP
:NONAME
   MAX-Y 16* 0 DO  0A000  I 5 *  +  VGA-Y>SegTable I CELLS + !	LOOP ;
EXECUTE
~TEMP	\ ÎaŸi À¶‰¡ ¶á { i·i »¡¶‘

\ for Hercules monochrome grahics card
DECIMAL 25 TO MAX-Y			\ 640X400 Ð¬w•¡; 400 / 16 = 25 º‰
HEX

CREATE 6854REGS 	\ 640X400 Ð¬w•¡µA Ï©¶aÐe 6845 A»¡¯aÈá Á¡‹¡ˆt
31 C, 28 C, 29 C, 08 C, 68 C, 02 C, 64 C, 65 C, 02 C, 03 C,

: HERC?  ( -- flag )
    03B5 PC@  4F DUP  03B5 PC!	100 0 DO LOOP
    03B5 PC@  ROT  03B5 PC!  = IF
	03BA PC@ 80 AND
	8000 0 DO 03BA PC@ 80 AND OVER <> IF UNLOOP DROP TRUE EXIT THEN LOOP
    THEN FALSE ;

: HERC-SET-GRAPHIC  ( -- )
   0A 0 DO I 03B4 PC! 6854REGS I + C@ 03B5 PC! LOOP
   1 03BF PC!		\ ‹aœÏ¢ ¡¡—aŸi Ðá¶w, ‹aœÏ¢ ÍA·¡»¡ 1 ·e ¬a¶w ¦‰ˆa“w
   [ BINARY ] 00001010 [ HEX ]
   03B8 PC!		\ ‹aœÏ¢ ÍA·¡»¡ 0 ·i ‹aœÏ¢ ¡¡—a¡ Îa¯¡
   -1 TO GRAPHIC? ;

\ ÐáÇIA¯a §¡—¡µ¡ ¡A¡¡Ÿ¡· ˆb º‰µA ”Ðe ­A‹a åËa º­¡· ÎaŸi  e—k
\ ‹aœÏ¢ ÍA·¡»¡ 1·e 0B000:0hµA¬á ¯¡¸b
\ Y ¹ÁÎa¡ ·¡ ­A‹a åËa ˆt·i ´è·a¡e X ¹ÁÎaŸi ¤a¡
\ µ¡Ïa­U º­¡¡ ³i ® ·¶·s“¡”a.
CREATE HERC-Y>SegTable MAX-Y 16* CELLS ALLOT

MARKER ~TEMP
:NONAME
   MAX-Y 16* 0 DO 0B000  I 4 MOD 200 *	+  I 4 /  5 *  +
		  HERC-Y>SegTable I CELLS + !		   LOOP ;
EXECUTE
~TEMP	\ ÎaŸi À¶‰¡ ¶á { i·i »¡¶‘

VARIABLE Y>SegTable
: Y>SEG  ( y -- segment_addr )
    CELLS Y>SegTable @ + @ ;

' VGA-SET-GRAPHIC VALUE 'SET-GRAPHIC

NONSTANDARD-WORDLIST SET-CURRENT
: SET-GRAPHIC  ( -- )	'SET-GRAPHIC EXECUTE ;
Ðe‹i·³Â‰b-WORDLIST SET-CURRENT

DECIMAL

\ ÉB¯aËa Y ¹ÁÎa· ÑÁ¡eµA Îa¯¡–E ¢…¸a—i·i ¤a¡ ¶á º‰¡ µ«‹±
\ ‹aœÏ¢ ¹ÁÎa¡“e 16y ¦Èá 16 ˆa¡º‰·i ˆbˆb 16 º‰ ¶á¡ µ«‹±
\ : UP-LINE
\    16* DUP 16 - DO
\      I 16 + Y>SEG  I Y>SEG
\      40 0 DO OVER I 2* L@  OVER I 2* L! LOOP	2DROP
\    LOOP ;

CODE UP-LINE  ( y -- )
   SI PUSH,
   BX DEC,
   5 # CL MOV,
   BX CL SHL,			\ BX = (VIR_Y@-1)@ * 32
   Y>SegTable ) BX ADD,
   16 # DX MOV,
1 L:
   SI SI XOR,
   DI DI XOR,
   SS:	0 [BX] ES MOV,
   SS: 32 [BX] DS MOV,
   40 # CX MOV,
   REPE, WORD MOVS,
   2 # BX ADD,
   DX DEC,
   1 L# JNE,
   SS AX MOV,
   AX DS MOV,
   SI POP,
   BX POP,
   NEXT,
END-CODE

\ ÉB¯aËa Y ¹ÁÎa· º‰·i »¡¶‘
\ : CLEAR-LINE
\    16* DUP 16 + SWAP
\    DO I Y>SEG
\	40 0 DO 0 OVER I 2* L! LOOP  DROP
\    LOOP ;

CODE CLEAR-LINE  ( y -- )
   5 # CL MOV,
   BX CL SHL,			\ BX = VIR_Y@ * 32
   Y>SegTable ) BX ADD,
   AX AX XOR,
   16 # DX MOV,
1 L:
   0 [BX] ES MOV,
   DI DI XOR,
   40 # CX MOV,
   REPE,
   WORD STOS,
   2 # BX ADD,
   DX DEC,
   1 L# JNE,
   BX POP,
   NEXT,
END-CODE

HEX
VARIABLE VSCR0
VARIABLE YY
VARIABLE XX
1000 CONSTANT VSCREEN-SIZE
0FFF CONSTANT VSCR-MASK
CREATE VSCREEN VSCREEN-SIZE CHARS ALLOT
: >VSCR-ADDR   ( offset -- c_addr )
    VSCR0 @ + VSCR-MASK AND VSCREEN + ;

DECIMAL

\ : EFONT!  ( x y char -- )
\    16* ENGFONT +	  \ x y font-addr
\    SWAP 16*		  \ x font-addr 16y
\    16 0 DO OVER I + C@ OVER I + Y>SEG 4 PICK LC! LOOP DROP 2DROP ;

CODE EFONT!  ( x y char -- )
   SI DX MOV,		\ MOV	DX,SI
   BX SI MOV,		\ MOV	SI,BX
   BX POP,		\ POP	BX
   DI POP,		\ POP	DI
   4 # CL MOV,		\ MOV	CL,#4
   SI CL SHL,		\ SHL	SI,CL
   ENGFONT # SI ADD,	\ ADD	SI,ENGFONT	; SI = font-addr
   CL INC,		\ INC	CL		; CL = 5
   BX CL SHL,		\ SHL	BX,CL		; BX = VIR_Y@ * 32
   Y>SegTable ) BX ADD,
   2 # CX MOV,
 15
   0 [BX] ES MOV, BYTE LODS, ES: AL 0 [DI] MOV, CX BX ADD, 1- ?DUP [IF] 0 >IN ! [THEN]
   0 [BX] ES MOV, BYTE LODS, ES: AL 0 [DI] MOV,
   DX SI MOV,
   BX POP,
   NEXT,
END-CODE

HEX
CREATE À¶‘‹i©
00000 , 00000 , 00000 , 00000 , 00000 , 00000 , 00000 , 00000 , \ À¶‘
00000 , 00000 , 00000 , 00000 , 00000 , 00000 , 00000 , 00000 ,

CREATE ‹aŸ±¸a‹i©
05555 , 0AAAA , 05555 , 0AAAA , 05555 , 0AAAA , 05555 , 0AAAA , \ ³a»¡ ´g·q
05555 , 0AAAA , 05555 , 0AAAA , 05555 , 0AAAA , 05555 , 0AAAA ,

BINARY
00001 CONSTANT À¶‘Á¡¬÷
00010 CONSTANT À¶‘º—¬÷
00001 CONSTANT À¶‘¤hÃ±
1000010001000001 CONSTANT À¶‘¸a	\ Á¡¬÷, º—¬÷, ¤hÃ± ¡¡– À¶‘¸a
0111110000000000 CONSTANT Á¡¬÷¥¥
0000001111100000 CONSTANT º—¬÷¥¥
0000000000011111 CONSTANT ¤hÃ±¥¥
1000001111111111 CONSTANT Á¡¬÷»¡¶‘¥¥
1111110000011111 CONSTANT º—¬÷»¡¶‘¥¥
1111111111100000 CONSTANT ¤hÃ±»¡¶‘¥¥
DECIMAL

VARIABLE HCHAR

\ VARIABLE H1FONT
\ VARIABLE H2FONT
\ VARIABLE H3FONT
\ : Y>SEG  ( y -- segment )   CELLS Y>SegTable @ + @ ;
\ : H2FONT!  ( x y Á¡¬÷‹i©ˆt º—¬÷‹i©ˆt -- )
\     H2FONT !
\     H1FONT !
\     16*
\     16 0 DO H1FONT @ @
\	      H2FONT @ @ OR
\	      OVER I + Y>SEG 3 PICK L!
\	      2 H1FONT +!  2 H2FONT +!
\	   LOOP  2DROP ;
\
\ : H3FONT!  ( x y Á¡¬÷‹i©ˆt º—¬÷‹i©ˆt ¤hÃ±‹i©ˆt -- )
\     H3FONT !
\     H2FONT !
\     H1FONT !
\     16*
\     16 0 DO H1FONT @ @
\	      H2FONT @ @ OR
\	      H3FONT @ @ OR
\	      OVER I + Y>SEG 3 PICK L!
\	      2 H1FONT +!  2 H2FONT +!	2 H3FONT +!
\	   LOOP  2DROP ;

\ ¹¡ÐsÑwÅ¡—aŸi §¥ ¸aŸ¡ ´ô“e ‹i© Å¡—aˆt·a¡ ¤aŽ‘
\ 'ÄñÏAÈá ­¢· Ðe‹i' ·¡º…Ó, ¸÷Š¥ »¡·q, ¸÷¥¡¯¡” (1991) 122-134½¢ Àq¹¡
\ À¶‘¸a“e 32Ÿi, ·AÒaÐa»¡ ´g·e Å¡—a“e 96(=32+64)·i ”á£¡µA µ©Ÿ±

CREATE Á¡¬÷‹i©Å¡—aÎa  ( ¹¡ÐsÑwÁ¡¬÷Å¡—a -- Á¡¬÷‹i©Å¡—a )
    96 C, 32 C,  0 C,  1 C,  2 C,  3 C,  4 C,  5 C,
     6 C,  7 C,  8 C,  9 C, 10 C, 11 C, 12 C, 13 C,
    14 C, 15 C, 16 C, 17 C, 18 C, 96 C, 96 C, 96 C,
    96 C, 96 C, 96 C, 96 C, 96 C, 96 C, 96 C, 96 C, ALIGN

CREATE º—¬÷‹i©Å¡—aÎa  ( ¹¡ÐsÑwº—¬÷Å¡—a -- º—¬÷‹i©Å¡—a )
    96 C, 96 C, 32 C,  0 C,  1 C,  2 C,  3 C,  4 C,
    96 C, 96 C,  5 C,  6 C,  7 C,  8 C,  9 C, 10 C,
    96 C, 96 C, 11 C, 12 C, 13 C, 14 C, 15 C, 16 C,
    96 C, 96 C, 17 C, 18 C, 19 C, 20 C, 96 C, 96 C, ALIGN

CREATE ¤hÃ±‹i©Å¡—aÎa  ( ¹¡ÐsÑw¤hÃ±Å¡—a -- ¤hÃ±‹i©Å¡—a )
    96 C, 32 C,  0 C,  1 C,  2 C,  3 C,  4 C,  5 C,
     6 C,  7 C,  8 C,  9 C, 10 C, 11 C, 12 C, 13 C,
    14 C, 15 C, 96 C, 16 C, 17 C, 18 C, 19 C, 20 C,
    21 C, 22 C, 23 C, 24 C, 25 C, 26 C, 96 C, 96 C, ALIGN

19 VALUE Å¡—a®
: *,   Å¡—a® * , ;

CREATE ¤hÃ±´ô“eÁ¡¬÷‹i©¤é£»  ( º—¬÷‹i©Å¡—a -- Á¡¬÷‹i©¤é )
    \ „a   „	„¡   „Á   „á   …A   …a	 …   …¡   …Á	…á
       0 *, 0 *, 0 *, 0 *, 0 *, 0 *, 0 *, 0 *, 1 *, 3 *, 3 *,
    \ †A   †a	†   †¡   †Á   †á   ‡A	 ‡a   ‡   ‡¡
       3 *, 1 *, 2 *, 4 *, 4 *, 4 *, 2 *, 1 *, 3 *, 0 *,

21 TO Å¡—a®
CREATE ¤hÃ±´ô“eº—¬÷‹i©¤é£»  ( Á¡¬÷‹i©Å¡—a -- º—¬÷‹i©¤é )
    \ ˆA   ŒA	A   ”A   ˜A   œA    A	 ¤A   ¨A   ¬A
      0 *, 1 *, 1 *, 1 *, 1 *, 1 *, 1 *, 1 *, 1 *, 1 *,
    \ °A   ´A	¸A   ¼A   ÀA   ÄA   ÈA	 ÌA   ÐA
      1 *, 1 *, 1 *, 1 *, 1 *, 0 *, 1 *, 1 *, 1 *,

19 TO Å¡—a®
CREATE ¤hÃ±·¶“eÁ¡¬÷‹i©¤é£»  ( º—¬÷‹i©Å¡—a -- Á¡¬÷‹i©¤é )
    \ „a   „	„¡   „Á   „á   …A   …a	 …   …¡   …Á	…á
       5 *, 5 *, 5 *, 5 *, 5 *, 5 *, 5 *, 5 *, 6 *, 7 *, 7 *,
    \ †A   †a	†   †¡   †Á   †á   ‡A	 ‡a   ‡   ‡¡
       7 *, 6 *, 6 *, 7 *, 7 *, 7 *, 6 *, 6 *, 7 *, 5 *,

21 TO Å¡—a®
CREATE ¤hÃ±·¶“eº—¬÷‹i©¤é£»  ( Á¡¬÷‹i©Å¡—a -- º—¬÷‹i©¤é )
    \ ˆA   ŒA	A   ”A   ˜A   œA    A	 ¤A   ¨A   ¬A
      2 *, 3 *, 3 *, 3 *, 3 *, 3 *, 3 *, 3 *, 3 *, 3 *,
    \ °A   ´A	¸A   ¼A   ÀA   ÄA   ÈA	 ÌA   ÐA
      3 *, 3 *, 3 *, 3 *, 3 *, 2 *, 3 *, 3 *, 3 *,

27 TO Å¡—a®
CREATE ¤hÃ±‹i©¤é£»  ( º—¬÷‹i©Å¡—a -- ¤hÃ±‹i©¤é )
    \ „a   „	„¡   „Á   „á   …A   …a	 …   …¡   …Á	…á
       0 *, 2 *, 0 *, 2 *, 1 *, 2 *, 1 *, 2 *, 3 *, 0 *, 2 *,
    \ †A   †a	†   †¡   †Á   †á   ‡A	 ‡a   ‡   ‡¡
       1 *, 3 *, 3 *, 1 *, 2 *, 1 *, 3 *, 3 *, 1 *, 1 *,

\ : CCFONT!  ( x y ¤hÃ±´ô“e16§¡ËaÐe‹iÅ¡—a -- )
\     DUP Á¡¬÷¥¥ AND 10 RSHIFT CHARS
\     Á¡¬÷‹i©Å¡—aÎa + C@ SWAP		  \ x y Á¡¬÷‹i©Å¡—a Ðe‹iÅ¡—a
\     º—¬÷¥¥ AND 5 RSHIFT CHARS
\     º—¬÷‹i©Å¡—aÎa + C@		  \ x y Á¡¬÷‹i©Å¡—a º—¬÷‹i©Å¡—a
\     2DUP OVER 31 > IF
\	  DROP 63 > IF ‹aŸ±¸a‹i© ELSE À¶‘‹i© THEN
\     ELSE DUP 31 > IF DROP 0 THEN
\	  CELLS ¤hÃ±´ô“eÁ¡¬÷‹i©¤é£» + @ + 5 LSHIFT Á¡¬÷‹i© +	    THEN
\     ROT ROT DUP 31 > IF NIP 63 > IF ‹aŸ±¸a‹i© ELSE À¶‘‹i© THEN
\     ELSE SWAP DUP 31 > IF DROP 0 THEN
\	  CELLS ¤hÃ±´ô“eº—¬÷‹i©¤é£» + @ + 5 LSHIFT º—¬÷‹i© +	    THEN
\     H2FONT! ;

CODE CCFONT!  ( x y ¤hÃ±´ô“e16§¡ËaÐe‹iÅ¡—a -- )
    CX POP,
    DX POP,
    BP PUSH,
    SI PUSH,
    DX PUSH,
    CX PUSH,
    BX SI MOV,				\ SI = Ðe‹iÅ¡—a
    Á¡¬÷¥¥ # BX AND,
    10 # CL MOV,
    BX CL SHR,
    Á¡¬÷‹i©Å¡—aÎa [BX] BL MOV,
    BX AX MOV,				\ AX = Á¡¬÷‹i©Å¡—aÎa
    SI BX MOV,
    º—¬÷¥¥ # BX AND,
    5 # CL MOV,
    BX CL SHR,
    º—¬÷‹i©Å¡—aÎa [BX] BL MOV,
    BX DX MOV,				\ DX = º—¬÷‹i©Å¡—aÎa
xhere ( HFONT!µA¬á šá´áµ© º­¡Ÿi ”á£¡µA q‹±)
    BINARY 00100000 DECIMAL # AL TEST,
    1 L# JZ,
    \ AX(=Á¡¬÷)ˆa À¶‘¸a·¡ˆáa ¢Òa‹i©·¡¡e
    À¶‘‹i© # SI MOV,
    BINARY 01000000 DECIMAL # AL TEST,
    2 L# JZ,
    ‹aŸ±¸a‹i© # SI MOV,
    2 L# JU,
1 L:
    BX BX XOR,
    BINARY 00100000 DECIMAL # DL TEST,
    3 L# JNZ,
    DX BX MOV,
    BX 1 SHL,
3 L:
    ¤hÃ±´ô“eÁ¡¬÷‹i©¤é£» [BX] SI MOV,
    AX SI ADD,
    5 # CL MOV,
    SI CL SHL,
    Á¡¬÷‹i© # SI ADD,
2 L:	\ SI = Á¡¬÷‹i©º­¡
    BINARY 00100000 DECIMAL # DL TEST,
    4 L# JZ,
    \ DX(=º—¬÷)ˆa À¶‘¸a·¡ˆáa ¢Òa‹i©·¡¡e
    À¶‘‹i© # DI MOV,
    BINARY 01000000 DECIMAL # DL TEST,
    5 L# JZ,
    ‹aŸ±¸a‹i© # DI MOV,
    5 L# JU,
4 L:
    BX BX XOR,
    BINARY 00100000 DECIMAL # AL TEST,
    6 L# JNZ,
    AX BX MOV,
    BX 1 SHL,
6 L:
    ¤hÃ±´ô“eº—¬÷‹i©¤é£» [BX] DI MOV,
    DX DI ADD,
    5 # CL MOV,
    DI CL SHL,
    º—¬÷‹i© # DI ADD,
5 L:	\ DI = º—¬÷‹i©º­¡
    BX POP,			\ BX = y
    DX POP,			\ DX = x
    5 # CL MOV,
    BX CL SHL,			\ BX = y * 32
    Y>SegTable ) BX ADD,
    2 # CX MOV,
  15
    WORD LODS, 0 [DI] AX OR, 0 [BX] ES MOV, BX DX XCHG, ES: AX 0 [BX] MOV, BX DX XCHG, CX DI ADD, CX BX ADD, 1- ?DUP [IF] 0 >IN ! [THEN]
    WORD LODS, 0 [DI] AX OR, 0 [BX] ES MOV, BX DX XCHG, ES: AX 0 [BX] MOV,
    SI POP,
    BP POP,
    BX POP,
    NEXT,
END-CODE

\ : CCCFONT!  ( x y ¤hÃ±·¶“e16§¡ËaÐe‹iÅ¡—a -- )
\     DUP Á¡¬÷¥¥ AND 10 RSHIFT CHARS
\     Á¡¬÷‹i©Å¡—aÎa + C@ SWAP	    \ x y Á¡¬÷‹i©Å¡—a Ðe‹iÅ¡—a
\     DUP º—¬÷¥¥ AND 5 RSHIFT CHARS
\     º—¬÷‹i©Å¡—aÎa + C@ SWAP	    \ x y Á¡¬÷‹i©Å¡—a º—¬÷‹i©Å¡—a Ðe‹iÅ¡—a
\     ¤hÃ±¥¥ AND CHARS
\     ¤hÃ±‹i©Å¡—aÎa + C@	    \ x y Á¡¬÷‹i©Å¡—a º—¬÷‹i©Å¡—a ¤hÃ±‹i©Å¡—a
\     DUP 31 > IF 63 > IF ‹aŸ±¸a‹i© ELSE À¶‘‹i© THEN
\     ELSE OVER DUP 31 > IF DROP 0 THEN
\	   CELLS ¤hÃ±‹i©¤é£» + @ + 5 LSHIFT ¤hÃ±‹i© + THEN
\     ROT ROT
\     2DUP OVER 31 > IF DROP 63 > IF ‹aŸ±¸a‹i© ELSE À¶‘‹i© THEN
\     ELSE DUP 31 > IF DROP 0 THEN
\	   CELLS ¤hÃ±·¶“eÁ¡¬÷‹i©¤é£» + @ + 5 LSHIFT Á¡¬÷‹i© +     THEN
\     ROT ROT DUP 31 > IF NIP 63 > IF ‹aŸ±¸a‹i© ELSE À¶‘‹i© THEN
\     ELSE SWAP DUP 31 > IF DROP 0 THEN
\	   CELLS ¤hÃ±·¶“eº—¬÷‹i©¤é£» + @ + 5 LSHIFT º—¬÷‹i© +     THEN
\     H3FONT! ;

\ : HFONT!  ( x y 16§¡ËaÐe‹iÅ¡—a -- )
\     DUP ¤hÃ±¥¥ AND 1 = IF CCFONT! ELSE CCCFONT! THEN ;

CODE HFONT!  ( x y 16§¡ËaÐe‹iÅ¡—a -- )
    CX POP,
    DX POP,
    BP PUSH,
    SI PUSH,
    DX PUSH,
    CX PUSH,
    BX SI MOV,				\ SI = Ðe‹iÅ¡—a
    Á¡¬÷¥¥ # BX AND,
    10 # CL MOV,
    BX CL SHR,
    Á¡¬÷‹i©Å¡—aÎa [BX] BL MOV,
    BX AX MOV,				\ AX = Á¡¬÷‹i©Å¡—aÎa
    SI BX MOV,
    º—¬÷¥¥ # BX AND,
    5 # CL MOV,
    BX CL SHR,
    º—¬÷‹i©Å¡—aÎa [BX] BL MOV,
    BX DX MOV,				\ DX = º—¬÷‹i©Å¡—aÎa
    SI BX MOV,
    ¤hÃ±¥¥ # BX AND,
    1 # BX CMP,
    0 L# JNZ,
    ( CCFONT! ¸÷· ´eµA¬á q‹¥ º­¡) # JMP,
0 L:
    ¤hÃ±‹i©Å¡—aÎa [BX] BL MOV,
    BX CX MOV,				\ CX = ¤hÃ±‹i©Å¡—aÎa
    BINARY 00100000 DECIMAL # CL TEST,
    1 L# JZ,
    \ CX(=¤hÃ±)ˆa À¶‘¸a·¡ˆáa ¢Òa‹i©·¡¡e
    À¶‘‹i© # BP MOV,
    BINARY 01000000 DECIMAL # CL TEST,
    2 L# JZ,
    ‹aŸ±¸a‹i© # BP MOV,
    2 L# JU,
1 L:
    BX BX XOR,
    BINARY 00100000 DECIMAL # DL TEST,
    3 L# JNZ,
    DX BX MOV,
    BX 1 SHL,
3 L:
    ¤hÃ±‹i©¤é£» [BX] BP MOV,
    CX BP ADD,
    5 # CL MOV,
    BP CL SHL,
    ¤hÃ±‹i© # BP ADD,
2 L:	\ BP = ¤hÃ±‹i©º­¡
    BINARY 00100000 DECIMAL # AL TEST,
    4 L# JZ,
    \ AX(=Á¡¬÷)ˆa À¶‘¸a·¡ˆáa ¢Òa‹i©·¡¡e
    À¶‘‹i© # SI MOV,
    BINARY 01000000 DECIMAL # AL TEST,
    5 L# JZ,
    ‹aŸ±¸a‹i© # SI MOV,
    5 L# JU,
4 L:
    BX BX XOR,
    BINARY 00100000 DECIMAL # DL TEST,
    6 L# JNZ,
    DX BX MOV,
    BX 1 SHL,
6 L:
    ¤hÃ±·¶“eÁ¡¬÷‹i©¤é£» [BX] SI MOV,
    AX SI ADD,
    5 # CL MOV,
    SI CL SHL,
    Á¡¬÷‹i© # SI ADD,
5 L:	\ SI = Á¡¬÷‹i©º­¡
    BINARY 00100000 DECIMAL # DL TEST,
    7 L# JZ,
    \ DX(=º—¬÷)ˆa À¶‘¸a·¡ˆáa ¢Òa‹i©·¡¡e
    À¶‘‹i© # DI MOV,
    BINARY 01000000 DECIMAL # DL TEST,
    8 L# JZ,
    ‹aŸ±¸a‹i© # DI MOV,
    8 L# JU,
7 L:
    BX BX XOR,
    BINARY 00100000 DECIMAL # AL TEST,
    9 L# JNZ,
    AX BX MOV,
    BX 1 SHL,
9 L:
    ¤hÃ±·¶“eº—¬÷‹i©¤é£» [BX] DI MOV,
    DX DI ADD,
    5 # CL MOV,
    DI CL SHL,
    º—¬÷‹i© # DI ADD,
8 L:	\ DI = º—¬÷‹i©º­¡
    BX POP,			\ BX = y
    DX POP,			\ DX = x
    5 # CL MOV,
    BX CL SHL,			\ BX = y * 32
    Y>SegTable ) BX ADD,
    2 # CX MOV,
  15
    WORD LODS, 0 [DI] AX OR, 0 [BP] AX OR, 0 [BX] ES MOV, BX DX XCHG, ES: AX 0 [BX] MOV, BX DX XCHG, CX DI ADD, CX BP ADD, CX BX ADD, 1- ?DUP [IF] 0 >IN ! [THEN]
    WORD LODS, 0 [DI] AX OR, 0 [BP] AX OR, 0 [BX] ES MOV, BX DX XCHG, ES: AX 0 [BX] MOV,
    SI POP,
    BP POP,
    BX POP,
    NEXT,
END-CODE

: xySTR!  ( x y c_addr u -- )
    BEGIN >R >R 2DUP R@ C@
	  DUP 128 < IF EFONT!
	  ELSE 8 LSHIFT R> CHAR+ DUP >R C@ OR HFONT! SWAP 1+ SWAP
	  THEN
	  SWAP 1+ SWAP R> CHAR+ R> 1- DUP 0=
    UNTIL 2DROP 2DROP ;

DECIMAL 30 TO MAX-Y

CREATE MAX-X*Table MAX-Y 1+ CELLS ALLOT
MARKER ~TEMP
:NONAME   MAX-Y 1+ 0 DO I MAX-X *  MAX-X*Table I CELLS +  !  LOOP ; EXECUTE
~TEMP
: MAX-X*  ( y -- MAX_X*y )   CELLS MAX-X*Table + @ ;

HEX
: SHOW-LINE  ( y -- )		\ ˆa¬wÑÁ¡e· y º‰·i ‹aœÏ¢ ÑÁ¡eµA Îa¯¡
    >R 1 MAX-X* 0				\ max-x 0  R: y
    BEGIN
       DUP R@ OVER >VSCR-ADDR DUP C@		\ max-x x x 0 c_addr char
       DUP 80 < IF NIP EFONT!
       ELSE 8 LSHIFT
	    SWAP CHAR+ C@ OR HFONT! CHAR+ THEN
       CHAR+ 2DUP =
    UNTIL 2DROP R> DROP ;

0 VALUE YTop

: SCROLL  ( -- )
    MAX-Y MAX-X* DUP MAX-X + SWAP
    DO BL I >VSCR-ADDR C! LOOP
    MAX-Y 1-  MAX-X 0 DO I OVER BL EFONT! LOOP DROP
    VSCR0 @ MAX-X + VSCR-MASK AND VSCR0 !
    YTop SHOW-LINE
    0 XX !  YTop YY ! ;

: VIR_X+!  ( n -- )
    VIR_X @ + MAX-X /MOD VIR_Y +! VIR_X !
    VIR_Y @ MAX-Y = IF SCROLL -1 VIR_Y +! THEN ;

: VSCR!  ( char -- )   VIR_Y @ MAX-X* VIR_X @ + >VSCR-ADDR C! ;

: multiEMIT  ( char -- )
    DUP –õ®A= IF DROP VIR_X @ VIR_Y @ BL EFONT! -1 VIR_X +! BL VSCR! EXIT THEN
    DUP 0D ( CR) =  IF DROP  0 VIR_X !				     EXIT THEN
    DUP 0A ( LF) =  IF DROP  VIR_Y @ 1+ MAX-Y < IF 1 VIR_Y +! EXIT THEN
			     SCROLL				     EXIT THEN
    VSCR!  1 VIR_X+! ;

: HEMIT  ( char -- )
    HCHAR @ 0= IF			\ ¬¡ ¯¡¸bÐa“e ‹i¸a
      DUP 80 < IF multiEMIT EXIT THEN	\ Ðe‹i·¡ ´a“¡¡e ‹a”¡ Â‰b
      VIR_X @ 1+ MAX-X = IF VIR_X @ multiEMIT BL multiEMIT THEN
      HCHAR ! EXIT			\ Àõ 8 §¡Ëa ¤e¸aŸi ˆi¢Ÿ¡
    THEN
    HCHAR @ multiEMIT  multiEMIT  0 HCHAR ! ;

\
\ Ðe‹i ·³b { i—i
\

CR .( Loading character input words)

CODE INT16h
   BX AX MOV,
   16 INT,
   AX BX MOV,
   NEXT,
END-CODE

\  a»¡ b ‹i®A ·³b ˜ ¶E½¢ ¶õ‹i®Aˆa ’‰v·a¡e Àq, ´a“¡¡e ˆá»µ
: ¶E½¢¶õ®A’‰Ÿ±?  ( -- flag )
	200 INT16h  [ BINARY ] 00000010 [ HEX ] AND 0= 0= ;

\  a»¡ b ‹i®A ·³b ˜ CapsLock ¬wÈµv·a¡e Àq, ´a“¡¡e ˆá»µ
: CapsLock?  ( -- flag )
	200 INT16h  [ BINARY ] 01000000 [ HEX ] AND 0= 0= ;

VARIABLE ‹i®A·³b¬wÈ
VARIABLE ‹i®AÉ·
VARIABLE £¡µÅ¬÷¸a
CREATE ‹i®A·³b¬wÈ‹¡´â  8 CELLS ALLOT
CREATE £¡µÅ¬÷¸a‹¡´â	 8 CELLS ALLOT
VARIABLE ¼·³b¬wÈ
VARIABLE ¼£¡µÅ¬÷¸a
: ´|¸a‹¡´â  ( -- )
    £¡µÅ¬÷¸a @	¼£¡µÅ¬÷¸a @ 7 AND CELLS £¡µÅ¬÷¸a‹¡´â + !
    1 ¼£¡µÅ¬÷¸a +! ;
: ¬wÈ‹¡´â  ( ¬wÈ -- )
    ¼·³b¬wÈ @ 7 AND CELLS ‹i®A·³b¬wÈ‹¡´â + !
    1 ¼·³b¬wÈ +! ;

DECIMAL
: |  ( "<spaces>name" -- )   ' , ;
: Äe ;
: µ¡É¡ aÈa:  ( width -- )
	CREATE , ;
: ;µ¡É¡ aÈa
	DOES>
	TUCK @				\ º­¡ ¹·ŸA Äe®
	‹i®A·³b¬wÈ @
	DUP ¬wÈ‹¡´â ´|¸a‹¡´â	      \ Ðe‹i‹i¸a·¡¡e
	* + 2* CELLS + CELL+
	DUP >R
	@ EXECUTE
	R> CELL+
	@ EXECUTE
	‹i®A·³b¬wÈ ! ;

0 CONSTANT >0 IMMEDIATE
1 CONSTANT >1 IMMEDIATE
2 CONSTANT >2 IMMEDIATE
3 CONSTANT >3 IMMEDIATE
5 CONSTANT >5 IMMEDIATE

HEX
\ ¤e—¡ˆa ÑÁ¡e µ¡Ÿe½¢{µA ·¶·a¡e ”a·qÐ—µA¬á ·³b¤h·q
: ¤e—¡¶áÃ¡¹¡¸÷	( -- )	 VIR_X @ 1+ MAX-X = ( -1|0) NEGATE VIR_X+! ;
: £¡µÅ¬÷¸a¥¡µa	 ( -- )   VIR_X @  VIR_Y @  £¡µÅ¬÷¸a @	HFONT! ;
: ‹a·	( -- )	 VIR_X @ VIR_Y @  BL  EFONT! ;
: À¶‘!  ( -- )   À¶‘¸a  £¡µÅ¬÷¸a !  1 ¬wÈ‹¡´â  ´|¸a‹¡´â ;
: £¡µÅ¬÷¸aÁ¡¬÷	( -- Á¡¬÷ )   £¡µÅ¬÷¸a @  Á¡¬÷¥¥  AND 0A RSHIFT ;
: £¡µÅ¬÷¸aº—¬÷	( -- º—¬÷ )   £¡µÅ¬÷¸a @  º—¬÷¥¥  AND 05 RSHIFT ;
: £¡µÅ¬÷¸a¤hÃ±	( -- ¤hÃ± )   £¡µÅ¬÷¸a @  ¤hÃ±¥¥  AND ;
: ·³b‰­¢  ( -- 0 )   £¡µÅ¬÷¸a¥¡µa 0 ;
: {¸a¤aŽ¡  ( ˆt »¡¶‘¥¥ -- )   £¡µÅ¬÷¸a @  AND	OR  £¡µÅ¬÷¸a ! ;
: Á¡¬÷¤aŽ¡  ( Á¡¬÷ -- )   0A LSHIFT  Á¡¬÷»¡¶‘¥¥ {¸a¤aŽ¡ ;
: º—¬÷¤aŽ¡  ( º—¬÷ -- )   05 LSHIFT  º—¬÷»¡¶‘¥¥ {¸a¤aŽ¡ ;
: ¤hÃ±¤aŽ¡  ( ¤hÃ± -- ) 	     ¤hÃ±»¡¶‘¥¥ {¸a¤aŽ¡ ;
: Á¡¬÷!   ( Á¡¬÷ -- 0 )   ¤e—¡¶áÃ¡¹¡¸÷	Á¡¬÷¤aŽ¡ ·³b‰­¢ ;
: º—¬÷!   ( º—¬÷ -- 0 ) 		º—¬÷¤aŽ¡ ·³b‰­¢ ;

: µÅ¬÷	 (    0 -- 16§¡Ëa¸a )	DROP	  £¡µÅ¬÷¸a @  À¶‘! ;
: µÅ+ch  ( char -- 16§¡Ëa¸a )	‹i®AÉ· !  £¡µÅ¬÷¸a @  À¶‘! ;
: µÅ+Á¡  ( Á¡¬÷ -- 16§¡Ëa¸a )	£¡µÅ¬÷¸a @  À¶‘!  SWAP Á¡¬÷¤aŽ¡ ;
: µÅ+º—  ( º—¬÷ -- 16§¡Ëa¸a )	£¡µÅ¬÷¸a @  À¶‘!  SWAP º—¬÷¤aŽ¡ ;

: –á¡	( –õ®A -- 0 )
    DROP -2 ¼£¡µÅ¬÷¸a +!
    ¼£¡µÅ¬÷¸a @ 7 AND CELLS £¡µÅ¬÷¸a‹¡´â + @  £¡µÅ¬÷¸a !  ·³b‰­¢ ;
: >–á  ( -- ´|¬wÈ )
    -2 ¼·³b¬wÈ +!
    ¼·³b¬wÈ @ 7 AND CELLS ‹i®A·³b¬wÈ‹¡´â + @ ;

CREATE Á¡¬÷>‰sÁ¡¬÷Îa  \ ‰sÁ¡¬÷·i  e—i® ·¶“e ˆA,”A,¤A,¬A,¸AµA ”Ð¬á“e 1,
		      \ ´a“¡¡e 0
\ *   À¶‘  ˆA	 ŒA   A   ”A	˜A   œA    A   ¤A   ¨A
  0 C, 0 C, 1 C, 0 C, 0 C, 1 C, 0 C, 0 C, 0 C, 1 C, 0 C,
\ ¬A   °A   ´A	 ¸A   ¼A   ÀA	ÄA   ÈA   ÌA   ÐA
  1 C, 0 C, 0 C, 1 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,  ALIGN

CREATE Á¡¬÷>¤hÃ±Îa  \ ¤hÃ±·¡ ´a“¥ Á¡¬÷ ˜A,¨A,¼AµA ”Ð¬á“e 0, ´a“¡¡e ¤hÃ±ˆt
\ *   À¶‘  ˆA	 ŒA   A   ”A	˜A   œA    A	¤A    ¨A
  0 C, 1 C, 2 C, 3 C, 5 C, 8 C, 0 C, 9 C, 11 C, 13 C, 0 C,
\ ¬A	°A    ´A    ¸A	  ¼A   ÀA    ÄA    ÈA	 ÌA    ÐA
  15 C, 16 C, 17 C, 18 C, 0 C, 19 C, 1A C, 1B C, 1C C, 1D C,  ALIGN

CREATE ¤hÃ±>Ñ»¤hÃ±Á¡¬÷Îa  \ ‰s¤hÃ±: ¶á8§¡Ëa“e Ñ»¤hÃ±ˆt, ´aœ8§¡Ëa“e Á¡¬÷ˆt
			  \ Ñ»¤hÃ±: ¶á8§¡Ëa“e	À¶‘ˆt, ´aœ8§¡Ëa“e Á¡¬÷ˆt
\  *   À¶‘  „B     „C	   „D	  „E	 „F	„G     „H     „I
   0 ,	0 , 0102 , 0103 , 020B , 0104 , 050E , 0514 , 0105 , 0107 ,
\  „J	  „K	 „L	„M     „N     „O     „P     „Q	   *	„S
  0902 , 0908 , 0909 , 090B , 0912 , 0913 , 0914 , 0108 ,  0 , 0109 ,
\  „T	  „U	 „V	„W     „X     „Y     „Z     „[	   „\	  „]
  130B , 010B , 010C , 010D , 010E , 0110 , 0111 , 0112 , 0113 , 0114 ,

CREATE œAÁ¡¬÷>‰s¤hÃ±Îa	\ œA‰Á ‰s¤hÃ±·i  e—i® ·¶“e Á¡¬÷·¡¡e ‰s¤hÃ±ˆt, ´a“¡¡e 0
\ *   À¶‘   ˆA   ŒA   A   ”A	 ˜A   œA    A	 ¤A    ¨A
  0 C, 0 C, 0A C, 0 C, 0 C, 0 C, 0 C, 0 C, 0B C, 0C C, 0 C,
\ ¬A	°A   ´A   ¸A   ¼A   ÀA	 ÄA   ÈA    ÌA	  ÐA
  0D C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0E C, 0F C, 10 C,  ALIGN

CREATE ‰s>Ñ»º—¬÷Îa  \ ‰sº—¬÷µA ”Ð ´|Ñ»º—¬÷ˆt, ‰sº—¬÷·¡ ´a“¡¡e 0
\   *	*   À¶‘ „a   „   „¡	„Á   „á    *	*
   0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,
\ …A   …a   …	  …¡   …Á    …á    *	*    †A    †a
   0 C, 0 C, 0 C, 0 C, 0D C, 0D C, 0 C, 0 C, 0D C, 0 C,
\ †   †¡    †Á    †á	 *    *    ‡A	‡a   ‡   ‡¡
  0 C, 14 C, 14 C, 14 C, 0 C, 0 C, 0 C, 0 C, 1B C, 0 C,

: Á¡¬÷>‰sÁ¡¬÷? ( Á¡¬÷ -- Á¡¬÷ 0 | ‰sÁ¡¬÷ -1 )
    DUP £¡µÅ¬÷¸aÁ¡¬÷ = IF
	DUP CHARS Á¡¬÷>‰sÁ¡¬÷Îa + C@ 1 = IF 1+ -1 EXIT THEN  THEN
    0 ;
: ‰sÁ¡¬÷?  ( Á¡¬÷ -- 0 | 16§¡ËaÐe‹i¸a )
    Á¡¬÷>‰sÁ¡¬÷? IF   Á¡¬÷¤aŽ¡ ·³b‰­¢
		 ELSE £¡µÅ¬÷¸a @ SWAP À¶‘!  Á¡¬÷¤aŽ¡ THEN ;
: Á¡¬÷>¤hÃ±?  ( Á¡¬÷ -- Á¡¬÷ 0 | ¤hÃ± -1 )
	DUP CHARS  Á¡¬÷>¤hÃ±Îa + C@ DUP IF NIP -1 EXIT THEN ;
: œAÁ¡¬÷>‰s¤hÃ±   ( Á¡¬÷ -- 0|‰s¤hÃ± )	 CHARS	œAÁ¡¬÷>‰s¤hÃ±Îa + C@ ;
: ‰s>Ñ»º—¬÷   ( º—¬÷ -- 0|Ñ»º—¬÷ )   CHARS  ‰s>Ñ»º—¬÷Îa + C@ ;
: ‰s>´|Ñ»¤hÃ± ( ¤hÃ± -- 0|Ñ»¤hÃ± )
	CELLS  ¤hÃ±>Ñ»¤hÃ±Á¡¬÷Îa +  @  8 RSHIFT  DUP À¶‘¤hÃ± <>  AND ;
: ¤hÃ±>¤hÃ±Á¡¬÷  ( ¤hÃ± -- ¤hÃ± Á¡¬÷ )
	CELLS ¤hÃ±>Ñ»¤hÃ±Á¡¬÷Îa + @  DUP 8 RSHIFT  SWAP 0FF AND ;

\ ”}­¡Ÿ¡ˆa ˜A, ¨A, ¼A·¡¡e µÅ¬÷, ´a“¡¡e ¤hÃ±µA ý‰¡ ‰­¢
: ¤hÃ±?  ( Á¡¬÷ -- 0 | 16§¡ËaÐe‹i¸a )
    Á¡¬÷>¤hÃ±?	IF   ¤hÃ±¤aŽ¡  ·³b‰­¢
		ELSE £¡µÅ¬÷¸a @  SWAP  À¶‘!  Á¡¬÷¤aŽ¡	THEN ;
: >3?  ( -- 3|4 )   £¡µÅ¬÷¸aº—¬÷  ‰s>Ñ»º—¬÷ 0= ( -1|0) 4 + ;
: >5?  ( -- 2|5 )   £¡µÅ¬÷¸a¤hÃ±  À¶‘¤hÃ± = ( -1|0) 3 * 5 + ;
: >6?  ( -- 2|6 )   £¡µÅ¬÷¸a¤hÃ±  À¶‘¤hÃ± = ( -1|0) 2* 2* 6 + ;

: Á¡¬÷>‰s¤hÃ±?	( Á¡¬÷ -- Á¡¬÷ 0 | ‰s¤hÃ± -1 )
    £¡µÅ¬÷¸a¤hÃ±
    CASE
      ( „B) 02 OF DUP ( ¬A) 0B = IF DROP 04 -1 EXIT THEN  ENDOF
      ( „S) 13 OF DUP ( ¬A) 0B = IF DROP 14 -1 EXIT THEN  ENDOF
      ( „E) 05 OF DUP ( ¸A) 0E = IF DROP 06 -1 EXIT THEN
		  DUP ( ÐA) 14 = IF DROP 07 -1 EXIT THEN  ENDOF
      ( „I) 09 OF DUP œAÁ¡¬÷>‰s¤hÃ± ?DUP IF NIP -1 EXIT THEN  ENDOF
    ENDCASE  0 ;

: ‰s¤hÃ±?  ( Á¡¬÷ -- 0 | 16§¡ËaÐe‹i¸a )
	Á¡¬÷>‰s¤hÃ±? IF  ¤hÃ±¤aŽ¡ ·³b‰­¢  ELSE  µÅ+Á¡  THEN ;
: Ñ»¤hÃ±  ( –õ®A -- 0 )
	DROP  £¡µÅ¬÷¸a¤hÃ±  ‰s>´|Ñ»¤hÃ± ¤hÃ±¤aŽ¡  ·³b‰­¢ ;

: º—¬÷>‰sº—¬÷?	( º—¬÷ -- º—¬÷ 0 | ‰sº—¬÷ -1 )
    £¡µÅ¬÷¸aº—¬÷
    CASE
      ( ‡a) 1B OF DUP ( ‡¡) 1D = IF DROP 1C -1 EXIT THEN
		  0 EXIT				    ENDOF
      ( …¡) 0D OF DUP ( „a) 03 = IF DROP 0E -1 EXIT THEN
		  DUP ( „) 04 = IF DROP 0F -1 EXIT THEN
		  DUP ( ‡¡) 1D = IF DROP 12 -1 EXIT THEN
		  0 EXIT				    ENDOF
      ( †) 14 OF DUP ( „á) 07 = IF DROP 15 -1 EXIT THEN
		  DUP ( …A) 0A = IF DROP 16 -1 EXIT THEN
		  DUP ( ‡¡) 1D = IF DROP 17 -1 EXIT THEN
		  0 EXIT				    ENDOF
    ENDCASE  0 ;

: ‰sº—¬÷?  ( º—¬÷ -- 0 | 16§¡ËaÐe‹i¸a )
	º—¬÷>‰sº—¬÷? IF  º—¬÷¤aŽ¡ ·³b‰­¢  ELSE  µÅ+º—  THEN ;
: Ñ»º—¬÷  ( –õ®A -- 0 )
	DROP  £¡µÅ¬÷¸aº—¬÷  ‰s>Ñ»º—¬÷ º—¬÷¤aŽ¡	·³b‰­¢ ;

\ £¡µÅ¬÷¸aµA¬á ¤hÃ±·i ¨… ‹i¸aŸi µÅ¬÷¸a¡ ¥¡‰¡
\ ‹a ¤hÃ±·i £¡µÅ¬÷¸a· Á¡¬÷µA ý‰¡ º—¬÷·i £¡µÅ¬÷¸aµA ý·q
: µÅ+Á¡º—  ( º—¬÷ -- 16§¡Ëa¸a )
    £¡µÅ¬÷¸a¤hÃ± ¤hÃ±>¤hÃ±Á¡¬÷		\ º—¬÷ ¬¤hÃ± Á¡¬÷
    SWAP ¤hÃ±¤aŽ¡ £¡µÅ¬÷¸a @		\ º—¬÷ Á¡¬÷ 16§¡ËaÐe‹i¸a
    À¶‘!  SWAP Á¡¬÷¤aŽ¡ 2 ¬wÈ‹¡´â ´|¸a‹¡´â  SWAP º—¬÷¤aŽ¡ ;

CREATE µw®A>Ðe‹i¸a
\  a> A  b>‡A  c>ÀA  d>´A  e>”A  f>œA  g>ÐA  h>…¡  i>„¡  j>„á
   308 , 41A , 310 , 30D , 305 , 307 , 314 , 40D , 405 , 407 ,
\  k>„a  l>‡¡  m>‡a  n>†  o>„  p>…A  q>¤A  r>ˆA  s>A  t>¬A
   403 , 41D , 41B , 414 , 404 , 40A , 309 , 302 , 304 , 30B ,
\  u>…a  v>ÌA  w>¸A  x>ÈA  y>†a  z>ÄA
   40B , 313 , 30E , 312 , 413 , 311 ,

CREATE µw¶õ®A>Ðe‹i¸a
\  A>A	 B>B   C>C   D>D   E>˜A  F>F   G>G   H>H   I>I	 J>J
   041 , 042 , 043 , 044 , 306 , 046 , 047 , 048 , 049 , 04A ,
\  K>K	 L>L   M>M   N>N   O>„Á  P>…  Q>¨A  R>ŒA  S>S	 T>°A
   04B , 04C , 04D , 04E , 406 , 40C , 30A , 303 , 053 , 30C ,
\  U>U	 V>V   W>¼A  X>X   Y>Y	 Z>Z
   055 , 056 , 30F , 058 , 059 , 05A ,

\ EKEY ¡ ¤h·e ˆtµA¬á ‹i®A· ¹·ŸAŸi '>‹i¸a-2¤é¯¢'¡ ñ‹±
\ 2¤é¯¢µA¬á ‹i®A ¹·ŸA“e ”a¬õ ˆa»¡:
\ Ðeµw¤aŽ‘(1), –õ®A(2), ”}­¡Ÿ¡(3), Ñ©­¡Ÿ¡(4), a á»¡(0)
: >‹i®Aˆt‰Á¹·ŸA-2¤é¯¢  ( ‹i®Aˆt -- ‹i®Aˆt' ¹·ŸA )
   0FF AND						\ special key “e ¢¯¡
   DUP BL = ¶E½¢¶õ®A’‰Ÿ±? AND IF DROP 0 1 EXIT THEN	\ Ðeµw¤aŽ‘‹i®A·¡¡e 0 1
   DUP –õ®A=  IF DROP 8 2 EXIT THEN			\ –õ‹i®A·¡¡e
   DUP [CHAR] A [CHAR] Z 1+ WITHIN ‹i®A·³b¬wÈ @ AND IF \ Ðe‹i·³b·¡‰¡ A-Z ·¡¡e
	[CHAR] A -  CELLS   µw¶õ®A>Ðe‹i¸a
	[ µw®A>Ðe‹i¸a µw¶õ®A>Ðe‹i¸a - ] LITERAL CapsLock? AND
	+ + @  DUP 0FF AND SWAP 8 RSHIFT EXIT			THEN
   DUP [CHAR] a [CHAR] z 1+ WITHIN ‹i®A·³b¬wÈ @ AND IF \ Ðe‹i·³b·¡‰¡ a-z ·¡¡e
	[CHAR] a -  CELLS   µw®A>Ðe‹i¸a
	[ µw¶õ®A>Ðe‹i¸a µw®A>Ðe‹i¸a - ] LITERAL CapsLock? AND
	+ + @  DUP 0FF AND SWAP 8 RSHIFT EXIT			THEN
   0 ;					\ a á»¡

\ 2¤é¯¢ ¸aÌe ¬wÈ
\  0 : µw¢… ·³b
\  1 : Ðe‹i·³b ¯¡¸b
\  2 : Á¡¬÷ ·³b
\  3 : Á¡¬÷+º—¬÷ ·³b ( Á¡¬÷µA À¶‘ ‹i¸a•¡ Ðá¶w )
\  4 : Á¡¬÷+‰sº—¬÷ ·³b ( Á¡¬÷µA À¶‘ ‹i¸a•¡ Ðá¶w )
\  5 : Á¡¬÷+º—¬÷+¤hÃ± ·³b
\  6 : Á¡¬÷+º—¬÷+‰s¤hÃ± ·³b
5 Äe µ¡É¡ aÈa: >‹i¸a-2¤é¯¢
\ ·³b|   a á»¡?  | Ðeµw¤aŽ‘?	|   –õ®A?    |	  ”}­¡Ÿ¡?    |	 Ñ©­¡Ÿ¡?   |
\ ¬wÈ----------------------------------------------------------------------
 ( 0) | ‹a·  | >0 | À¶‘! | >1 | ‹a· | >0  | ‹a·    | >0  | ‹a·    | >0
 ( 1) | ‹a·  | >1 | ‹a·  | >0 | ‹a· | >1  | Á¡¬÷!   | >2  | º—¬÷!   | >3
 ( 2) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | ‰sÁ¡¬÷? | >2  | º—¬÷!   | >3
 ( 3) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | ¤hÃ±?   | >5? | ‰sº—¬÷? | >3?
 ( 4) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | ¤hÃ±?   | >5? | µÅ+º—   | >3
 ( 5) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | ‰s¤hÃ±? | >6? | µÅ+Á¡º— | >3
 ( 6) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | µÅ+Á¡   | >2  | µÅ+Á¡º— | >3
;µ¡É¡ aÈa

\
\  3¤é¯¢ ¸aÌe ·³bµA Ï©¶aÐe { i—i
\

CREATE œA¤hÃ±>‰s¤hÃ±Îa	\ œA‰Á ‰s¤hÃ±·i  e—i® ·¶“e ¤hÃ±·¡¡e ‰s¤hÃ±ˆt, ´a“¡¡e 0
\  *   À¶‘   „B    „C	  „D	„E    „F    „G	  „H	„I
   0 C,  0 C, 0A C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,
\ „J	„K    „L    „M	  „N	„O    „P    „Q	   *	„S
   0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C, 0B C,  0 C, 0C C,
\ „T	„U    „V    „W	  „X	„Y    „Z    „[	  „\	„]
   0 C, 0D C,  0 C,  0 C,  0 C,  0 C,  0 C, 0E C, 0F C, 10 C, ALIGN

: ¤hÃ±!    ( ¤hÃ± -- 0 )	  ¤hÃ±¤aŽ¡ ·³b‰­¢ ;
: µÅ+¤hÃ±  ( ¤hÃ± -- 16§¡Ëa¸a )   £¡µÅ¬÷¸a @  À¶‘!  SWAP ¤hÃ±¤aŽ¡ ;
: œA¤hÃ±>‰s¤hÃ±  ( ¤hÃ± -- 0|‰s¤hÃ± )	CHARS  œA¤hÃ±>‰s¤hÃ±Îa + C@ ;
: ¤hÃ±>‰s¤hÃ±?	( ¤hÃ± -- ¤hÃ± 0 | ‰s¤hÃ± -1 )
    £¡µÅ¬÷¸a¤hÃ±
    CASE
      ( „B) 02 OF DUP ( „B) 02 = IF DROP 03 -1 EXIT THEN
		      ( „U) 15 = IF DROP 04 -1 EXIT THEN  ENDOF
      ( „S) 13 OF DUP ( „U) 15 = IF DROP 14 -1 EXIT THEN  ENDOF
      ( „U) 15 OF DUP ( „U) 15 = IF DROP 16 -1 EXIT THEN  ENDOF
      ( „E) 05 OF DUP ( „X) 18 = IF DROP 06 -1 EXIT THEN
		  DUP ( „]) 1D = IF DROP 07 -1 EXIT THEN  ENDOF
      ( „I) 09 OF DUP œA¤hÃ±>‰s¤hÃ± ?DUP IF NIP -1 EXIT THEN  ENDOF
    ENDCASE  0 ;
: ‰s¤hÃ±?  ( ¤hÃ± -- 0 | 16§¡ËaÐe‹i¸a )
	¤hÃ±>‰s¤hÃ±? IF  ¤hÃ±¤aŽ¡ ·³b‰­¢  ELSE  µÅ+¤hÃ±  THEN ;

CREATE 3¤é®A>Ðe‹i¸a
\  !>„X  ">"   #>#   $>$   %>%   &>&   '>ÈA  (>(   )>)   *>*   +>+   ,>,
   518 , 022 , 023 , 024 , 025 , 026 , 312 , 028 , 029 , 02A , 02B , 02C ,
\  ->-	 .>.   />…¡  0>ÄA  1>„]  2>„V  3>„S  4>†a  5>‡A  6>„¡  7>…  8>‡
   02D , 02E , 40D , 311 , 51D , 516 , 513 , 413 , 41A , 405 , 40C , 41C ,
\  9>†  :>:   ;>¤A  <>2   =>=	 >>3   ?>?   @>@   A>„H  B>!   C>„K  D>„J
   414 , 03A , 309 , 032 , 03D , 033 , 03F , 040 , 508 , 021 , 50B , 50A ,
\  E>„Z  F>„C  G>/   H>'   I>8   J>4   K>5   L>6   M>1   N>0   O>9   P>>
   51A , 503 , 02F , 027 , 038 , 034 , 035 , 036 , 031 , 030 , 039 , 03E ,
\  Q>„\  R>„Á  S>„G  T>;   U>7	 V>„P  W>„[  X>„T  Y>8	 Z>„Y  [>[   \>\
   51C , 406 , 507 , 03B , 037 , 510 , 51B , 514 , 038 , 519 , 05B , 05C ,
\  ]>]	 ^>^   _>_   `>`   a>„W  b>†  c>…A  d>‡¡  e>…a  f>„a  g>‡a  h>A
   05D , 05E , 05F , 060 , 517 , 414 , 40A , 41D , 40B , 403 , 41B , 304 ,
\  i> A  j>´A  k>ˆA  l>¸A  m>ÐA  n>¬A  o>ÀA  p>ÌA  q>„U  r>„  s>„E  t>„á
   308 , 30D , 302 , 30E , 314 , 30B , 310 , 313 , 515 , 404 , 505 , 407 ,
\  u>”A  v>…¡  w>„I  x>„B  y>œA  z>„Q  {>{   |>|   }>}	 ~>~
   305 , 40D , 509 , 502 , 307 , 511 , 07B , 07C , 07D , 07E ,

\ EKEY ¡ ¤h·e ˆtµA¬á ‹i®A· ¹·ŸAŸi '>‹i¸a-3¤é¯¢'·a¡ ñ‹±
\ 3¤é¯¢µA¬á ‹i®A ¹·ŸA“e ”a¬õ ˆa»¡:
\   Ðeµw¤aŽ‘(1), –õ®A(2), Á¡¬÷(3), º—¬÷(4), ¤hÃ±(5), a á»¡(0)
: >‹i®Aˆt‰Á¹·ŸA-3¤é¯¢  ( ‹i®Aˆt -- ‹i®Aˆt' ¹·ŸA )
   0FF AND						\ special key “e ¢¯¡
   DUP BL = ¶E½¢¶õ®A’‰Ÿ±? AND IF DROP 0 1 EXIT THEN	\ Ðeµw¤aŽ‘‹i®A·¡¡e 0 1
   DUP –õ®A=  IF DROP 8 2 EXIT THEN			\ –õ‹i®A·¡¡e
   DUP BL > 0= IF 0 EXIT THEN				\ ¹A´á¢…¸aa §¥Äe·¡¡e
   ‹i®A·³b¬wÈ @ 0= IF 0 EXIT THEN			\ µw¢…·³b
   CapsLock? IF
       DUP [CHAR] A [CHAR] Z 1+ WITHIN IF
	   [ CHAR a CHAR A - ] LITERAL +
       ELSE DUP [CHAR] a [CHAR] z 1+ WITHIN IF
	   [ CHAR A CHAR a - ] LITERAL +    THEN THEN THEN
   [CHAR] ! - CELLS 3¤é®A>Ðe‹i¸a + @
   DUP 0FF AND SWAP 8 RSHIFT ;

\ 3¤é¯¢ ¸aÌe ¬wÈ
\  0 : µw¢… ·³b
\  1 : Ðe‹i·³b ¯¡¸b
\  2 : Á¡¬÷ ·³b
\  3 : Á¡¬÷+º—¬÷ ·³b ( Á¡¬÷µA À¶‘ ‹i¸a•¡ Ðá¶w )
\  4 : Á¡¬÷+‰sº—¬÷ ·³b ( Á¡¬÷µA À¶‘ ‹i¸a•¡ Ðá¶w )
\  5 : Á¡¬÷+º—¬÷+¤hÃ± ·³b
\  6 : Á¡¬÷+º—¬÷+‰s¤hÃ± ·³b
6 Äe µ¡É¡ aÈa: >‹i¸a-3¤é¯¢
\ ·³b|   a á»¡?  | Ðeµw¤aŽ‘?	|   –õ®A?    |	   Á¡¬÷?    |	 º—¬÷?	    |	¤hÃ±?	   |
\ ¬wÈ--------------------------------------------------------------------------------------
 ( 0) | ‹a·  | >0 | À¶‘! | >1 | ‹a· | >0  | ‹a·    | >0 | ‹a·    | >0  | ‹a·     | >0
 ( 1) | ‹a·  | >1 | ‹a·  | >0 | ‹a· | >1  | Á¡¬÷!   | >2 | º—¬÷!   | >3  | ¤hÃ±!    | >5
 ( 2) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | ‰sÁ¡¬÷? | >2 | º—¬÷!   | >3  | ¤hÃ±!    | >5
 ( 3) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | µÅ+Á¡   | >2 | ‰sº—¬÷? | >3? | ¤hÃ±!    | >5
 ( 4) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | µÅ+Á¡   | >2 | µÅ+º—   | >3  | ¤hÃ±!    | >5
 ( 5) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | µÅ+Á¡   | >2 | µÅ+º—   | >3  | ‰s¤hÃ±?  | >5?
 ( 6) | µÅ+ch | >1 | µÅ¬÷  | >0 | –á¡ | >–á | µÅ+Á¡   | >2 | µÅ+º—   | >3  | µÅ+¤hÃ±  | >5
;µ¡É¡ aÈa

' >‹i¸a-2¤é¯¢         VALUE '>‹i¸a
' >‹i®Aˆt‰Á¹·ŸA-2¤é¯¢ VALUE '>‹i®Aˆt‰Á¹·ŸA
: >‹i¸a 	  '>‹i¸a         EXECUTE ;
: >‹i®Aˆt‰Á¹·ŸA   '>‹i®Aˆt‰Á¹·ŸA EXECUTE ;

NONSTANDARD-WORDLIST SET-CURRENT
: 2BUL ( -- )
    ['] >‹i¸a-2¤é¯¢         TO '>‹i¸a
    ['] >‹i®Aˆt‰Á¹·ŸA-2¤é¯¢ TO '>‹i®Aˆt‰Á¹·ŸA ;
: 3BUL ( -- )
    ['] >‹i¸a-3¤é¯¢         TO '>‹i¸a
    ['] >‹i®Aˆt‰Á¹·ŸA-3¤é¯¢ TO '>‹i®Aˆt‰Á¹·ŸA ;
Ðe‹i·³Â‰b-WORDLIST SET-CURRENT

: HEKEY
   ‹i®AÉ· @ ?DUP IF		\ ‹i®AÉ·µA ‹i¸aˆa ·¶·a¡e ‹a ‹i¸aŸi ¥¡‘
     DUP 0FF00 AND		\ ‹i®AÉ·µA – ‹i¸aˆa ·¶·a¡e ¶á 8 §¡ËaŸi ¥¡‘
     IF DUP 8 RSHIFT
	SWAP 0FF AND
	‹i®AÉ· ! EXIT THEN
     0 ‹i®AÉ· ! EXIT  THEN	\ ‹i®AÉ·· Ðe ‹i¸aŸi ¥¡‘
   £¡µÅ¬÷¸a @  À¶‘¸a  <>  ‹i®A·³b¬wÈ @  AND	IF  £¡µÅ¬÷¸a¥¡µa  THEN
   BEGIN  BEGIN PAUSE RX? UNTIL  RX@  >‹i®Aˆt‰Á¹·ŸA  >‹i¸a  ?DUP UNTIL
	\ BEGIN ... UNTIL ·i ¨a¹a aµ© ˜ ”á£¡µA q·e ˆt·e
	\   8 §¡Ëa : Ðe ‹i¸a : ‹a·
	\  16 §¡Ëa : – ‹i¸a : µÅ¬÷ , µÅ+Á¡ , µÅ+Á¡º— , ...
	\  16 §¡Ëa : ­A ‹i¸a : µÅ¬÷+ch ( char ˆt·e ‹i®AÉ· µA —i´á ·¶·q )
   DUP 0FF00 AND IF		\ 16§¡Ëa Ðe‹i¸a·¡¡e a á»¡ ‹i¸aŸi ‹i®AÉ·µA ”q·q
     DUP 8 RSHIFT SWAP 0FF AND	\ ”á£¡: ¶á8§¡Ëa ´aœ8§¡Ëa
     ‹i®AÉ· @ ?DUP IF		\ ”á£¡: ¶á8§¡Ëa ´aœ8§¡Ëa char
       SWAP 8 LSHIFT OR THEN
     ‹i®AÉ· !
   THEN ;

: HEKEY?
    £¡µÅ¬÷¸a @ À¶‘¸a <> ‹i®AÉ· @ OR IF -1 ELSE RX? THEN ;

: SET-TEXT-I/O	( -- )
    ['] RX? TO 'ekey?
    ['] RX@ TO 'ekey
    ['] TX! TO 'emit ;

: SET-HGRAPHIC-I/O
    ['] HEKEY? TO 'ekey?
    ['] HEKEY  TO 'ekey
    ['] HEMIT  TO 'emit ;

NONSTANDARD-WORDLIST SET-CURRENT

DECIMAL VARIABLE Œq¤b·±
0 60 CELLS 60 CELLS HAT multiI/O  multiI/O BUILD
0 60 CELLS 60 CELLS HAT HCURSOR   HCURSOR BUILD

: TEXT
    textmode# SET-MODE
    SET-TEXT-I/O
    0 TO GRAPHIC?
    multiI/O SLEEP  HCURSOR SLEEP ;

HEX
CODE ReadClockCount  ( -- ud )
    BX PUSH,
    AX AX XOR,	\ MOV AH,00
    1A INT,
    DX PUSH,
    CX BX MOV,
    NEXT,
END-CODE

DECIMAL
: Œq¤b·±¹¡¸é  ( -- )
    ReadClockCount
    BEGIN 2DUP DNEGATE ReadClockCount D+ DROP UNTIL 2DROP
    ReadClockCount
    -1 0 DO PAUSE 0 0 BL EFONT! 0 0 BL EFONT!
	    2DUP DNEGATE ReadClockCount D+ DROP
	    IF 2DROP I Œq¤b·± ! UNLOOP EXIT THEN LOOP
    2DROP -1 Œq¤b·± ! ;

: HGRAPHIC
    VGA? IF
	3 TO textmode#
	['] VGA-SET-GRAPHIC TO 'SET-GRAPHIC
	VGA-Y>SegTable Y>SegTable !
	30 TO MAX-Y		\ 640X480 Ð¬w•¡; 480 / 16 = 30 º‰
    ELSE HERC? IF
	7 TO textmode#
	['] HERC-SET-GRAPHIC TO 'SET-GRAPHIC
	HERC-Y>SegTable Y>SegTable !
	25 TO MAX-Y		\ 640X400 Ð¬w•¡; 400 / 16 = 25 º‰
	MAX-Y 0 DO 1 MAX-X* 0 DO I J BL EFONT! LOOP LOOP
    ELSE SET-TEXT-I/O  0 TO GRAPHIC?
	 ." Korean characters can be displayed only on VGA or Hercules Graphics screen."
	 multiI/O SLEEP HCURSOR SLEEP EXIT
    THEN THEN
    VSCREEN VSCREEN-SIZE CHARS BL FILL
    SET-GRAPHIC
    0 HCHAR !
    0 ‹i®A·³b¬wÈ !
    0 ‹i®AÉ· !
    À¶‘!
    0 VSCR0 !
    0 YY !  0 VIR_Y !
    0 XX !  0 VIR_X !
    SET-HGRAPHIC-I/O
    multiI/O AWAKE  HCURSOR SLEEP  Œq¤b·±¹¡¸é  HCURSOR AWAKE ;

Ðe‹i·³Â‰b-WORDLIST SET-CURRENT

: NEW-SET-I/O
    GRAPHIC? IF SET-HGRAPHIC-I/O ELSE SET-TEXT-I/O THEN ;

HEX
: NEW-hi
    DOSCommand>PAD
    GET-MODE TO OldMode# HGRAPHIC hi
    ." ·‰e‰Á ¹A´e‰Á §¡Íw·i ¶á ·¥Èá‘U º­¡a Ða·¡ÉI wykoh¡ ¥¡ º¯³¯¡µ¡." CR
    S" BLOCKS.BLK" MAPPED-TO-BLOCK  QUIT ;

' NEW-SET-I/O TO 'init-i/o
' NEW-hi TO 'boot

: XX+!	( n -- )
   XX @ + MAX-X /MOD YY +! XX ! ;

FALSE VALUE SCREEN-UPDATED?

HEX
:NONAME multiI/O ACTIVATE
	BEGIN
	   PAUSE
	   YY @ MAX-X* XX @ + DUP VIR_Y @ MAX-X* VIR_X @ + <	IF
	      FALSE TO SCREEN-UPDATED?
	      YY @ VIR_Y @ < IF YY @ 1+ MAX-X*
			     ELSE VIR_Y @ MAX-X* VIR_X @ + THEN
	      SWAP
	      BEGIN DUP >VSCR-ADDR C@
		    DUP 80 <				IF
			 XX @ YY @ ROT EFONT! 1 XX+!	ELSE
			 8 LSHIFT >R
			 CHAR+ DUP >VSCR-ADDR C@ R> OR
			 XX @ YY @ ROT HFONT! 2 XX+!	THEN
	      CHAR+ 2DUP > 0= UNTIL 2DROP			ELSE
	      TRUE TO SCREEN-UPDATED?
	      DROP VIR_X @ XX ! VIR_Y @ YY !			THEN
	AGAIN
; EXECUTE

:NONAME HCURSOR ACTIVATE
	BEGIN
	  Œq¤b·± @ 0 DO PAUSE LOOP
	  SCREEN-UPDATED?				    IF
	    GRAPHIC?					IF
		‹i®A·³b¬wÈ @	?DUP		   IF
		    1-			  IF
		    £¡µÅ¬÷¸a¥¡µa	  ELSE
		    XX @ YY @ [CHAR] _ EFONT! THEN ELSE
		    XX @ YY @ [CHAR] - EFONT!	   THEN THEN
	    Œq¤b·± @ 0 DO PAUSE LOOP
	    GRAPHIC?				   IF
		XX @ YY @ BL EFONT!
		‹i®A·³b¬wÈ @		      IF
		    XX @ CHAR+ YY @ BL EFONT! THEN THEN     THEN
	AGAIN
; EXECUTE

Ðe‹i·³Â‰b-WORDLIST SET-CURRENT

HEX
CODE textAT-XY	( column row -- )
    2 # AH MOV,
    DX POP,
    BL DH MOV,
    BX BX XOR,
    10 INT,
    BX POP,
    NEXT,
END-CODE

FORTH-WORDLIST SET-CURRENT

\   AT-XY	( u1 u2 -- )			\ FACILITY
\		Perform implementation-dependent steps so that the next
\		character displayed will appear in column u1, row u2 of the
\		user output device, the upper left corner of which is column
\		zero, row zero.  An ambiguous condition exists if the
\		operation cannot be performed on the user output Adevice
\		with the specified parameters.
: AT-XY
   GRAPHIC? IF DUP YY ! VIR_Y ! DUP XX ! VIR_X !
   ELSE textAT-XY THEN ;

\   PAGE	( -- )				\ FACILITY
\		Move to another page for output. Actual function depends on
\		the output device.  On a terminal, PAGE clears the screen
\		and resets the cursor position to the upper left corner. On
\		a printer, PAGE performs a form feed.
DECIMAL
: PAGE
    GRAPHIC? IF MAX-Y 0 DO 1 MAX-X* 0 DO
		    BL J MAX-X* I + >VSCR-ADDR C!  I J BL EFONT!
		LOOP LOOP
		0 TO YTop
    ELSE 0 0 AT-XY 25 0 DO 80 0 DO BL EMIT LOOP LOOP
    THEN 0 0 AT-XY ;

: BYE	OldMode# SET-MODE  BYE ;

HGRAPHIC

SET-CURRENT  SET-ORDER
BASE !
