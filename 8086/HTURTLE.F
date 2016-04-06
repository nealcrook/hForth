\
\ HTURTLE.F
\ Turtle graphics package for hForth
\ Words names are in Korean.
\
\ HTURTLE.EXE“e HF86EXE.EXEŸi ¯¡¸bÐe Ò ”a·q ®…¬á¡ Í¡¯a Ìa·©—i·i µ©a¬á
\  e—e ˆõ·³“¡”a.
\
\       << OPTIONAL.F
\       << ASM8086.F
\       << COREEXT.F
\       << MSDOS.F
\       BL PARSE MULTI.F    INCLUDED
\       BL PARSE HIOMULT2.F INCLUDED
\       BL PARSE HTURTLE.F  INCLUDED
\       SAVE-SYSTEM-AS HTURTLE.EXE
\
\ 1996. 2. 21. \ Wonyong Koh

BASE @
GET-ORDER  GET-CURRENT
Ðe‹i·³Â‰b-WORDLIST GET-ORDER 1 + SET-ORDER

MARKER ~TURTLE

DECIMAL
CREATE sin16384
     0 ,    286 ,    572 ,    857 ,   1143 ,   1428 ,	1713 ,	 1997 ,   2280 ,   2563 ,
  2845 ,   3126 ,   3406 ,   3686 ,   3964 ,   4240 ,	4516 ,	 4790 ,   5063 ,   5334 ,
  5604 ,   5872 ,   6138 ,   6402 ,   6664 ,   6924 ,	7182 ,	 7438 ,   7692 ,   7943 ,
  8192 ,   8438 ,   8682 ,   8923 ,   9162 ,   9397 ,	9630 ,	 9860 ,  10087 ,  10311 ,
 10531 ,  10749 ,  10963 ,  11174 ,  11381 ,  11585 ,  11786 ,	11982 ,  12176 ,  12365 ,
 12551 ,  12733 ,  12911 ,  13085 ,  13255 ,  13421 ,  13583 ,	13741 ,  13894 ,  14044 ,
 14189 ,  14330 ,  14466 ,  14598 ,  14726 ,  14849 ,  14968 ,	15082 ,  15191 ,  15296 ,
 15396 ,  15491 ,  15582 ,  15668 ,  15749 ,  15826 ,  15897 ,	15964 ,  16026 ,  16083 ,
 16135 ,  16182 ,  16225 ,  16262 ,  16294 ,  16322 ,  16344 ,	16362 ,  16374 ,  16382 ,
 16384 ,  16382 ,  16374 ,  16362 ,  16344 ,  16322 ,  16294 ,	16262 ,  16225 ,  16182 ,
 16135 ,  16083 ,  16026 ,  15964 ,  15897 ,  15826 ,  15749 ,	15668 ,  15582 ,  15491 ,
 15396 ,  15296 ,  15191 ,  15082 ,  14968 ,  14849 ,  14726 ,	14598 ,  14466 ,  14330 ,
 14189 ,  14044 ,  13894 ,  13741 ,  13583 ,  13421 ,  13255 ,	13085 ,  12911 ,  12733 ,
 12551 ,  12365 ,  12176 ,  11982 ,  11786 ,  11585 ,  11381 ,	11174 ,  10963 ,  10749 ,
 10531 ,  10311 ,  10087 ,   9860 ,   9630 ,   9397 ,	9162 ,	 8923 ,   8682 ,   8438 ,
  8192 ,   7943 ,   7692 ,   7438 ,   7182 ,   6924 ,	6664 ,	 6402 ,   6138 ,   5872 ,
  5604 ,   5334 ,   5063 ,   4790 ,   4516 ,   4240 ,	3964 ,	 3686 ,   3406 ,   3126 ,
  2845 ,   2563 ,   2280 ,   1997 ,   1713 ,   1428 ,	1143 ,	  857 ,    572 ,    286 ,
     0 ,   -286 ,   -572 ,   -857 ,  -1143 ,  -1428 ,  -1713 ,	-1997 ,  -2280 ,  -2563 ,
 -2845 ,  -3126 ,  -3406 ,  -3686 ,  -3964 ,  -4240 ,  -4516 ,	-4790 ,  -5063 ,  -5334 ,
 -5604 ,  -5872 ,  -6138 ,  -6402 ,  -6664 ,  -6924 ,  -7182 ,	-7438 ,  -7692 ,  -7943 ,
 -8192 ,  -8438 ,  -8682 ,  -8923 ,  -9162 ,  -9397 ,  -9630 ,	-9860 , -10087 , -10311 ,
-10531 , -10749 , -10963 , -11174 , -11381 , -11585 , -11786 , -11982 , -12176 , -12365 ,
-12551 , -12733 , -12911 , -13085 , -13255 , -13421 , -13583 , -13741 , -13894 , -14044 ,
-14189 , -14330 , -14466 , -14598 , -14726 , -14849 , -14968 , -15082 , -15191 , -15296 ,
-15396 , -15491 , -15582 , -15668 , -15749 , -15826 , -15897 , -15964 , -16026 , -16083 ,
-16135 , -16182 , -16225 , -16262 , -16294 , -16322 , -16344 , -16362 , -16374 , -16382 ,
-16384 , -16382 , -16374 , -16362 , -16344 , -16322 , -16294 , -16262 , -16225 , -16182 ,
-16135 , -16083 , -16026 , -15964 , -15897 , -15826 , -15749 , -15668 , -15582 , -15491 ,
-15396 , -15296 , -15191 , -15082 , -14968 , -14849 , -14726 , -14598 , -14466 , -14330 ,
-14189 , -14044 , -13894 , -13741 , -13583 , -13421 , -13255 , -13085 , -12911 , -12733 ,
-12551 , -12365 , -12176 , -11982 , -11786 , -11585 , -11381 , -11174 , -10963 , -10749 ,
-10531 , -10311 , -10087 ,  -9860 ,  -9630 ,  -9397 ,  -9162 ,	-8923 ,  -8682 ,  -8438 ,
 -8192 ,  -7943 ,  -7692 ,  -7438 ,  -7182 ,  -6924 ,  -6664 ,	-6402 ,  -6138 ,  -5872 ,
 -5604 ,  -5334 ,  -5063 ,  -4790 ,  -4516 ,  -4240 ,  -3964 ,	-3686 ,  -3406 ,  -3126 ,
 -2845 ,  -2563 ,  -2280 ,  -1997 ,  -1713 ,  -1428 ,  -1143 ,	 -857 ,   -572 ,   -286 ,
    -0 ,    286 ,    572 ,    857 ,   1143 ,   1428 ,	1713 ,	 1997 ,   2280 ,   2563 ,
  2845 ,   3126 ,   3406 ,   3686 ,   3964 ,   4240 ,	4516 ,	 4790 ,   5063 ,   5334 ,
  5604 ,   5872 ,   6138 ,   6402 ,   6664 ,   6924 ,	7182 ,	 7438 ,   7692 ,   7943 ,
  8192 ,   8438 ,   8682 ,   8923 ,   9162 ,   9397 ,	9630 ,	 9860 ,  10087 ,  10311 ,
 10531 ,  10749 ,  10963 ,  11174 ,  11381 ,  11585 ,  11786 ,	11982 ,  12176 ,  12365 ,
 12551 ,  12733 ,  12911 ,  13085 ,  13255 ,  13421 ,  13583 ,	13741 ,  13894 ,  14044 ,
 14189 ,  14330 ,  14466 ,  14598 ,  14726 ,  14849 ,  14968 ,	15082 ,  15191 ,  15296 ,
 15396 ,  15491 ,  15582 ,  15668 ,  15749 ,  15826 ,  15897 ,	15964 ,  16026 ,  16083 ,
 16135 ,  16182 ,  16225 ,  16262 ,  16294 ,  16322 ,  16344 ,	16362 ,  16374 ,  16382 ,
 16384 ,  16382 ,  16374 ,  16362 ,  16344 ,  16322 ,  16294 ,	16262 ,  16225 ,  16182 ,
 16135 ,  16083 ,  16026 ,  15964 ,  15897 ,  15826 ,  15749 ,	15668 ,  15582 ,  15491 ,
 15396 ,  15296 ,  15191 ,  15082 ,  14968 ,  14849 ,  14726 ,	14598 ,  14466 ,  14330 ,
 14189 ,  14044 ,  13894 ,  13741 ,  13583 ,  13421 ,  13255 ,	13085 ,  12911 ,  12733 ,
 12551 ,  12365 ,  12176 ,  11982 ,  11786 ,  11585 ,  11381 ,	11174 ,  10963 ,  10749 ,
 10531 ,  10311 ,  10087 ,   9860 ,   9630 ,   9397 ,	9162 ,	 8923 ,   8682 ,   8438 ,
  8192 ,   7943 ,   7692 ,   7438 ,   7182 ,   6924 ,	6664 ,	 6402 ,   6138 ,   5872 ,
  5604 ,   5334 ,   5063 ,   4790 ,   4516 ,   4240 ,	3964 ,	 3686 ,   3406 ,   3126 ,
  2845 ,   2563 ,   2280 ,   1997 ,   1713 ,   1428 ,	1143 ,	  857 ,    572 ,    286 ,
     0 ,   -286 ,   -572 ,   -857 ,  -1143 ,  -1428 ,  -1713 ,	-1997 ,  -2280 ,  -2563 ,
 -2845 ,  -3126 ,  -3406 ,  -3686 ,  -3964 ,  -4240 ,  -4516 ,	-4790 ,  -5063 ,  -5334 ,
 -5604 ,  -5872 ,  -6138 ,  -6402 ,  -6664 ,  -6924 ,  -7182 ,	-7438 ,  -7692 ,  -7943 ,
 -8192 ,  -8438 ,  -8682 ,  -8923 ,  -9162 ,  -9397 ,  -9630 ,	-9860 , -10087 , -10311 ,
-10531 , -10749 , -10963 , -11174 , -11381 , -11585 , -11786 , -11982 , -12176 , -12365 ,
-12551 , -12733 , -12911 , -13085 , -13255 , -13421 , -13583 , -13741 , -13894 , -14044 ,
-14189 , -14330 , -14466 , -14598 , -14726 , -14849 , -14968 , -15082 , -15191 , -15296 ,
-15396 , -15491 , -15582 , -15668 , -15749 , -15826 , -15897 , -15964 , -16026 , -16083 ,
-16135 , -16182 , -16225 , -16262 , -16294 , -16322 , -16344 , -16362 , -16374 , -16382 ,
-16384 , -16382 , -16374 , -16362 , -16344 , -16322 , -16294 , -16262 , -16225 , -16182 ,
-16135 , -16083 , -16026 , -15964 , -15897 , -15826 , -15749 , -15668 , -15582 , -15491 ,
-15396 , -15296 , -15191 , -15082 , -14968 , -14849 , -14726 , -14598 , -14466 , -14330 ,
-14189 , -14044 , -13894 , -13741 , -13583 , -13421 , -13255 , -13085 , -12911 , -12733 ,
-12551 , -12365 , -12176 , -11982 , -11786 , -11585 , -11381 , -11174 , -10963 , -10749 ,
-10531 , -10311 , -10087 ,  -9860 ,  -9630 ,  -9397 ,  -9162 ,	-8923 ,  -8682 ,  -8438 ,
 -8192 ,  -7943 ,  -7692 ,  -7438 ,  -7182 ,  -6924 ,  -6664 ,	-6402 ,  -6138 ,  -5872 ,
 -5604 ,  -5334 ,  -5063 ,  -4790 ,  -4516 ,  -4240 ,  -3964 ,	-3686 ,  -3406 ,  -3126 ,
 -2845 ,  -2563 ,  -2280 ,  -1997 ,  -1713 ,  -1428 ,  -1143 ,	 -857 ,   -572 ,   -286 ,
    -0 ,    286 ,    572 ,    857 ,   1143 ,   1428 ,	1713 ,	 1997 ,   2280 ,   2563 ,
  2845 ,   3126 ,   3406 ,   3686 ,   3964 ,   4240 ,	4516 ,	 4790 ,   5063 ,   5334 ,
  5604 ,   5872 ,   6138 ,   6402 ,   6664 ,   6924 ,	7182 ,	 7438 ,   7692 ,   7943 ,
  8192 ,   8438 ,   8682 ,   8923 ,   9162 ,   9397 ,	9630 ,	 9860 ,  10087 ,  10311 ,
 10531 ,  10749 ,  10963 ,  11174 ,  11381 ,  11585 ,  11786 ,	11982 ,  12176 ,  12365 ,
 12551 ,  12733 ,  12911 ,  13085 ,  13255 ,  13421 ,  13583 ,	13741 ,  13894 ,  14044 ,
 14189 ,  14330 ,  14466 ,  14598 ,  14726 ,  14849 ,  14968 ,	15082 ,  15191 ,  15296 ,
 15396 ,  15491 ,  15582 ,  15668 ,  15749 ,  15826 ,  15897 ,	15964 ,  16026 ,  16083 ,
 16135 ,  16182 ,  16225 ,  16262 ,  16294 ,  16322 ,  16344 ,	16362 ,  16374 ,  16382 ,

HEX
\ : PLOT  ( x y -- )
\     Y>SEG SWAP 8 /MOD SWAP >R 	  \ seg_addr x/8  R: x_mod_8
\     2DUP LC@ R> CHARS XMASK + C@ OR ROT ROT LC! ;

CODE PLOT  ( x y -- )
    BX 1 SHL,
    Y>SegTable ) BX ADD,
    0 [BX] ES MOV,
    BX POP,
    BX CX MOV,
    BX 1 SHR,
    BX 1 SHR,
    BX 1 SHR,
    ES: 0 [BX] AL MOV,
    1 # AH MOV,
    CL NOT,
    7 # CL AND,
    AH CL ROL,
    AH AL OR,
    ES: AL 0 [BX] MOV,
    BX POP,
    NEXT,
END-CODE

\ : 2ROT
\     >R >R 2SWAP R> R> 2SWAP ;
\
\ : LINE  ( x1 y1 x2 y2--)
\     2OVER 2OVER ROT - ABS >R - ABS R> MAX 2 <
\     IF 2DROP PLOT EXIT THEN
\     2OVER 2OVER ROT + 1+ 2/ >R + 1+ 2/ R>
\     2DUP 2ROT RECURSE RECURSE ;

VARIABLE Delta
VARIABLE Delta/2

\ y changing faster than x
CODE steep640	\ on entry, ax = delta x, bx = delta y, cx=x1, dx=y1
    BX BP MOV,			\ for counter
    BX 1 SHR,
    BX Delta/2 ) MOV,		\ halfy
    BX BX XOR,			\ clear for cmp
6 L:
    BX PUSH,
    CX PUSH,		\ x
    DX BX MOV,
    BX 1 SHL,
    Y>SegTable ) BX ADD,
    0 [BX] ES MOV,
    CX BX MOV,
    BX 1 SHR,
    BX 1 SHR,
    BX 1 SHR,
    ES: 0 [BX] AL MOV,
    1 # AH MOV,
    CL NOT,
    7 # CL AND,
    AH CL ROL,
    AH AL OR,
    ES: AL 0 [BX] MOV,
    CX POP,
    BX POP,
    DX INC,			\ y is always increasing
    MAX-Y 16* # DX CMP,
    8 L# JL,
    DX DX XOR,
8 L:
    Delta ) BX ADD,		\ = bx + delta_y
    Delta/2 ) BX CMP,		\ bx > halfy ?
    7 L# JLE,
    SI BX SUB,			\ bx - delta_y
    DI CX ADD,			\ inc or dec x
    MAX-X 8 * # CX SUB,
    7 L# JNS,
    MAX-X 8 * # CX ADD,
    7 L# JNS,
    MAX-X 8 * # CX ADD,
7 L:
    BP DEC,
    6 L# JGE,
    BP POP,
    SI POP,
    BX POP,
    NEXT,
END-CODE

\ on exit, cx=x1, dx=y1, ax=x2, bx=y2
CODE line640  ( x1 y1 x2 y2 -- )	\ writes to screen directly
    AX POP,
    DX POP,
    CX POP,
    SI PUSH,   ( used to hold direction)
    BP PUSH,   ( used as counter)
\ see if we'll inc or dec x, y (draws in any direction)
    DX BX SUB,			\ bx <- y2-y1 (delta y)
    2 L# JGE,
    BX DX ADD,			\ dx <- y2
    BX NEG,			\ abs(delta y)
    CX AX XCHG,
2 L:
    BX SI MOV,                  \ delta_y(BX) to SI
    CX AX SUB,                  \ x2 - x1 = delta_x
    1 # DI MOV,                 \ di to increment x
    4 L# JGE,
    -1 # DI MOV,                \ di to decrement x
    AX NEG,                     \ abs(delta x)
4 L:
    \ adjust x1(CX), y1(DX) in proper range
    AX PUSH,
    DX PUSH,
    CX AX MOV,
    CWD,
    MAX-X 8 * # BP MOV,
    BP IDIV,
    DX DX OR,
    1 L# JNS,
    BP DX ADD,
1 L:
    DX CX MOV,
    DX POP,
    DX AX MOV,
    CWD,
    MAX-Y 16* # BP MOV,
    BP IDIV,
    DX DX OR,
    8 L# JNS,
    BP DX ADD,
8 L:
    AX POP,
    AX Delta ) MOV,		\ abs(delta x)
    BX AX CMP,			\ delta_x - delta_y
    5 L# JGE,
    ' steep640 # JMP,           \ y changes faster than x
5 L:
\ x changing faster than y
    AX BP MOV,			\ for counter
    AX 1 SHR,
    AX Delta/2 ) MOV,		\ halfx
    BX BX XOR,			\ clear for cmp
6 L:
    BX PUSH,
    CX PUSH,		\ x
    DX BX MOV,
    BX 1 SHL,
    Y>SegTable ) BX ADD,
    0 [BX] ES MOV,
    CX BX MOV,
    BX 1 SHR,
    BX 1 SHR,
    BX 1 SHR,
    ES: 0 [BX] AL MOV,
    1 # AH MOV,
    CL NOT,
    7 # CL AND,
    AH CL ROL,
    AH AL OR,
    ES: AL 0 [BX] MOV,
    CX POP,
    BX POP,
    DI CX ADD,			\ inc or dec x
    MAX-X 8 * # CX SUB,
    9 L# JNS,
    MAX-X 8 * # CX ADD,
    9 L# JNS,
    MAX-X 8 * # CX ADD,
9 L:
    SI BX ADD,			\ = bx + delta_y
    Delta/2 ) BX CMP,		\ bx > halfx ?
    7 L# JLE,
    Delta ) BX SUB,		\ bx - delta_x
    DX INC,			\ y is always increasing
    MAX-Y 16* # DX CMP,
    7 L# JL,
    DX DX XOR,
7 L:
    BP DEC,
    6 L# JGE,
    BP POP,
    SI POP,
    BX POP,
    NEXT,
END-CODE

\ y changing faster than x
CODE xsteep640   \ on entry, ax = delta x, bx = delta y, cx=x1, dx=y1
    BX BP MOV,			\ for counter
    BX 1 SHR,
    BX Delta/2 ) MOV,		\ halfy
    BX BX XOR,			\ clear for cmp
6 L:
    BX PUSH,
    CX PUSH,		\ x
    DX BX MOV,
    BX 1 SHL,
    Y>SegTable ) BX ADD,
    0 [BX] ES MOV,
    CX BX MOV,
    BX 1 SHR,
    BX 1 SHR,
    BX 1 SHR,
    ES: 0 [BX] AL MOV,
    1 # AH MOV,
    CL NOT,
    7 # CL AND,
    AH CL ROL,
    AH AL XOR,
    ES: AL 0 [BX] MOV,
    CX POP,
    BX POP,
    DX INC,			\ y is always increasing
    MAX-Y 16* # DX CMP,
    8 L# JL,
    DX DX XOR,
8 L:
    Delta ) BX ADD,		\ = bx + delta_y
    Delta/2 ) BX CMP,		\ bx > halfy ?
    7 L# JLE,
    SI BX SUB,			\ bx - delta_y
    DI CX ADD,			\ inc or dec x
    MAX-X 8 * # CX SUB,
    7 L# JNS,
    MAX-X 8 * # CX ADD,
    7 L# JNS,
    MAX-X 8 * # CX ADD,
7 L:
    BP DEC,
    6 L# JGE,
    BP POP,
    SI POP,
    BX POP,
    NEXT,
END-CODE

\ on exit, cx=x1, dx=y1, ax=x2, bx=y2
CODE xline640  ( x1 y1 x2 y2 -- )        \ writes to screen directly
    AX POP,
    DX POP,
    CX POP,
    SI PUSH,   ( used to hold direction)
    BP PUSH,   ( used as counter)
\ see if we'll inc or dec x, y (draws in any direction)
    DX BX SUB,			\ bx <- y2-y1 (delta y)
    2 L# JGE,
    BX DX ADD,			\ dx <- y2
    BX NEG,			\ abs(delta y)
    CX AX XCHG,
2 L:
    BX SI MOV,                  \ delta_y(BX) to SI
    CX AX SUB,                  \ x2 - x1 = delta_x
    1 # DI MOV,                 \ di to increment x
    4 L# JGE,
    -1 # DI MOV,                \ di to decrement x
    AX NEG,                     \ abs(delta x)
4 L:
    \ adjust x1(CX), y1(DX) in proper range
    MAX-X 8 * # CX SUB,
    1 L# JNS,
    MAX-X 8 * # CX ADD,
    1 L# JNS,
    MAX-X 8 * # CX ADD,
1 L:
    MAX-Y 16* # DX SUB,
    8 L# JNS,
    MAX-Y 16* # DX ADD,
    8 L# JNS,
    MAX-Y 16* # DX ADD,
8 L:
    AX Delta ) MOV,		\ abs(delta x)
    BX AX CMP,			\ delta_x - delta_y
    5 L# JGE,
    ' xsteep640 # JMP,          \ y changes faster than x
5 L:
\ x changing faster than y
    AX BP MOV,			\ for counter
    AX 1 SHR,
    AX Delta/2 ) MOV,		\ halfx
    BX BX XOR,			\ clear for cmp
6 L:
    BX PUSH,
    CX PUSH,		\ x
    DX BX MOV,
    BX 1 SHL,
    Y>SegTable ) BX ADD,
    0 [BX] ES MOV,
    CX BX MOV,
    BX 1 SHR,
    BX 1 SHR,
    BX 1 SHR,
    ES: 0 [BX] AL MOV,
    1 # AH MOV,
    CL NOT,
    7 # CL AND,
    AH CL ROL,
    AH AL XOR,
    ES: AL 0 [BX] MOV,
    CX POP,
    BX POP,
    DI CX ADD,			\ inc or dec x
    MAX-X 8 * # CX SUB,
    9 L# JNS,
    MAX-X 8 * # CX ADD,
    9 L# JNS,
    MAX-X 8 * # CX ADD,
9 L:
    SI BX ADD,			\ = bx + delta_y
    Delta/2 ) BX CMP,		\ bx > halfx ?
    7 L# JLE,
    Delta ) BX SUB,		\ bx - delta_x
    DX INC,			\ y is always increasing
    MAX-Y 16* # DX CMP,
    7 L# JL,
    DX DX XOR,
7 L:
    BP DEC,
    6 L# JGE,
    BP POP,
    SI POP,
    BX POP,
    NEXT,
END-CODE

DECIMAL
10 CONSTANT scale
0 VALUE Ÿ¥¦•?
VARIABLE ¤wÐ·
MAX-X 8 * 2/ VALUE xOffset
MAX-Y 16 * 2/ VALUE yOffset
VARIABLE xCoord  xOffset xCoord !
VARIABLE yCoord  yOffset yCoord !

: ¦•—i´á ( -- )     FALSE TO Ÿ¥¦•? ;
: ¦•a ( -- )     TRUE  TO Ÿ¥¦•? ;
: º‰ÑÁ¡e ( y -- )   MAX-Y SWAP - TO YTop ;
: µ¥ÑÁ¡e ( -- )     PAGE ;

CODE sin* ( ˆáŸ¡ ˆb•¡ -- ˆáŸ¡*sin[ˆb•¡] )
\ : sin*   CELLS sin16384 + @ M* 16384 SM/REM NIP ;
    BX 1 SHL,
    sin16384 [BX] BX MOV,
    AX POP,
    BX IMUL,
    BX BX XOR,
    AX 1 SHL,
    DX 1 RCL,
    AX 1 SHL,
    DX 1 RCL,
    DX BX ADC,
    NEXT,
END-CODE

CODE cos* ( ˆáŸ¡ ˆb•¡ -- ˆáŸ¡*cos[ˆb•¡] )
\ : cos*   90 + CELLS sin16384 + @ M* 16384 SM/REM NIP ;
    90 # BX ADD,
    BX 1 SHL,
    sin16384 [BX] BX MOV,
    AX POP,
    BX IMUL,
    BX BX XOR,
    AX 1 SHL,
    DX 1 RCL,
    AX 1 SHL,
    DX 1 RCL,
    DX BX ADC,
    NEXT,
END-CODE

: ¡.‹a´á ( x y -- )
    scale / yOffset SWAP -	    \ x y1
    SWAP scale / xOffset + SWAP     \ x1 y1
    Ÿ¥¦•? IF 2DUP xCoord @ yCoord @ line640 THEN
    yCoord !  xCoord ! ;

: ˆá¦‚¥¡µa ( -- )
    xCoord @  8 ¤wÐ· @ 270 + sin* +
    yCoord @  8 ¤wÐ· @ 270 + cos* -
    2DUP xCoord @ yCoord @ xline640                 \ x1 y1
    xCoord @  16 ¤wÐ· @ sin* +
    yCoord @  16 ¤wÐ· @ cos* -                      \ x1 y1 x2 y2
    2SWAP 2OVER xline640                            \ x2 y2
    xCoord @  8 ¤wÐ· @ 90 + sin* +
    yCoord @  8 ¤wÐ· @ 90 + cos* -                  \ x2 y2 x3 y3
    2SWAP 2OVER xline640                            \ x3 y3
    xCoord @  yCoord @  xline640 ;

: ÑÁ¡e»¡¶¡ ( -- )   YTop PAGE 0 OVER AT-XY TO YTop ˆá¦‚¥¡µa ;

HEX
: ˆ{·e i ( xt 'name2' -- )
    DUP xt>name ?DUP 0= IF -12 THROW THEN
    SWAP head, linkLast
    C@ DUP
    040 AND IF IMMEDIATE    THEN
    020 AND IF COMPILE-ONLY THEN ;

DECIMAL
' IMMEDIATE  ˆ{·e i  ¤a¡
' RECURSE    ˆ{·e i  –A¦‰œá
' IF         ˆ{·e i  ¡e
' ELSE       ˆ{·e i  ´a“¡¡e
' THEN       ˆ{·e i  œa
' BEGIN      ˆ{·e i  ·¡¹A¦Èá
' UNTIL      ˆ{·e i  Œa»¡
' WHILE      ˆ{·e i  •·´e
' REPEAT     ˆ{·e i  ˆá—sÐ
' DO         ˆ{·e i   ‘
' LOOP       ˆ{·e i  •©´a
' I          ˆ{·e i  ˆa
' CONSTANT   ˆ{·e i  Šˆ·eˆt
' VARIABLE   ˆ{·e i  ¢Ÿeˆt
' DUP        ˆ{·e i  ¥Aa
' OVER       ˆ{·e i  ˆåá
' DROP       ˆ{·e i  ¤áa
' SWAP       ˆ{·e i  ¤aŽ¡
' ROT        ˆ{·e i  •©a
' >R         ˆ{·e i  >–A
' R>         ˆ{·e i  –A>
' R@         ˆ{·e i  –A@
' AND        ˆ{·e i  {ÐqA
' OR         ˆ{·e i  {´a¶‰œá
' XOR        ˆ{·e i  {˜a¡
' MOD        ˆ{·e i  a á»¡
' CR         ˆ{·e i  ”a·qº‰
' WORDS      ˆ{·e i   iÍa
' .S         ˆ{·e i  .”á£¡

' THEN       ˆ{·e i  ENDIF
' =          ˆ{·e i  ==
' BYE        ˆ{·e i  {
' BYE        ˆ{·e i  EXIT
' BYE        ˆ{·e i  exit

¢Ÿeˆt ˆa¡¶áÃ¡
¢Ÿeˆt ­A¡¶áÃ¡

: ¹A¸aŸ¡¡   ( -- )
	ˆá¦‚¥¡µa
	0 ¤wÐ· !
	0 ˆa¡¶áÃ¡ !
	0 ­A¡¶áÃ¡ !
	0 0 ¡.‹a´á
	ˆá¦‚¥¡µa ;

: ÑÁ¡e»¡¶¡   ( -- )
	¦•—i´á ¹A¸aŸ¡¡ ÑÁ¡e»¡¶¡ ¦•a ;

: ½¡ˆ…ÑÁ¡e ( -- )    8 º‰ÑÁ¡e  ÑÁ¡e»¡¶¡ ;
: µ¥ÑÁ¡e   ( -- )    µ¥ÑÁ¡e    ÑÁ¡e»¡¶¡ ;

: •¡.µ¡Ÿe½¢  ( ˆb•¡ -- )
	ˆá¦‚¥¡µa
	¤wÐ· @ +
	¥Aa 0 < ¡e		     ( ˆb•¡ˆa 0¥¡”a ¸b·a¡e)
		   ·¡¹A¦Èá 360 +    ( 0 ·¡¬w·¡ –I ˜Œa»¡ 360·i ”áÐq)
		   ¥Aa -1 > Œa»¡
		 ´a“¡¡e 360 a á»¡   ( 0¥¡”a Ça¡e 360·a¡ a’… a á»¡Ÿi q‹±)
		 œa
	¤wÐ· !
	ˆá¦‚¥¡µa ;

: •¡.¶E½¢     -1 * •¡.µ¡Ÿe½¢ ;

:  eÇq.ˆa    ( dx dy -- )
	ˆá¦‚¥¡µa
	­A¡¶áÃ¡ @ +		( dx y+dy )
	¥Aa ­A¡¶áÃ¡ ! 	( dx y+dy )
	¤aŽ¡ ˆa¡¶áÃ¡ @ +	( y+dy x+dx )
	¥Aa ˆa¡¶áÃ¡ ! 	( y+dy x+dx )
	¤aŽ¡			( x+dx y+dy )
	¡.‹a´á
	ˆá¦‚¥¡µa ;

: ´|·a¡   ( l -- )
	¥Aa			( l l )
	¤wÐ· @ sin*		( l dx )
	¤aŽ¡			( dx l )
	¤wÐ· @ cos*		( dx dy )
	 eÇq.ˆa  ;

: –á¡	   ( ˆáŸ¡ -- )
	-1 *  ´|·a¡ ;

: __µ¡Ÿe½¢.ÑÉ©   ( ˆáŸ¡ ÒU®  -- )
	0  ‘   5 •¡.µ¡Ÿe½¢   ¥Aa ´|·a¡  5 •¡.µ¡Ÿe½¢	•©´a   ¤áa ;

: µ¡Ÿe½¢.ÑÉ©	( ¤e»¡Ÿq ˆb•¡ -- )
	¤aŽ¡  355 2034 */	( ˆb•¡ ˆáŸ¡ ) ( ÑÉ©·i 10•¡³¢ a’´á ‹aŸ± )
					( 2*pi*r*ˆb•¡/360*10 = pi*r*ˆb•¡/18 )
					( pi = 355/113 = 3.141593 )
	¥Aa >–A			( –A•©·¡ ”á£¡µA ˆáŸ¡Ÿi ˆi¢Ÿ¡)
	ˆåá  10 /		( ˆb•¡ ˆáŸ¡ ÒU® )
	__µ¡Ÿe½¢.ÑÉ©		( ˆb•¡ )
	10 a á»¡		( ˆb•¡_a á»¡ )
	¥Aa –A>		( ˆb•¡_a á»¡ ˆb•¡_a á»¡ ˆáŸ¡ )
	( 10·a¡ a’… a á»¡ ˆb•¡µA Ð”wÐa“e ˆáŸ¡ eÇq ´|·a¡ ˆq)
	*  10 /  ´|·a¡ 	( ˆb•¡_a á»¡ )
	•¡.µ¡Ÿe½¢  ;

: µ¡Ÿe½¢.¶¥ ( ¤e»¡Ÿq -- )   360 µ¡Ÿe½¢.ÑÉ© ;

: __¶E½¢.ÑÉ©	( ˆáŸ¡ ÒU®  -- )
	0  ‘   5 •¡.¶E½¢   ¥Aa ´|·a¡	5 •¡.¶E½¢   •©´a   ¤áa ;

: ¶E½¢.ÑÉ©	( ¤e»¡Ÿq ˆb•¡ -- )
	¤aŽ¡  355 2034 */	( ˆb•¡ ˆáŸ¡ ) ( ÑÉ©·i 10•¡³¢ a’´á ‹aŸ± )
					( 2*pi*r*ˆb•¡/360*10 = pi*r*ˆb•¡/18 )
	¥Aa >–A			( –A•©·¡ ”á£¡µA ˆáŸ¡Ÿi ˆi¢Ÿ¡)
	ˆåá  10 /		( ˆb•¡ ˆáŸ¡ ÒU® )
	__¶E½¢.ÑÉ©		( ˆb•¡ )
	10 a á»¡		( ˆb•¡_a á»¡ )
	¥Aa –A>		( ˆb•¡_a á»¡ ˆb•¡_a á»¡ ˆáŸ¡ )
	     ( 10·a¡ a’… a á»¡ ˆb•¡µA Ð”wÐa“e ˆáŸ¡ eÇq ´|·a¡ ˆq)
	*  10 /  ´|·a¡ 	( ˆb•¡_a á»¡ )
	•¡.¶E½¢  ;

: ¶E½¢.¶¥ ( ¤e»¡Ÿq -- )   360 ¶E½¢.ÑÉ© ;

: ‘A¡¡	 ( Ça‹¡ -- )
	4 0   ‘   ¥Aa ´|·a¡	90 •¡.µ¡Ÿe½¢   •©´a   ¤áa ;

: ‰sÃ¥‘A¡¡   ( -- )
	100 ‘A¡¡  200 ‘A¡¡  300 ‘A¡¡  400 ‘A¡¡ ;

: ”a·¡´a¡¥—a   ( -- )
	45 •¡.µ¡Ÿe½¢
	4 0  ‘	‰sÃ¥‘A¡¡  90 •¡.µ¡Ÿe½¢	•©´a ;

: ‹µ¤i	 ( Ça‹¡ -- )
	¥Aa ´|·a¡  ¥Aa ‘A¡¡	–á¡ ;

: µa¬õ‹µ¤i   ( Ça‹¡ -- )
	6 0  ‘	¥Aa ‹µ¤i  60 •¡.µ¡Ÿe½¢  •©´a ;

: ¤aœqˆ§¡   ( -- )
	100 µa¬õ‹µ¤i  400 µa¬õ‹µ¤i ;

: ¹·¼	 ( Ça‹¡ -- )
	¥Aa  90 µ¡Ÿe½¢.ÑÉ©  90 •¡.µ¡Ÿe½¢
	      90 µ¡Ÿe½¢.ÑÉ©  90 •¡.µ¡Ÿe½¢ ;

: ¹	 ( Ça‹¡ -- )
	8 0  ‘	 ¥Aa  ¹·¼ 45 •¡.µ¡Ÿe½¢   •©´a   ¤áa ;

: ”aŸe¹·¼   ( Ça‹¡ -- )
	¥Aa  60 µ¡Ÿe½¢.ÑÉ©  120 •¡.µ¡Ÿe½¢
	      60 µ¡Ÿe½¢.ÑÉ©  120 •¡.µ¡Ÿe½¢ ;

: ”aŸe¹     ( Ça‹¡ -- )
	6 0  ‘	 ¥Aa  ”aŸe¹·¼ 60 •¡.µ¡Ÿe½¢   •©´a   ¤áa ;

: Ð•¬i	 ( Ça‹¡ -- )
	¥Aa  90 ¶E½¢.ÑÉ©  ¥Aa 90 µ¡Ÿe½¢.ÑÉ©
	¥Aa  90 ¶E½¢.ÑÉ©	 90 µ¡Ÿe½¢.ÑÉ© ;

: Ð	 ( Ça‹¡ -- )
	9 0  ‘	 ¥Aa Ð•¬i  160 •¡.µ¡Ÿe½¢   •©´a  ¤áa ;

: ˆbÑw	   ( Ça‹¡ ¡¡¬áŸ¡® -- )
	360 ˆåá /  ¤aŽ¡	( Ça‹¡ •©_ˆb•¡ ¡¡¬áŸ¡® )
	0  ‘  ˆåá ´|·a¡  ¥Aa •¡.µ¡Ÿe½¢  •©´a
	¤áa ¤áa ;

: ¥i   ( ¥e·_® ¡y¤å -- )
	ˆåá ˆåá *
	0  ‘  600 ´|·a¡  ˆåá ˆåá 360 * ¤aŽ¡ / •¡.µ¡Ÿe½¢  •©´a
	¤áa ¤áa ;

: ”aˆbÑw   ( Ça‹¡ ˆb•¡ -- )
	¤wÐ· @ >–A		      ( –A•©·¡”á£¡µA Àá·q ¤wÐ··i ˆi¢Ÿ¡)
	·¡¹A¦Èá
	   ˆåá ´|·a¡ ¥Aa •¡.µ¡Ÿe½¢
	¤wÐ· @	–A@ = Œa»¡	      ( ¤wÐ··¡ Àá·q¤wÐ·‰Á ˆ{´a»© ˜Œa»¡ –AÎ‰·¡)
	¤áa ¤áa  –A> ¤áa ;	      ( ˆt”á£¡µÁ –A•©·¡ ”á£¡Ÿi À÷­¡ )

: ”aˆbÑw¸aœw5	 5 0  ‘  450  72 ”aˆbÑw  72 •¡.µ¡Ÿe½¢  •©´a ;
: ”aˆbÑw¸aœw4	 4 0  ‘  700 135 ”aˆbÑw  90 •¡.µ¡Ÿe½¢  •©´a ;
: ”aˆbÑw¸aœw12	12 0  ‘  15 •¡.µ¡Ÿe½¢  ¦•—i´á  400 ´|·a¡  ¦•a
			 200 135 ”aˆbÑw  15 •¡.µ¡Ÿe½¢  •©´a ;

: a¢	 ( ˆa»¡ˆb•¡ ˆa»¡‹©·¡ ˆa»¡Ã¡‹¡® -- )
	>–A				( ˆa»¡Ã¡‹¡®Ÿi –A•©·¡”á£¡µA ˆi¢Ÿ¡)
	–A@ ¡e				( '–A@ 0 <> ¡e' ‰Á ˆ{·q)
	   ˆåá •¡.¶E½¢
	   ¥Aa 2 * ´|·a¡
	   ˆåá ˆåá –A@ 1 - –A¦‰œá
	   ¥Aa 2 * –á¡
	   ˆåá 2 * •¡.µ¡Ÿe½¢
	   ¥Aa ´|·a¡
	   ˆåá ˆåá –A@ 1 - –A¦‰œá
	   –á¡  •¡.¶E½¢
	´a“¡¡e	¤áa ¤áa œa
	–A> ¤áa ;

¢Ÿeˆt §¡“iÇa‹¡  20 §¡“iÇa‹¡ !
: ¶w   ( a·¡ -- )
	¥Aa  0 = ¡e  §¡“iÇa‹¡ @ ´|·a¡
	      ´a“¡¡e  ¥Aa  0 > ¡e  ¥Aa 1 - –A¦‰œá	(  a·¡-1 ¶w )
				    90 •¡.µ¡Ÿe½¢
				    1 ˆåá - –A¦‰œá	(  1-a·¡ ¶w )
			    ´a“¡¡e  -1 ˆåá - –A¦‰œá	( -1-a·¡ ¶w )
				    90 •¡.¶E½¢
				     1 ˆåá + –A¦‰œá	(  1+a·¡ ¶w )
	      œa  œa
	¤áa ;

: ·¥¬a i
    ½¡ˆ…ÑÁ¡e
    ." '“iÎ‘'·a¡ ¼e ˆá¦‚‹aŸ± Ïa¡‹aœ‘·i ¯¡¸bÐs“¡”a."              ”a·qº‰ ”a·qº‰
    ." ¯¡Ç± i—i·i ¥¡a¡e             ' iÍa'           œa‰¡ Ã¡¯¡‰¡"  ”a·qº‰
    ." ¯¡Ç± i·i ´á˜ý‰A ³a“e»¡ ¥¡a¡e '•¡¶‘ i •¡¶‘ i'  Àáœñ Ã¡¯¡‰¡"  ”a·qº‰
    ." DOS¡ •©´a ˆaa¡e             '{'           ·¡œa‰¡ Ã¡¯³¯¡µ¡." ”a·qº‰
    ." ‹aŸ±‰Á ‹i¸aŸi ˆ{·¡ ¥¡a¡e 'µ¥ÑÁ¡e' ˜a¡ ¥¡a¡e '½¡ˆ…ÑÁ¡e'·¡œa‰¡ Ã¡¯³¯¡µ¡"
    ”a·qº‰
    ¦•a
    300 ¹  450 ¹  600 ¹
    ¦•—i´á  90 •¡.¶E½¢  2000 ´|·a¡  ¦•a
    900 ”aŸe¹  700 ”aŸe¹  500 ”aŸe¹
    ¦•—i´á  4000 –á¡  90 •¡.µ¡Ÿe½¢  ¦•a
    300 Ð
    1 ‹i®A·³b¬wÈ ! ;       \ Ðe‹i·³b

·¥¬a i

: TURTLE-hi
    DOSCommand>PAD
    GET-MODE TO OldMode# HGRAPHIC hi
    ." ·‰e‰Á ¹A´e‰Á §¡Íw·i Ða·¡ÉI wykoh¡ ¥¡ º¯³¯¡µ¡." CR
    S" BLOCKS.BLK" MAPPED-TO-BLOCK
    ·¥¬a i  QUIT ;

' TURTLE-hi TO 'boot

( ”a·q·i Àa¡ ¯¡Åa¥¡¯³¯¡µ¡. )
( ”a·¡´a¡¥—a )
( 400 ‹µ¤i  400 µa¬õ‹µ¤i  ¤aœqˆ§¡ )
( 400 3 ˆbÑw   400 5 ˆbÑw   400 7 ˆbÑw )
( 5 2 ¥i  7 2 ¥i  7 3 ¥i  8 3 ¥i  9 2 ¥i  9 4 ¥i  10 3 ¥i  11 3 ¥i  11 5 ¥i )
( ”aˆbÑw¸aœw5 )
( ”aˆbÑw¸aœw5 )
( ”aˆbÑw¸aœw12 )
( 30 400 4 a¢ )
( 20 250 5 a¢ )
( 20 250 6 a¢ )
( 50 §¡“iÇa‹¡ !   9 ¶w )
( 20 §¡“iÇa‹¡ !  12 ¶w )

SET-CURRENT  SET-ORDER
BASE !
