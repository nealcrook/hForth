\ ASSEMBLER TEST PROGRAM

CHAR " PARSE CPU" ENVIRONMENT? DROP
CHAR " PARSE 8086" COMPARE
[IF] CR .( This assembler is for 8086 only.) ABORT [THEN]

BASE @
GET-ORDER
MARKER ~ASMTEST

VARIABLE TEST-POINTER

: 2CONSTANT
    CREATE , , DOES> 2@ ;

: INIT-TEST   ( -- )
    xhere TEST-POINTER ! ;

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : ZZ	 (  N1 ... NN -- )
      xhere
      BEGIN ( n ... n addr)
	 1-  DUP  C@
	 ROT <> IF
	     TEST-POINTER @ 10 DUMP
	     -1 ABORT" failed"
	 THEN
	 DUP
      TEST-POINTER @ =	UNTIL
      DROP
      INIT-TEST ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : ZZ	 (  N1 ... NN -- )
      xhere
      BEGIN ( n ... n addr)
	 1-  DUP  codeB@
	 ROT <> IF
	     TEST-POINTER @ 10 DUMP
	     -1 ABORT" failed"
	 THEN
	 DUP
      TEST-POINTER @ =	UNTIL
      DROP
      INIT-TEST ;
[THEN]

\ ---------- all is in hex from here to end

HEX

ALSO ASSEMBLER

INIT-ASM

\ ----------

1234 CONSTANT BVAR
1235 CONSTANT WVAR
1237 CONSTANT LVAR

123B CONSTANT SUBR

9876 5432 2CONSTANT FARSUB

\ ----------

INIT-TEST

AAA,								 37 ZZ
AAD,							     D5  0A ZZ
AAM,							     D4  0A ZZ
AAS,								 3F ZZ

DX BX ADC,						     11  D3 ZZ
0 [BX+SI] CX ADC,					     13  08 ZZ
CX 0 [BX+SI] ADC,					     11  08 ZZ
3456 # WVAR ) ADC,			     81  16  35  12  56  34 ZZ
5 # AX ADC,						 15  05  00 ZZ

\ ----------

BX SP ADD,						     01  DC ZZ
ES: 967 [BX+DI] BP ADD, 			 26  03  A9  67  09 ZZ
AX 967 [BX+DI] ADD,				     01  81  67  09 ZZ
AX 0 [BX+DI] ADD,					     01  01 ZZ
6789 # SI ADD,					     81  C6  89  67 ZZ
5432 # WVAR ) WORD ADD, 		     81  06  35  12  32  54 ZZ
5 # AX ADD,						 05  05  00 ZZ
SP DI AND,						     21  E7 ZZ
DS: 1234 [BP+SI] BL AND,			 3E  22  9A  34  12 ZZ
BP CS: 4567 [BP+SI] AND,			 2E  21  AA  67  45 ZZ
BP 0 [BP+SI] AND,					     21  2A ZZ
6789 # BX AND,					     81  E3  89  67 ZZ
1234 # WVAR ) WORD AND, 		     81  26  35  12  34  12 ZZ
5 # AL AND,						     24  05 ZZ

\ ----------

xhere 48 - # CALL,					 E8  B5  FF ZZ
FARSUB # FAR CALL,				 9A  76  98  32  54 ZZ
WVAR [BP+DI] CALL,				     FF  93  35  12 ZZ
0 [BP+DI] CALL, 					     FF  13 ZZ
CX CALL,						     FF  D1 ZZ
LVAR ) FAR CALL,				     FF  1E  37  12 ZZ

CBW,								 98 ZZ
CLC,								 F8 ZZ
CLD,								 FC ZZ
CLI,								 FA ZZ
CMC,								 F5 ZZ

\ ----------

67 # CH CMP,						 82  FD  67 ZZ
 AH BH CMP,						     38  E7 ZZ
0 [SI] CL CMP,						     3A  0C ZZ
SS:  AL 0 [SI] CMP,					 36  38  04 ZZ
 9678 # WVAR ) WORD CMP,		     81  3E  35  12  78  96 ZZ
 5432 # AX CMP, 					 3D  32  54 ZZ

BYTE CMPS,							 A6 ZZ
WORD CMPS,							 A7 ZZ

CWD,								 99 ZZ

\ ----------

DAA,								 27 ZZ
DAS,								 2F ZZ

BX DEC, 							 4B ZZ
BL DEC, 						     FE  CB ZZ
BVAR [SI] BYTE DEC,				     FE  8C  34  12 ZZ
WVAR [DI] WORD DEC,				     FF  8D  35  12 ZZ
0 [SI] WORD DEC,					     FF  0C ZZ

CL DIV, 						     F6  F1 ZZ
BX DIV, 						     F7  F3 ZZ
BVAR ) BYTE DIV,				     F6  36  34  12 ZZ
HLT,								 F4 ZZ

\ ----------

CL IDIV,						     F6  F9 ZZ
BX IDIV,						     F7  FB ZZ
BVAR ) BYTE IDIV,				     F6  3E  34  12 ZZ

CL IMUL,						     F6  E9 ZZ
BX IMUL,						     F7  EB ZZ
BVAR ) BYTE IMUL,				     F6  2E  34  12 ZZ

7B # AL IN,						     E4  7B ZZ
7B # AX IN,						     E5  7B ZZ
DX AL IN,							 EC ZZ
DX AX IN,							 ED ZZ

\ ----------

AX INC, 							 40 ZZ
AL INC, 						     FE  C0 ZZ
5 [SI] BYTE INC,					 FE  44  05 ZZ
0 [BP] WORD INC,					 FF  46  00 ZZ

17 INT, 						     CD  17 ZZ
3 INT,								 CC ZZ

INTO,								 CE ZZ

IRET,								 CF ZZ

\ ----------

xhere 3 + JA,						     77  01 ZZ
xhere 4 - JA,						     77  FA ZZ

xhere 3 + JAE,						     73  01 ZZ
xhere 4 - JNC,						     73  FA ZZ

xhere 3 + JB,						     72  01 ZZ
xhere 4 - JC,						     72  FA ZZ

xhere 3 + JBE,						     76  01 ZZ
xhere 4 - JBE,						     76  FA ZZ

\ ----------

xhere 3 + JCXZ, 					     E3  01 ZZ

xhere 3 + JE,						     74  01 ZZ
xhere 4 - JZ,						     74  FA ZZ

xhere 3 + JG,						     7F  01 ZZ
xhere 4 - JG,						     7F  FA ZZ

xhere 3 + JGE,						     7D  01 ZZ
xhere 4 - JGE,						     7D  FA ZZ

\ ----------

xhere 3 + JL,						     7C  01 ZZ
xhere 4 - JL,						     7C  FA ZZ

xhere 3 + JLE,						     7E  01 ZZ
xhere 4 - JLE,						     7E  FA ZZ

xhere 7 + JU,						     EB  05 ZZ

xhere 1234 + # JMP,					 E9  31  12 ZZ
FARSUB # FAR JMP,				 EA  76  98  32  54 ZZ
WVAR ) JMP,					     FF  26  35  12 ZZ
BX JMP, 						     FF  E3 ZZ
LVAR [SI] FAR JMP,				     FF  AC  37  12 ZZ
0 [SI] FAR JMP, 					     FF  2C ZZ

\ ----------

xhere 3 + JNE,						     75  01 ZZ
xhere 4 - JNZ,						     75  FA ZZ

xhere 3 + JNO,						     71  01 ZZ
xhere 3 + JNS,						     79  01 ZZ

xhere 3 + JPO,						     7B  01 ZZ
xhere 4 - JPO,						     7B  FA ZZ

\ ----------

xhere 3 + JO,						     70  01 ZZ

xhere 3 + JPE,						     7A  01 ZZ
xhere 4 - JPE,						     7A  FA ZZ

xhere 3 + JS,						     78  01 ZZ

LAHF,								 9F ZZ

LVAR [BX] AX LDS,				     C5  87  37  12 ZZ
0 [BX] AX LDS,						     C5  07 ZZ

LVAR [BX] BX LEA,				     8D  9F  37  12 ZZ
0 [BX] BX LEA,						     8D  1F ZZ

\ ----------

LVAR [BX] BX LES,				     C4  9F  37  12 ZZ
0 [BX] BX LES,						     C4  1F ZZ

LOCK, BYTE LODS,					     F0  AC ZZ
WORD LODS,							 AD ZZ

xhere 8 - LOOP, 					     E2  F6 ZZ
xhere 9 - LOOPE,					     E1  F5 ZZ
xhere 3 + LOOPNE,					     E0  01 ZZ

\ ----------

0 [SI] SI MOV,						     8B  34 ZZ
2 [SI] SI MOV,						 8B  74  02 ZZ
DS: AL BVAR [BP] MOV,				 3E  88  86  34  12 ZZ
AX WVAR [BX] MOV,				     89  87  35  12 ZZ
AX 0 [BX] MOV,						     89  07 ZZ
DS: BVAR [BP] AL MOV,				 3E  8A  86  34  12 ZZ
WVAR [BX] AX MOV,				     8B  87  35  12 ZZ
DX CX MOV,						     8B  CA ZZ
DX AX MOV,						     8B  C2 ZZ
WVAR ) BP MOV,					     8B  2E  35  12 ZZ
BP WVAR ) MOV,					     89  2E  35  12 ZZ
9876 # DX MOV,						 BA  76  98 ZZ
1 # DX MOV,						 BA  01  00 ZZ
1 # DL MOV,						     B2  01 ZZ
LVAR  # WORD  WVAR ) MOV,		      C7 06  35  12  37  12 ZZ
67 # BVAR ) BYTE MOV,				 C6  06  34  12  67 ZZ
CX SS MOV,						     8E  D1 ZZ
DS CX MOV,						     8C  D9 ZZ
WVAR ) ES MOV,					     8E  06  35  12 ZZ
CS WVAR ) MOV,					     8C  0E  35  12 ZZ

BYTE MOVS,							 A4 ZZ
WORD MOVS,							 A5 ZZ

\ ----------

CL MUL, 						     F6  E1 ZZ
BX MUL, 						     F7  E3 ZZ
BVAR ) BYTE MUL,				     F6  26  34  12 ZZ

BL NEG, 						     F6  DB ZZ
BX NEG, 						     F7  DB ZZ
WVAR [BX] WORD NEG,				     F7  9F  35  12 ZZ

NOP,								 90 ZZ
BL NOT, 						     F6  D3 ZZ
BX NOT, 						     F7  D3 ZZ
WVAR [BX] WORD NOT,				     F7  97  35  12 ZZ
0 [BX] WORD NOT,					     F7  17 ZZ

\ ----------

BH DL OR,						     08  FA ZZ
0 [SI] DH OR,						     0A  34 ZZ
BL 0 [SI] OR,						     08  1C ZZ
6789 # BX OR,					     81  CB  89  67 ZZ
7698 # WVAR ) WORD OR,			     81  0E  35  12  98  76 ZZ
5  # AX OR,						 0D  05  00 ZZ

44 # AX OUT,						     E7  44 ZZ
45 # AL OUT,						     E6  45 ZZ
DX AX OUT,							 EF ZZ
DX AL OUT,							 EE ZZ

\ ----------

AX POP, 							 58 ZZ
ES POP, 							 07 ZZ
WVAR [BX] WORD POP,				     8F  87  35  12 ZZ

POPF,								 9D ZZ

AX PUSH,							 50 ZZ
CS PUSH,							 0E ZZ
WVAR [BX] WORD PUSH,				     FF  B7  35  12 ZZ
0 [BX] WORD PUSH,					     FF  37 ZZ

PUSHF,								 9C ZZ

\ ----------

CX 1 RCL,						     D1  D1 ZZ
AX CL RCL,						     D3  D0 ZZ
WVAR )	  WORD RCL,				     D1  16  35  12 ZZ
WVAR ) CL WORD RCL,				     D3  16  35  12 ZZ

CL 1 RCR,						     D0  D9 ZZ
AL CL RCR,						     D2  D8 ZZ
BVAR )	  BYTE RCR,				     D0  1E  34  12 ZZ
BVAR ) CL BYTE RCR,				     D2  1E  34  12 ZZ

\ ----------

REP, BYTE LODS, 					     F3  AC ZZ
REPE, BYTE LODS,					     F3  AC ZZ
REPNE, BYTE LODS,					     F2  AC ZZ

RET,								 C3 ZZ
5 +RET, 						 C2  05  00 ZZ
FAR RET,							 CB ZZ
1234 FAR +RET,						 CA  34  12 ZZ


\ ----------

CL 1 ROR,						     D0  C9 ZZ

AL CL ROR,						     D2  C8 ZZ
BVAR )	   BYTE ROR,				     D0  0E  34  12 ZZ
BVAR ) CL  BYTE ROR,				     D2  0E  34  12 ZZ


CL 1 ROL,						     D0  C1 ZZ
AL CL ROL,						     D2  C0 ZZ
BVAR )	  BYTE ROL,				     D0  06  34  12 ZZ
BVAR ) CL BYTE ROL,				     D2  06  34  12 ZZ

SAHF,								 9E ZZ


\ ----------

CL 1 SHL,						     D0  E1 ZZ
AL CL SHL,						     D2  E0 ZZ
BVAR )	  BYTE SHL,				     D0  26  34  12 ZZ
BVAR ) CL BYTE SHL,				     D2  26  34  12 ZZ

\ ----------

CL 1 SAR,						     D0  F9 ZZ
AL CL SAR,						     D2  F8 ZZ
BVAR )	  BYTE SAR,				     D0  3E  34  12 ZZ
BVAR ) CL BYTE SAR,				     D2  3E  34  12 ZZ

CH BH SBB,						     18  EF ZZ
0 [SI] CX SBB,						     1B  0C ZZ
CL 0 [SI] SBB,						     18  0C ZZ
6789 # BX SBB,					     81  DB  89  67 ZZ
9988 # WVAR ) WORD SBB, 		     81  1E  35  12  88  99 ZZ
5 # AX SBB,						 1D  05  00 ZZ

\ ----------

BYTE SCAS,							 AE ZZ
WORD SCAS,							 AF ZZ

CL 1 SHR,						     D0  E9 ZZ
AL CL SHR,						     D2  E8 ZZ
BVAR )	  BYTE SHR,				     D0  2E  34  12 ZZ
BVAR ) CL BYTE SHR,				     D2  2E  34  12 ZZ

STC,								 F9 ZZ

STD,								 FD ZZ

\ ----------

STI,								 FB ZZ

BYTE STOS,							 AA ZZ
WORD STOS,							 AB ZZ

DH DL SUB,						     28  F2 ZZ
0 [SI] CX SUB,						     2B  0C ZZ
DL 0 [SI] SUB,						     28  14 ZZ
6789 # BX SUB,					     81  EB  89  67 ZZ
1234  # WVAR ) WORD SUB,		     81  2E  35  12  34  12 ZZ
5 # AX SUB,						 2D  05  00 ZZ

\ ----------

SI SI TEST,						     85  F6 ZZ
CX 0 [SI] TEST, 					     85  0C ZZ
6789 # BX TEST, 				     F7  C3  89  67 ZZ
1239 # WVAR ) WORD TEST,		     F7  06  35  12  39  12 ZZ
5 # AX TEST,						 A9  05  00 ZZ

WAIT,								 9B ZZ

DI AX XCHG,							 97 ZZ
BL AL XCHG,						     86  C3 ZZ
BX CX XCHG,						     87  CB ZZ
DX WVAR ) XCHG, 				     87  16  35  12 ZZ

\ ----------

XLAT,								 D7 ZZ

BX SI XOR,						     31  DE ZZ
0 [SI] CX XOR,						     33  0C ZZ
DX 0 [SI] XOR,						     31  14 ZZ
6789 # BX XOR,					     81  F3  89  67 ZZ
1234 # WVAR ) WORD XOR, 		     81  36  35  12  34  12 ZZ
5 # AX XOR,						 35  05  00 ZZ

\ ----------

INIT-ASM
1 L:	BX POP,
	BX POP,
	1 L# JNZ,				     5B  5B  75  FC ZZ

	2 L# JNZ,
	BX POP,
	BX POP,
2 L:	BX POP, 				 75  02  5B  5B  5B ZZ

3 L:	BX POP,
	4 L# JNZ,
	BX POP,
	3 L# JNZ,
4 L:	BX POP, 			 5B  75  03  5B  75  FA  5B ZZ

5 L:	BX POP,
	6 L# JNZ,
	BX POP,
	6 L# JNZ,
	BX POP,
6 L:	BX POP,
	5 L# JNZ,	      5B  75  04  5B  75  01  5B  5B  75 F6 ZZ


\ ---------- back to decimal here

~ASMTEST
SET-ORDER
BASE !

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
