TITLE hForth 8086 EXE Model

PAGE 62,132     ;62 lines per page, 132 characters per line

;===============================================================
;
;       hForth 8086 EXE model v0.9.7 by Wonyong Koh, 1995
;
;
; 1996. 2. 10.
;       Revise FM/MOD and SM/REM to catch result-out-of-range error in
;               '80000. 2 FM/MOD'.
; 1996. 1. 19.
;       Rename 'x,' to 'code,'; 'x@' to 'code@'; 'x!' to 'code!';
;               'xb@' to 'codeB@' and 'xb!' to 'codeB!'.
; 1996. 1. 7
;       Rename non-Standard 'parse-word' to PARSE-WORD.
; 1995. 12. 2
;       Drop '?doLIST' and revise 'optiCOMPILE,'.
; 1995. 11. 28
;       Drop 'LIT,:' all together.
;       Return CELLS to non-IMMEDIATE definition.
;
; Changes from 0.9.6
;
; 1995. 11. 25.
;       Make 'lastXT' VALUE word.
; 1995. 11. 23.
;       Revise doCREATE, CREATE, pipe, DOES>, and >BODY.
;               'pipe' is no longer processor-dependent.
; 1995. 11. 17.
;       Move ERASE to ASM8086.F.
;
; Changes from 0.9.5
;
; 1995. 11. 15.
;       Fix MOVE to check whether 'u' is 0.
;       Add ERASE.
; 1995. 11. 5.
;       Revise 'orig+', 'dosys+', etc to catch 'DO IF LOOP' mismatch.
; 1995. 10. 30.
;       Change 'lastName' to VALUE type. Remove '(lastName)'.
;
; Changes from 0.9.2
;
; 1995. 9. 6.
;       Move terminal input buffer (TIB) at the end of the memory to
;               prevent accidental overwriting it. It was too close
;               to HERE and might be overwritten by ALLOT or , .
;       TIB address is only known to REFILL . Revise REFILL .
;       Move PAD also with TIB.
; 1995. 9. 5.
;       Revise EVALUATE for FILE words.
; 1995. 8. 21
;       Mr. Chris Jakeman kindly report several bugs and made suggestions.
;       CHARS is added in the definition of /STRING .
;       '1chars/' is introduced to convert # address units to # chars.
;       'skipPARSE' is introduced. 'parse-word' and 'WORD' are
;               redefined using it.
;
; Changes from 0.9.0
;
; 1995. 7. 21.
;       Make HERE VALUE type and remove 'hereP'. Revise 'xhere'
;               and remove 'TOxhere'.
;       Make SOURCE-ID VALUE type, replace TOsource-id with
;               "TO SOURCE-ID" and remove TOsource-id .
; 1995. 7. 20.
;       Make 'ekey? , 'ekey , 'emit? , 'emit , 'init-i/o , 'prompt
;               and 'boot VALUE type and replace "'emit @ EXECUTE"
;               with "'emit EXECUTE".
; 1995. 7. 19.
;       Add doVALUE , doTO , VALUE and TO .
;       Replace 'DUP' with '?DUP' in the definition of "(')".
;       Replace 'CREATEd' with 'doCREATE' and remove CREATEd .
; 1995. 7. 6.
;       Move "'init-i/o @ EXECUTE" from QUIT to THROW according
;       to the suggestion from Mr. Chris Jakeman.
; 1995. 6. 25.
;       Fix code definition of SPACES .
; 1995. 6. 14.
;       Revise $ENVIR for portability.
;       'CR' is a system dependent definition.
; 1995. 6. 9.
;       Rename '.ok' and '.OKay' as '.prompt' and '.ok' respectively.
; 1995. 6. 5.
;       Fix SOURCE-ID .
; 1995. 5. 2.
;       Redefine $CONST .
;
;;      hForth EXE ¡¡•I·e ­A‹a åËa¡ a’å 8086· ¡A¡¡Ÿ¡ ¡¡•IµA  xÂ´á
;;      hForth RAM ¡¡•I·i ‰¡Áa¬á  e—i´ö·s“¡”a.
;;
;;      hForth RAM ¡¡•I‰Á ”aŸe ¸ñ—i·i ´aœµA ¸â´ö·s“¡”a. ‹¡‰´á ¸÷·—i·i
;;      Ðaa•¡ ‰¡Ã¡»¡ ´g‰¡ Å¡—a ­A‹a åËa· µ¡Ïa­U º­¡Ÿi ”až‹¡ ¶áÐe
;;      Å¡—a { i 4ˆ e·i ”áÐ–·s“¡”a. ´á­Q§iŸ¡ ¤aÈw¥¥µA¬á  aÇa¡µÁ ¡y
;;      ˆ· °w¸ñ ¸÷·Ÿi ¤aŽ´ö·s“¡”a.
;;
;;      1. ¬a¸å· Š¹¡Ÿi ¤aŽ´ö·s“¡”a. hForth RAM ¡¡•IµA¬á“e Å¡—a, ·¡Ÿq,
;;         ¸aža ¸aŸ¡ˆa ´aœÀáœñ ¡¡– ¬ãµa ·¶´ö»¡ e
;;
;;             //·µ‹¡/·¡Ÿq/·¡ŸqˆaŸ¡Ç±/Å¡—a>
;;
;;         hForth EXE ¡¡•IµA¬á“e Å¡—a ¸aŸ¡Ÿi 8086 Ïa¡­A¬á· ”aŸe
;;         ­A‹a åËa¡ ´aœÀáœñ µ«‰v·s“¡”a
;;
;;             CS ­A‹a åËa:     //·¡ŸqˆaŸ¡Ç±/Å¡—a>
;;             DS, SS ­A‹a åËa: //¯¡Ç±Îa/·µ‹¡/·¡Ÿq>
;;
;;         µa‹¡¬á '¯¡Ç±Îa(xt)'“e Å¡—a· ¯¡¸b µ¡Ïa­U º­¡·³“¡”a. $NAME
;;          aÇa¡Ÿi ”áÐa‰¡ $CODE, $COLON, $CONST, $VAR, $USER, $ENVIR
;;          aÇa¡Ÿi ¤aŽ´ö·s“¡”a. RAM ¡¡•I· $VAR“e ¡¡– $CONSTŸi °á¬á
;;         ‰¡Áv·s“¡”a.
;;
;;      2. head,µÁ name>xtŸi ¤aŽ´ö·s“¡”a. ¯¡Ç±Îa(xt)Ÿi head,µA‰A ˆå‘A
;;         º‰ ® ·¶‹¡ ˜¢…µA : , CONSTANT , CREATE ,   VARIABLE·i ROM
;;         ¡¡•IÀáœñ ¤aŽ´ö·s“¡”a. name>xtˆa ROM ¡¡•I‰Á ˆ{´a¹v‹¡ ˜¢…µA
;;         (search-wordlist)· ‹¡‰´á ¸÷·Ÿi ROM ¡¡•I‰Á ˆ{‰A
;;         ¤aŽ´ö·s“¡”a (°w¸ñ ¸÷·“e ‰¡Ã© Ï©¶aˆa ´ô·s“¡”a).
;;
;;      3. CS: ¡ww·i $NEXT  aÇa¡µÁ ‹¡‰´á ¸÷·-doLIT, doCONST,
;;         doCREATE, doUSER, doLOOP, do+LOOP, 0branch, branch-µA
;;         ”áÐ–·s“¡”a. doVARŸi ¨–·s“¡”a.
;;
;;      4. Å¡—a ­A‹a åËa· µ¡Ïa­U º­¡·¥ 'Å¡—a-º­¡(code-addr)'œa“e
;;         ¸ažaÑw·i •¡·³Ð–·s“¡”a. x@ , x! , xb@ , xb!· ‘A ‹¡‰´á ¸÷·Ÿi
;;         ”áÐ–·s“¡”a. x@, x!, xPŸi °á¬á ?call , COMPILE, , optiCOMPILE,
;;         , THEN , >BODY , pipe , rake , xhere , TOxhere ‹¡‰´á ¸÷·Ÿi
;          ‰¡Áv·s“¡”a. xb@µÁ xb!“e aº—µA ”áÐi ´á­Q§iœáˆa Å¡—a ¸aŸ¡µA
;          ¤a·¡Ëaˆt·i ·ª‰¡ ³i ® ·¶‰A ý´á –´ö·s“¡”a.
;;
;;      5. S" , SLITERAL , ."·i ¤aŽ´ö·s“¡”a. D$  aÇa¡µÁ do."µÁ doS"·i
;;         ¨–·s“¡”a.
;;
;;      6. ¬a¶w¸a ¢Ÿeˆt statusµÁ followerˆa Å¡—a ¸aŸ¡· º­¡Ÿi ˆaŸ¡Ç¡‰A
;;         Ð–·s“¡”a. ˆb ¸b´ó·¡ ¸a¯¥· ¬a¶w¸a ¢Ÿeˆt·i Àx·i ® ·¶•¡¢
;;         Å¡—a ¸aŸ¡µA ˆaŸ¡Ç±ˆt·i ý´ö·s“¡”a. wakeµÁ PAUSEŸi
;;         ¤aŽ´ö·s“¡”a. ¤aŽ… °w¸ñ ¸÷·“e RAM ¡¡•I¥¡”a 6% ¸÷•¡ “aŸ³“¡”a.
;;         wakeµÁ PAUSEŸi ‹¡‰´á¡ ¸÷·Ð–·s“¡”a. wakeµÁ PAUSE· ‹¡‰´á
;;         ¸÷·“e RAM ¡¡•I· °w¸ñ ¸÷·¥¡”a 30% ¸÷•¡ ¨aŸs“¡”a.
;;
;;      7. '+'µÁ '-' —w· µaœá °w¸ñ ¸÷·—i·i ‹¡‰´á ¸÷·¡ ¤aŽ´ö·s“¡”a.
;;         ¶¥œ· °w¸ñ ¸÷·—i·e ”õ¦›·± i¡ q‰a –´ö·s“¡”a.
;;
;;
;       hForth EXE model is derived from hForth RAM model and adapted
;       to segmented 8086 memory model.
;
;       Differences from hForth RAM model is described below. No low
;       level CODE definitions is changed and only four words to access
;       code segment address are added. Some macros in the assembler
;       source and high level colon definitions are redefined.
;
;       1. The structure of the dictionary is changed. Code space is
;          separated into different 8086 segment. Name and data spaces
;          are combined in hForth EXE model as below
;
;               CS segment:    //pointer_to_name/code>
;               DS,SS segment: //xt/link/name>
;
;          while they are intermingled in hForth RAM model as below
;
;               //link/name/pointer_to_name/code>
;
;          where xt is the starting address of code. $NAME macro is added
;          and $CODE, $COLON, $CONST, $VAR, $USER and $ENVIR macros are
;          redefined in assembly source. $VAR in RAM model source is
;          replaced with $CONST.
;
;       2. 'head,' and 'name>xt' are redefined. Redefine ':', 'CONSTANT',
;          'CREATE', 'VARIABLE' similar to hForth 8086 ROM model since xt
;          can be given to 'head,'. Set code definition of
;          '(search-wordlist)' same as in ROM model since 'name>xt' is
;          the same as ROM model redefined (although colon definition need
;          not be changed at all).
;
;       3. CS: suffix is added into $NEXT macro and CODE definitions -
;          'doLIT', 'doCONST', 'doCREATE', 'doUSER', 'doLOOP', 'do+LOOP',
;          '0branch', 'branch'. 'doVAR' is removed.
;
;       4. New data type 'code-addr' in introduced which is offset in CS:
;          segment. CODE definitions 'x@', 'x!', 'xb@' and 'xb!' and system
;          variable 'xP' is added. '?call', 'COMPILE,', ; 'optiCOMPILE,',
;          'THEN', '>BODY', 'pipe', 'rake', 'xhere' ; and 'TOxhere' are
;          redefined using 'x@', 'x!' and 'xP'. 'xb@' and 'xb!' will be used
;          by assembler to read and write byte values in code space.
;
;       5. 'S"', 'SLITERAL' and '."' are redefined. D$ macro and 'do."'
;          and 'doS"' are dropped.
;
;       6. USER variable 'status' and 'follower' points code space
;          addresses. Pointer to user variable area are added into code
;          space for each task. Revise 'wake' and 'PAUSE'. High level
;          definitions of 'wake' and 'PAUSE' are about 6% slower compared to
;          RAM model. CODE definitions of 'wake' and 'PAUSE' are given,
;          which makes task-switching 30% faster than RAM model.
;
;       7. Many high level colon definitions such as '+' and '-' are
;          redefined as CODE definitions. Colon definitions are left as
;          comments in assembly source.
;
;===============================================================
;
;       8086/8 register usages
;       Double segment model. DS and SS are same but CS is different.
;       SP:     data stack pointer
;       BP:     return stack pointer
;       SI:     Forth virtual machine instruction pointer
;       BX:     top of data stack item
;       All other registers are free.
;
;       Structure of a task
;       userP points follower.
;       //userP//<return_stack//<data_stack//
;       //user_area/user1/taskName/throwFrame/stackTop/status/follower/sp0/rp0
;
;===============================================================

;***************
; Assembly Constants
;***************

CHARR           EQU     1               ;byte size of a character
CELLL           EQU     2               ;byte size of a cell
MaxChar         EQU     0FFh            ;Extended character set
                                        ;  Use 07Fh for ASCII only
MaxSigned       EQU     07FFFh          ;max value of signed integer
MaxUnsigned     EQU     0FFFFh          ;max value of unsigned integer
MAXNegative     EQU     8000h           ;max value of negative integer
                                        ;  Used in doDO

PADSize         EQU     258             ;PAD area size
RTCells         EQU     64              ;return stack size
DTCells         EQU     256             ;data stack size

BASEE           EQU     10              ;default radix
OrderDepth      EQU     10              ;depth of search order stack

COMPO           EQU     020h            ;lexicon compile only bit
IMMED           EQU     040h            ;lexicon immediate bit
SEMAN           EQU     080h            ;lexicon compilation semantics bit
MASKK           EQU     1Fh             ;lexicon bit mask
                                        ;extended character set
                                        ;maximum name length = 1Fh

BKSPP           EQU     8               ;backspace
TABB            EQU     9               ;tab
LFF             EQU     10              ;line feed
CRR             EQU     13              ;carriage return
DEL             EQU     127             ;delete

CALLL           EQU     0E890h          ;NOP CALL opcodes

; Memory allocation
;   code segment        ||code>--||
;   data segment        ||name/data>WORDworkarea|--//--|PAD|TIB||

; Initialize assembly variables

_SLINK  = 0                                     ;force a null link
_FLINK  = 0                                     ;force a null link
_ENVLINK = 0                                    ;farce a null link

;***************
; Assembly macros
;***************

;       Adjust an address to the next cell boundary.

$ALIGN  MACRO
        EVEN                                    ;for 16 bit systems
        ENDM

;       Add a name to name space of dictionary. To be used to store THROW
;       message in name space. THROW messages won't be needed if target
;       system do not need names of Forth words.

$STR    MACRO   LABEL,STRING
LABEL:
        _LEN    = $
        DB      0,STRING
        _CODE   = $
ORG     _LEN
        DB      _CODE-_LEN-1
ORG     _CODE
        $ALIGN
        ENDM

;       Compile a definition header in name space.

$NAME   MACRO   LEX,NAME,LABEL,AddrNAME,LINK
        $ALIGN                                  ;force to cell boundary
        DW      LABEL                           ;xt
        DW      LINK                            ;link
        _NAME   = $
        LINK    = $                             ;link points to a name string
        AddrNAME = $
        DB      LEX,NAME                        ;name string
        $ALIGN
        ENDM

;       Compile a code definition.

$CODE   MACRO   NAME,LABEL
        DW      NAME
LABEL:                                          ;assembly label
        ENDM

;       Compile a colon definition.

$COLON  MACRO   NAME,LABEL
        $CODE   NAME,LABEL
        NOP                                     ;align to cell boundary
        CALL    DoLIST                          ;include CALL doLIST
        ENDM

;       Compile a system CONSTANT and VARIABLE.

$CONST  MACRO   NAME,LABEL,VALUE
        DW      CompileCONST
        $CODE   NAME,LABEL
        NOP
        CALL    DoCONST
        DW      VALUE
        $NEXT
        ENDM

;       Compile a system VALUE header.

$VALUE  MACRO   NAME,LABEL,OFFSET
        $CODE   NAME,LABEL
        NOP
        CALL    DoVALUE
        DW      OFFSET
        ENDM

;       Compile a system USER variable.

$USER   MACRO   NAME,LABEL,OFFSET
        $CODE   NAME,LABEL
        NOP
        CALL    DoUSER
        DW      OFFSET
        ENDM

;       Compile a environment query string header.

$ENVIR  MACRO   LEX,NAME,LABEL
        $ALIGN                                  ;force to cell boundary
        DW      LABEL                           ;xt
        DW      _ENVLINK                        ;link
        _ENVLINK = $                            ;link points to a name string
        DB      LEX,NAME                        ;name string
        $ALIGN
        DW      _ENVLINK
        ENDM

;       Assemble inline direct threaded code ending.

CSDummy LABEL WORD              ; used like:  lods  cs:CSDummy

$NEXT   MACRO
        LODS    CS:CSDummy                      ;next code address into AX
        JMP     AX                              ;jump directly to code address
        $ALIGN
        ENDM

;===============================================================

FIRST   SEGMENT PARA PUBLIC 'CODES'
FIRST   ENDS

;===============================================================

DATA    SEGMENT

                $STR    SystemIDStr,'hForth 8086 EXE Model'
                $STR    VersionStr,'0.9.7'

; system variables.

                $ALIGN                          ;align to cell boundary
AddrTickEKEYQ   DW      RXQ                     ;'ekey?
AddrTickEKEY    DW      RXFetch                 ;'ekey
AddrTickEMITQ   DW      TXQ                     ;'emit?
AddrTickEMIT    DW      TXStore                 ;'emit
AddrTickINIT_IO DW      Set_IO                  ;'init-i/o
AddrTickPrompt  DW      DotOK                   ;'prompt
AddrTickBoot    DW      HI                      ;'boot
AddrSOURCE_ID   DW      0                       ;SOURCE-ID
AddrHERE        DW      DTOP                    ;data space pointer
AddrXHere       DW      CTOP                    ;code space pointer
AddrTickDoWord  DW      OptiCOMPILEComma        ;nonimmediate word - compilation
                DW      EXECUTE                 ;nonimmediate word - interpretation
                DW      DoubleAlsoComma         ;not found word - compilateion
                DW      DoubleAlso              ;not found word - interpretation
                DW      EXECUTE                 ;immediate word - compilation
                DW      EXECUTE                 ;immediate word - interpretation
AddrBASE        DW      10                      ;BASE
AddrMemTop      DW      0FFFEh                  ;memTop
AddrLastXT      DW      0                       ;lastXT
AddrRakeVar     DW      0                       ;rakeVar
AddrNumberOrder DW      2                       ;#order
                DW      AddrFORTH_WORDLIST      ;search order stack
                DW      AddrNONSTANDARD_WORDLIST
                DW      (OrderDepth-2) DUP (0)
AddrCurrent     DW      AddrFORTH_WORDLIST      ;current pointer
AddrFORTH_WORDLIST DW   LASTFORTH               ;FORTH-WORDLIST
                DW      AddrNONSTANDARD_WORDLIST;wordlist link
                DW      FORTH_WORDLISTName      ;name of the WORDLIST
AddrNONSTANDARD_WORDLIST DW      LASTSYSTEM     ;NONSTANDARD-WORDLIST
                DW      0                       ;wordlist link
                DW      NONSTANDARD_WORDLISTName;name of the WORDLIST
AddrEnvQList    DW      LASTENV                 ;envQList
AddrUserP       DW      SysUserP                ;user pointer
SysTask         DW      SysUserP                ;system task's tid
SysUser1        DW      ?                       ;user1
SysTaskName     DW      SystemTaskName          ;taskName
SysThrowFrame   DW      ?                       ;throwFrame
SysStackTop     DW      ?                       ;stackTop
SysStatus       DW      XSysStatus              ;status
SysUserP:
SysFollower     DW      XSysFollower            ;follower
                DW      SPP                     ;system task's sp0
                DW      RPP                     ;system task's rp0

AddrNumberOrder0 DW     2                       ;#order0
                DW      AddrFORTH_WORDLIST      ;search order stack
                DW      AddrNONSTANDARD_WORDLIST
                DW      (OrderDepth-2) DUP (0)

AddrAbortQMsg   DW      2 DUP (?)
AddrBalance     DW      ?
AddrErrWord     DW      2 DUP (?)
AddrHLD         DW      ?
AddrLastName    DW      ?
AddrSourceVar   DW      2 DUP (?)
AddrToIN        DW      ?
AddrSTATE       DW      ?
AddrSpecialCompQ DW      ?

RStack          DW      RTCells DUP (0AAAAh)    ;to see how deep stack grows
RPP             EQU     $-CELLL
DStack          DW      DTCells DUP (05555h)    ;to see how deep stack grows
SPP             EQU     $-CELLL

; THROW code messages

        $STR    NullString,''
        $STR    THROWMsg_01,'ABORT'
        $STR    THROWMsg_02,'ABORT"'
        $STR    THROWMsg_03,'stack overflow'
        $STR    THROWMsg_04,'stack underflow'
        $STR    THROWMsg_05,'return stack overflow'
        $STR    THROWMsg_06,'return stack underflow'
        $STR    THROWMsg_07,'do-loops nested too deeply during execution'
        $STR    THROWMsg_08,'dictionary overflow'
        $STR    THROWMsg_09,'invalid memory address'
        $STR    THROWMsg_10,'division by zero'
        $STR    THROWMsg_11,'result out of range'
        $STR    THROWMsg_12,'argument type mismatch'
        $STR    THROWMsg_13,'undefined word'
        $STR    THROWMsg_14,'interpreting a compile-only word'
        $STR    THROWMsg_15,'invalid FORGET'
        $STR    THROWMsg_16,'attempt to use zero-length string as a name'
        $STR    THROWMsg_17,'pictured numeric output string overflow'
        $STR    THROWMsg_18,'parsed string overflow'
        $STR    THROWMsg_19,'definition name too long'
        $STR    THROWMsg_20,'write to a read-only location'
        $STR    THROWMsg_21,'unsupported operation (e.g., AT-XY on a too-dumb terminal)'
        $STR    THROWMsg_22,'control structure mismatch'
        $STR    THROWMsg_23,'address alignment exception'
        $STR    THROWMsg_24,'invalid numeric argument'
        $STR    THROWMsg_25,'return stack imbalance'
        $STR    THROWMsg_26,'loop parameters unavailable'
        $STR    THROWMsg_27,'invalid recursion'
        $STR    THROWMsg_28,'user interrupt'
        $STR    THROWMsg_29,'compiler nesting'
        $STR    THROWMsg_30,'obsolescent feature'
        $STR    THROWMsg_31,'>BODY used on non-CREATEd definition'
        $STR    THROWMsg_32,'invalid name argument (e.g., TO xxx)'
        $STR    THROWMsg_33,'block read exception'
        $STR    THROWMsg_34,'block write exception'
        $STR    THROWMsg_35,'invalid block number'
        $STR    THROWMsg_36,'invalid file position'
        $STR    THROWMsg_37,'file I/O exception'
        $STR    THROWMsg_38,'non-existent file'
        $STR    THROWMsg_39,'unexpected end of file'
        $STR    THROWMsg_40,'invalid BASE for floating point conversion'
        $STR    THROWMsg_41,'loss of precision'
        $STR    THROWMsg_42,'floating-point divide by zero'
        $STR    THROWMsg_43,'floating-point result out of range'
        $STR    THROWMsg_44,'floating-point stack overflow'
        $STR    THROWMsg_45,'floating-point stack underflow'
        $STR    THROWMsg_46,'floating-point invalid argument'
        $STR    THROWMsg_47,'compilation word list deleted'
        $STR    THROWMsg_48,'invalid POSTPONE'
        $STR    THROWMsg_49,'search-order overflow'
        $STR    THROWMsg_50,'search-order underflow'
        $STR    THROWMsg_51,'compilation word list changed'
        $STR    THROWMsg_52,'control-flow stack overflow'
        $STR    THROWMsg_53,'exception stack overflow'
        $STR    THROWMsg_54,'floating-point underflow'
        $STR    THROWMsg_55,'floating-point unidentified fault'
        $STR    THROWMsg_56,'QUIT'
        $STR    THROWMsg_57,'exception in sending or receiving a character'
        $STR    THROWMsg_58,'[IF], [ELSE], or [THEN] exception'

        DW      THROWMsg_58,THROWMsg_57,THROWMsg_56,THROWMsg_55
        DW      THROWMsg_54,THROWMsg_53,THROWMsg_52,THROWMsg_51
        DW      THROWMsg_50,THROWMsg_49,THROWMsg_48,THROWMsg_47
        DW      THROWMsg_46,THROWMsg_45,THROWMsg_44,THROWMsg_43
        DW      THROWMsg_42,THROWMsg_41,THROWMsg_40,THROWMsg_39
        DW      THROWMsg_38,THROWMsg_37,THROWMsg_36,THROWMsg_35
        DW      THROWMsg_34,THROWMsg_33,THROWMsg_32,THROWMsg_31
        DW      THROWMsg_30,THROWMsg_29,THROWMsg_28,THROWMsg_27
        DW      THROWMsg_26,THROWMsg_25,THROWMsg_24,THROWMsg_23
        DW      THROWMsg_22,THROWMsg_21,THROWMsg_20,THROWMsg_19
        DW      THROWMsg_18,THROWMsg_17,THROWMsg_16,THROWMsg_15
        DW      THROWMsg_14,THROWMsg_13,THROWMsg_12,THROWMsg_11
        DW      THROWMsg_10,THROWMsg_09,THROWMsg_08,THROWMsg_07
        DW      THROWMsg_06,THROWMsg_05,THROWMsg_04,THROWMsg_03
        DW      THROWMsg_02,THROWMsg_01
AddrTHROWMsgTbl:

                $NAME   3,'RX?',RXQ,NameRXQ,_SLINK
                $NAME   3,'RX@',RXFetch,NameRXFetch,_SLINK
                $NAME   SEMAN+3,'TX?',TXQ,NameTXQ,_SLINK
                $NAME   3,'TX!',TXStore,NameTXStore,_SLINK
                $NAME   2,'CR',CR,NameCR,_FLINK
                $NAME   3,'BYE',BYE,NameBYE,_FLINK
                $NAME   2,'hi',HI,NameHI,_SLINK
                $STR    SystemIDQStr,'systemID'
                $STR    VersionQStr,'version'
                $STR    HiStr1,' by Wonyong Koh, 1995'
                $STR    HiStr2,'ALL noncommercial and commercial uses are granted.'
                $STR    HiStr3,'Please send comment, bug report and suggestions to:'
                $STR    HiStr4,'  wykoh@pado.krict.re.kr or 82-42-861-4245 (FAX)'
                $NAME   4,'COLD',COLD,NameCOLD,_SLINK
                $NAME   7,'set-i/o',Set_IO,NameSet_IO,_SLINK
                $STR    Set_IOstr,'CON'
                $NAME   8,'redirect',Redirect,NameRedirect,_SLINK
                $NAME   6,'asciiz',ASCIIZ,NameASCIIZ,_SLINK
                $NAME   5,'stdin',STDIN,NameSTDIN,_SLINK
                $NAME   IMMED+2,'<<',FROM,NameFROM,_SLINK
                $STR    FROMstr,'Do not use << in a definition.'
                $NAME   5,'same?',SameQ,NameSameQ,_SLINK
                $NAME   17,'(search-wordlist)',ParenSearch_Wordlist,NameParenSearch_Wordlist,_SLINK
                $NAME   5,'?call',QCall,NameQCall,_SLINK
                $NAME   COMPO+4,'pipe',Pipe,NamePipe,_SLINK
                $NAME   3,'xt,',xtComma,NamextComma,_SLINK
                $NAME   COMPO+13,'compileCREATE',CompileCREATE,NameCompileCREATE,_SLINK
                $NAME   COMPO+12,'compileCONST',CompileCONST,NameCompileCONST,_SLINK
                $NAME   COMPO+5,'doLIT',DoLIT,NameDoLIT,_SLINK
                $NAME   COMPO+7,'doCONST',DoCONST,NameDoCONST,_SLINK
                $NAME   COMPO+8,'doCREATE',DoCREATE,NameDoCREATE,_SLINK
                $NAME   COMPO+7,'doVALUE',DoVALUE,NameDoVALUE,_SLINK
                $NAME   COMPO+4,'doTO',DoTO,NameDoTO,_SLINK
                $NAME   COMPO+6,'doUSER',DoUSER,NameDoUSER,_SLINK
                $NAME   COMPO+6,'doLIST',DoLIST,NameDoLIST,_SLINK
                $NAME   COMPO+6,'doLOOP',DoLOOP,NameDoLOOP,_SLINK
                $NAME   COMPO+7,'do+LOOP',DoPLOOP,NameDoPLOOP,_SLINK
                $NAME   COMPO+7,'0branch',ZBranch,NameZBranch,_SLINK
                $NAME   COMPO+6,'branch',Branch,NameBranch,_SLINK
                $NAME   COMPO+3,'rp@',RPFetch,NameRPFetch,_SLINK
                $NAME   COMPO+3,'rp!',RPStore,NameRPStore,_SLINK
                $NAME   3,'sp@',SPFetch,NameSPFetch,_SLINK
                $NAME   3,'sp!',SPStore,NameSPStore,_SLINK
                $NAME   3,'um+',UMPlus,NameUMPlus,_SLINK
                $NAME   5,'code!',CodeStore,NameCodeStore,_SLINK
                $NAME   6,'codeB!',CodeBStore,NameCodeBStore,_SLINK
                $NAME   5,'code@',CodeFetch,NameCodeFetch,_SLINK
                $NAME   6,'codeB@',CodeBFetch,NameCodeBFetch,_SLINK
                $NAME   5,'code,',CodeComma,NameCodeComma,_SLINK
                $NAME   5,'ALIGN',ALIGNN,NameALIGNN,_FLINK
                $NAME   7,'ALIGNED',ALIGNED,NameALIGNED,_FLINK
                $NAME   5,'CELLS',CELLS,NameCELLS,_FLINK
                $NAME   5,'CHARS',CHARS,NameCHARS,_FLINK
                $NAME   7,'1chars/',OneCharsSlash,NameOneCharsSlash,_SLINK
                $NAME   1,'!',Store,NameStore,_FLINK
                $NAME   2,'0<',ZeroLess,NameZeroLess,_FLINK
                $NAME   2,'0=',ZeroEquals,NameZeroEquals,_FLINK
                $NAME   2,'2*',TwoStar,NameTwoStar,_FLINK
                $NAME   2,'2/',TwoSlash,NameTwoSlash,_FLINK
                $NAME   COMPO+2,'>R',ToR,NameToR,_FLINK
                $NAME   1,'@',Fetch,NameFetch,_FLINK
                $NAME   3,'AND',ANDD,NameANDD,_FLINK
                $NAME   2,'C!',CStore,NameCStore,_FLINK
                $NAME   2,'C@',CFetch,NameCFetch,_FLINK
                $NAME   4,'DROP',DROP,NameDROP,_FLINK
                $NAME   3,'DUP',DUPP,NameDUPP,_FLINK
                $NAME   7,'EXECUTE',EXECUTE,NameEXECUTE,_FLINK
                $NAME   COMPO+4,'EXIT',EXIT,NameEXIT,_FLINK
                $NAME   4,'MOVE',MOVE,NameMOVE,_FLINK
                $NAME   2,'OR',ORR,NameORR,_FLINK
                $NAME   4,'OVER',OVER,NameOVER,_FLINK
                $NAME   COMPO+2,'R>',RFrom,NameRFrom,_FLINK
                $NAME   COMPO+2,'R@',RFetch,NameRFetch,_FLINK
                $NAME   4,'SWAP',SWAP,NameSWAP,_FLINK
                $NAME   3,'XOR',XORR,NameXORR,_FLINK
                $NAME   SEMAN+7,'#order0',NumberOrder0,NameNumberOrder0,_SLINK
                $NAME   6,"'ekey?",TickEKEYQ,NameTickEKEYQ,_SLINK
                $NAME   5,"'ekey",TickEKEY,NameTickEKEY,_SLINK
                $NAME   6,"'emit?",TickEMITQ,NameTickEMITQ,_SLINK
                $NAME   5,"'emit",TickEMIT,NameTickEMIT,_SLINK
                $NAME   9,"'init-i/o",TickINIT_IO,NameTickINIT_IO,_SLINK
                $NAME   7,"'prompt",TickPrompt,NameTickPrompt,_SLINK
                $NAME   5,"'boot",TickBoot,NameTickBoot,_SLINK
                $NAME   9,'SOURCE-ID',SOURCE_ID,NameSOURCE_ID,_FLINK
                $NAME   4,'HERE',HERE,NameHERE,_FLINK
                $NAME   5,'xhere',XHere,NameXHere,_SLINK
                $NAME   SEMAN+7,"'doWord",TickDoWord,NameTickDoWord,_SLINK
                $NAME   SEMAN+4,'BASE',BASE,NameBASE,_FLINK
                $NAME   SEMAN+11,'THROWMsgTbl',THROWMsgTbl,NameTHROWMsgTbl,_SLINK
                $NAME   6,'memTop',MemTop,NameMemTop,_SLINK
                $NAME   6,'lastXT',LastXT,NameLastXT,_SLINK
                $NAME   SEMAN+7,'rakeVar',RakeVar,NameRakeVar,_SLINK
                $NAME   SEMAN+6,'#order',NumberOrder,NameNumberOrder,_SLINK
                $NAME   SEMAN+7,'current',Current,NameCurrent,_SLINK
                $NAME   SEMAN+14,'FORTH-WORDLIST',FORTH_WORDLIST,NameFORTH_WORDLIST,_FLINK
FORTH_WORDLISTName      EQU     _NAME-0
                $NAME   SEMAN+20,'NONSTANDARD-WORDLIST',NONSTANDARD_WORDLIST,NameNONSTANDARD_WORDLIST,_FLINK
NONSTANDARD_WORDLISTName EQU    _NAME-0
                $NAME   SEMAN+8,'envQList',EnvQList,NameEnvQList,_SLINK
                $NAME   SEMAN+5,'userP',UserP,NameUserP,_SLINK
                $NAME   SEMAN+10,'SystemTask',SystemTask,NameSystemTask,_SLINK
SystemTaskName  EQU     _NAME-0
                $NAME   8,'follower',Follower,NameFollower,_SLINK
                $NAME   6,'status',Status,NameStatus,_SLINK
                $NAME   8,'stackTop',StackTop,NameStackTop,_SLINK
                $NAME   10,'throwFrame',ThrowFrame,NameThrowFrame,_SLINK
                $NAME   8,'taskName',TaskName,NameTaskName,_SLINK
                $NAME   5,'user1',User1,NameUser1,_SLINK
                $ENVIR  8,'systemID',SystemID
                $ENVIR  7,'version',Version
                $ENVIR  15,'/COUNTED-STRING',SlashCOUNTED_STRING
                $ENVIR  5,'/HOLD',SlashHOLD
                $ENVIR  4,'/PAD',SlashPAD
                $ENVIR  17,'ADDRESS-UNIT-BITS',ADDRESS_UNIT_BITS
                $ENVIR  4,'CORE',CORE
                $ENVIR  7,'FLOORED',FLOORED
                $ENVIR  8,'MAX-CHAR',MAX_CHAR
                $ENVIR  5,'MAX-D',MAX_D
                $ENVIR  5,'MAX-N',MAX_N
                $ENVIR  5,'MAX-U',MAX_U
                $ENVIR  6,'MAX-UD',MAX_UD
                $ENVIR  18,'RETURN-STACK-CELLS',RETURN_STACK_CELLS
                $ENVIR  11,'STACK-CELLS',STACK_CELLS
                $ENVIR  9,'EXCEPTION',EXCEPTION
                $ENVIR  13,'EXCEPTION-EXT',EXCEPTION_EXT
                $ENVIR  9,'WORDLISTS',WORDLISTS
                $NAME   3,"(')",ParenTick,NameParenTick,_SLINK
                $NAME   4,'(d.)',ParenDDot,NameParenDDot,_SLINK
                $NAME   3,'.ok',DotOK,NameDotOK,_SLINK
                $STR    DotOKStr,'ok'
                $NAME   7,'.prompt',DotPrompt,NameDotOK,_SLINK
                $NAME   SEMAN+1,'0',Zero,NameZero,_SLINK
                $NAME   SEMAN+1,'1',One,NameOne,_SLINK
                $NAME   SEMAN+2,'-1',MinusOne,NameMinusOne,_SLINK
                $NAME   SEMAN+9,'abort"msg',AbortQMsg,NameAbortQMsg,_SLINK
                $NAME   SEMAN+3,'bal',Balance,NameBalance,_SLINK
                $NAME   5,'orig+',OrigPlus,NameOrigPlus,_SLINK
                $NAME   5,'orig-',OrigMinus,NameOrigMinus,_SLINK
                $NAME   5,'dest+',DestPlus,NameDestPlus,_SLINK
                $NAME   5,'dest-',DestMinus,NameDestMinus,_SLINK
                $NAME   6,'dosys+',DoSysPlus,NameDoSysPlus,_SLINK
                $NAME   6,'dosys-',DoSysMinus,NameDoSysMinus,_SLINK
                $NAME   5,'cell-',CellMinus,NameCellMinus,_SLINK
                $NAME   12,'COMPILE-ONLY',COMPILE_ONLY,NameCOMPILE_ONLY,_SLINK
                $NAME   COMPO+4,'doDO',DoDO,NameDoDO,_SLINK
                $NAME   SEMAN+7,'errWord',ErrWord,NameErrWord,_SLINK
                $NAME   5,'head,',HeadComma,NameHeadComma,_SLINK
                $STR    HEADCstr,'redefine '
                $NAME   SEMAN+3,'hld',HLD,NameHLD,_SLINK
                $NAME   9,'interpret',Interpret,NameInterpret,_SLINK
                $NAME   12,'optiCOMPILE,',OptiCOMPILEComma,NameOptiCOMPILEComma,_SLINK
                $NAME   10,'singleOnly',SingleOnly,NameSingleOnly,_SLINK
                $NAME   11,'singleOnly,',SingleOnlyComma,NameSingleOnlyComma,_SLINK
                $NAME   12,'(doubleAlso)',ParenDoubleAlso,NameParenDoubleAlso,_SLINK
                $NAME   10,'doubleAlso',DoubleAlso,NameDoubleAlso,_SLINK
                $NAME   11,'doubleAlso,',DoubleAlsoComma,NameDoubleAlsoComma,_SLINK
                $NAME   IMMED+2,'-.',MinusDot,NameMinusDot,_SLINK
                $NAME   8,'lastName',LastName,NameLastName,_SLINK
                $NAME   8,'linkLast',LinkLast,NameLinkLast,_SLINK
                $NAME   7,'name>xt',NameToXT,NameNameToXT,_SLINK
                $NAME   5,'pack"',PackQuote,NamePackQuote,_SLINK
                $NAME   9,'skipPARSE',SkipPARSE,NameSkipPARSE,_SLINK
                $NAME   12,'specialComp?',SpecialCompQ,NameSpecialCompQ,_SLINK
                $NAME   10,'PARSE-WORD',PARSE_WORD,NamePARSE_WORD,_SLINK
                $NAME   COMPO+4,'rake',rake,Namerake,_SLINK
                $NAME   3,'rp0',RPZero,NameRPZero,_SLINK
                $NAME   6,'search',Search,NameSearch,_SLINK
                $NAME   SEMAN+9,'sourceVar',SourceVar,NameSourceVar,_SLINK
                $NAME   3,'sp0',SPZero,NameSPZero,_SLINK
                $NAME   COMPO+5,'PAUSE',PAUSE,NamePAUSE,_SLINK
                $NAME   COMPO+4,'wake',Wake,NameWake,_SLINK
                $NAME   1,'#',NumberSign,NameNumberSign,_FLINK
                $NAME   2,'#>',NumberSignGreater,NameNumberSignGreater,_FLINK
                $NAME   2,'#S',NumberSignS,NameNumberSignS,_FLINK
                $NAME   1,"'",Tick,NameTick,_FLINK
                $NAME   1,'+',Plus,NamePlus,_FLINK
                $NAME   2,'+!',PlusStore,NamePlusStore,_FLINK
                $NAME   1,',',Comma,NameComma,_FLINK
                $NAME   1,'-',Minus,NameMinus,_FLINK
                $NAME   1,'.',Dot,NameDot,_FLINK
                $NAME   1,'/',Slash,NameSlash,_FLINK
                $NAME   4,'/MOD',SlashMOD,NameSlashMOD,_FLINK
                $NAME   7,'/STRING',SlashSTRING,NameSlashSTRING,_FLINK
                $NAME   2,'1+',OnePlus,NameOnePlus,_FLINK
                $NAME   2,'1-',OneMinus,NameOneMinus,_FLINK
                $NAME   2,'2!',TwoStore,NameTwoStore,_FLINK
                $NAME   2,'2@',TwoFetch,NameTwoFetch,_FLINK
                $NAME   5,'2DROP',TwoDROP,NameTwoDROP,_FLINK
                $NAME   4,'2DUP',TwoDUP,NameTwoDUP,_FLINK
                $NAME   5,'2SWAP',TwoSWAP,NameTwoSWAP,_FLINK
                $NAME   1,':',COLON,NameCOLON,_FLINK
                $NAME   7,':NONAME',ColonNONAME,NameColonNONAME,_FLINK
                $NAME   IMMED+COMPO+1,';',Semicolon,NameSemicolon,_FLINK
                $NAME   1,'<',LessThan,NameLessThan,_FLINK
                $NAME   2,'<#',LessNumberSign,NameLessNumberSign,_FLINK
                $NAME   1,'=',Equals,NameEquals,_FLINK
                $NAME   1,'>',GreaterThan,NameGreaterThan,_FLINK
                $NAME   SEMAN+3,'>IN',ToIN,NameToIN,_FLINK
                $NAME   7,'>NUMBER',ToNUMBER,NameToNUMBER,_FLINK
                $NAME   4,'?DUP',QuestionDUP,NameQuestionDUP,_FLINK
                $NAME   5,'ABORT',ABORT,NameABORT,_FLINK
                $NAME   6,'ACCEPT',ACCEPT,NameACCEPT,_FLINK
                $NAME   IMMED+COMPO+5,'AGAIN',AGAIN,NameAGAIN,_FLINK
                $NAME   IMMED+COMPO+5,'AHEAD',AHEAD,NameAHEAD,_FLINK
                $NAME   SEMAN+2,'BL',BLank,NameBLank,_FLINK
                $NAME   5,'CATCH',CATCH,NameCATCH,_FLINK
                $NAME   5,'CELL+',CELLPlus,NameCELLPlus,_FLINK
                $NAME   5,'CHAR+',CHARPlus,NameCHARPlus,_FLINK
                $NAME   COMPO+8,'COMPILE,',COMPILEComma,NameCOMPILEComma,_FLINK
                $NAME   8,'CONSTANT',CONSTANT,NameCONSTANT,_FLINK
                $NAME   5,'COUNT',COUNT,NameCOUNT,_FLINK
                $NAME   6,'CREATE',CREATE,NameCREATE,_FLINK
                $NAME   2,'D+',DPlus,NameDPlus,_FLINK
                $NAME   2,'D.',DDot,NameDDot,_FLINK
                $NAME   7,'DECIMAL',DECIMAL,NameDECIMAL,_FLINK
                $NAME   5,'DEPTH',DEPTH,NameDEPTH,_FLINK
                $NAME   7,'DNEGATE',DNEGATE,NameDNEGATE,_FLINK
                $NAME   4,'EKEY',EKEY,NameEKEY,_FLINK
                $NAME   4,'EMIT',EMIT,NameEMIT,_FLINK
                $NAME   4,'FIND',FIND,NameFIND,_FLINK
                $NAME   6,'FM/MOD',FMSlashMOD,NameFMSlashMOD,_FLINK
                $NAME   11,'GET-CURRENT',GET_CURRENT,NameGET_CURRENT,_FLINK
                $NAME   4,'HOLD',HOLD,NameHOLD,_FLINK
                $NAME   COMPO+1,'I',I,NameI,_FLINK
                $NAME   IMMED+COMPO+2,'IF',IFF,NameIFF,_FLINK
                $NAME   6,'INVERT',INVERT,NameINVERT,_FLINK
                $NAME   3,'KEY',KEY,NameKEY,_FLINK
                $NAME   IMMED+COMPO+7,'LITERAL',LITERAL,NameLITERAL,_FLINK
                $NAME   6,'NEGATE',NEGATE,NameNEGATE,_FLINK
                $NAME   3,'NIP',NIP,NameNIP,_FLINK
                $NAME   5,'PARSE',PARSE,NamePARSE,_FLINK
                $NAME   4,'QUIT',QUIT,NameQUIT,_FLINK
                $STR    QUITstr,' Exeption # '
                $NAME   6,'REFILL',REFILL,NameREFILL,_FLINK
                $NAME   3,'ROT',ROT,NameROT,_FLINK
                $NAME   3,'S>D',SToD,NameSToD,_FLINK
                $NAME   15,'SEARCH-WORDLIST',SEARCH_WORDLIST,NameSEARCH_WORDLIST,_FLINK
                $NAME   4,'SIGN',SIGN,NameSIGN,_FLINK
                $NAME   6,'SOURCE',SOURCE,NameSOURCE,_FLINK
                $NAME   5,'SPACE',SPACE,NameSPACE,_FLINK
                $NAME   SEMAN+5,'STATE',STATE,NameSTATE,_FLINK
                $NAME   IMMED+COMPO+4,'THEN',THENN,NameTHENN,_FLINK
                $NAME   5,'THROW',THROW,NameTHROW,_FLINK
                $NAME   4,'TYPE',TYPEE,NameTYPEE,_FLINK
                $NAME   2,'U<',ULess,NameULess,_FLINK
                $NAME   3,'UM*',UMStar,NameUMStar,_FLINK
                $NAME   6,'UM/MOD',UMSlashMOD,NameUMSlashMOD,_FLINK
                $NAME   COMPO+6,'UNLOOP',UNLOOP,NameUNLOOP,_FLINK
                $NAME   6,'WITHIN',WITHIN,NameWITHIN,_FLINK
                $NAME   IMMED+COMPO+1,'[',LeftBracket,NameLeftBracket,_FLINK
                $NAME   1,']',RightBracket,NameRightBracket,_FLINK
                $NAME   IMMED+1,'(',Paren,NameParen,_FLINK
                $NAME   1,'*',Star,NameStar,_FLINK
                $NAME   2,'*/',StarSlash,NameStarSlash,_FLINK
                $NAME   5,'*/MOD',StarSlashMOD,NameStarSlashMOD,_FLINK
                $NAME   IMMED+COMPO+5,'+LOOP',PlusLOOP,NamePlusLOOP,_FLINK
                $NAME   IMMED+COMPO+2,'."',DotQuote,NameDotQuote,_FLINK
                $NAME   5,'2OVER',TwoOVER,NameTwoOVER,_FLINK
                $NAME   5,'>BODY',ToBODY,NameToBODY,_FLINK
                $NAME   IMMED+COMPO+6,'ABORT"',ABORTQuote,NameABORTQuote,_FLINK
                $NAME   3,'ABS',ABSS,NameABSS,_FLINK
                $NAME   5,'ALLOT',ALLOT,NameALLOT,_FLINK
                $NAME   IMMED+COMPO+5,'BEGIN',BEGIN,NameBEGIN,_FLINK
                $NAME   2,'C,',CComma,NameCComma,_FLINK
                $NAME   4,'CHAR',CHAR,NameCHAR,_FLINK
                $NAME   IMMED+COMPO+2,'DO',DO,NameDO,_FLINK
                $NAME   IMMED+COMPO+5,'DOES>',DOESGreater,NameDOESGreater,_FLINK
                $NAME   IMMED+COMPO+4,'ELSE',ELSEE,NameELSEE,_FLINK
                $NAME   12,'ENVIRONMENT?',ENVIRONMENTQuery,NameENVIRONMENTQuery,_FLINK
                $NAME   8,'EVALUATE',EVALUATE,NameEVALUATE,_FLINK
                $NAME   4,'FILL',FILL,NameFILL,_FLINK
                $NAME   9,'IMMEDIATE',IMMEDIATE,NameIMMEDIATE,_FLINK
                $NAME   COMPO+1,'J',J,NameJ,_FLINK
                $NAME   IMMED+COMPO+5,'LEAVE',LEAVEE,NameLEAVEE,_FLINK
                $NAME   IMMED+COMPO+4,'LOOP',LOOPP,NameLOOPP,_FLINK
                $NAME   6,'LSHIFT',LSHIFT,NameLSHIFT,_FLINK
                $NAME   2,'M*',MStar,NameMStar,_FLINK
                $NAME   3,'MAX',MAX,NameMAX,_FLINK
                $NAME   3,'MIN',MIN,NameMIN,_FLINK
                $NAME   3,'MOD',MODD,NameMODD,_FLINK
                $NAME   IMMED+COMPO+8,'POSTPONE',POSTPONE,NamePOSTPONE,_FLINK
                $NAME   IMMED+COMPO+7,'RECURSE',RECURSE,NameRECURSE,_FLINK
                $NAME   IMMED+COMPO+6,'REPEAT',REPEAT,NameREPEAT,_FLINK
                $NAME   6,'RSHIFT',RSHIFT,NameRSHIFT,_FLINK
                $NAME   IMMED+COMPO+8,'SLITERAL',SLITERAL,NameSLITERAL,_FLINK
                $NAME   IMMED+2,'S"',SQuote,NameSQuote,_FLINK
                $STR    SQUOTstr1,'Use of S" in interpretation state is non-portable.'
                $STR    SQUOTstr2,'Use instead  CHAR " PARSE word" or BL PARSE word .'
                $NAME   6,'SM/REM',SMSlashREM,NameSMSlashREM,_FLINK
                $NAME   6,'SPACES',SPACES,NameSPACES,_FLINK
                $NAME   IMMED+2,'TO',TO,NameTO,_FLINK
                $NAME   2,'U.',UDot,NameUDot,_FLINK
                $NAME   IMMED+COMPO+5,'UNTIL',UNTIL,NameUNTIL,_FLINK
                $NAME   5,'VALUE',VALUE,NameVALUE,_FLINK
                $NAME   8,'VARIABLE',VARIABLE,NameVARIABLE,_FLINK
                $NAME   IMMED+COMPO+5,'WHILE',WHILE,NameWHILE,_FLINK
                $NAME   4,'WORD',WORDD,NameWORDD,_FLINK
                $NAME   IMMED+COMPO+3,"[']",BracketTick,NameBracketTick,_FLINK
                $NAME   IMMED+COMPO+6,'[CHAR]',BracketCHAR,NameBracketCHAR,_FLINK
                $NAME   IMMED+1,'\',Backslash,NameBackslash,_FLINK
                $NAME   5,'EKEY?',EKEYQuestion,NameEKEYQuestion,_FLINK
                $NAME   5,'EMIT?',EMITQuestion,NameEMITQuestion,_FLINK

LASTENV         EQU     _ENVLINK-0
LASTSYSTEM      EQU     _SLINK-0        ;last SYSTEM word name address
LASTFORTH       EQU     _FLINK-0        ;last FORTH word name address

DTOP            EQU     $-0             ;next available memory in data space

DATA    ENDS

;===============================================================

CODE    SEGMENT PARA PUBLIC 'CODES'

ASSUME  CS:CODE,DS:DATA,SS:DATA

;***************
; Main entry points and COLD start data
;***************

DS0             DW      DATA
CSEnd           DW      CTOP
DSEnd           DW      DTOP

XSysStatus      DW      Wake                    ;for multitasker
XSysFollower    DW      XSysStatus              ;for multitasker
                DW      SysUserP                ;for multitasker

ORIG:           MOV     BP,DS                   ;save PSP segment
                MOV     DX,CS
                ADD     DX,1000h                ;64KB full segment
                MOV     ES,DX                   ;data segment start after code
                MOV     BX,CS:[DS0]             ;DX = old data segment
                MOV     DS,BX
                MOV     CX,DSEnd
                MOV     SI,CX
                DEC     SI
                MOV     DI,SI
                STD                             ;move data segment after
                REP MOVSB                       ;full 64KB code segment
                MOV     AH,04Ah                 ;extend code segment to 64KB
                MOV     BX,2010h                ;PSP + code + data
                MOV     ES,BP
                INT     21h
                JNC     ORIG2
OutMemory:      MOV     AH,009h
                MOV     DX,CS
                MOV     DS,DX
                MOV     DX,offset OutMemStr
                INT     21h
                MOV     AH,4Ch
                INT     21h                     ;terminate program
OutMemStr       DB      0Dh,0Ah,'Insufficient memory!!!'
                DB      0Dh,0Ah,'This program needs 129KB.',0Dh,0Ah,'$'

ORIG2:          MOV     DS,DX                   ;new data segment
                MOV     CS:[DS0],DX             ;save data segment
                CLI                             ;disable interrupts, old 808x CPU bug
                MOV     SS,DX                   ;SS is same as DS
                MOV     SP,OFFSET SPP           ;initialize SP
                STI                             ;enable interrupts
                MOV     BP,OFFSET RPP           ;initialize RP
                CLD                             ;direction flag, increment
                XOR     AX,AX                   ;MS-DOS only
                MOV     CS:Redirect1stQ,AX      ;MS-DOS only
                JMP     COLD                    ;to high level cold start

;***************
; System dependent words -- Must be re-definded for each system.
;***************
; I/O words must be redefined if serial communication is used instead of
; keyboard. Following words are for MS-DOS system.

;   RX?         ( -- flag )
;               Return true if key is pressed.

                $CODE   NameRXQ,RXQ
                PUSH    BX
                MOV     AH,0Bh                  ;get input status of STDIN
                INT     021h
                CBW
                MOV     BX,AX
                $NEXT

;   RX@         ( -- u )
;               Receive one keyboard event u.

                $CODE   NameRXFetch,RXFetch
                PUSH    BX
                XOR     BX,BX
                MOV     AH,08h                  ;MS-DOS Read Keyboard
                INT     021h
                ADD     BL,AL                   ;MOV BL,AL and OR AL,AL
                JNZ     RXFET1                  ;extended character code?
                INT     021h
                MOV     BH,AL
RXFET1:         $NEXT

;   TX?         ( -- flag )
;               Return true if output device is ready or device state is
;               indeterminate.

                $CONST  NameTXQ,TXQ,-1          ;always true for MS-DOS

;   TX!         ( u -- )
;               Send char to the output device.

                $CODE   NameTXStore,TXStore
                MOV     DX,BX                   ;char in DL
                MOV     AH,02h                  ;MS-DOS Display output
                INT     021H                    ;display character
                POP     BX
                $NEXT

;   CR          ( -- )                          \ CORE
;               Carriage return and linefeed.
;
;   : CR        carriage-return-char EMIT  linefeed-char EMIT ;

                $COLON  NameCR,CR
                DW      DoLIT,CRR,EMIT,DoLIT,LFF,EMIT,EXIT

;   BYE         ( -- )                          \ TOOLS EXT
;               Return control to the host operation system, if any.

                $CODE   NameBYE,BYE
                MOV     AX,04C00h               ;close all files and
                INT     021h                    ;  return to MS-DOS
                $ALIGN

;   hi          ( -- )
;
;   : hi        CR S" systemID" ENVIRONMENT? DROP TYPE SPACE [CHAR] v EMIT
;                  S" version"  ENVIRONMENT? DROP TYPE
;               ."  by Wonyong Koh, 1995" CR
;               ." ALL noncommercial and commercial uses are granted." CR
;               ." Please send comment, bug report and suggestions to:" CR
;               ."   wykoh@pado.krict.re.kr or 82-42-861-4245 (FAX)" CR ;

                $COLON  NameHI,HI
                DW      CR,DoLIT,SystemIDQStr,COUNT,ENVIRONMENTQuery,DROP,TYPEE
                DW      SPACE,DoLIT,'v',EMIT
                DW      DoLIT,VersionQStr,COUNT,ENVIRONMENTQuery,DROP,TYPEE
                DW      DoLIT,HiStr1,COUNT,TYPEE,CR
                DW      DoLIT,HiStr2,COUNT,TYPEE,CR
                DW      DoLIT,HiStr3,COUNT,TYPEE,CR
                DW      DoLIT,HiStr4,COUNT,TYPEE,CR,EXIT

;   COLD        ( -- )
;               The cold start sequence execution word.
;
;   : COLD      sp0 sp! rp0 rp!                 \ initialize stack
;               'init-i/o EXECUTE
;               'boot EXECUTE
;               QUIT ;                          \ start interpretation

                $COLON  NameCOLD,COLD
                DW      SPZero,SPStore,RPZero,RPStore
                DW      TickINIT_IO,EXECUTE,TickBoot,EXECUTE
                DW      QUIT

;   set-i/o ( -- )
;               Set input/output device.
;
;   : set-i/o   S" CON" stdin ;                 \ MS-DOS only

                $COLON  NameSet_IO,Set_IO
                DW      DoLIT,Set_IOstr         ;MS-DOS only
                DW      COUNT,STDIN             ;MS-DOS only
                DW      EXIT

; Following words are for MS-DOS only.
; File input using MS-DOS redirection function without using FILE words.

;   redirect    ( c-addr -- flag )
;               Redirect standard input from the device identified by ASCIIZ
;               string stored at c-addr. Return error code.

                $CODE   NameRedirect,Redirect
                MOV     DX,BX
                MOV     AX,CS:Redirect1stQ
                OR      AX,AX
                JZ      REDIRECT2
                MOV     AH,03Eh
                MOV     BX,CS:RedirHandle
                INT     021h            ; close previously opend file
REDIRECT2:      MOV     AX,03D00h               ; open file read-only
                MOV     CS:Redirect1stQ,AX      ; set Redirect1stQ true
                INT     021h
                JC      REDIRECT1       ; if error
                MOV     CS:RedirHandle,AX
                XOR     CX,CX
                MOV     BX,AX
                MOV     AX,04600H
                INT     021H
                JC      REDIRECT1
                XOR     AX,AX
REDIRECT1:      MOV     BX,AX
                $NEXT
Redirect1stQ    DW      0               ; true after the first redirection
RedirHandle     DW      ?               ; redirect file handle

;   asciiz      ( ca1 u -- ca2 )
;               Return ASCIIZ string.
;
;   : asciiz    HERE SWAP 2DUP + 0 SWAP C! CHARS MOVE HERE ;

                $COLON  NameASCIIZ,ASCIIZ
                DW      HERE,SWAP,TwoDUP,Plus,DoLIT,0
                DW      SWAP,CStore,CHARS,MOVE,HERE,EXIT

;   stdin       ( ca u -- )
;
;   : stdin     asciiz redirect ?DUP
;               IF -38 THROW THEN       \ non-existent file
;               ; COMPILE-ONLY

                $COLON  NameSTDIN,STDIN
                DW      ASCIIZ,Redirect,QuestionDUP,ZBranch,STDIN1
                DW      DoLIT,-38,THROW
STDIN1:         DW      EXIT

;   <<          ( "<spaces>ccc" -- )
;               Redirect input from the file 'ccc'. Should be used only in
;               interpretation state.
;
;   : <<        STATE @ IF ." Do not use '<<' in a definition." ABORT THEN
;               PARSE-WORD stdin SOURCE >IN !  DROP ; IMMEDIATE

                $COLON  NameFROM,FROM
                DW      DoLIT,AddrSTATE,Fetch,ZBranch,FROM1
                DW      CR
                DW      DoLIT,FROMstr
                DW      COUNT,TYPEE,ABORT
FROM1:          DW      PARSE_WORD,STDIN,SOURCE,DoLIT,AddrToIN,Store,DROP,EXIT

;***************
; Non-Standard words - Processor-dependent definitions
;       16 bit Forth for 8086/8
;***************

;   PAUSE       ( -- )
;               Stop current task and transfer control to the task of which
;               'status' USER variable is stored in 'follower' USER variable
;               of current task.
;
;   : PAUSE     rp@ DUP sp@ stackTop !  follower @ code@ >R ; COMPILE-ONLY
;
;                 $COLON  NamePAUSE,PAUSE
;                 DW      RPFetch,DUPP,SPFetch,StackTop,Store
;                 DW      Follower,Fetch,CodeFetch,ToR,EXIT

                $CODE   NamePAUSE,PAUSE
                PUSH    BX
                XCHG    BP,SP
                PUSH    SI
                XCHG    BP,SP
                PUSH    BP
                MOV     BX,WORD PTR AddrUserP
StackTopOffset = SysStackTop - SysUserP
                MOV     [BX+StackTopOffset],SP
FollowerOffset = SysFollower - SysUserP
                MOV     BX,[BX+FollowerOffset]
                MOV     SI,CS:[BX]
                $NEXT

;   wake        ( -- )
;               Wake current task.
;
;   : wake      R> CELL+ code@ userP !  \ userP points 'follower' of current task
;               stackTop @ sp! DROP     \ set data stack
;               rp! ; COMPILE-ONLY      \ set return stack
;
;                 $COLON  NameWake,Wake
;                 DW      RFrom,CELLPlus,CodeFetch,DoLIT,AddrUserP,Store
;                 DW      StackTop,Fetch,SPStore,DROP,RPStore,EXIT

                $CODE   NameWake,Wake
                MOV     BX,CS:[SI+CELLL]
                MOV     WORD PTR AddrUserP,BX
                MOV     SP,[BX+StackTopOffset]
                POP     BP
                XCHG    BP,SP
                POP     SI
                XCHG    BP,SP
                POP     BX
                $NEXT

;   same?       ( c-addr1 c-addr2 u -- -1|0|1 )
;               Return 0 if two strings, ca1 u and ca2 u, are same; -1 if
;               string, ca1 u is smaller than ca2 u; 1 otherwise. Used by
;               '(search-wordlist)'. Code definition is preferred to speed up
;               interpretation. Colon definition is shown below.
;
;   : same?     ?DUP IF         \ null strings are always same
;                  0 DO OVER C@ OVER C@ XOR
;                       IF UNLOOP C@ SWAP C@ > 2* 1+ EXIT THEN
;                       CHAR+ SWAP CHAR+ SWAP
;                  LOOP
;               THEN 2DROP 0 ;
;
;                 $COLON  NameSameQ,SameQ
;                 DW      QuestionDUP,ZBranch,SAMEQ4
;                 DW      DoLIT,0,DoDO
; SAMEQ3:         DW      OVER,CFetch,OVER,CFetch,XORR,ZBranch,SAMEQ2
;                 DW      UNLOOP,CFetch,SWAP,CFetch,GreaterThan
;                 DW      TwoStar,OnePlus,EXIT
; SAMEQ2:         DW      CHARPlus,SWAP,CHARPlus
;                 DW      DoLOOP,SAMEQ3
; SAMEQ4:         DW      TwoDROP,DoLIT,0,EXIT

                $CODE   NameSameQ,SameQ
                MOV     CX,BX
                MOV     AX,DS
                MOV     ES,AX
                MOV     DX,SI           ;save SI
                MOV     BX,-1
                POP     DI
                POP     SI
                OR      CX,CX
                JZ      SAMEQ5
                REPE CMPSB
                JL      SAMEQ1
                JZ      SAMEQ5
                INC     BX
SAMEQ5:         INC     BX
SAMEQ1:         MOV     SI,DX
                $NEXT

;   (search-wordlist)   ( c-addr u wid -- 0 | xt f 1 | xt f -1)
;               Search word list for a match with the given name.
;               Return execution token and not-compile-only flag and
;               -1 or 1 ( IMMEDIATE) if found. Return 0 if not found.
;
;   : (search-wordlist)
;               ROT >R SWAP DUP 0= IF -16 THROW THEN
;                               \ attempt to use zero-length string as a name
;               >R              \ wid  R: ca1 u
;               BEGIN @         \ ca2  R: ca1 u
;                  DUP 0= IF R> R> 2DROP EXIT THEN      \ not found
;                  DUP COUNT [ =MASK ] LITERAL AND R@ = \ ca2 ca2+char f
;                     IF   R> R@ SWAP DUP >R            \ ca2 ca2+char ca1 u
;                          same?                        \ ca2 flag
;                   \ ELSE DROP -1      \ unnecessary since ca2+char is not 0.
;                     THEN
;               WHILE cell-             \ pointer to next word in wordlist
;               REPEAT
;               R> R> 2DROP DUP name>xt SWAP            \ xt ca2
;               C@ 2DUP [ =seman ] LITERAL AND 0= 0=    \ xt char xt f
;               AND TO specialComp?
;               DUP [ =compo ] LITERAL AND 0= SWAP
;               [ =immed ] LITERAL AND 0= 2* 1+ ;
;
;                 $COLON  NameParenSearch_Wordlist,ParenSearch_Wordlist
;                 DW      ROT,ToR,SWAP,DUPP,ZBranch,PSRCH6
;                 DW      ToR
; PSRCH1:         DW      Fetch
;                 DW      DUPP,ZBranch,PSRCH9
;                 DW      DUPP,COUNT,DoLIT,MASKK,ANDD,RFetch,Equals
;                 DW      ZBranch,PSRCH5
;                 DW      RFrom,RFetch,SWAP,DUPP,ToR,SameQ
; PSRCH5:         DW      ZBranch,PSRCH3
;                 DW      CellMinus,Branch,PSRCH1
; PSRCH3:         DW      RFrom,RFrom,TwoDROP,DUPP,NameToXT,SWAP
;                 DW      CFetch,TwoDUP,DoLIT,SEMAN,ANDD,ZeroEquals,ZeroEquals
;                 DW      ANDD,DoTO,AddrSpecialCompQ
;                 DW      DUPP,DoLIT,COMPO,ANDD,ZeroEquals,SWAP
;                 DW      DoLIT,IMMED,ANDD,ZeroEquals,TwoStar,OnePlus,EXIT
; PSRCH9:         DW      RFrom,RFrom,TwoDROP,EXIT
; PSRCH6:         DW      DoLIT,-16,THROW

                $CODE   NameParenSearch_Wordlist,ParenSearch_Wordlist
                POP     AX      ;u
                POP     DX      ;c-addr
                OR      AX,AX
                JNZ     PSRCH1
                MOV     BX,-16  ;attempt to use zero-length string as a name
                JMP     THROW
PSRCH1:         PUSH    SI
                MOV     CX,DS
                MOV     ES,CX
                SUB     CX,CX
PSRCH2:         MOV     BX,[BX]
                OR      BX,BX
                JZ      PSRCH4          ; end of wordlist?
                MOV     CL,[BX]
                SUB     BX,CELLL        ;pointer to nextword
                AND     CL,MASKK        ;max name length = MASKK
                CMP     CL,AL
                JNZ     PSRCH2
                MOV     SI,DX
                MOV     DI,BX
                ADD     DI,CELLL+CHARR
                REPE CMPSB
                JNZ     PSRCH2
                POP     SI
                PUSH    [BX-CELLL]      ;xt
                MOV     AL,0FFh
                MOV     CL,[BX+CELLL]
                AND     AL,CL           ;test SEMAN = 080h
                CBW
                CWD
                AND     DX,[BX-CELLL]
                MOV     AddrSpecialCompQ,DX
                XOR     DX,DX
                TEST    CL,COMPO
                JNZ     PSRCH5
                DEC     DX
PSRCH5:         PUSH    DX
                TEST    CL,IMMED
                MOV     BX,-1
                JZ      PSRCH3
                NEG     BX
PSRCH3:         $NEXT
PSRCH4:         POP     SI
                $NEXT

;   ?call       ( xt1 -- xt1 0 | code-addr xt2 )
;               Return xt of the CALLed run-time word if xt starts with machine
;               CALL instruction and leaves the next cell address after the
;               CALL instruction. Otherwise leaves the original xt1 and zero.
;
;   : ?call     DUP code@ call-code =
;               IF   CELL+ DUP code@ SWAP CELL+ DUP ROT + EXIT THEN
;                       \ Direct Threaded Code 8086 relative call
;               0 ;
;
;                 $COLON  NameQCall,QCall
;                 DW      DUPP,CodeFetch,DoLIT,CALLL,Equals,ZBranch,QCALL1
;                 DW      CELLPlus,DUPP,CodeFetch,SWAP,CELLPlus,DUPP,ROT,Plus
;                 DW      EXIT
; QCALL1:         DW      DoLIT,0,EXIT

                $CODE   NameQCall,QCall
                MOV     AX,CS:[BX]
                CMP     AX,CALLL
                JE      QCALL1
                PUSH    BX
                XOR     BX,BX
                $NEXT
QCALL1:         ADD     BX,2*CELLL
                PUSH    BX
                ADD     BX,CS:[BX-CELLL]
                $NEXT

;   xt,         ( xt1 -- xt2 )
;               Take a run-time word xt1 for :NONAME , CONSTANT , VARIABLE and
;               CREATE . Return xt2 of current definition.
;
;   : xt,       xhere ALIGNED DUP TO xhere SWAP
;               call-code code,         \ Direct Threaded Code
;               xhere CELL+ - code, ;   \ 8086 relative call

                $COLON  NamextComma,xtComma
                DW      XHere,ALIGNED,DUPP,DoTO,AddrXHere,SWAP
                DW      DoLIT,CALLL,CodeComma
                DW      XHere,CELLPlus,Minus,CodeComma,EXIT

;                 $CODE   NamextComma,xtComma
;                 MOV     AX,AddrXHere
;                 XCHG    BX,AX
;                 INC     BX
;                 AND     BX,0FFFEh
;                 MOV     CS:[BX],CALLL
;                 MOV     CX,BX
;                 ADD     CX,2*CELLL
;                 MOV     AddrXHere,CX
;                 SUB     AX,CX
;                 MOV     CS:[BX+CELLL],AX
;                 $NEXT

;   doLIT       ( -- x )
;               Push an inline literal.

                $CODE   NameDoLIT,DoLIT
                PUSH    BX
                LODS    CS:CSDummy
                MOV     BX,AX
                $NEXT

;   doCONST     ( -- x )
;               Run-time routine of CONSTANT and initializable system VARIABLE.

                $CODE   NameDoCONST,DoCONST
                POP     DI
                PUSH    BX
                MOV     BX,CS:[DI]
                $NEXT

;   doCREATE    ( -- a-addr )
;               Run-time routine of CREATE. Return address of data space.
;               Structure of CREATEd word:
;               |compile_xt|name_ptr| call-doCREATE | 0 or DOES>_xt | a-addr |
;
;   : doCREATE    SWAP            \ switch BX and top of 8086 stack item
;                 DUP CELL+ code@ SWAP code@ ?DUP IF EXECUTE THEN
;                 ; COMPILE-ONLY
;
;                 $COLON  NameDoCREATE,DoCREATE
;                 DW      SWAP,CELLPlus,DUPP,CodeFetch,SWAP,CodeFetch
;                 DW      QuestionDUP,ZBranch,DOCREAT1
;                 DW      EXECUTE
; DOCREAT1:       DW      EXIT

                $CODE   NameDoCREATE,DoCREATE
                MOV     DI,SP
                XCHG    BX,[DI]
                MOV     AX,CS:[BX]
                MOV     BX,CS:[BX+CELLL]
                OR      AX,AX
                JNZ     DOCREAT1
                $NEXT
DOCREAT1:       JMP     AX
                $ALIGN

;   doVALUE     ( -- x )
;               Run-time routine of VALUE. Return the value of VALUE word.

                $CODE   NameDoVALUE,DoVALUE
                MOV     DI,SP
                XCHG    BX,[DI]
                MOV     BX,CS:[BX]
                MOV     BX,[BX]
                $NEXT

;   doTO        ( x -- )
;               Run-time routine of TO. Store x at the address in the
;               following cell.

                $CODE   NameDoTO,DoTO
                LODS    CS:CSDummy
                XCHG    BX,AX
                MOV     [BX],AX
                POP     BX
                $NEXT

;   doUSER      ( -- a-addr )
;               Run-time routine of USER. Return address of data space.

                $CODE   NameDoUSER,DoUSER
                POP     DI
                PUSH    BX
                MOV     BX,CS:[DI]
                ADD     BX,AddrUserP
                $NEXT

;   doLIST      ( -- ) ( R: -- nest-sys )
;               Process colon list.

                $CODE   NameDoLIST,DoLIST
                XCHG    BP,SP                   ;exchange pointers
                PUSH    SI                      ;push return stack
                XCHG    BP,SP                   ;restore the pointers
                POP     SI                      ;new list address
                $NEXT

;   doLOOP      ( -- ) ( R: loop-sys1 -- | loop-sys2 )
;               Run time routine for LOOP.

                $CODE   NameDoLOOP,DoLOOP
                INC     WORD PTR [BP]           ;increase loop count
                JO      DoLOOP1                 ;?loop end
                MOV     SI,CS:[SI]              ;no, go back
                $NEXT
DoLOOP1:        ADD     SI,CELLL                ;yes, continue past the branch offset
                ADD     BP,2*CELLL              ;clear return stack
                $NEXT

;   do+LOOP     ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;               Run time routine for +LOOP.

                $CODE   NameDoPLOOP,DoPLOOP
                ADD     WORD PTR [BP],BX        ;increase loop count
                JO      DoPLOOP1                ;?loop end
                MOV     SI,CS:[SI]              ;no, go back
                POP     BX
                $NEXT
DoPLOOP1:       ADD     SI,CELLL                ;yes, continue past the branch offset
                ADD     BP,2*CELLL              ;clear return stack
                POP     BX
                $NEXT

;   0branch     ( flag -- )
;               Branch if flag is zero.

                $CODE   NameZBranch,ZBranch
                OR      BX,BX                   ;?flag=0
                JZ      ZBRAN1                  ;yes, so branch
                ADD     SI,CELLL                ;point IP to next cell
                POP     BX
                $NEXT
ZBRAN1:         MOV     SI,CS:[SI]              ;IP:=(IP)
                POP     BX
                $NEXT

;   branch      ( -- )
;               Branch to an inline address.

                $CODE   NameBranch,Branch
                MOV     SI,CS:[SI]              ;IP:=(IP)
                $NEXT

;   rp@         ( -- a-addr )
;               Push the current RP to the data stack.

                $CODE   NameRPFetch,RPFetch
                PUSH    BX
                MOV     BX,BP
                $NEXT

;   rp!         ( a-addr -- )
;               Set the return stack pointer.

                $CODE   NameRPStore,RPStore
                MOV     BP,BX
                POP     BX
                $NEXT

;   sp@         ( -- a-addr )
;               Push the current data stack pointer.

                $CODE   NameSPFetch,SPFetch
                PUSH    BX
                MOV     BX,SP
                $NEXT

;   sp!         ( a-addr -- )
;               Set the data stack pointer.

                $CODE   NameSPStore,SPStore
                MOV     SP,BX
                POP     BX
                $NEXT

;   um+         ( u1 u2 -- u3 1|0 )
;               Add two unsigned numbers, return the sum and carry.

                $CODE   NameUMPlus,UMPlus
                XOR     CX,CX
                POP     AX
                ADD     BX,AX
                PUSH    BX                      ;push sum
                RCL     CX,1                    ;get carry
                MOV     BX,CX
                $NEXT

;   code!       ( x code-addr -- )
;               Store x at a code space address.

                $CODE   NameCodeStore,CodeStore
                POP     CS:[BX]
                POP     BX
                $NEXT

;   codeB!      ( b code-addr -- )
;               Store byte at a code space address.

                $CODE   NameCodeBStore,CodeBStore
                POP     AX
                MOV     CS:[BX],AL
                POP     BX
                $NEXT

;   code@       ( code-addr -- x )
;               Push the contents at code space addr to the data stack.

                $CODE   NameCodeFetch,CodeFetch
                MOV     BX,CS:[BX]
                $NEXT

;   codeB@      ( code-addr -- b )
;               Push the contents at code space byte addr to the data stack.

                $CODE   NameCodeBFetch,CodeBFetch
                MOV     BL,CS:[BX]
                XOR     BH,BH
                $NEXT

;   code,       ( x -- )
;               Reserve one cell in code space and store x in it.
;
;   : code,     xhere DUP CELL+ TO xhere code! ; COMPILE-ONLY
;
;                 $COLON  NameCodeComma,CodeComma
;                 DW      XHere,DUPP,CELLPlus,DoTO,AddrXHere,CodeStore,EXIT

                $CODE   NameCodeComma,CodeComma
                MOV     DI,AddrXHere
                MOV     CS:[DI],BX
                ADD     DI,CELLL
                POP     BX
                MOV     AddrXHere,DI
                $NEXT

;***************
; Standard words - Processor-dependent definitions
;       16 bit Forth for 8086/8
;***************

;   ALIGN       ( -- )                          \ CORE
;               Align the data space pointer.
;
;   : ALIGN     HERE ALIGNED TO HERE ;

                $COLON  NameALIGNN,ALIGNN
                DW      HERE,ALIGNED,DoTO,AddrHERE,EXIT

;   ALIGNED     ( addr -- a-addr )              \ CORE
;               Align address to the cell boundary.
;
;   : ALIGNED   DUP 0 cell-size UM/MOD DROP DUP
;               IF cell-size SWAP - THEN + ;
;
;                 $COLON  NameALIGNED,ALIGNED
;                 DW      DUPP,DoLIT,0,DoLIT,CELLL
;                 DW      UMSlashMOD,DROP,DUPP
;                 DW      ZBranch,ALGN1
;                 DW      DoLIT,CELLL,SWAP,Minus
; ALGN1:          DW      Plus,EXIT

                $CODE   NameALIGNED,ALIGNED
                INC     BX
                AND     BX,0FFFEh
                $NEXT

;   CELLS       ( n1 -- n2 )                    \ CORE
;               Calculate number of address units for n1 cells.
;
;   : CELLS     cell-size * ;   \ slow, very portable
;   : CELLS     2* ;            \ fast, must be redefined for each system
;
;                 $COLON  NameCELLS,CELLS
;                 DW      TwoStar,EXIT

                $CODE   NameCELLS,CELLS
                SHL     BX,1
                $NEXT

;   CHARS       ( n1 -- n2 )                    \ CORE
;               Calculate number of address units for n1 characters.
;
;   : CHARS     char-size * ;   \ slow, very portable
;   : CHARS     ;               \ fast, must be redefined for each system

               $COLON  NameCHARS,CHARS
               DW      EXIT

;   1chars/     ( n1 -- n2 )
;               Calculate number of chars for n1 address units.
;
;   : 1chars/   1 CHARS / ;     \ slow, very portable
;   : 1chars/   ;               \ fast, must be redefined for each system

                $COLON  NameOneCharsSlash,OneCharsSlash
                DW      EXIT

;   !           ( x a-addr -- )                 \ CORE
;               Store x at a aligned address.

                $CODE   NameStore,Store
                POP     [BX]
                POP     BX
                $NEXT

;   0<          ( n -- flag )                   \ CORE
;               Return true if n is negative.

                $CODE   NameZeroLess,ZeroLess
                MOV     AX,BX
                CWD             ;sign extend
                MOV     BX,DX
                $NEXT

;   0=          ( x -- flag )                   \ CORE
;               Return true if x is zero.

                $CODE   NameZeroEquals,ZeroEquals
                OR      BX,BX
                MOV     BX,-1
                JZ      ZEQUAL1
                INC     BX
ZEQUAL1:        $NEXT

;   2*          ( x1 -- x2 )                    \ CORE
;               Bit-shift left, filling the least significant bit with 0.

                $CODE   NameTwoStar,TwoStar
                SHL     BX,1
                $NEXT

;   2/          ( x1 -- x2 )                    \ CORE
;               Bit-shift right, leaving the most significant bit unchanged.

                $CODE   NameTwoSlash,TwoSlash
                SAR     BX,1
                $NEXT

;   >R          ( x -- ) ( R: -- x )            \ CORE
;               Move top of the data stack item to the return stack.

                $CODE   NameToR,ToR
                SUB     BP,CELLL                ;adjust RP
                MOV     [BP],BX
                POP     BX
                $NEXT

;   @           ( a-addr -- x )                 \ CORE
;               Push the contents at a-addr to the data stack.

                $CODE   NameFetch,Fetch
                MOV     BX,[BX]
                $NEXT

;   AND         ( x1 x2 -- x3 )                 \ CORE
;               Bitwise AND.

                $CODE   NameANDD,ANDD
                POP     AX
                AND     BX,AX
                $NEXT

;   C!          ( char c-addr -- )              \ CORE
;               Store char at c-addr.

                $CODE   NameCStore,CStore
                POP     AX
                MOV     [BX],AL
                POP     BX
                $NEXT

;   C@          ( c-addr -- char )              \ CORE
;               Fetch the character stored at c-addr.

                $CODE   NameCFetch,CFetch
                MOV     BL,[BX]
                XOR     BH,BH
                $NEXT

;   DROP        ( x -- )                        \ CORE
;               Discard top stack item.

                $CODE   NameDROP,DROP
                POP     BX
                $NEXT

;   DUP         ( x -- x x )                    \ CORE
;               Duplicate the top stack item.

                $CODE   NameDUPP,DUPP
                PUSH    BX
                $NEXT

;   EXECUTE     ( i*x xt -- j*x )               \ CORE
;               Perform the semantics indentified by execution token, xt.

                $CODE   NameEXECUTE,EXECUTE
                MOV     AX,BX
                POP     BX
                JMP     AX                      ;jump to the code address
                $ALIGN

;   EXIT        ( -- ) ( R: nest-sys -- )       \ CORE
;               Return control to the calling definition.

                $CODE   NameEXIT,EXIT
                XCHG    BP,SP                   ;exchange pointers
                POP     SI                      ;pop return stack
                XCHG    BP,SP                   ;restore the pointers
                $NEXT

;   MOVE        ( addr1 addr2 u -- )            \ CORE
;               Copy u address units from addr1 to addr2 if u is greater
;               than zero. This word is CODE defined since no other Standard
;               words can handle address unit directly.

                $CODE   NameMOVE,MOVE
                POP     DI
                POP     DX
                OR      BX,BX
                JZ      MOVE2
                MOV     CX,BX
                XCHG    DX,SI                   ;save SI
                MOV     AX,DS
                MOV     ES,AX                   ;set ES same as DS
                CMP     SI,DI
                JC      MOVE1
                REP MOVSB
                MOV     SI,DX
MOVE2:          POP     BX
                $NEXT
MOVE1:          STD
                ADD     DI,CX
                DEC     DI
                ADD     SI,CX
                DEC     SI
                REP MOVSB
                CLD
                MOV     SI,DX
                POP     BX
                $NEXT

;   OR          ( x1 x2 -- x3 )                 \ CORE
;               Return bitwise inclusive-or of x1 with x2.

                $CODE   NameORR,ORR
                POP     AX
                OR      BX,AX
                $NEXT

;   OVER        ( x1 x2 -- x1 x2 x1 )           \ CORE
;               Copy second stack item to top of the stack.

                $CODE   NameOVER,OVER
                MOV     DI,SP
                PUSH    BX
                MOV     BX,[DI]
                $NEXT

;   R>          ( -- x ) ( R: x -- )            \ CORE
;               Move x from the return stack to the data stack.

                $CODE   NameRFrom,RFrom
                PUSH    BX
                MOV     BX,[BP]
                ADD     BP,CELLL                ;adjust RP
                $NEXT

;   R@          ( -- x ) ( R: x -- x )          \ CORE
;               Copy top of return stack to the data stack.

                $CODE   NameRFetch,RFetch
                PUSH    BX
                MOV     BX,[BP]
                $NEXT

;   SWAP        ( x1 x2 -- x2 x1 )              \ CORE
;               Exchange top two stack items.

                $CODE   NameSWAP,SWAP
                MOV     DI,SP
                XCHG    BX,[DI]
                $NEXT

;   XOR         ( x1 x2 -- x3 )                 \ CORE
;               Bitwise exclusive OR.

                $CODE   NameXORR,XORR
                POP     AX
                XOR     BX,AX
                $NEXT

;***************
; System constants and variables
;***************

;   #order0     ( -- a-addr )
;               Start address of default search order.

                $CONST  NameNumberOrder0,NumberOrder0,AddrNumberOrder0

;   'ekey?      ( -- a-addr )
;               Execution vector of EKEY?.

                $VALUE  NameTickEKEYQ,TickEKEYQ,AddrTickEKEYQ

;   'ekey       ( -- a-addr )
;               Execution vector of EKEY.

                $VALUE  NameTickEKEY,TickEKEY,AddrTickEKEY

;   'emit?      ( -- a-addr )
;               Execution vector of EMIT?.

                $VALUE  NameTickEMITQ,TickEMITQ,AddrTickEMITQ

;   'emit       ( -- a-addr )
;               Execution vector of EMIT.

                $VALUE  NameTickEMIT,TickEMIT,AddrTickEMIT

;   'init-i/o   ( -- a-addr )
;               Execution vector to initialize input/output devices.

                $VALUE  NameTickINIT_IO,TickINIT_IO,AddrTickINIT_IO

;   'prompt     ( -- a-addr )
;               Execution vector of '.prompt'.

                $VALUE  NameTickPrompt,TickPrompt,AddrTickPrompt

;   'boot       ( -- a-addr )
;               Execution vector of COLD.

                $VALUE  NameTickBoot,TickBoot,AddrTickBoot

;   SOURCE-ID   ( -- 0 | -1 )                   \ CORE EXT
;               Identify the input source. -1 for string (via EVALUATE) and
;               0 for user input device.

                $VALUE  NameSOURCE_ID,SOURCE_ID,AddrSOURCE_ID

;   HERE        ( -- addr )                     \ CORE
;               Return data space pointer.

                $VALUE  NameHERE,HERE,AddrHERE

;   xhere       ( -- code-addr )
;               Return next available code space address.

                $VALUE  NameXHere,XHere,AddrXHere

;   'doWord     ( -- a-addr )
;               Execution vectors for 'interpret'.

                $CONST  NameTickDoWord,TickDoWord,AddrTickDoWord

;   BASE        ( -- a-addr )                   \ CORE
;               Return the address of the radix base for numeric I/O.

                $CONST  NameBASE,BASE,AddrBASE

;   THROWMsgTbl ( -- a-addr )                   \ CORE
;               Return the address of the THROW message table.

                $CONST  NameTHROWMsgTbl,THROWMsgTbl,AddrTHROWMsgTbl

;   memTop      ( -- a-addr )
;               Top of free memory.

                $VALUE  NameMemTop,MemTop,AddrMemTop

;   lastXT      ( -- xt )
;               Return xt of the last definition.

                $VALUE  NameLastXT,LastXT,AddrLastXT

;   rakeVar     ( -- a-addr )
;               Used by 'rake' to gather LEAVE.

                $CONST  NameRakeVar,RakeVar,AddrRakeVar

;   #order      ( -- a-addr )
;               Hold the search order stack depth.

                $CONST  NameNumberOrder,NumberOrder,AddrNumberOrder

;   current     ( -- a-addr )
;               Point to the wordlist to be extended.

                $CONST  NameCurrent,Current,AddrCurrent

;   FORTH-WORDLIST   ( -- wid )                 \ SEARCH
;               Return wid of Forth wordlist.

                $CONST  NameFORTH_WORDLIST,FORTH_WORDLIST,AddrFORTH_WORDLIST

;   NONSTANDARD-WORDLIST   ( -- wid )
;               Return wid of non-standard wordlist.

                $CONST  NameNONSTANDARD_WORDLIST,NONSTANDARD_WORDLIST,AddrNONSTANDARD_WORDLIST

;   envQList    ( -- wid )
;               Return wid of ENVIRONMENT? string list. Never put this wid in
;               search-order. It should be used only by SET-CURRET to add new
;               environment query string after addition of a complete wordset.

                $CONST  NameEnvQList,EnvQList,AddrEnvQList

;   userP       ( -- a-addr )
;               Return address of USER variable area of current task.

                $CONST  NameUserP,UserP,AddrUserP

;   SystemTask  ( -- a-addr )
;               Return system task's tid.

                $CONST  NameSystemTask,SystemTask,SysTask

;   follower    ( -- a-addr )
;               Point next task's 'status' USER variable.

                $USER   NameFollower,Follower,SysFollower-SysUserP

;   status      ( -- a-addr )
;               Status of current task. Point 'pass' or 'wake'.

                $USER   NameStatus,Status,SysStatus-SysUserP

;   stackTop    ( -- a-addr )
;               Store current task's top of stack position.

                $USER   NameStackTop,StackTop,SysStackTop-SysUserP

;   throwFrame  ( -- a-addr )
;               THROW frame for CATCH and THROW need to be saved for eack task.

                $USER   NameThrowFrame,ThrowFrame,SysThrowFrame-SysUserP

;   taskName    ( -- a-addr )
;               Current task's task ID.

                $USER   NameTaskName,TaskName,SysTaskName-SysUserP

;   user1       ( -- a-addr )
;               One free USER variable for each task.

                $USER   NameUser1,User1,SysUser1-SysUserP

; ENVIRONMETN? strings can be searched using SEARCH-WORDLIST and can be
; EXECUTEd. This wordlist is completely hidden to Forth system except
; ENVIRONMENT? .

SystemID:
                NOP
                CALL    DoLIST
                DW      DoLIT,SystemIDStr,COUNT,EXIT

Version:
                NOP
                CALL    DoLIST
                DW      DoLIT,VersionStr,COUNT,EXIT

SlashCOUNTED_STRING:
                NOP
                CALL    DoCONST
                DW      MaxChar

SlashHOLD:
                NOP
                CALL    DoCONST
                DW      PADSize

SlashPAD:
                NOP
                CALL    DoCONST
                DW      PADSize

ADDRESS_UNIT_BITS:
                NOP
                CALL    DoCONST
                DW      8

CORE:
                NOP
                CALL    DoCONST
                DW      -1

FLOORED:
                NOP
                CALL    DoCONST
                DW      -1              ;true

MAX_CHAR:
                NOP
                CALL    DoCONST
                DW      MaxChar         ;max value of character set

MAX_D:
                NOP
                CALL    DoLIST
                DW      DoLIT,MaxUnsigned,DoLIT,MaxSigned,EXIT

MAX_N:
                NOP
                CALL    DoCONST
                DW      MaxSigned

MAX_U:
                NOP
                CALL    DoCONST
                DW      MaxUnsigned

MAX_UD:
                NOP
                CALL    DoLIST
                DW      DoLIT,MAX_U,DoLIT,MAX_U,EXIT

RETURN_STACK_CELLS:
                NOP
                CALL    DoCONST
                DW      RTCells

STACK_CELLS:
                NOP
                CALL    DoCONST
                DW      DTCells

EXCEPTION:
                NOP
                CALL    DoCONST
                DW      -1

EXCEPTION_EXT:
                NOP
                CALL    DoCONST
                DW      -1

WORDLISTS:
                NOP
                CALL    DoCONST
                DW      OrderDepth

;***************
; Non-Standard words - Colon definitions
;***************

;   (')         ( "<spaces>name" -- xt 1 | xt -1 )
;               Parse a name, find it and return execution token and
;               -1 or 1 ( IMMEDIATE) if found
;
;   : (')       PARSE-WORD search ?DUP IF NIP EXIT THEN
;               errWord 2!      \ if not found error
;               -13 THROW ;     \ undefined word

                $COLON  NameParenTick,ParenTick
                DW      PARSE_WORD,Search,QuestionDUP,ZBranch,PTICK1
                DW      NIP,EXIT
PTICK1:         DW      DoLIT,AddrErrWord,TwoStore,DoLIT,-13,THROW

;   (d.)        ( d -- c-addr u )
;               Convert a double number to a string.
;
;   : (d.)      SWAP OVER  DUP 0< IF  DNEGATE  THEN
;               <#  #S ROT SIGN  #> ;

                $COLON  NameParenDDot,ParenDDot
                DW      SWAP,OVER,DUPP,ZeroLess,ZBranch,PARDD1
                DW      DNEGATE
PARDD1:         DW      LessNumberSign,NumberSignS,ROT
                DW      SIGN,NumberSignGreater,EXIT

;   .ok         ( -- )
;               Display 'ok'.
;
;   : .ok       ." ok" ;

                $COLON  NameDotOK,DotOK
                DW      DoLIT,DotOKStr
                DW      COUNT,TYPEE,EXIT

;   .prompt         ( -- )
;               Disply Forth prompt. This word is vectored.
;
;   : .prompt   'prompt EXECUTE ;

                $COLON  NameDotOK,DotPrompt
                DW      TickPrompt,EXECUTE,EXIT

;   0           ( -- 0 )
;               Return zero.

                $CONST  NameZero,Zero,0

;   1           ( -- 1 )
;               Return one.

                $CONST  NameOne,One,1

;   -1          ( -- -1 )
;               Return -1.

                $CONST  NameMinusOne,MinusOne,-1

;   abort"msg   ( -- a-addr )
;               Abort" error message string address.

                $CONST  NameAbortQMsg,AbortQMsg,AddrAbortQMsg

;   bal         ( -- a-addr )
;               Check for match of contol structure. bal must be zero at the
;               end of colon definition. Otherwise -22 THROW . From least
;               significant bit, 4 bits are reserved for 'orig'; 4 bits for
;               'dest'; 4 bits for 'do-sys'. Most significant 4 bits are free
;               which may be used for 'case-sys'.

                $CONST  NameBalance,Balance,AddrBalance

;   orig+       ( -- )
;               Increase bal by 1.
;
;   : orig+     1 bal +! ;
;
;                 $COLON  NameOrigPlus,OrigPlus
;                 DW      DoLIT,1,DoLIT,AddrBalance,PlusStore,EXIT

                $CODE   NameOrigPlus,OrigPlus
                INC     AddrBalance
                $NEXT

;   orig-       ( -- )
;               Decrease bal by 1.
;
;   : orig-     -1 bal +! ;
;
;                 $COLON  NameOrigMinus,OrigMinus
;                 DW      DoLIT,-1,DoLIT,AddrBalance,PlusStore,EXIT

                $CODE   NameOrigMinus,OrigMinus
                DEC     AddrBalance
                $NEXT

;   dest+       ( -- )
;               Increase bal by 16.
;
;   : dest+     16 bal +! ;
;
;                 $COLON  NameDestPlus,DestPlus
;                 DW      DoLIT,16,DoLIT,AddrBalance,PlusStore,EXIT
;
                $CODE   NameDestPlus,DestPlus
                MOV     AX,0010h
                ADD     AddrBalance,AX
                $NEXT

;   dest-       ( -- )
;               Decrease bal by 16.
;
;   : dest-     -16 bal +! ;
;
;                 $COLON  NameDestMinus,DestMinus
;                 DW      DoLIT,-16,DoLIT,AddrBalance,PlusStore,EXIT
;
                $CODE   NameDestMinus,DestMinus
                MOV     AX,0010h
                SUB     AddrBalance,AX
                $NEXT

;   dosys+       ( -- )
;               Increase bal by 256.
;
;   : dosys+    256 bal +! ;
;
;                 $COLON  NameDoSysPlus,DoSysPlus
;                 DW      DoLIT,256,DoLIT,AddrBalance,PlusStore,EXIT
;
                $CODE   NameDoSysPlus,DoSysPlus
                MOV     AX,0100h
                ADD     AddrBalance,AX
                $NEXT

;   dosys-       ( -- )
;               Decrease bal by 256.
;
;   : dosys-    -256 bal +! ;
;
;                 $COLON  NameDoSysMinus,DoSysMinus
;                 DW      DoLIT,-256,DoLIT,AddrBalance,PlusStore,EXIT
;
                $CODE   NameDoSysMinus,DoSysMinus
                MOV     AX,0100h
                SUB     AddrBalance,AX
                $NEXT

;   cell-       ( a-addr1 -- a-addr2 )
;               Return previous aligned cell address.
;
;   : cell-     [ cell-size NEGATE ] LITERAL + ;
;
;                $COLON  NameCellMinus,CellMinus
;                DW      DoLIT,0-CELLL,Plus,EXIT

                $CODE   NameCellMinus,CellMinus
                SUB     BX,CELLL
                $NEXT

;   COMPILE-ONLY   ( -- )
;               Make the most recent definition an compile-only word.
;
;   : COMPILE-ONLY   lastName [ =compo ] LITERAL OVER @ OR SWAP ! ;

                $COLON  NameCOMPILE_ONLY,COMPILE_ONLY
                DW      LastName,DoLIT,COMPO,OVER,Fetch,ORR,SWAP,Store,EXIT

;   doDO        ( n1 n2 -- ) ( R: -- n1 n2-n1-max_negative )
;               Run-time funtion of DO.
;
;   : doDO      >R max-negative + R> OVER - SWAP R> SWAP >R SWAP >R >R ;
;
;                 $COLON  NameDoDO,DoDO
;                 DW      ToR,DoLIT,MAXNegative,Plus,RFrom
;                 DW      OVER,Minus,SWAP,RFrom,SWAP,ToR,SWAP,ToR,ToR,EXIT

                $CODE   NameDoDO,DoDO
                SUB     BP,2*CELLL
                POP     AX
                ADD     AX,8000h
                MOV     [BP+CELLL],AX
                SUB     BX,AX
                MOV     [BP],BX
                POP     BX
                $NEXT

;   errWord     ( -- a-addr )
;               Last found word. To be used to display the word causing error.

                $CONST  NameErrWord,ErrWord,AddrErrWord

;   head,       ( xt "<spaces>name" -- )
;               Parse a word and build a dictionary entry.
;
;   : head,     TO lastXT
;               PARSE-WORD DUP 0=
;               IF errWord 2! -16 THROW THEN
;                               \ attempt to use zero-length string as a name
;               DUP =mask > IF -19 THROW THEN   \ definition name too long
;               2DUP search NIP NIP             \ name exist?
;               IF ." redefine " 2DUP TYPE THEN \ warn if redefined
;               ALIGN lastXT ,                  \ align and store xt
;               GET-CURRENT @ ,                 \ build wordlist link
;               HERE DUP >R pack" ALIGNED TO HERE \ pack the name in name space
;               R> DUP lastXT cell- code!       \ store name addr in code space
;               TO lastName ;

                $COLON  NameHeadComma,HeadComma
                DW      DoTO,AddrLastXT
                DW      PARSE_WORD,DUPP,ZBranch,HEADC1
                DW      DUPP,DoLIT,MASKK,GreaterThan,ZBranch,HEADC3
                DW      DoLIT,-19,THROW
HEADC3:         DW      TwoDUP,Search,NIP,NIP,ZBranch,HEADC2
                DW      DoLIT,HEADCstr
                DW      COUNT,TYPEE,TwoDUP,TYPEE
HEADC2:         DW      ALIGNN,LastXT,Comma
                DW      GET_CURRENT,Fetch,Comma
                DW      HERE,DUPP,ToR,PackQuote,ALIGNED,DoTO,AddrHERE
                DW      RFrom,DUPP,LastXT,CellMinus,CodeStore
                DW      DoTO,AddrLastName,EXIT
HEADC1:         DW      DoLIT,AddrErrWord,TwoStore,DoLIT,-16,THROW

;   hld         ( -- a-addr )
;               Hold a pointer in building a numeric output string.

                $CONST  NameHLD,HLD,AddrHLD

;   interpret   ( i*x -- j*x )
;               Intrepret input string.
;
;   : interpret BEGIN  DEPTH 0< IF -4 THROW THEN        \ stack underflow
;                      PARSE-WORD DUP
;               WHILE  2DUP errWord 2!
;                      search               \ ca u 0 | xt f -1 | xt f 1
;                      DUP IF
;                        SWAP STATE @ OR 0= \ compile-only in interpretation
;                        IF -14 THROW THEN  \ interpreting a compile-only word
;                      THEN
;                      1+ 2* STATE @ 1+ + CELLS 'doWord + @ EXECUTE
;               REPEAT 2DROP ;

                $COLON  NameInterpret,Interpret
INTERP1:        DW      DEPTH,ZeroLess,ZBranch,INTERP2
                DW      DoLIT,-4,THROW
INTERP2:        DW      PARSE_WORD,DUPP,ZBranch,INTERP3
                DW      TwoDUP,DoLIT,AddrErrWord,TwoStore
                DW      Search,DUPP,ZBranch,INTERP5
                DW      SWAP,DoLIT,AddrSTATE,Fetch,ORR,ZBranch,INTERP4
INTERP5:        DW      OnePlus,TwoStar,DoLIT,AddrSTATE,Fetch,OnePlus,Plus,CELLS
                DW      DoLIT,AddrTickDoWord,Plus,Fetch,EXECUTE
                DW      Branch,INTERP1
INTERP3:        DW      TwoDROP,EXIT
INTERP4:        DW      DoLIT,-14,THROW

;   optiCOMPILE, ( xt -- )
;               Optimized COMPILE, . Reduce doLIST ... EXIT sequence if
;               xt is COLON definition which contains less than two words.
;
;   : optiCOMPILE,
;               DUP ?call ['] doLIST = IF
;                   DUP code@ ['] EXIT = IF         \ if first word is EXIT
;                     2DROP EXIT THEN
;                   DUP CELL+ code@ ['] EXIT = IF   \ if second word is EXIT
;                     code@ DUP ['] doLIT XOR   \ make sure it is not literal
;                     IF SWAP THEN THEN
;               THEN THEN DROP COMPILE, ;
;
;                 $COLON  NameOptiCOMPILEComma,OptiCOMPILEComma
;                 DW      DUPP,QCall,DoLIT,DoLIST,Equals,ZBranch,OPTC2
;                 DW      DUPP,CodeFetch,DoLIT,EXIT,Equals,ZBranch,OPTC1
;                 DW      TwoDROP,EXIT
; OPTC1:          DW      DUPP,CELLPlus,CodeFetch,DoLIT,EXIT,Equals
;                 DW      ZBranch,OPTC2
;                 DW      CodeFetch,DUPP,DoLIT,DoLIT,XORR,ZBranch,OPTC2
;                 DW      SWAP
; OPTC2:          DW      DROP,COMPILEComma,EXIT

                $CODE   NameOptiCOMPILEComma,OptiCOMPILEComma
                CMP     CS:[BX],CALLL
                JNE     OPTC1
                MOV     AX,CS:[BX+CELLL]
                ADD     AX,BX
                ADD     AX,2*CELLL
                CMP     AX,OFFSET DoLIST
                JNE     OPTC1
                MOV     DX,OFFSET EXIT
                MOV     AX,CS:[BX+2*CELLL]
                CMP     AX,DX
                JE      OPTC2
                CMP     DX,CS:[BX+3*CELLL]
                JNE     OPTC1
                CMP     AX,OFFSET DoLIT
                JE      OPTC1
                MOV     BX,AX
OPTC1:          JMP     COMPILEComma
OPTC2:          POP     BX
                $NEXT

;   singleOnly  ( c-addr u -- x )
;               Handle the word not found in the search-order. If the string
;               is legal, leave a single cell number in interpretation state.
;
;   : singleOnly
;               0 DUP 2SWAP OVER C@ [CHAR] -
;               = DUP >R IF 1 /STRING THEN
;               >NUMBER IF -13 THROW THEN       \ undefined word
;               2DROP R> IF NEGATE THEN ;

                $COLON  NameSingleOnly,SingleOnly
                DW      DoLIT,0,DUPP,TwoSWAP,OVER,CFetch,DoLIT,'-'
                DW      Equals,DUPP,ToR,ZBranch,SINGLEO4
                DW      DoLIT,1,SlashSTRING
SINGLEO4:       DW      ToNUMBER,ZBranch,SINGLEO1
                DW      DoLIT,-13,THROW
SINGLEO1:       DW      TwoDROP,RFrom,ZBranch,SINGLEO2
                DW      NEGATE
SINGLEO2:       DW      EXIT

;   singleOnly, ( c-addr u -- )
;               Handle the word not found in the search-order. Compile a
;               single cell number in compilation state.
;
;   : singleOnly,
;               singleOnly LITERAL ;

                $COLON  NameSingleOnlyComma,SingleOnlyComma
                DW      SingleOnly,LITERAL,EXIT

;   (doubleAlso) ( c-addr u -- x 1 | x x 2 )
;               If the string is legal, leave a single or double cell number
;               and size of the number.
;
;   : (doubleAlso)
;               0 DUP 2SWAP OVER C@ [CHAR] -
;               = DUP >R IF 1 /STRING THEN
;               >NUMBER ?DUP
;               IF   1- IF -13 THROW THEN     \ more than one char is remained
;                    DUP C@ [CHAR] . XOR      \ last char is not '.'
;                    IF -13 THROW THEN        \ undefined word
;                    R> IF DNEGATE THEN
;                    2 EXIT               THEN
;               2DROP R> IF NEGATE THEN       \ single number
;               1 ;

                $COLON  NameParenDoubleAlso,ParenDoubleAlso
                DW      DoLIT,0,DUPP,TwoSWAP,OVER,CFetch,DoLIT,'-'
                DW      Equals,DUPP,ToR,ZBranch,DOUBLEA1
                DW      DoLIT,1,SlashSTRING
DOUBLEA1:       DW      ToNUMBER,QuestionDUP,ZBranch,DOUBLEA4
                DW      OneMinus,ZBranch,DOUBLEA3
DOUBLEA2:       DW      DoLIT,-13,THROW
DOUBLEA3:       DW      CFetch,DoLIT,'.',Equals,ZBranch,DOUBLEA2
                DW      RFrom,ZBranch,DOUBLEA5
                DW      DNEGATE
DOUBLEA5:       DW      DoLIT,2,EXIT
DOUBLEA4:       DW      TwoDROP,RFrom,ZBranch,DOUBLEA6
                DW      NEGATE
DOUBLEA6:       DW      DoLIT,1,EXIT

;   doubleAlso  ( c-addr u -- x | x x )
;               Handle the word not found in the search-order. If the string
;               is legal, leave a single or double cell number in
;               interpretation state.
;
;   : doubleAlso
;               (doubleAlso) DROP ;

                $COLON  NameDoubleAlso,DoubleAlso
                DW      ParenDoubleAlso,DROP,EXIT

;   doubleAlso, ( c-addr u -- )
;               Handle the word not found in the search-order. If the string
;               is legal, compile a single or double cell number in
;               compilation state.
;
;   : doubleAlso,
;               (doubleAlso) 1- IF SWAP LITERAL THEN LITERAL ;

                $COLON  NameDoubleAlsoComma,DoubleAlsoComma
                DW      ParenDoubleAlso,OneMinus,ZBranch,DOUBC1
                DW      SWAP,LITERAL
DOUBC1:         DW      LITERAL,EXIT

;   -.          ( -- )
;               You don't need this word unless you care that '-.' returns
;               double cell number 0. Catching illegal number '-.' in this way
;               is easier than make 'interpret' catch this exeption.
;
;   : -.        -13 THROW ; IMMEDIATE   \ undefined word

                $COLON  NameMinusDot,MinusDot
                DW      DoLIT,-13,THROW

;   lastName    ( -- c-addr )
;               Return the address of the last definition name.

                $VALUE  NameLastName,LastName,AddrLastName

;   linkLast    ( -- )
;               Link the word being defined to the current wordlist.
;               Do nothing if the last definition is made by :NONAME .
;
;   : linkLast  lastXT lastName name>xt =
;               IF lastName GET-CURRENT !  0 TO lastXT THEN ;

                $COLON  NameLinkLast,LinkLast
                DW      LastXT,LastName,NameToXT,Equals,ZBranch,LINKL1
                DW      LastName,GET_CURRENT,Store,DoLIT,0,DoTO,AddrLastXT
LINKL1:         DW      EXIT

;                 $CODE   NameLinkLast,LinkLast
;                 MOV     AX,AddrLastXT
;                 MOV     DI,AddrLastName
;                 CMP     AX,[DI-2*CELLL]
;                 JNE     LINKL1
;                 MOV     AddrCurrent,DI
;                 XOR     AX,AX
;                 MOV     AddrLastXT,AX
; LINKL1:         $NEXT

;   name>xt     ( c-addr -- xt )
;               Return execution token using counted string at c-addr.
;
;   : name>xt   cell- cell- @ ;
;
;                 $COLON  NameNameToXT,NameToXT
;                 DW      CellMinus,CellMinus,Fetch,EXIT

                $CODE   NameNameToXT,NameToXT
                MOV     BX,[BX-2*CELLL]
                $NEXT

;   pack"       ( c-addr u a-addr -- a-addr2 )
;               Place a string c-addr u at a-addr and gives the next
;               cell-aligned address. Fill the rest of the last cell with
;               null character.
;
;   : pack"     2DUP SWAP CHARS + CHAR+ ALIGNED DUP >R  \ ca u aa aa+u+1
;               cell- 0 SWAP !                  \ fill 0 at the end of string
;               2DUP C! CHAR+ SWAP              \ c-addr a-addr+1 u
;               CHARS MOVE R> ; COMPILE-ONLY
;
;                 $COLON  NamePackQuote,PackQuote
;                 DW      TwoDUP,SWAP,CHARS,Plus,CHARPlus,ALIGNED,DUPP,ToR
;                 DW      CellMinus,DoLIT,0,SWAP,Store
;                 DW      TwoDUP,CStore,CHARPlus,SWAP
;                 DW      CHARS,MOVE,RFrom,EXIT

                $CODE   NamePackQuote,PackQuote
                MOV     DI,BX
                MOV     DX,SI
                MOV     AX,DS
                MOV     ES,AX
                POP     CX
                POP     SI
                MOV     BYTE PTR [DI],CL
                INC     DI
                REP MOVSB
                TEST    DI,1            ;odd address?
                JZ      PACKQ1
                MOV     BYTE PTR [DI],0
                INC     DI
PACKQ1:         MOV     BX,DI
                MOV     SI,DX
                $NEXT

;   PARSE-WORD  ( "<spaces>ccc<space>" -- c-addr u )
;               Skip leading spaces and parse a word. Return the name.
;
;   : PARSE-WORD   BL skipPARSE ;
;
;                 $COLON  NamePARSE_WORD,PARSE_WORD
;                 DW      DoLIT,' ',SkipPARSE,EXIT

                $CODE   NamePARSE_WORD,PARSE_WORD
                PUSH    BX
                MOV     BX,' '
                JMP     SkipPARSE
                $ALIGN

;   pipe        ( -- ) ( R: xt -- )
;               Connect most recently defined word to code following DOES>.
;               Structure of CREATEd word:
;               |compile_xt|name_ptr| call-doCREATE | 0 or DOES>_xt | a-addr |
;
;   : pipe      lastName name>xt ?call DUP IF   \ code-addr xt2
;                   ['] doCREATE = IF
;                   R> SWAP code!       \ change DOES> code of CREATEd word
;                   EXIT
;               THEN THEN
;               -32 THROW       \ invalid name argument, no-CREATEd last name
;               ; COMPILE-ONLY

                $COLON  NamePipe,Pipe
                DW      LastName,NameToXT,QCall,DUPP,ZBranch,PIPE2
                DW      DoLIT,DoCREATE,Equals,ZBranch,PIPE2
                DW      RFrom,SWAP,CodeStore,EXIT
PIPE2:          DW      DoLIT,-32,THROW

;   skipPARSE   ( char "<chars>ccc<char>" -- c-addr u )
;               Skip leading chars and parse a word using char as a
;               delimeter. Return the name.
;
;   : skipPARSE
;               >R SOURCE >IN @ /STRING    \ c_addr u  R: char
;               DUP IF
;                  BEGIN  OVER C@ R@ =
;                  WHILE  1- SWAP CHAR+ SWAP DUP 0=
;                  UNTIL  R> DROP EXIT
;                  ELSE THEN
;                  DROP SOURCE DROP - 1chars/ >IN ! R> PARSE EXIT
;               THEN R> DROP ;
;
;                 $COLON  NameSkipPARSE,SkipPARSE
;                 DW      ToR,SOURCE,DoLIT,AddrToIN,Fetch,SlashSTRING
;                 DW      DUPP,ZBranch,SKPAR1
; SKPAR2:         DW      OVER,CFetch,RFetch,Equals,ZBranch,SKPAR3
;                 DW      OneMinus,SWAP,CHARPlus,SWAP
;                 DW      DUPP,ZeroEquals,ZBranch,SKPAR2
;                 DW      RFrom,DROP,EXIT
; SKPAR3:         DW      DROP,SOURCE,DROP,Minus,OneCharsSlash
;                 DW      DoLIT,AddrToIN,Store,RFrom,PARSE,EXIT
; SKPAR1:         DW      RFrom,DROP,EXIT

                $CODE   NameSkipPARSE,SkipPARSE
                MOV     AH,BL
                MOV     DX,SI
                MOV     SI,AddrSourceVar+CELLL
                MOV     BX,AddrSourceVar
                MOV     CX,AddrToIN
                ADD     SI,CX
                SUB     BX,CX
                MOV     CX,SI
                OR      BX,BX
                JZ      PARSW1
PARSW5:         LODSB
                CMP     AL,AH
                JNE     PARSW4
                DEC     BX
                OR      BX,BX
                JNZ     PARSW5
                MOV     AX,AddrSourceVar
                MOV     AddrToIN,AX
PARSW1:         PUSH    SI
                MOV     SI,DX
                $NEXT
PARSW4:         DEC     SI
                SUB     SI,AddrSourceVar+CELLL
                MOV     AddrToIN,SI
                XOR     BX,BX
                MOV     BL,AH
                MOV     SI,DX
                JMP     PARSE
                $ALIGN

;   specialComp? ( -- flag )
;               Return flag for whether or not the last found word has
;               special compilation semantics.

                $VALUE  NameSpecialCompQ,SpecialCompQ,AddrSpecialCompQ

;   rake        ( C: do-sys -- )
;               Gathers LEAVEs.
;
;   : rake      DUP code, rakeVar @
;               BEGIN  2DUP U<
;               WHILE  DUP code@ xhere ROT code!
;               REPEAT rakeVar ! DROP
;               ?DUP IF POSTPONE THEN THEN ;    \ check for ?DO

                $COLON  Namerake,rake
                DW      DUPP,CodeComma,DoLIT,AddrRakeVar,Fetch
rake1:          DW      TwoDUP,ULess,ZBranch,rake2
                DW      DUPP,CodeFetch,XHere,ROT,CodeStore,Branch,rake1
rake2:          DW      DoLIT,AddrRakeVar,Store,DROP
                DW      QuestionDUP,ZBranch,rake3
                DW      THENN
rake3:          DW      EXIT

;   rp0         ( -- a-addr )
;               Pointer to bottom of the return stack.
;
;   : rp0       userP @ CELL+ CELL+ @ ;

                $COLON  NameRPZero,RPZero
                DW      DoLIT,AddrUserP,Fetch,CELLPlus,CELLPlus,Fetch,EXIT

;   search      ( c-addr u -- c-addr u 0 | xt f 1 | xt f -1)
;               Search dictionary for a match with the given name. Return
;               execution token, not-compile-only flag and -1 or 1
;               ( IMMEDIATE) if found; c-addr u 0 if not.
;
;   : search    #order @ DUP                    \ not found if #order is 0
;               IF 0
;                  DO 2DUP                      \ ca u ca u
;                     I CELLS #order CELL+ + @  \ ca u ca u wid
;                     (search-wordlist)         \ ca u; 0 | w f 1 | w f -1
;                     ?DUP IF                   \ ca u; 0 | w f 1 | w f -1
;                        >R 2SWAP 2DROP R> UNLOOP EXIT \ xt f 1 | xt f -1
;                     THEN                      \ ca u
;                  LOOP 0                       \ ca u 0
;               THEN ;

                $COLON  NameSearch,Search
                DW      NumberOrder,Fetch,DUPP,ZBranch,SEARCH1
                DW      DoLIT,0,DoDO
SEARCH2:        DW      TwoDUP,I,CELLS,NumberOrder,CELLPlus,Plus,Fetch
                DW      ParenSearch_Wordlist,QuestionDUP,ZBranch,SEARCH3
                DW      ToR,TwoSWAP,TwoDROP,RFrom,UNLOOP,EXIT
SEARCH3:        DW      DoLOOP,SEARCH2
                DW      DoLIT,0
SEARCH1:        DW      EXIT

;   sourceVar   ( -- a-addr )
;               Hold the current count and address of the terminal input buffer.

                $CONST  NameSourceVar,SourceVar,AddrSourceVar

;   sp0         ( -- a-addr )
;               Pointer to bottom of the data stack.
;
;   : sp0       userP @ CELL+ @ ;

                $COLON  NameSPZero,SPZero
                DW      DoLIT,AddrUserP,Fetch,CELLPlus,Fetch,EXIT

;***************
; Essential Standard words - Colon definitions
;***************

;   #           ( ud1 -- ud2 )                  \ CORE
;               Extract one digit from ud1 and append the digit to
;               pictured nemiric output string. ( ud2 = ud1 / BASE )
;
;   : #         0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP
;               9 OVER < [ CHAR A CHAR 9 1 + - ] LITERAL AND +
;               [ CHAR 0 ] LITERAL + HOLD R> ;
;
;                 $COLON  NameNumberSign,NumberSign
;                 DW      DoLIT,0,DoLITFetch,AddrBASE,UMSlashMOD,ToR
;                 DW      DoLITFetch,AddrBASE,UMSlashMOD,SWAP
;                 DW      DoLIT,9,OVER,LessThan,DoLIT,'A'-'9'-1,ANDD,Plus
;                 DW      DoLIT,'0',Plus,HOLD,RFrom,EXIT

                $CODE   NameNumberSign,NumberSign
                XOR     DX,DX
                MOV     AX,BX
                MOV     CX,AddrBASE
                DIV     CX              ;0:TOS / BASE
                MOV     BX,AX           ;quotient
                POP     AX
                DIV     CX
                PUSH    AX              ;BX:AX = ud2
                MOV     AL,DL
                CMP     AL,9
                JBE     NUMSN1
                ADD     AL,'A'-'9'-1
NUMSN1:         ADD     AL,'0'
                MOV     DI,AddrHLD
                DEC     DI
                MOV     AddrHLD,DI
                MOV     [DI],AL
                $NEXT

;   #>          ( xd -- c-addr u )              \ CORE
;               Prepare the output string to be TYPE'd.
;               ||HERE>WORD/#-work-area|
;
;   : #>        2DROP hld @ HERE size-of-PAD + OVER - 1chars/ ;

                $COLON  NameNumberSignGreater,NumberSignGreater
                DW      TwoDROP,DoLIT,AddrHLD,Fetch,HERE,DoLIT,PADSize*CHARR,Plus
                DW      OVER,Minus,OneCharsSlash,EXIT

;   #S          ( ud -- 0 0 )                   \ CORE
;               Convert ud until all digits are added to the output string.
;
;   : #S        BEGIN # 2DUP OR 0= UNTIL ;

                $COLON  NameNumberSignS,NumberSignS
NUMSS1:         DW      NumberSign,TwoDUP,ORR
                DW      ZeroEquals,ZBranch,NUMSS1
                DW      EXIT

;   '           ( "<spaces>name" -- xt )        \ CORE
;               Parse a name, find it and return xt.
;
;   : '         (') DROP ;

                $COLON  NameTick,Tick
                DW      ParenTick,DROP,EXIT

;   +           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
;               Add top two items and gives the sum.
;
;   : +         um+ DROP ;
;
;                 $COLON  NamePlus,Plus
;                 DW      UMPlus,DROP,EXIT

                $CODE   NamePlus,Plus
                POP     AX
                ADD     BX,AX
                $NEXT

;   +!          ( n|u a-addr -- )               \ CORE
;               Add n|u to the contents at a-addr.
;
;   : +!        SWAP OVER @ + SWAP ! ;
;
;                 $COLON  NamePlusStore,PlusStore
;                 DW      SWAP,OVER,Fetch,Plus
;                 DW      SWAP,Store,EXIT

                $CODE   NamePlusStore,PlusStore
                POP     AX
                ADD     [BX],AX
                POP     BX
                $NEXT

;   ,           ( x -- )                        \ CORE
;               Reserve one cell in data space and store x in it.
;
;   : ,         HERE !  HERE CELL+ TO HERE ;
;
;                 $COLON  NameComma,Comma
;                 DW      HERE,Store,HERE,CELLPlus,DoTO,AddrHERE,EXIT

                $CODE   NameComma,Comma
                MOV     DI,AddrHERE
                MOV     [DI],BX
                ADD     DI,CELLL
                MOV     AddrHERE,DI
                POP     BX
                $NEXT

;   -           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
;               Subtract n2|u2 from n1|u1, giving the difference n3|u3.
;
;   : -         NEGATE + ;
;
;                 $COLON  NameMinus,Minus
;                 DW      NEGATE,Plus,EXIT
;
                $CODE   NameMinus,Minus
                POP     AX
                SUB     AX,BX
                MOV     BX,AX
                $NEXT

;   .           ( n -- )                        \ CORE
;               Display a signed number followed by a space.
;
;   : .         S>D D. ;

                $COLON  NameDot,Dot
                DW      SToD,DDot,EXIT

;   /           ( n1 n2 -- n3 )                 \ CORE
;               Divide n1 by n2, giving single-cell quotient n3.
;
;   : /         /MOD NIP ;

                $COLON  NameSlash,Slash
                DW      SlashMOD,NIP,EXIT

;   /MOD        ( n1 n2 -- n3 n4 )              \ CORE
;               Divide n1 by n2, giving single-cell remainder n3 and
;               single-cell quotient n4.
;
;   : /MOD      >R S>D R> FM/MOD ;
;
;                 $COLON  NameSlashMOD,SlashMOD
;                 DW      ToR,SToD,RFrom,FMSlashMOD,EXIT

                $CODE   NameSlashMOD,SlashMOD
                POP     AX
                CWD
                PUSH    AX
                PUSH    DX
                JMP     FMSlashMOD
                $ALIGN

;   /STRING     ( c-addr1 u1 n -- c-addr2 u2 )  \ STRING
;               Adjust the char string at c-addr1 by n chars.
;
;   : /STRING   CHARS DUP >R - SWAP R> CHARS + SWAP ;
;
;                 $COLON  NameSlashSTRING,SlashSTRING
;                 DW      CHARS,DUPP,ToR,Minus
;                 DW      SWAP,RFrom,CHARS,Plus,SWAP,EXIT

                $CODE   NameSlashSTRING,SlashSTRING
                POP     AX
                SUB     AX,BX
                POP     DX
                ADD     DX,BX
                PUSH    DX
                MOV     BX,AX
                $NEXT

;   1+          ( n1|u1 -- n2|u2 )              \ CORE
;               Increase top of the stack item by 1.
;
;   : 1+        1 + ;
;
;                 $COLON  NameOnePlus,OnePlus
;                 DW      DoLIT,1,Plus,EXIT

                $CODE   NameOnePlus,OnePlus
                INC     BX
                $NEXT

;   1-          ( n1|u1 -- n2|u2 )              \ CORE
;               Decrease top of the stack item by 1.
;
;   : 1-        -1 + ;
;
;                 $COLON  NameOneMinus,OneMinus
;                 DW      DoLIT,-1,Plus,EXIT

                $CODE   NameOneMinus,OneMinus
                DEC     BX
                $NEXT

;   2!          ( x1 x2 a-addr -- )             \ CORE
;               Store the cell pare x1 x2 at a-addr, with x2 at a-addr and
;               x1 at the next consecutive cell.
;
;   : 2!        SWAP OVER ! CELL+ ! ;
;
;                 $COLON  NameTwoStore,TwoStore
;                 DW      SWAP,OVER,Store,CELLPlus,Store,EXIT
;
                $CODE   NameTwoStore,TwoStore
                POP     [BX]
                POP     [BX+CELLL]
                POP     BX
                $NEXT

;   2@          ( a-addr -- x1 x2 )             \ CORE
;               Fetch the cell pair stored at a-addr. x2 is stored at a-addr
;               and x1 at the next consecutive cell.
;
;   : 2@        DUP CELL+ @ SWAP @ ;
;
;                 $COLON  NameTwoFetch,TwoFetch
;                 DW      DUPP,CELLPlus,Fetch,SWAP,Fetch,EXIT

                $CODE   NameTwoFetch,TwoFetch
                PUSH    [BX+CELLL]
                MOV     BX,[BX]
                $NEXT

;   2DROP       ( x1 x2 -- )                    \ CORE
;               Drop cell pair x1 x2 from the stack.
;
;                 $COLON  NameTwoDROP,TwoDROP
;                 DW      DROP,DROP,EXIT

                $CODE   NameTwoDROP,TwoDROP
                POP     BX
                POP     BX
                $NEXT

;   2DUP        ( x1 x2 -- x1 x2 x1 x2 )        \ CORE
;               Duplicate cell pair x1 x2.
;
;                 $COLON  NameTwoDUP,TwoDUP
;                 DW      OVER,OVER,EXIT

                $CODE   NameTwoDUP,TwoDUP
                MOV     DI,SP
                PUSH    BX
                PUSH    [DI]
                $NEXT

;   2SWAP       ( x1 x2 x3 x4 -- x3 x4 x1 x2 )  \ CORE
;               Exchange the top two cell pairs.
;
;   : 2SWAP     ROT >R ROT R> ;
;
;                 $COLON  NameTwoSWAP,TwoSWAP
;                 DW      ROT,ToR,ROT,RFrom,EXIT

                $CODE   NameTwoSWAP,TwoSWAP
                POP     AX
                POP     CX
                POP     DX
                PUSH    AX
                PUSH    BX
                PUSH    DX
                MOV     BX,CX
                $NEXT

;   :           ( -- ; <string> )               \ CORE
;               Start a new colon definition using next word as its name.
;
;   : :         xhere ALIGNED CELL+ TO xhere :NONAME head, ;

                $COLON  NameCOLON,COLON
                DW      XHere,ALIGNED,CELLPlus,DoTO,AddrXHere
                DW      ColonNONAME,HeadComma,EXIT

;   :NONAME     ( -- xt )                       \ CORE EXT
;               Create an execution token xt, enter compilation state and
;               start the current definition.
;
;   : :NONAME   STATE @ IF -29 THROW THEN
;               ['] doLIST xt,  0 bal ! ] ;

                $COLON  NameColonNONAME,ColonNONAME
                DW      DoLIT,AddrSTATE,Fetch,ZBranch,NONAME1
                DW      DoLIT,-29,THROW
NONAME1:        DW      DoLIT,DoLIST,xtComma,DoLIT,0,DoLIT,AddrBalance,Store
                DW      RightBracket,EXIT

;   ;           ( -- )                          \ CORE
;               Terminate a colon definition.
;
;   : ;         bal @ IF -22 THROW THEN         \ control structure mismatch
;               POSTPONE EXIT linkLast          \ add EXIT at the end of the
;                               \ definition and link the word to wordlist
;               [ ; COMPILE-ONLY IMMEDIATE

                $COLON  NameSemicolon,Semicolon
                DW      DoLIT,AddrBalance,Fetch,ZBranch,SEMI1
                DW      DoLIT,-22,THROW
SEMI1:          DW      DoLIT,EXIT,COMPILEComma
                DW      LinkLast,LeftBracket,EXIT

;   <           ( n1 n2 -- flag )               \ CORE
;               Returns true if n1 is less than n2.
;
;   : <         2DUP XOR 0<             \ same sign?
;               IF DROP 0< EXIT THEN    \ different signs, true if n1 <0
;               - 0< ;                  \ same signs, true if n1-n2 <0
;
;                 $COLON  NameLessThan,LessThan
;                 DW      TwoDUP,XORR,ZeroLess,ZBranch,LESS1
;                 DW      DROP,ZeroLess,EXIT
; LESS1:          DW      Minus,ZeroLess,EXIT

                $CODE   NameLessThan,LessThan
                POP     AX
                SUB     AX,BX
                MOV     BX,-1
                JL      LESS1
                INC     BX
LESS1:          $NEXT

;   <#          ( -- )                          \ CORE
;               Initiate the numeric output conversion process.
;               ||HERE>WORD/#-work-area|
;
;   : <#        HERE size-of-PAD + hld ! ;

                $COLON  NameLessNumberSign,LessNumberSign
                DW      HERE,DoLIT,PADSize*CHARR,Plus,DoLIT,AddrHLD,Store,EXIT

;   =           ( x1 x2 -- flag )               \ CORE
;               Return true if top two are equal.
;
;   : =         XORR 0= ;
;
;                 $COLON  NameEquals,Equals
;                 DW      XORR,ZeroEquals,EXIT

                $CODE   NameEquals,Equals
                POP     AX
                CMP     BX,AX
                MOV     BX,-1
                JE      EQUAL1
                INC     BX
EQUAL1:         $NEXT

;   >           ( n1 n2 -- flag )               \ CORE
;               Returns true if n1 is greater than n2.
;
;   : >         SWAP < ;
;
;                 $COLON  NameGreaterThan,GreaterThan
;                 DW      SWAP,LessThan,EXIT

                $CODE   NameGreaterThan,GreaterThan
                POP     AX
                SUB     AX,BX
                MOV     BX,-1
                JG      GREAT1
                INC     BX
GREAT1:         $NEXT

;   >IN         ( -- a-addr )
;               Hold the character pointer while parsing input stream.

                $CONST  NameToIN,ToIN,AddrToIN

;   >NUMBER     ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )    \ CORE
;               Add number string's value to ud1. Leaves string of any
;               unconverted chars.
;
;   : >NUMBER   BEGIN  DUP
;               WHILE  >R  DUP >R C@                    \ ud char  R: u c-addr
;                      DUP [ CHAR 9 1+ ] LITERAL [CHAR] A WITHIN
;                          IF DROP R> R> EXIT THEN
;                      [ CHAR 0 ] LITERAL - 9 OVER <
;                      [ CHAR A CHAR 9 1 + - ] LITERAL AND -
;                      DUP 0 BASE @ WITHIN
;               WHILE  SWAP BASE @ UM* DROP ROT BASE @ UM* D+ R> R> 1 /STRING
;               REPEAT DROP R> R>
;               THEN ;
;
;                 $COLON  NameToNUMBER,ToNUMBER
; TONUM1:         DW      DUPP,ZBranch,TONUM3
;                 DW      ToR,DUPP,ToR,CFetch,DUPP
;                 DW      DoLIT,'9'+1,DoLIT,'A',WITHIN,ZeroEquals,ZBranch,TONUM2
;                 DW      DoLIT,'0',Minus,DoLIT,9,OVER,LessThan
;                 DW      DoLIT,'A'-'9'-1,ANDD,Minus,DUPP
;                 DW      DoLIT,0,DoLIT,AddrBASE,Fetch,WITHIN,ZBranch,TONUM2
;                 DW      SWAP,DoLIT,AddrBASE,Fetch,UMStar,DROP,ROT,DoLIT,AddrBASE,Fetch
;                 DW      UMStar,DPlus,RFrom,RFrom,DoLIT,1,SlashSTRING
;                 DW      Branch,TONUM1
; TONUM2:         DW      DROP,RFrom,RFrom
; TONUM3:         DW      EXIT

                $CODE   NameToNUMBER,ToNUMBER
                POP     DI
TONUM4:         OR      BX,BX
                JZ      TONUM2
                XOR     CX,CX
                MOV     CL,[DI]
                SUB     CX,'0'
                JS      TONUM2          ;not valid digit
                CMP     CX,'9'-'0'
                JLE     TONUM3
                CMP     CX,'A'-'0'
                JL      TONUM2          ;not valid digit
                SUB     CX,'A'-'9'-1
TONUM3:         CMP     CX,AddrBASE
                JGE     TONUM2          ;not valid digit
                POP     AX
                MUL     AddrBASE
                POP     DX
                PUSH    AX
                MOV     AX,DX
                MUL     AddrBASE
                ADD     AX,CX
                POP     CX
                ADC     DX,CX
                PUSH    AX
                PUSH    DX
                INC     DI
                DEC     BX
                JMP     TONUM4
TONUM2:         PUSH    DI
                $NEXT

;   ?DUP        ( x -- x x | 0 )                \ CORE
;               Duplicate top of the stack if it is not zero.
;
;   : ?DUP      DUP IF DUP THEN ;
;
;                 $COLON  NameQuestionDUP,QuestionDUP
;                 DW      DUPP,ZBranch,QDUP1
;                 DW      DUPP
; QDUP1:          DW      EXIT

                $CODE   NameQuestionDUP,QuestionDUP
                OR      BX,BX
                JZ      QDUP1
                PUSH    BX
QDUP1:          $NEXT

;   ABORT       ( i*x -- ) ( R: j*x -- )        \ EXCEPTION EXT
;               Reset data stack and jump to QUIT.
;
;   : ABORT     -1 THROW ;

                $COLON  NameABORT,ABORT
                DW      DoLIT,-1,THROW

;   ACCEPT      ( c-addr +n1 -- +n2 )           \ CORE
;               Accept a string of up to +n1 chars. Return with actual count.
;               Implementation-defined editing. Stops at EOL# .
;               Supports backspace and delete editing.
;
;   : ACCEPT    >R 0
;               BEGIN  DUP R@ <                 \ ca n2 f  R: n1
;               WHILE  EKEY max-char AND
;                      DUP BL <
;                      IF   DUP  cr# = IF ROT 2DROP R> DROP EXIT THEN
;                           DUP  tab# =
;                           IF   DROP 2DUP + BL DUP EMIT SWAP C! 1+
;                           ELSE DUP  bsp# =
;                                SWAP del# = OR
;                                IF DROP DUP
;                                       \ discard the last char if not 1st char
;                                IF 1- bsp# EMIT BL EMIT bsp# EMIT THEN THEN
;                           THEN
;                      ELSE >R 2DUP CHARS + R> DUP EMIT SWAP C! 1+  THEN
;                      THEN
;               REPEAT SWAP  R> 2DROP ;

                $COLON  NameACCEPT,ACCEPT
                DW      ToR,DoLIT,0
ACCPT1:         DW      DUPP,RFetch,LessThan,ZBranch,ACCPT5
                DW      EKEY,DoLIT,MaxChar,ANDD
                DW      DUPP,DoLIT,' ',LessThan,ZBranch,ACCPT3
                DW      DUPP,DoLIT,CRR,Equals,ZBranch,ACCPT4
                DW      ROT,TwoDROP,RFrom,DROP,EXIT
ACCPT4:         DW      DUPP,DoLIT,TABB,Equals,ZBranch,ACCPT6
                DW      DROP,TwoDUP,Plus,DoLIT,' ',DUPP,EMIT,SWAP,CStore,OnePlus
                DW      Branch,ACCPT1
ACCPT6:         DW      DUPP,DoLIT,BKSPP,Equals
                DW      SWAP,DoLIT,DEL,Equals,ORR,ZBranch,ACCPT1
                DW      DUPP,ZBranch,ACCPT1
                DW      OneMinus,DoLIT,BKSPP,EMIT,DoLIT,' ',EMIT,DoLIT,BKSPP,EMIT
                DW      Branch,ACCPT1
ACCPT3:         DW      ToR,TwoDUP,CHARS,Plus,RFrom,DUPP,EMIT,SWAP,CStore
                DW      OnePlus,Branch,ACCPT1
ACCPT5:         DW      SWAP,RFrom,TwoDROP,EXIT

;   AGAIN       ( C: dest -- )                  \ CORE EXT
;               Resolve backward reference dest. Typically used as
;               BEGIN ... AGAIN . Move control to the location specified by
;               dest on execution.
;
;   : AGAIN     POSTPONE branch code, dest- ; COMPILE-ONLY IMMEDIATE

                $COLON  NameAGAIN,AGAIN
                DW      DoLIT,Branch,COMPILEComma,CodeComma,DestMinus,EXIT

;   AHEAD       ( C: -- orig )                  \ TOOLS EXT
;               Put the location of a new unresolved forward reference onto
;               control-flow stack.
;
;   : AHEAD     POSTPONE branch xhere 0 code, orig+ ; COMPILE-ONLY IMMEDIATE

                $COLON  NameAHEAD,AHEAD
                DW      DoLIT,Branch,COMPILEComma
                DW      XHere,DoLIT,0,CodeComma,OrigPlus,EXIT

;   BL          ( -- char )                     \ CORE
;               Return the value of the blank character.
;
;   : BL        blank-char-value EXIT ;

                $CONST NameBLank,BLank,' '

;   CATCH       ( i*x xt -- j*x 0 | i*x n )     \ EXCEPTION
;               Push an exception frame on the exception stack and then execute
;               the execution token xt in such a way that control can be
;               transferred to a point just after CATCH if THROW is executed
;               during the execution of xt.
;
;   : CATCH     sp@ >R throwFrame @ >R          \ save error frame
;               rp@ throwFrame !  EXECUTE       \ execute
;               R> throwFrame !                 \ restore error frame
;               R> DROP  0 ;                    \ no error

                $COLON  NameCATCH,CATCH
                DW      SPFetch,ToR,ThrowFrame,Fetch,ToR
                DW      RPFetch,ThrowFrame,Store,EXECUTE
                DW      RFrom,ThrowFrame,Store
                DW      RFrom,DROP,DoLIT,0,EXIT

;   CELL+       ( a-addr1 -- a-addr2 )          \ CORE
;               Return next aligned cell address.
;
;   : CELL+     cell-size + ;
;
;                 $COLON  NameCELLPlus,CELLPlus
;                 DW      DoLIT,CELLL,Plus,EXIT

                $CODE   NameCELLPlus,CELLPlus
                ADD     BX,CELLL
                $NEXT

;   CHAR+       ( c-addr1 -- c-addr2 )          \ CORE
;               Returns next character-aligned address.
;
;   : CHAR+     char-size + ;
;
;                 $COLON  NameCHARPlus,CHARPlus
;                 DW      DoLIT,CHARR,Plus,EXIT

                $CODE   NameCHARPlus,CHARPlus
                INC     BX
                $NEXT

;   COMPILE,    ( xt -- )                       \ CORE EXT
;               Compile the execution token on data stack into current
;               colon definition.
;               Structure of words with special compilation action
;                   for default compilation behavior
;               |compile_xt|name_ptr| call-doCREATE | 0 or DOES>_xt | a-addr |
;
;   : COMPILE,  specialComp? OVER = IF DUP cell- cell- code@ EXECUTE EXIT THEN
;               code, ;
;
;                 $COLON  NameCOMPILEComma,COMPILEComma
;                 DW      SpecialCompQ,OVER,Equals,ZBranch,COMPILEC1
;                 DW      DUPP,CellMinus,CellMinus,CodeFetch,EXECUTE,EXIT
; COMPILEC1:      DW      CodeComma,EXIT

                $CODE   NameCOMPILEComma,COMPILEComma
                CMP     BX,AddrSpecialCompQ
                JE      COMPILEC1
                MOV     DI,AddrXHere
                MOV     CS:[DI],BX
                ADD     DI,CELLL
                POP     BX
                MOV     AddrXHere,DI
                $NEXT
COMPILEC1:      MOV     AX,CS:[BX-2*CELLL]
                JMP     AX
                $ALIGN

;   compileCONST ( xt -- )
;               Compile a CONSTANT word of which xt is given.
;               Structure of CONSTANT word:
;               |compile_xt|name_ptr| call-doCONST | x |
;
;   : compileCONST
;               CELL+ CELL+ code@ POSTPONE LITERAL ;
;
;                 $COLON  NameCompileCONST,CompileCONST
;                 DW      CELLPlus,CELLPlus,CodeFetch,LITERAL,EXIT

                $CODE   NameCompileCONST,CompileCONST
                MOV     CX,CS:[BX+2*CELLL]
                MOV     DI,AddrXHere
                MOV     AX,OFFSET DoLIT
                MOV     CS:[DI],AX
                ADD     DI,CELLL
                MOV     CS:[DI],CX
                ADD     DI,CELLL
                POP     BX
                MOV     AddrXHere,DI
                $NEXT

;   CONSTANT    ( x "<spaces>name" -- )         \ CORE
;               name Execution: ( -- x )
;               Create a definition for name which pushes x on the stack on
;               execution.
;
;   : CONSTANT  xhere ALIGNED TO xhere
;               ['] compileCONST code,
;               xhere CELL+ TO xhere
;               ['] doCONST xt, head,
;               code, linkLast
;               lastName [ =seman ] LITERAL OVER @ OR SWAP ! ;

                $COLON  NameCONSTANT,CONSTANT
                DW      XHere,ALIGNED,DoTO,AddrXHere
                DW      DoLIT,CompileCONST,CodeComma
                DW      XHere,CELLPlus,DoTO,AddrXHere
                DW      DoLIT,DoCONST,xtComma,HeadComma
                DW      CodeComma,LinkLast
                DW      LastName,DoLIT,SEMAN,OVER,Fetch,ORR,SWAP,Store,EXIT

;   COUNT       ( c-addr1 -- c-addr2 u )        \ CORE
;               Convert counted string to string specification. c-addr2 is
;               the next char-aligned address after c-addr1 and u is the
;               contents at c-addr1.
;
;   : COUNT     DUP CHAR+ SWAP C@ ;
;
;                 $COLON  NameCOUNT,COUNT
;                 DW      DUPP,CHARPlus,SWAP,CFetch,EXIT

                $CODE   NameCOUNT,COUNT
                MOV     AX,BX
                INC     AX
                MOV     BL,[BX]
                XOR     BH,BH
                PUSH    AX
                $NEXT

;   compileCREATE ( xt -- )
;               Compile a CREATEd word of which xt is given.
;               Structure of CREATEd word:
;               |compile_xt|name_ptr| call-doCREATE | 0 or DOES>_xt | a-addr |
;
;   : compileCREATE
;               DUP CELL+ CELL+ code@           \ 0 or DOES>_xt
;               IF COMPILE, EXIT THEN
;               CELL+ CELL+ CELL+ code@ LITERAL ;

                $COLON  NameCompileCREATE,CompileCREATE
                DW      DUPP,CELLPlus,CELLPlus,CodeFetch,ZBranch,COMPCREAT1
                DW      COMPILEComma,EXIT
COMPCREAT1:     DW      CELLPlus,CELLPlus,CELLPlus,CodeFetch,LITERAL,EXIT

;   CREATE      ( "<spaces>name" -- )           \ CORE
;               name Execution: ( -- a-addr )
;               Create a data object in RAM/ROM data space, which return
;               data object address on execution
;
;   : CREATE    xhere ALIGNED TO xhere
;               ['] compileCREATE code,
;               xhere CELL+ TO xhere    \ reserve space for nfa
;               ['] doCREATE xt, head,
;               0 code,                 \ no DOES> code yet
;               ALIGN HERE code,        \ >BODY returns this address
;               linkLast                \ link CREATEd word to current wordlist
;               lastName [ =seman ] LITERAL OVER @ OR SWAP ! ;

                $COLON  NameCREATE,CREATE
                DW      XHere,ALIGNED,DoTO,AddrXHere
                DW      DoLIT,CompileCREATE,CodeComma
                DW      XHere,CELLPlus,DoTO,AddrXHere
                DW      DoLIT,DoCREATE,xtComma,HeadComma,DoLIT,0,CodeComma
                DW      ALIGNN,HERE,CodeComma,LinkLast
                DW      LastName,DoLIT,SEMAN,OVER,Fetch,ORR,SWAP,Store,EXIT

;   D+          ( d1|ud1 d2|ud2 -- d3|ud3 )     \ DOUBLE
;               Add double-cell numbers.
;
;   : D+        >R SWAP >R um+ R> R> + + ;
;
;                 $COLON  NameDPlus,DPlus
;                 DW      ToR,SWAP,ToR,UMPlus
;                 DW      RFrom,RFrom,Plus,Plus,EXIT

                $CODE   NameDPlus,DPlus
                POP     CX
                POP     DX
                POP     AX
                ADD     CX,AX
                ADC     BX,DX
                PUSH    CX
                $NEXT

;   D.          ( d -- )                        \ DOUBLE
;               Display d in free field format followed by a space.
;
;   : D.        (d.) TYPE SPACE ;

                $COLON  NameDDot,DDot
                DW      ParenDDot,TYPEE,SPACE,EXIT

;   DECIMAL     ( -- )                          \ CORE
;               Set the numeric conversion radix to decimal 10.
;
;   : DECIMAL   10 BASE ! ;

                $COLON  NameDECIMAL,DECIMAL
                DW      DoLIT,10,DoLIT,AddrBASE,Store,EXIT

;   DEPTH       ( -- +n )                       \ CORE
;               Return the depth of the data stack.
;
;   : DEPTH     sp@ sp0 SWAP - cell-size / ;

                $COLON  NameDEPTH,DEPTH
                DW      SPFetch,SPZero,SWAP,Minus
                DW      DoLIT,CELLL,Slash,EXIT

;   DNEGATE     ( d1 -- d2 )                    \ DOUBLE
;               Two's complement of double-cell number.
;
;   : DNEGATE   INVERT >R INVERT 1 um+ R> + ;
;
;                 $COLON  NameDNEGATE,DNEGATE
;                 DW      INVERT,ToR,INVERT
;                 DW      DoLIT,1,UMPlus
;                 DW      RFrom,Plus,EXIT

                $CODE   NameDNEGATE,DNEGATE
                POP   AX
                NEG   AX
                PUSH  AX
                ADC   BX,0
                NEG   BX
                $NEXT

;   EKEY        ( -- u )                        \ FACILITY EXT
;               Receive one keyboard event u.
;
;   : EKEY      BEGIN PAUSE EKEY? UNTIL 'ekey EXECUTE ;

                $COLON  NameEKEY,EKEY
EKEY1:          DW      PAUSE,EKEYQuestion,ZBranch,EKEY1
                DW      TickEKEY,EXECUTE,EXIT

;   EMIT        ( x -- )                        \ CORE
;               Send a character to the output device.
;
;   : EMIT      'emit EXECUTE ;
;
;                 $COLON  NameEMIT,EMIT
;                 DW      TickEMIT,EXECUTE,EXIT

                $CODE   NameEMIT,EMIT
                MOV     AX,AddrTickEMIT
                JMP     AX
                $ALIGN

;   FIND        ( c-addr -- c-addr 0 | xt 1 | xt -1)     \ SEARCH
;               Search dictionary for a match with the given counted name.
;               Return execution token and -1 or 1 ( IMMEDIATE) if found;
;               c-addr 0 if not found.
;
;   : FIND      DUP COUNT search ?DUP IF NIP ROT DROP EXIT THEN
;               2DROP 0 ;

                $COLON  NameFIND,FIND
                DW      DUPP,COUNT,Search,QuestionDUP,ZBranch,FIND1
                DW      NIP,ROT,DROP,EXIT
FIND1:          DW      TwoDROP,DoLIT,0,EXIT

;   FM/MOD      ( d n1 -- n2 n3 )               \ CORE
;               Signed floored divide of double by single. Return mod n2
;               and quotient n3.
;
;   : FM/MOD    DUP >R 2DUP XOR >R >R DUP 0< IF DNEGATE THEN
;               R@ ABS UM/MOD DUP 0<
;               IF DUP 08000h XOR IF -11 THROW THEN THEN \ result out of range
;               SWAP R> 0< IF NEGATE THEN
;               SWAP R> 0< IF NEGATE OVER IF R@ ROT - SWAP 1- THEN THEN
;               R> DROP ;
;
;                 $COLON  6,'FM/MOD',FMSlashMOD,_FLINK
;                 DW      DUPP,ToR,TwoDUP,XORR,ToR,ToR,DUPP,ZeroLess
;                 DW      ZBranch,FMMOD1
;                 DW      DNEGATE
; FMMOD1:         DW      RFetch,ABSS,UMSlashMOD,DUPP,ZeroLess,ZBranch,FMMOD2
;                 DW      DUPP,DoLIT,08000h,XORR,ZBranch,FMMOD2
;                 DW      DoLIT,-11,THROW
; FMMOD2:         DW      SWAP,RFrom,ZeroLess,ZBranch,FMMOD3
;                 DW      NEGATE
; FMMOD3:         DW      SWAP,RFrom,ZeroLess,ZBranch,FMMOD4
;                 DW      NEGATE,OVER,ZBranch,FMMOD4
;                 DW      RFetch,ROT,Minus,SWAP,OneMinus
; FMMOD4:         DW      RFrom,DROP,EXIT

                $CODE   NameFMSlashMOD,FMSlashMOD
                POP     DX
                POP     AX
                OR      DX,DX
                JS      FMMOD2
                OR      BX,BX
                JZ      FMMOD1
                JS      FMMOD3
                CMP     DX,BX
                JAE     FMMOD6
                DIV     BX              ;positive dividend, positive divisor
                CMP     AX,08000h
                JA      FMMOD6
                PUSH    DX
                MOV     BX,AX
                $NEXT
FMMOD3:         NEG     BX              ;positive dividend, negative divisor
                CMP     DX,BX
                JAE     FMMOD6
                DIV     BX
                CMP     AX,08000h
                JA      FMMOD6
                OR      DX,DX
                JZ      FMMOD7          ;modulo = 0
                SUB     DX,BX
                NOT     AX              ;AX=-AX-1
                PUSH    DX
                MOV     BX,AX
                $NEXT
FMMOD2:         NEG     AX              ;DNEGATE
                ADC     DX,0
                NEG     DX
                OR      BX,BX
                JZ      FMMOD1
                JS      FMMOD4
                CMP     DX,BX           ;negative dividend, positive divisor
                JAE     FMMOD6
                DIV     BX
                CMP     AX,08000h
                JA      FMMOD6
                OR      DX,DX
                JZ      FMMOD7
                SUB     BX,DX
                NOT     AX              ;AX=-AX-1
                PUSH    BX
                MOV     BX,AX
                $NEXT
FMMOD7:         NEG     AX
                PUSH    DX
                MOV     BX,AX
                $NEXT
FMMOD4:         NEG     BX              ;negative dividend, negative divisor
                CMP     DX,BX
                JAE     FMMOD6
                DIV     BX
                CMP     AX,08000h
                JA      FMMOD6
                NEG     DX
                MOV     BX,AX
                PUSH    DX
                $NEXT
FMMOD6:         MOV     BX,-11          ;result out of range
                JMP     THROW
FMMOD1:         MOV     BX,-10          ;divide by zero
                JMP     THROW
                $ALIGN

;   GET-CURRENT   ( -- wid )                    \ SEARCH
;               Return the indentifier of the compilation wordlist.
;
;   : GET-CURRENT   current @ ;

                $COLON  NameGET_CURRENT,GET_CURRENT
                DW      DoLIT,AddrCurrent,Fetch,EXIT

;   HOLD        ( char -- )                     \ CORE
;               Add char to the beginning of pictured numeric output string.
;
;   : HOLD      hld @  1 CHARS - DUP hld ! C! ;
;
;                 $COLON  NameHOLD,HOLD
;                 DW      DoLIT,AddrHLD,Fetch,DoLIT,0-CHARR,Plus
;                 DW      DUPP,DoLIT,AddrHLD,Store,CStore,EXIT

                $CODE   NameHOLD,HOLD
                MOV     DI,AddrHLD
                DEC     DI
                MOV     AddrHLD,DI
                MOV     [DI],BL
                POP     BX
                $NEXT

;   I           ( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
;               Push the innermost loop index.
;
;   : I         rp@ [ 1 CELLS ] LITERAL + @
;               rp@ [ 2 CELLS ] LITERAL + @  +  ; COMPILE-ONLY
;
;                 $COLON  NameI,I
;                 DW      RPFetch,DoLIT,CELLL,Plus,Fetch
;                 DW      RPFetch,DoLIT,2*CELLL,Plus,Fetch,Plus,EXIT

                $CODE   NameI,I
                PUSH    BX
                MOV     BX,[BP]
                ADD     BX,[BP+2]
                $NEXT

;   IF          Compilation: ( C: -- orig )             \ CORE
;               Run-time: ( x -- )
;               Put the location of a new unresolved forward reference orig
;               onto the control flow stack. On execution jump to location
;               specified by the resolution of orig if x is zero.
;
;   : IF        POSTPONE 0branch xhere 0 code, orig+
;               ; COMPILE-ONLY IMMEDIATE

                $COLON  NameIFF,IFF
                DW      DoLIT,ZBranch,COMPILEComma,XHere
                DW      DoLIT,0,CodeComma,OrigPlus,EXIT

;   INVERT      ( x1 -- x2 )                    \ CORE
;               Return one's complement of x1.
;
;   : INVERT    -1 XOR ;
;
;                 $COLON  NameINVERT,INVERT
;                 DW      DoLIT,-1,XORR,EXIT

                $CODE   NameINVERT,INVERT
                NOT     BX
                $NEXT

;   KEY         ( -- char )                     \ CORE
;               Receive a character. Do not display char.
;
;   : KEY       EKEY max-char AND ;

                $COLON  NameKEY,KEY
                DW      EKEY,DoLIT,MaxChar,ANDD,EXIT

;   LITERAL     Compilation: ( x -- )           \ CORE
;               Run-time: ( -- x )
;               Append following run-time semantics. Put x on the stack on
;               execution
;
;   : LITERAL   POSTPONE doLIT code, ; COMPILE-ONLY IMMEDIATE

                $COLON  NameLITERAL,LITERAL
                DW      DoLIT,DoLIT,COMPILEComma,CodeComma,EXIT

;   NEGATE      ( n1 -- n2 )                    \ CORE
;               Return two's complement of n1.
;
;   : NEGATE    INVERT 1+ ;
;
;                 $COLON  NameNEGATE,NEGATE
;                 DW      INVERT,OnePlus,EXIT

                $CODE   NameNEGATE,NEGATE
                NEG     BX
                $NEXT

;   NIP         ( n1 n2 -- n2 )                 \ CORE EXT
;               Discard the second stack item.
;
;   : NIP       SWAP DROP ;
;
;                 $COLON  NameNIP,NIP
;                 DW      SWAP,DROP,EXIT

                $CODE   NameNIP,NIP
                POP     AX
                $NEXT

;   PARSE       ( char "ccc<char>"-- c-addr u )         \ CORE EXT
;               Scan input stream and return counted string delimited by char.
;
;   : PARSE     >R  SOURCE >IN @ /STRING        \ c-addr u  R: char
;               DUP IF
;                  OVER CHARS + OVER       \ c-addr c-addr+u c-addr  R: char
;                  BEGIN  DUP C@ R@ XOR
;                  WHILE  CHAR+ 2DUP =
;                  UNTIL  DROP OVER - 1chars/ DUP
;                  ELSE   NIP  OVER - 1chars/ DUP CHAR+
;                  THEN   >IN +!
;               THEN   R> DROP EXIT ;
;
;                 $COLON  5,'PARSE',PARSE,_FLINK
;                 DW      ToR,SOURCE,DoLIT,AddrToIN,Fetch,SlashSTRING
;                 DW      DUPP,ZBranch,PARSE4
;                 DW      OVER,CHARS,Plus,OVER
; PARSE1:         DW      DUPP,CFetch,RFetch,XORR,ZBranch,PARSE3
;                 DW      CHARPlus,TwoDUP,Equals,ZBranch,PARSE1
; PARSE2:         DW      DROP,OVER,Minus,DUPP,OneCharsSlash,Branch,PARSE5
; PARSE3:         DW      NIP,OVER,Minus,DUPP,OneCharsSlash,CHARPlus
; PARSE5:         DW      DoLIT,AddrToIN,PlusStore
; PARSE4:         DW      RFrom,DROP,EXIT

                $CODE   NamePARSE,PARSE
                MOV     AH,BL
                MOV     DX,SI
                MOV     SI,AddrSourceVar+CELLL
                MOV     BX,AddrSourceVar
                MOV     CX,AddrToIN
                ADD     SI,CX
                SUB     BX,CX
                MOV     CX,SI
                PUSH    SI
                OR      BX,BX
                JZ      PARSE1
PARSE5:         LODSB
                CMP     AL,AH
                JE      PARSE4
                DEC     BX
                OR      BX,BX
                JNZ     PARSE5
                MOV     BX,SI
                SUB     SI,AddrSourceVar+CELLL
                SUB     BX,CX
                MOV     AddrToIN,SI
PARSE1:         MOV     SI,DX
                $NEXT
PARSE4:         MOV     BX,SI
                SUB     SI,AddrSourceVar+CELLL
                SUB     BX,CX
                DEC     BX
                MOV     AddrToIN,SI
                MOV     SI,DX
                $NEXT

;   QUIT        ( -- ) ( R: i*x -- )            \ CORE
;               Empty the return stack, store zero in SOURCE-ID, make the user
;               input device the input source, and start text interpreter.
;
;   : QUIT      BEGIN
;                 rp0 rp!  0 TO SOURCE-ID  POSTPONE [
;                 BEGIN CR REFILL DROP SPACE    \ REFILL returns always true
;                       ['] interpret CATCH ?DUP 0=
;                 WHILE STATE @ 0= IF .prompt THEN
;                 REPEAT
;                 DUP -1 XOR IF                                 \ ABORT
;                 DUP -2 = IF SPACE abort"msg 2@ TYPE    ELSE   \ ABORT"
;                 SPACE errWord 2@ TYPE
;                 SPACE [CHAR] ? EMIT SPACE
;                 DUP -1 -58 WITHIN IF ." Exeption # " . ELSE \ undefined exeption
;                 CELLS THROWMsgTbl + @ COUNT TYPE       THEN THEN THEN
;                 sp0 sp!
;               AGAIN ;

                $COLON  NameQUIT,QUIT
QUIT1:          DW      RPZero,RPStore,DoLIT,0,DoTO,AddrSOURCE_ID,LeftBracket
QUIT2:          DW      CR,REFILL,DROP,SPACE
                DW      DoLIT,Interpret,CATCH,QuestionDUP,ZeroEquals
                DW      ZBranch,QUIT3
                DW      DoLIT,AddrSTATE,Fetch,ZeroEquals,ZBranch,QUIT2
                DW      DotPrompt,Branch,QUIT2
QUIT3:          DW      DUPP,DoLIT,-1,XORR,ZBranch,QUIT5
                DW      DUPP,DoLIT,-2,Equals,ZBranch,QUIT4
                DW      SPACE,DoLIT,AddrAbortQMsg,TwoFetch,TYPEE,Branch,QUIT5
QUIT4:          DW      SPACE,DoLIT,AddrErrWord,TwoFetch,TYPEE
                DW      SPACE,DoLIT,'?',EMIT,SPACE
                DW      DUPP,DoLIT,-1,DoLIT,-58,WITHIN,ZBranch,QUIT7
                DW      DoLIT,QUITstr
                DW      COUNT,TYPEE,Dot,Branch,QUIT5
QUIT7:          DW      CELLS,DoLIT,AddrTHROWMsgTbl,Plus,Fetch,COUNT,TYPEE
QUIT5:          DW      SPZero,SPStore,Branch,QUIT1

;   REFILL      ( -- flag )                     \ CORE EXT
;               Attempt to fill the input buffer from the input source. Make
;               the result the input buffer, set >IN to zero, and return true
;               if successful. Return false if the input source is a string
;               from EVALUATE.
;
;   : REFILL    SOURCE-ID IF 0 EXIT THEN
;               memTop [ size-of-PAD CHARS ] LITERAL - DUP
;               size-of-PAD ACCEPT sourceVar 2!
;               0 >IN ! -1 ;

                $COLON  NameREFILL,REFILL
                DW      SOURCE_ID,ZBranch,REFIL1
                DW      DoLIT,0,EXIT
REFIL1:         DW      MemTop,DoLIT,0-PADSize*CHARR,Plus,DUPP
                DW      DoLIT,PADSize*CHARR,ACCEPT,DoLIT,AddrSourceVar,TwoStore
                DW      DoLIT,0,DoLIT,AddrToIN,Store,DoLIT,-1,EXIT

;   ROT         ( x1 x2 x3 -- x2 x3 x1 )        \ CORE
;               Rotate the top three data stack items.
;
;   : ROT       >R SWAP R> SWAP ;
;
;                 $COLON  NameROT,ROT
;                 DW      ToR,SWAP,RFrom,SWAP,EXIT

                 $CODE  NameROT,ROT
                 POP    AX
                 POP    CX
                 PUSH   AX
                 PUSH   BX
                 MOV    BX,CX
                 $NEXT

;   S>D         ( n -- d )                      \ CORE
;               Convert a single-cell number n to double-cell number.
;
;   : S>D       DUP 0< ;
;
;                 $COLON  NameSToD,SToD
;                 DW      DUPP,ZeroLess,EXIT

                $CODE   NameSToD,SToD
                PUSH    BX
                MOV     AX,BX
                CWD
                MOV     BX,DX
                $NEXT

;   SEARCH-WORDLIST     ( c-addr u wid -- 0 | xt 1 | xt -1)     \ SEARCH
;               Search word list for a match with the given name.
;               Return execution token and -1 or 1 ( IMMEDIATE) if found.
;               Return 0 if not found.
;
;   : SEARCH-WORDLIST
;               (search-wordlist) DUP IF NIP THEN ;

                $COLON  NameSEARCH_WORDLIST,SEARCH_WORDLIST
                DW      ParenSearch_Wordlist,DUPP,ZBranch,SRCHW1
                DW      NIP
SRCHW1:         DW      EXIT

;   SIGN        ( n -- )                        \ CORE
;               Add a minus sign to the numeric output string if n is negative.
;
;   : SIGN      0< IF [CHAR] - HOLD THEN ;

                $COLON  NameSIGN,SIGN
                DW      ZeroLess,ZBranch,SIGN1
                DW      DoLIT,'-',HOLD
SIGN1:          DW      EXIT

;                 $CODE   NameSIGN,SIGN
;                 OR      BX,BX
;                 JNS     SIGN1
;                 MOV     AL,'-'
;                 MOV     DI,AddrHLD
;                 DEC     DI
;                 MOV     AddrHLD,DI
;                 MOV     [DI],AL
; SIGN1:          POP     BX
;                 $NEXT

;   SOURCE      ( -- c-addr u )                 \ CORE
;               Return input buffer string.
;
;   : SOURCE    sourceVar 2@ ;

                $COLON  NameSOURCE,SOURCE
                DW      DoLIT,AddrSourceVar,TwoFetch,EXIT

;   SPACE       ( -- )                          \ CORE
;               Send the blank character to the output device.
;
;   : SPACE     32 EMIT ;
;
;                 $COLON  NameSPACE,SPACE
;                 DW      DoLIT,' ',EMIT,EXIT

                $CODE   NameSPACE,SPACE
                PUSH    BX
                MOV     BX,' '
                MOV     AX,AddrTickEMIT
                JMP     AX
                $ALIGN

;   STATE       ( -- a-addr )                   \ CORE
;               Return the address of a cell containing compilation-state flag
;               which is true in compilation state or false otherwise.

                $CONST  NameSTATE,STATE,AddrSTATE

;   THEN        Compilation: ( C: orig -- )     \ CORE
;               Run-time: ( -- )
;               Resolve the forward reference orig.
;
;   : THEN      xhere SWAP code! orig- ; COMPILE-ONLY IMMEDIATE

                $COLON  NameTHENN,THENN
                DW      XHere,SWAP,CodeStore,OrigMinus,EXIT

;   THROW       ( k*x n -- k*x | i*x n )        \ EXCEPTION
;               If n is not zero, pop the topmost exception frame from the
;               exception stack, along with everything on the return stack
;               above the frame. Then restore the condition before CATCH and
;               transfer control just after the CATCH that pushed that
;               exception frame.
;
;   : THROW     ?DUP
;               IF   throwFrame @ rp!   \ restore return stack
;                    R> throwFrame !    \ restore THROW frame
;                    R> SWAP >R sp!     \ restore data stack
;                    DROP R>
;                    'init-i/o EXECUTE
;               THEN ;

                $COLON  NameTHROW,THROW
                DW      QuestionDUP,ZBranch,THROW1
                DW      ThrowFrame,Fetch,RPStore,RFrom,ThrowFrame,Store
                DW      RFrom,SWAP,ToR,SPStore,DROP,RFrom
                DW      TickINIT_IO,EXECUTE
THROW1:         DW      EXIT

;   TYPE        ( c-addr u -- )                 \ CORE
;               Display the character string if u is greater than zero.
;
;   : TYPE      ?DUP IF 0 DO DUP C@ EMIT CHAR+ LOOP THEN DROP ;
;
;                 $COLON  NameTYPEE,TYPEE
;                 DW      QuestionDUP,ZBranch,TYPE2
;                 DW      DoLIT,0,DoDO
; TYPE1:          DW      DUPP,CFetch,EMIT,CHARPlus,DoLOOP,TYPE1
; TYPE2:          DW      DROP,EXIT

                $CODE   NameTYPEE,TYPEE
                POP     DI
                OR      BX,BX
                JZ      TYPE2
                PUSH    SI
                SUB     BP,CELLL
                MOV     [BP],BX
                MOV     BX,DI
TYPE4:          MOV     DI,BX
                XOR     BX,BX
                MOV     BL,[DI]
                INC     DI
                PUSH    DI
                MOV     SI,OFFSET TYPE3
                MOV     AX,AddrTickEMIT
                JMP     AX
TYPE1:          DEC     WORD PTR [BP]
                JNZ     TYPE4
                POP     SI
                ADD     BP,CELLL
TYPE2:          POP     BX
                $NEXT
TYPE3           DW      TYPE1

;   U<          ( u1 u2 -- flag )               \ CORE
;               Unsigned compare of top two items. True if u1 < u2.
;
;   : U<        2DUP XOR 0< IF NIP 0< EXIT THEN - 0< ;
;
;                 $COLON  NameULess,ULess
;                 DW      TwoDUP,XORR,ZeroLess
;                 DW      ZBranch,ULES1
;                 DW      NIP,ZeroLess,EXIT
; ULES1:          DW      Minus,ZeroLess,EXIT

                $CODE   NameULess,ULess
                POP     AX
                SUB     AX,BX
                MOV     BX,-1
                JB      ULES1
                INC     BX
ULES1:          $NEXT

;   UM*         ( u1 u2 -- ud )                 \ CORE
;               Unsigned multiply. Return double-cell product.
;
;   : UM*       0 SWAP cell-size-in-bits 0 DO
;                  DUP um+ >R >R DUP um+ R> +
;                  R> IF >R OVER um+ R> + THEN     \ if carry
;               LOOP ROT DROP ;
;
;                 $COLON  NameUMStar,UMStar
;                 DW      DoLIT,0,SWAP,DoLIT,CELLL*8,DoLIT,0,DoDO
; UMST1:          DW      DUPP,UMPlus,ToR,ToR
;                 DW      DUPP,UMPlus,RFrom,Plus,RFrom
;                 DW      ZBranch,UMST2
;                 DW      ToR,OVER,UMPlus,RFrom,Plus
; UMST2:          DW      DoLOOP,UMST1
;                 DW      ROT,DROP,EXIT

                $CODE   NameUMStar,UMStar
                POP     AX
                MUL     BX
                PUSH    AX
                MOV     BX,DX
                $NEXT

;   UM/MOD      ( ud u1 -- u2 u3 )              \ CORE
;               Unsigned division of a double-cell number ud by a single-cell
;               number u1. Return remainder u2 and quotient u3.
;
;   : UM/MOD    DUP 0= IF -10 THROW THEN        \ divide by zero
;               2DUP U< IF
;                  NEGATE cell-size-in-bits 0
;                  DO   >R DUP um+ >R >R DUP um+ R> + DUP
;                       R> R@ SWAP >R um+ R> OR
;                       IF >R DROP 1+ R> THEN
;                       ELSE DROP THEN
;                       R>
;                  LOOP DROP SWAP EXIT
;               ELSE -11 THROW          \ result out of range
;               THEN ;
;
;                 $COLON  NameUMSlashMOD,UMSlashMOD
;                 DW      DUPP,ZBranch,UMM5
;                 DW      TwoDUP,ULess,ZBranch,UMM4
;                 DW      NEGATE,DoLIT,CELLL*8,DoLIT,0,DoDO
; UMM1:           DW      ToR,DUPP,UMPlus,ToR,ToR,DUPP,UMPlus,RFrom,Plus,DUPP
;                 DW      RFrom,RFetch,SWAP,ToR,UMPlus,RFrom,ORR,ZBranch,UMM2
;                 DW      ToR,DROP,OnePlus,RFrom,Branch,UMM3
; UMM2:           DW      DROP
; UMM3:           DW      RFrom,DoLOOP,UMM1
;                 DW      DROP,SWAP,EXIT
; UMM5:           DW      DoLIT,-10,THROW
; UMM4:           DW      DoLIT,-11,THROW

                $CODE   NameUMSlashMOD,UMSlashMOD
                OR      BX,BX
                JZ      UMM1
                POP     DX
                CMP     DX,BX
                JAE     UMM2
                POP     AX
                DIV     BX
                PUSH    DX
                MOV     BX,AX
                $NEXT
UMM1:           MOV     BX,-10          ;divide by zero
                JMP     THROW
UMM2:           MOV     BX,-11          ;result out of range
                JMP     THROW
                $ALIGN

;   UNLOOP      ( -- ) ( R: loop-sys -- )       \ CORE
;               Discard loop-control parameters for the current nesting level.
;               An UNLOOP is required for each nesting level before the
;               definition may be EXITed.
;
;   : UNLOOP    R> R> R> 2DROP >R EXIT ;
;
;                 $COLON  NameUNLOOP,UNLOOP
;                 DW      RFrom,RFrom,RFrom,TwoDROP,ToR,EXIT

                $CODE   NameUNLOOP,UNLOOP
                ADD     BP,2*CELLL
                $NEXT

;   WITHIN      ( n1|u1 n2|n2 n3|u3 -- flag )   \ CORE EXT
;               Return true if (n2|u2<=n1|u1 and n1|u1<n3|u3) or
;               (n2|u2>n3|u3 and (n2|u2<=n1|u1 or n1|u1<n3|u3)).
;
;   : WITHIN    OVER - >R - R> U< ;
;
;                 $COLON  NameWITHIN,WITHIN
;                 DW      OVER,Minus,ToR                  ;ul <= u < uh
;                 DW      Minus,RFrom,ULess,EXIT

                $CODE   NameWITHIN,WITHIN
                POP     AX
                SUB     BX,AX
                POP     DX
                SUB     DX,AX
                CMP     DX,BX
                MOV     BX,-1
                JB      WITHIN1
                INC     BX
WITHIN1:        $NEXT

;   [           ( -- )                          \ CORE
;               Enter interpretation state.
;
;   : [         0 STATE ! ; COMPILE-ONLY IMMEDIATE

                $COLON  NameLeftBracket,LeftBracket
                DW      DoLIT,0,DoLIT,AddrSTATE,Store,EXIT

;   ]           ( -- )                          \ CORE
;               Enter compilation state.
;
;   : ]         -1 STATE ! ;

                $COLON  NameRightBracket,RightBracket
                DW      DoLIT,-1,DoLIT,AddrSTATE,Store,EXIT

;***************
; Rest of CORE words and two facility words, EKEY? and EMIT?
;***************
;       Following definitions can be removed from assembler source and
;       can be colon-defined later.

;   (           ( "ccc<)>" -- )                 \ CORE
;               Ignore following string up to next ) . A comment.
;
;   : (         [CHAR] ) PARSE 2DROP ;

                $COLON  NameParen,Paren
                DW      DoLIT,')',PARSE,TwoDROP,EXIT

;   *           ( n1|u1 n2|u2 -- n3|u3 )        \ CORE
;               Multiply n1|u1 by n2|u2 giving a single product.
;
;   : *         UM* DROP ;
;
;                 $COLON  NameStar,Star
;                 DW      UMStar,DROP,EXIT

                $CODE   NameStar,Star
                POP     AX
                IMUL    BX
                MOV     BX,AX
                $NEXT

;   */          ( n1 n2 n3 -- n4 )              \ CORE
;               Multiply n1 by n2 producing double-cell intermediate,
;               then divide it by n3. Return single-cell quotient.
;
;   : */        */MOD NIP ;

                $COLON  NameStarSlash,StarSlash
                DW      StarSlashMOD,NIP,EXIT

;   */MOD       ( n1 n2 n3 -- n4 n5 )           \ CORE
;               Multiply n1 by n2 producing double-cell intermediate,
;               then divide it by n3. Return single-cell remainder and
;               single-cell quotient.
;
;   : */MOD     >R M* R> FM/MOD ;
;
;                 $COLON  NameStarSlashMOD,StarSlashMOD
;                 DW      ToR,MStar,RFrom,FMSlashMOD,EXIT

                $CODE   NameStarSlashMOD,StarSlashMOD
                POP     AX
                POP     CX
                IMUL    CX
                PUSH    AX
                PUSH    DX
                JMP     FMSlashMOD
                $ALIGN

;   +LOOP       Compilation: ( C: do-sys -- )   \ CORE
;               Run-time: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;               Terminate a DO-+LOOP structure. Resolve the destination of all
;               unresolved occurences of LEAVE.
;               On execution add n to the loop index. If loop index did not
;               cross the boundary between loop_limit-1 and loop_limit,
;               continue execution at the beginning of the loop. Otherwise,
;               finish the loop.
;
;   : +LOOP     POSTPONE do+LOOP rake dosys- ; COMPILE-ONLY IMMEDIATE

                $COLON  NamePlusLOOP,PlusLOOP
                DW      DoLIT,DoPLOOP,COMPILEComma,rake,DoSysMinus,EXIT

;   ."          ( "ccc<">" -- )                 \ CORE
;               Run-time ( -- )
;               Compile an inline string literal to be typed out at run time.
;
;   : ."        [CHAR] " PARSE
;               ALIGN HERE DUP LITERAL
;               pack" ALIGNED TO HERE
;               POSTPONE COUNT POSTPONE TYPE ; COMPILE-ONLY IMMEDIATE

                $COLON  NameDotQuote,DotQuote
                DW      DoLIT,'"',PARSE
                DW      ALIGNN,HERE,DUPP,LITERAL
                DW      PackQuote,ALIGNED,DoTO,AddrHERE
                DW      DoLIT,COUNT,COMPILEComma,DoLIT,TYPEE,COMPILEComma,EXIT

;   2OVER       ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )      \ CORE
;               Copy cell pair x1 x2 to the top of the stack.
;
;   : 2OVER     >R >R 2DUP R> R> 2SWAP ;
;
;                 $COLON  NameTwoOVER,TwoOVER
;                 DW      ToR,ToR,TwoDUP,RFrom,RFrom,TwoSWAP,EXIT

                $CODE   NameTwoOVER,TwoOVER
                MOV     DI,SP
                PUSH    BX
                PUSH    [DI+2*CELLL]
                MOV     BX,[DI+CELLL]
                $NEXT

;   >BODY       ( xt -- a-addr )                \ CORE
;               Push data field address of CREATEd word.
;
;   : >BODY     ?call DUP IF                    \ code-addr xt2
;                   ['] doCREATE = IF           \ should be call-doCREATE
;                   CELL+ code@ EXIT
;               THEN THEN
;               -31 THROW ;             \ >BODY used on non-CREATEd definition

                $COLON  NameToBODY,ToBODY
                DW      QCall,DUPP,ZBranch,TBODY1
                DW      DoLIT,DoCREATE,Equals,ZBranch,TBODY1
                DW      CELLPlus,CodeFetch,EXIT
TBODY1:         DW      DoLIT,-31,THROW

;   ABORT"      ( "ccc<">" -- )                 \ EXCEPTION EXT
;               Run-time ( i*x x1 -- | i*x ) ( R: j*x -- | j*x )
;               Conditional abort with an error message.
;
;   : ABORT"    S" POSTPONE ROT
;               POSTPONE IF POSTPONE abort"msg POSTPONE 2!
;               -2 POSTPONE LITERAL POSTPONE THROW
;               POSTPONE ELSE POSTPONE 2DROP POSTPONE THEN
;               ;  COMPILE-ONLY IMMEDIATE

                $COLON  NameABORTQuote,ABORTQuote
                DW      SQuote,DoLIT,ROT,COMPILEComma
                DW      IFF,DoLIT,AbortQMsg,COMPILEComma ; IF is immediate
                DW      DoLIT,TwoStore,COMPILEComma
                DW      DoLIT,-2,LITERAL                 ; LITERAL is immediate
                DW      DoLIT,THROW,COMPILEComma
                DW      ELSEE,DoLIT,TwoDROP,COMPILEComma ; ELSE and THEN are
                DW      THENN,EXIT                       ; immediate

;   ABS         ( n -- u )                      \ CORE
;               Return the absolute value of n.
;
;   : ABS       DUP 0< IF NEGATE THEN ;
;
;                 $COLON  NameABSS,ABSS
;                 DW      DUPP,ZeroLess,ZBranch,ABS1
;                 DW      NEGATE
; ABS1:           DW      EXIT

                $CODE   NameABSS,ABSS
                OR      BX,BX
                JNS     ABS1
                NEG     BX
ABS1:           $NEXT

;   ALLOT       ( n -- )                        \ CORE
;               Allocate n bytes in data space.
;
;   : ALLOT     HERE + TO HERE ;

                $COLON  NameALLOT,ALLOT
                DW      HERE,Plus,DoTO,AddrHERE,EXIT

;   BEGIN       ( C: -- dest )                  \ CORE
;               Start an infinite or indefinite loop structure. Put the next
;               location for a transfer of control, dest, onto the data
;               control stack.
;
;   : BEGIN     xhere dest+ ; COMPILE-ONLY IMMDEDIATE

                $COLON  NameBEGIN,BEGIN
                DW      XHere,DestPlus,EXIT

;   C,          ( char -- )                     \ CORE
;               Compile a character into data space.
;
;   : C,        HERE C!  HERE CHAR+ TO HERE ;
;
;                 $COLON  NameCComma,CComma
;                 DW      HERE,CStore,HERE,CHARPlus,DoTO,AddrHERE,EXIT

                $CODE   NameCComma,CComma
                MOV     DI,AddrHERE
                MOV     [DI],BL
                INC     DI
                MOV     AddrHERE,DI
                POP     BX
                $NEXT

;   CHAR        ( "<spaces>ccc" -- char )       \ CORE
;               Parse next word and return the value of first character.
;
;   : CHAR      PARSE-WORD DROP C@ ;

                $COLON  NameCHAR,CHAR
                DW      PARSE_WORD,DROP,CFetch,EXIT

;   DO          Compilation: ( C: -- do-sys )   \ CORE
;               Run-time: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
;               Start a DO-LOOP structure in a colon definition. Place do-sys
;               on control-flow stack, which will be resolved by LOOP or +LOOP.
;
;   : DO        0  POSTPONE doDO xhere dosys+   \ 0 for rake
;               ; COMPILE-ONLY IMMEDIATE

                $COLON  NameDO,DO
                DW      DoLIT,0,DoLIT,DoDO,COMPILEComma,XHere,DoSysPlus,EXIT

;   DOES>       ( C: colon-sys1 -- colon-sys2 ) \ CORE
;               Build run time code of the data object CREATEd.
;
;   : DOES>     bal @ IF -22 THROW THEN         \ control structure mismatch
;               0 bal !
;               POSTPONE pipe ['] doLIST xt, DROP ; COMPILE-ONLY IMMEDIATE

                $COLON  NameDOESGreater,DOESGreater
                DW      DoLIT,AddrBalance,Fetch,ZBranch,DOES1
                DW      DoLIT,-22,THROW
DOES1:          DW      DoLIT,0,DoLIT,AddrBalance,Store
                DW      DoLIT,Pipe,COMPILEComma
                DW      DoLIT,DoLIST,xtComma,DROP,EXIT

;   ELSE        Compilation: ( C: orig1 -- orig2 )      \ CORE
;               Run-time: ( -- )
;               Start the false clause in an IF-ELSE-THEN structure.
;               Put the location of new unresolved forward reference orig2
;               onto control-flow stack.
;
;   : ELSE      POSTPONE AHEAD SWAP POSTPONE THEN ; COMPILE-ONLY IMMDEDIATE

                $COLON  NameELSEE,ELSEE
                DW      AHEAD,SWAP,THENN,EXIT

;   ENVIRONMENT?   ( c-addr u -- false | i*x true )     \ CORE
;               Environment query.
;
;   : ENVIRONMENT?
;               envQList SEARCH-WORDLIST
;               DUP >R IF EXECUTE THEN R> ;

                $COLON  NameENVIRONMENTQuery,ENVIRONMENTQuery
                DW      DoLIT,AddrEnvQList,SEARCH_WORDLIST
                DW      DUPP,ToR,ZBranch,ENVRN1
                DW      EXECUTE
ENVRN1:         DW      RFrom,EXIT

;   EVALUATE    ( i*x c-addr u -- j*x )         \ CORE
;               Evaluate the string. Save the input source specification.
;               Store -1 in SOURCE-ID.
;
;   : EVALUATE  SOURCE >R >R >IN @ >R  SOURCE-ID >R
;               -1 TO SOURCE-ID
;               sourceVar 2!  0 >IN !  interpret
;               R> TO SOURCE-ID
;               R> >IN ! R> R> sourceVar 2! ;

                $COLON  NameEVALUATE,EVALUATE
                DW      SOURCE,ToR,ToR,DoLIT,AddrToIN,Fetch,ToR,SOURCE_ID,ToR
                DW      DoLIT,-1,DoTO,AddrSOURCE_ID
                DW      DoLIT,AddrSourceVar,TwoStore,DoLIT,0,DoLIT,AddrToIN,Store,Interpret
                DW      RFrom,DoTO,AddrSOURCE_ID
                DW      RFrom,DoLIT,AddrToIN,Store,RFrom,RFrom,DoLIT,AddrSourceVar,TwoStore,EXIT

;   FILL        ( c-addr u char -- )            \ CORE
;               Store char in each of u consecutive characters of memory
;               beginning at c-addr.
;
;   : FILL      ROT ROT ?DUP IF 0 DO 2DUP C! CHAR+ LOOP THEN 2DROP ;
;
;                 $COLON  NameFILL,FILL
;                 DW      ROT,ROT,QuestionDUP,ZBranch,FILL2
;                 DW      DoLIT,0,DoDO
; FILL1:          DW      TwoDUP,CStore,CHARPlus,DoLOOP,FILL1
; FILL2:          DW      TwoDROP,EXIT

                $CODE   NameFILL,FILL
                POP     CX
                MOV     DX,SI
                POP     SI
                OR      CX,CX
                JZ      FILL1
                MOV     [SI],BL
                MOV     AX,DS
                MOV     ES,AX
                MOV     DI,SI
                DEC     CX
                INC     DI
                REP MOVSB
FILL1:          MOV     SI,DX
                POP     BX
                $NEXT

;   IMMEDIATE   ( -- )                          \ CORE
;               Make the most recent definition an immediate word.
;
;   : IMMEDIATE   lastName [ =immed ] LITERAL OVER @ OR SWAP ! ;

                $COLON  NameIMMEDIATE,IMMEDIATE
                DW      LastName,DoLIT,IMMED,OVER,Fetch,ORR,SWAP,Store,EXIT

;   J           ( -- n|u ) ( R: loop-sys -- loop-sys )  \ CORE
;               Push the index of next outer loop.
;
;   : J         rp@ [ 3 CELLS ] LITERAL + @
;               rp@ [ 4 CELLS ] LITERAL + @  +  ; COMPILE-ONLY
;
;                 $COLON  NameJ,J
;                 DW      RPFetch,DoLIT,3*CELLL,Plus,Fetch
;                 DW      RPFetch,DoLIT,4*CELLL,Plus,Fetch,Plus,EXIT

                $CODE   NameJ,J
                PUSH    BX
                MOV     BX,[BP+2*CELLL]
                ADD     BX,[BP+3*CELLL]
                $NEXT

;   LEAVE       ( -- ) ( R: loop-sys -- )       \ CORE
;               Terminate definite loop, DO|?DO  ... LOOP|+LOOP, immediately.
;
;   : LEAVE     POSTPONE UNLOOP POSTPONE branch
;               xhere rakeVar DUP @ code, ! ; COMPILE-ONLY IMMEDIATE

                $COLON  NameLEAVEE,LEAVEE
                DW      DoLIT,UNLOOP,COMPILEComma,DoLIT,Branch,COMPILEComma
                DW      XHere,DoLIT,AddrRakeVar,DUPP,Fetch,CodeComma,Store,EXIT

;   LOOP        Compilation: ( C: do-sys -- )   \ CORE
;               Run-time: ( -- ) ( R: loop-sys1 -- loop-sys2 )
;               Terminate a DO|?DO ... LOOP structure. Resolve the destination
;               of all unresolved occurences of LEAVE.
;
;   : LOOP      POSTPONE doLOOP rake dosys- ; COMPILE-ONLY IMMEDIATE

                $COLON  NameLOOPP,LOOPP
                DW      DoLIT,DoLOOP,COMPILEComma,rake,DoSysMinus,EXIT

;   LSHIFT      ( x1 u -- x2 )                  \ CORE
;               Perform a logical left shift of u bit-places on x1, giving x2.
;               Put 0 into the least significant bits vacated by the shift.
;
;   : LSHIFT    ?DUP IF 0 DO 2* LOOP THEN ;
;
;                 $COLON  NameLSHIFT,LSHIFT
;                 DW      QuestionDUP,ZBranch,LSHIFT2
;                 DW      DoLIT,0,DoDO
; LSHIFT1:        DW      TwoStar,DoLOOP,LSHIFT1
; LSHIFT2:        DW      EXIT

                $CODE   NameLSHIFT,LSHIFT
                MOV     CX,BX
                POP     BX
                OR      CX,CX
                JZ      LSHIFT2
                SHL     BX,CL
LSHIFT2:        $NEXT

;   M*          ( n1 n2 -- d )                  \ CORE
;               Signed multiply. Return double product.
;
;   : M*        2DUP XOR 0< >R ABS SWAP ABS UM* R> IF DNEGATE THEN ;
;
;                 $COLON  NameMStar,MStar
;                 DW      TwoDUP,XORR,ZeroLess,ToR,ABSS,SWAP,ABSS
;                 DW      UMStar,RFrom,ZBranch,MSTAR1
;                 DW      DNEGATE
; MSTAR1:         DW      EXIT

                $CODE   NameMStar,MStar
                POP     AX
                IMUL    BX
                PUSH    AX
                MOV     BX,DX
                $NEXT

;   MAX         ( n1 n2 -- n3 )                 \ CORE
;               Return the greater of two top stack items.
;
;   : MAX       2DUP < IF SWAP THEN DROP ;
;
;                 $COLON  NameMAX,MAX
;                 DW      TwoDUP,LessThan,ZBranch,MAX1
;                 DW      SWAP
; MAX1:           DW      DROP,EXIT

                $CODE   NameMAX,MAX
                POP     AX
                CMP     AX,BX
                JLE     MAX1
                MOV     BX,AX
MAX1:           $NEXT

;   MIN         ( n1 n2 -- n3 )                 \ CORE
;               Return the smaller of top two stack items.
;
;   : MIN       2DUP > IF SWAP THEN DROP ;
;
;                 $COLON  NameMIN,MIN
;                 DW      TwoDUP,GreaterThan,ZBranch,MIN1
;                 DW      SWAP
; MIN1:           DW      DROP,EXIT

                $CODE   NameMIN,MIN
                POP     AX
                CMP     AX,BX
                JGE     MIN1
                MOV     BX,AX
MIN1:           $NEXT

;   MOD         ( n1 n2 -- n3 )                 \ CORE
;               Divide n1 by n2, giving the single cell remainder n3.
;               Returns modulo of floored division in this implementation.
;
;   : MOD       /MOD DROP ;

                $COLON  NameMODD,MODD
                DW      SlashMOD,DROP,EXIT

;   POSTPONE    ( "<spaces>name" -- )           \ CORE
;               Parse name and find it. Append compilation semantics of name
;               to current definition.
;               Structure of words with special compilation action
;                   for default compilation behavior
;               |compile_xt|name_ptr| call-doCREATE | 0 or DOES>_xt | a-addr |
;
;   : POSTPONE  (') 0< IF
;                   specialComp? OVER = IF      \ special compilation action
;                       DUP POSTPONE LITERAL
;                       cell- cell- code@
;                       POSTPONE LITERAL
;                       POSTPONE EXECUTE EXIT  THEN
;                   POSTPONE LITERAL                    \ non-IMMEDIATE
;                   POSTPONE code, EXIT        THEN
;               code, ; COMPILE-ONLY IMMEDIATE          \ IMMEDIATE

                $COLON  NamePOSTPONE,POSTPONE
                DW      ParenTick,ZeroLess,ZBranch,POSTP1
                DW      SpecialCompQ,OVER,Equals,ZBranch,POSTP2
                DW      DUPP,LITERAL,CellMinus,CellMinus,CodeFetch
                DW      LITERAL,DoLIT,EXECUTE,CodeComma,EXIT
POSTP2:         DW      LITERAL,DoLIT,CodeComma
POSTP1:         DW      CodeComma,EXIT

;   RECURSE     ( -- )                          \ CORE
;               Append the execution semactics of the current definition to
;               the current definition.
;
;   : RECURSE   lastXT COMPILE, ; COMPILE-ONLY IMMEDIATE

                $COLON  NameRECURSE,RECURSE
                DW      LastXT,COMPILEComma,EXIT

;   REPEAT      ( C: orig dest -- )             \ CORE
;               Terminate a BEGIN-WHILE-REPEAT indefinite loop. Resolve
;               backward reference dest and forward reference orig.
;
;   : REPEAT    AGAIN THEN ; COMPILE-ONLY IMMEDIATE

                $COLON  NameREPEAT,REPEAT
                DW      AGAIN,THENN,EXIT

;   RSHIFT      ( x1 u -- x2 )                  \ CORE
;               Perform a logical right shift of u bit-places on x1, giving x2.
;               Put 0 into the most significant bits vacated by the shift.
;
;   : RSHIFT    ?DUP IF
;                       0 SWAP  cell-size-in-bits SWAP -
;                       0 DO  2DUP D+  LOOP
;                       NIP
;                    THEN ;
;
;                 $COLON  NameRSHIFT,RSHIFT
;                 DW      QuestionDUP,ZBranch,RSHIFT2
;                 DW      DoLIT,0,SWAP,DoLIT,CELLL*8,SWAP,Minus,DoLIT,0,DoDO
; RSHIFT1:        DW      TwoDUP,DPlus,DoLOOP,RSHIFT1
;                 DW      NIP
; RSHIFT2:        DW      EXIT

                $CODE   NameRSHIFT,RSHIFT
                MOV     CX,BX
                POP     BX
                OR      CX,CX
                JZ      RSHIFT2
                SHR     BX,CL
RSHIFT2:        $NEXT

;   SLITERAL    ( c-addr1 u -- )                \ STRING
;               Run-time ( -- c-addr2 u )
;               Compile a string literal. Return the string on execution.
;
;   : SLITERAL   ALIGN HERE DUP LITERAL
;                pack" ALIGNED TO HERE
;                POSTPONE COUNT ; COMPILE-ONLY IMMEDIATE

                $COLON  NameSLITERAL,SLITERAL
                DW      ALIGNN,HERE,DUPP,LITERAL
                DW      PackQuote,ALIGNED,DoTO,AddrHERE
                DW      DoLIT,COUNT,COMPILEComma,EXIT

;   S"          Compilation: ( "ccc<">" -- )    \ CORE
;               Run-time: ( -- c-addr u )
;               Parse ccc delimetered by " . Return the string specification
;               c-addr u on execution.
;
;   : S"        [CHAR] " PARSE
;               STATE @ IF POSTPONE SLITERAL EXIT THEN
;               CR ." Use of S" in interpretation state is non-portable."
;               CR ." Use  CHAR " PARSE string" or BL PARSE word  instead."
;               ; IMMEDIATE

                $COLON  NameSQuote,SQuote
                DW      DoLIT,'"',PARSE,DoLIT,AddrSTATE,Fetch,ZBranch,SQUOT1
                DW      SLITERAL,EXIT
SQUOT1:         DW      CR
                DW      DoLIT,SQUOTstr1
                DW      COUNT,TYPEE,CR
                DW      DoLIT,SQUOTstr2
                DW      COUNT,TYPEE,EXIT

;   SM/REM      ( d n1 -- n2 n3 )               \ CORE
;               Symmetric divide of double by single. Return remainder n2
;               and quotient n3.
;
;   : SM/REM    OVER >R >R DUP 0< IF DNEGATE THEN
;               R@ ABS UM/MOD DUP 0<
;               IF DUP 08000h XOR IF -11 THROW THEN THEN \ result out of range
;               R> R@ XOR 0< IF NEGATE THEN
;               R> 0< IF SWAP NEGATE SWAP THEN ;
;
;                 $COLON  6,'SM/REM',SMSlashREM,_FLINK
;                 DW      OVER,ToR,ToR,DUPP,ZeroLess,ZBranch,SMREM1
;                 DW      DNEGATE
; SMREM1:         DW      RFetch,ABSS,UMSlashMOD,DUPP,ZeroLess,ZBranch,SMREM4
;                 DW      DUPP,DoLIT,08000h,XORR,ZBranch,SMREM4
;                 DW      DoLIT,-11,THROW
; SMREM4:         DW      RFrom,RFetch,XORR,ZeroLess,ZBranch,SMREM2
;                 DW      NEGATE
; SMREM2:         DW      RFrom,ZeroLess,ZBranch,SMREM3
;                 DW      SWAP,NEGATE,SWAP
; SMREM3:         DW      EXIT

                $CODE   NameSMSlashREM,SMSlashREM
                POP     DX
                POP     AX
                OR      DX,DX
                JS      SMREM2
                OR      BX,BX
                JZ      SMREM1
                JS      SMREM3
                CMP     DX,BX
                JAE     SMREM6
                DIV     BX              ;positive dividend, positive divisor
                CMP     AX,08000h
                JA      SMREM6
                PUSH    DX
                MOV     BX,AX
                $NEXT
SMREM3:         NEG     BX              ;positive dividend, negative divisor
                CMP     DX,BX
                JAE     SMREM6
                DIV     BX
                CMP     AX,08000h
                JA      SMREM6
                MOV     BX,AX
                PUSH    DX
                NEG     BX
                $NEXT
SMREM2:         NEG     AX              ;DNEGATE
                ADC     DX,0
                NEG     DX
                OR      BX,BX
                JZ      SMREM1
                JS      SMREM4
                CMP     DX,BX           ;negative dividend, positive divisor
                JAE     SMREM6
                DIV     BX
                CMP     AX,08000h
                JA      SMREM6
                NEG     DX
                MOV     BX,AX
                PUSH    DX
                NEG     BX
                $NEXT
SMREM4:         NEG     BX              ;negative dividend, negative divisor
                CMP     DX,BX
                JAE     SMREM6
                DIV     BX
                CMP     AX,08000h
                JA      SMREM6
                NEG     DX
                MOV     BX,AX
                PUSH    DX
                $NEXT
SMREM6:         MOV     BX,-11          ;result out of range
                JMP     THROW
SMREM1:         MOV     BX,-10          ;divide by zero
                JMP     THROW
                $ALIGN

;   SPACES      ( n -- )                        \ CORE
;               Send n spaces to the output device if n is greater than zero.
;
;   : SPACES    ?DUP IF 0 DO SPACE LOOP THEN ;
;
;                 $COLON  NameSPACES,SPACES
;                 DW      QuestionDUP,ZBranch,SPACES2
;                 DW      DoLIT,0,DoDO
; SPACES1:        DW      SPACE,DoLOOP,SPACES1
; SPACES2:        DW      EXIT

                $CODE   NameSPACES,SPACES
                OR      BX,BX
                JZ      SPACES2
                PUSH    SI
                SUB     BP,CELLL
                MOV     [BP],BX
                MOV     BX,' '
SPACES4:        PUSH    BX
                MOV     SI,OFFSET SPACES3
                MOV     AX,AddrTickEMIT
                JMP     AX
SPACES1:        DEC     WORD PTR [BP]
                JNZ     SPACES4
                POP     SI
                ADD     BP,CELLL
SPACES2:        POP     BX
                $NEXT
SPACES3         DW      SPACES1

;   TO          Interpretation: ( x "<spaces>name" -- ) \ CORE EXT
;               Compilation:    ( "<spaces>name" -- )
;               Run-time:       ( x -- )
;               Store x in name.
;
;   : TO        ' ?call ?DUP IF         \ should be CALL
;                 ['] doVALUE =         \ verify VALUE marker
;                 IF code@ STATE @
;                    IF POSTPONE doTO code, EXIT THEN
;                    ! EXIT
;                    THEN THEN
;               -32 THROW ; IMMEDIATE   \ invalid name argument (e.g. TO xxx)

                $COLON  NameTO,TO
                DW      Tick,QCall,QuestionDUP,ZBranch,TO1
                DW      DoLIT,DoVALUE,Equals,ZBranch,TO1
                DW      CodeFetch,DoLIT,AddrSTATE,Fetch,ZBranch,TO2
                DW      DoLIT,DoTO,COMPILEComma,CodeComma,EXIT
TO2:            DW      Store,EXIT
TO1:            DW      DoLIT,-32,THROW

;   U.          ( u -- )                        \ CORE
;               Display u in free field format followed by space.
;
;   : U.        0 D. ;

                $COLON  NameUDot,UDot
                DW      DoLIT,0,DDot,EXIT

;   UNTIL       ( C: dest -- )                  \ CORE
;               Terminate a BEGIN-UNTIL indefinite loop structure.
;
;   : UNTIL     POSTPONE 0branch code, dest- ; COMPILE-ONLY IMMEDIATE

                $COLON  NameUNTIL,UNTIL
                DW      DoLIT,ZBranch,COMPILEComma,CodeComma,DestMinus,EXIT

;   VALUE       ( x "<spaces>name" -- )         \ CORE EXT
;               name Execution: ( -- x )
;               Create a value object with initial value x.
;
;   : VALUE     xhere ALIGNED CELL+ TO xhere
;               ['] doVALUE xt, head,
;               ALIGN HERE code,
;               , linkLast ; \ store x and link CREATEd word to current wordlist

                $COLON  NameVALUE,VALUE
                DW      XHere,ALIGNED,CELLPlus,DoTO,AddrXHere
                DW      DoLIT,DoVALUE,xtComma,HeadComma
                DW      ALIGNN,HERE,CodeComma
                DW      Comma,LinkLast,EXIT

;   VARIABLE    ( "<spaces>name" -- )           \ CORE
;               name Execution: ( -- a-addr )
;               Parse a name and create a variable with the name.
;               Resolve one cell of data space at an aligned address.
;               Return the address on execution.
;
;   : VARIABLE  xhere ALIGNED TO xhere
;               ['] compileCONST code,
;               xhere CELL+ TO xhere
;               ['] doCONST xt, head,
;               ALIGN HERE
;               1 CELLS ALLOT           \ allocate one cell in data space
;               code, linkLast
;               lastName [ =seman ] LITERAL OVER @ OR SWAP ! ;

                $COLON  NameVARIABLE,VARIABLE
                DW      XHere,ALIGNED,DoTO,AddrXHere
                DW      DoLIT,CompileCONST,CodeComma
                DW      XHere,CELLPlus,DoTO,AddrXHere
                DW      DoLIT,DoCONST,xtComma,HeadComma
                DW      ALIGNN,HERE,DoLIT,1*CELLL,ALLOT
                DW      CodeComma,LinkLast
                DW      LastName,DoLIT,SEMAN,OVER,Fetch,ORR,SWAP,Store,EXIT

;   WHILE       ( C: dest -- orig dest )        \ CORE
;               Put the location of a new unresolved forward reference orig
;               onto the control flow stack under the existing dest. Typically
;               used in BEGIN ... WHILE ... REPEAT structure.
;
;   : WHILE     POSTPONE IF SWAP ; COMPILE-ONLY IMMEDIATE

                $COLON  NameWHILE,WHILE
                DW      IFF,SWAP,EXIT

;   WORD        ( char "<chars>ccc<char>" -- c-addr )   \ CORE
;               Skip leading delimeters and parse a word. Return the address
;               of a transient region containing the word as counted string.
;
;   : WORD      skipPARSE HERE pack" DROP HERE ;

                $COLON  NameWORDD,WORDD
                DW      SkipPARSE,HERE,PackQuote,DROP,HERE,EXIT

;   [']         Compilation: ( "<spaces>name" -- )      \ CORE
;               Run-time: ( -- xt )
;               Parse name. Return the execution token of name on execution.
;
;   : [']       ' POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

                $COLON  NameBracketTick,BracketTick
                DW      Tick,LITERAL,EXIT

;   [CHAR]      Compilation: ( "<spaces>name" -- )      \ CORE
;               Run-time: ( -- char )
;               Parse name. Return the value of the first character of name
;               on execution.
;
;   : [CHAR]    CHAR POSTPONE LITERAL ; COMPILE-ONLY IMMEDIATE

                $COLON  NameBracketCHAR,BracketCHAR
                DW      CHAR,LITERAL,EXIT

;   \           ( "ccc<eol>" -- )               \ CORE EXT
;               Parse and discard the remainder of the parse area.
;
;   : \         SOURCE >IN ! DROP ; IMMEDIATE

                $COLON  NameBackslash,Backslash
                DW      SOURCE,DoLIT,AddrToIN,Store,DROP,EXIT

; Optional Facility words

;   EKEY?       ( -- flag )                     \ FACILITY EXT
;               If a keyboard event is available, return true.
;
;   : EKEY?     'ekey? EXECUTE ;
;
;                 $COLON  NameEKEYQuestion,EKEYQuestion
;                 DW      TickEKEYQ,EXECUTE,EXIT

                $CODE   NameEKEYQuestion,EKEYQuestion
                MOV     AX,AddrTickEKEYQ
                JMP     AX
                $ALIGN

;   EMIT?       ( -- flag )                     \ FACILITY EXT
;               flag is true if the user output device is ready to accept data
;               and the execution of EMIT in place of EMIT? would not have
;               suffered an indefinite delay. If device state is indeterminate,
;               flag is true.
;
;   : EMIT?     'emit? EXECUTE ;
;
;                 $COLON  NameEMITQuestion,EMITQuestion
;                 DW      TickEMITQ,EXECUTE,EXIT

                $CODE   NameEMITQuestion,EMITQuestion
                MOV     AX,AddrTickEMITQ
                JMP     AX
                $ALIGN

;===============================================================

CTOP            EQU     $-0             ;next available memory in dictionary

CODE    ENDS
END     ORIG
;===============================================================
