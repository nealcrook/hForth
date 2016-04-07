\ $Id: armtest.fth,v 1.4 1998/10/23 05:27:31 crook Exp $
\
\ test file to exercise all ARM op-codes in order to compare ARM's assembler
\ with my Forth ARM assembler
\ generated from armtest.s and armtest.lst
\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ using Gforth, invoke thus:
\ gforth asmarm.fth armtest.fth -e bye
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ 16-jan-1998 nac

\ todo: instructions commented with BUG
\       rest from the file
\       remaining instructions
\       method for testing instructions that are illegal (should be
\       caught by the assembler) - probably need to make the assembler
\       THROW then add a vectored CATCH handler that this test suite
\       can modify.
\       for all shifters, check that the illegal values are detected.

MARKER *armtest*
ALSO ASSEMBLER
CR .( +++ Start of armtest - marker is *armtest*) CR

\ set up vectors to dump code in DATA space
' , 'ASM,32 !
' HERE  'ASM. !
' ! 'ASM! !
' @ 'ASM@ !

INIT-ASM HEX

.( --- Simple branches) CR
10 10 10 MK-SYM-TABLE

\ forward branches don't get resolved yet.. so can't use zz
\ backward branches should get resolved straight away, so use zz.
01 L# b,                \ fw    EA000002 zz
02 L# cc b,             \ fw    3A000002 zz
03 L# eq b,             \ fw    0AFFFFFF zz
03 L. r0 r0 r0 orr,             E1800000 zz \ == NOP
01 L. r0 r0 r0 orr,             E1800000 zz \ == NOP
02 L. r0 r0 r0 orr,             E1800000 zz \ == NOP
01 L# b,                        EAFFFFFC zz
01 L# b,                        EAFFFFFB zz
05 L# bl,               \ fw    EB000002 zz
06 L# cc bl,            \ fw    3B000002 zz
07 L# eq bl,            \ fw    0BFFFFFF zz
07 L. r0 r0 r0 orr,             E1800000 zz \ == NOP
05 L. r0 r0 r0 orr,             E1800000 zz \ == NOP
06 L. r0 r0 r0 orr,             E1800000 zz \ == NOP
05 L# bl,                       EBFFFFFC zz
05 L# bl,                       EBFFFFFB zz
LOCAL-RESOLVED

.( --- SWI) CR
01 swi,                         EF000001 zz
340123 swi,                     EF340123 zz
01 eq swi,                      0F000001 zz

.( --- DP Format 1) CR
0 # r0 mov,                     E3A00000 zz
1 # r5 r3 add,                  E2853001 zz
3E8 # r7 cmp,                   E3570FFA zz
ff00 # r8 r9 bic,               E3C89CFF zz

.( --- DP Format 2) CR
r1 r2 mov,                      E1A02001 zz
r2 r3 r4 add,                   E0834002 zz
r9 r7 cmp,                      E1570009 zz

.( --- DP Format 3) CR
2 # lsr r0 r2 mov,              E1A02120 zz
2 # lsl r0 r2 mov,              E1A02100 zz
3 # lsl r5 r5 r9 add,           E0859185 zz
3 # lsl r5 r5 r9 rsb,           E0659185 zz
4 # lsr r8 r9 r10 sub,          E049A228 zz
r3 ror r4 r12 mov,              E1A0C374 zz

.( --- DP Shifter operands) CR
3c0000 # r2 mov,                E3A0270F zz
3 # r2 mov,                     E3A02003 zz
r6 r2 mov,                      E1A02006 zz
5 # lsl r7 r2 mov,              E1A02287 zz
r9 lsl r7 r2 mov,               E1A02917 zz
9 # asr r9 r2 mov,              E1A024C9 zz
rrx r12 r2 mov,                 E1A0206C zz
3c0000 # r3 r3 eq add,          0283370F zz
3 # r3 r3 eq adds,              02933003 zz
r6 r3 r4 add,                   E0834006 zz
5 # lsl r7 r3 r7 adds,          E0937287 zz
r9 lsl r7 r3 r8 add,            E0838917 zz

.( *** Expect assembler error for the next 1 -- reserved instruction) CR
9 # asr r9 r3 r9 nv adds,       F09394C9 zz
9 # asr r9 r3 r9 ne adds,       109394C9 zz
rrx r12 r3 r10 mi add,          4083A06C zz

.( --- Multiply instructions) CR
r1 r2 r4 mul,                   E0040192 zz
r1 r2 r4 muls,                  E0140192 zz
r3 r9 r8 r7 mla,                E0273998 zz
r3 r2 r8 r4 smull,              E0C84392 zz
r1 r0 r8 r6 umull,              E0886190 zz
r1 r0 r8 r5 umlal,              E0A85190 zz
\
.( --- Addressing modes for 32-bit load/store) CR
[ 5 # R5 ] R7 LDR,              E5957005 zz
[ -5 # R5 ] R7 EQ LDR,          05157005 zz
[ R6 R5 ] R7 NE LDR,            17957006 zz
[ -R6 R5 ] R7 CS LDR,           27157006 zz
[ 4 # LSR R6 R5 ] R7 HS LDR,    27957226 zz
[ 4 # LSR -R6 R5 ] R7 CC LDR,   37157226 zz
[ RRX R6 R5 ] R7 LO LDR,        37957066 zz
[ RRX -R6 R5 ] R7 MI LDR,       47157066 zz
![ 5 # R5 ] R7 PL LDR,          55B57005 zz
![ -5 # R5 ] R7 VS LDR,         65357005 zz
![ R6 R5 ] R7 VC LDR,           77B57006 zz
![ -R6 R5 ] R7 HI LDR,          87357006 zz
![ 4 # LSR R6 R5 ] R7 LS LDR,   97B57226 zz
![ 4 # LSR -R6 R5 ] R7 GE LDR,  A7357226 zz
![ RRX R6 R5 ] R7 LT LDR,       B7B57066 zz
![ RRX -R6 R5 ] R7 GT LDR,      C7357066 zz
E # [ R5 ] R7 LE LDR,           D495700E zz
-E # [ R5 ] R7 AL LDR,          E415700E zz
\ TODO
\   92 00000114         ;; reserved instruction (Rm=Rn with post-indexing)
\   93 00000114         ;; ldr r7, [r5], r5, lsr #4
\   94 00000114         ;; ldr r7, [r5], -r5, asr #4
\   95 00000114         ;; ldr r7, [r5], r5, rrx
4 # LSR R5 [ R4 ] R7 LDR,       E6947225 zz
4 # ASR -R5 [ R3 ] R7 LDR,      E6137245 zz
RRX R5 [ R2 ] R7 LDR,           E6927065 zz
4 # LSR R5 [ R2 ] R7 LDR,       E6927225 zz
4 # ASR -R5 [ R2 ] R7 LDR,      E6127245 zz
RRX R5 [ R2 ] R7 LDR,           E6927065 zz

.( --- One of each unsigned Word/Byte LDR/STR, fixed addressing mode) CR
[ R6 R5 ] R7 EQ LDR,            07957006 zz
[ R6 R5 ] R7 GT STR,            C7857006 zz
[ R6 R5 ] R7 LT LDRB,           B7D57006 zz
[ R6 R5 ] R7 NE STRB,           17C57006 zz
\ TODO make these generate an error (currently they just gen a bad opcode)
\  107 0000013C         ;; translate not allowed in pre-indexed form
\  108 0000013C         ;; ldreqt r7, [r5, r6]
\  109 0000013C         ;; strgtt r7, [r5, r6]
\  110 0000013C         ;; ldrltbt r7, [r5, r6]
\  111 0000013C         ;; strnebt r7, [r5, r6]
R6 [ R5 ] R7 EQ LDRT,           06B57006 zz
R6 [ R5 ] R7 GT STRT,           C6A57006 zz
R6 [ R5 ] R7 LT LDRBT,          B6F57006 zz
R6 [ R5 ] R7 NE STRBT,          16E57006 zz

\ this is like immediate of 0, pre-indexed, no write-back
[ R0 ] R1 LDR,                  E5901000 zz
[ 0 # R0 ] R1 LDR,              E5901000 zz

.( --- 16-bit signed and unsigned LDR) CR
[ R0 ] R1 LDRH,                 E1D010B0 zz
.( *** Expect assembler error for the next 1 - undefined effect) CR
\ destination same as written-back base
5 # [ R1 ] R1 LDRH,             E0D110B5 zz
5 # [ R1 ] R7 LDRH,             E0D170B5 zz
-9 # [ R2 ] R1 LDRH,            E05210B9 zz
R3 [ R11 ] R1 LDRH,             E09B10B3 zz
-R5 [ R4 ] R1 LDRH,             E01410B5 zz
[ 2 # R3 ] R8 LDRH,             E1D380B2 zz
[ -6 # R13 ] R12 LDRH,          E15DC0B6 zz
![ 2 # R3 ] R8 LDRH,            E1F380B2 zz
![ -6 # R13 ] R12 LDRH,         E17DC0B6 zz
[ R6 R5 ] R7 LDRH,              E19570B6 zz
[ -R6 R5 ] R7 LDRH,             E11570B6 zz
![ R6 R5 ] R7 LDRH,             E1B570B6 zz
![ -R6 R5 ] R7 LDRH,            E13570B6 zz
2 # [ R9 ] R3 LDRH,             E0D930B2 zz
[ R0 ] R1 LDRSH,                E1D010F0 zz
5 # [ R1 ] R7 LDRSH,            E0D170F5 zz
-9 # [ R2 ] R1 LDRSH,           E05210F9 zz
R3 [ R11 ] R1 LDRSH,            E09B10F3 zz
-R5 [ R4 ] R1 LDRSH,            E01410F5 zz
[ 2 # R3 ] R8 LDRSH,            E1D380F2 zz
[ -6 # R13 ] R12 LDRSH,         E15DC0F6 zz
![ 2 # R3 ] R8 LDRH,            E1F380B2 zz
![ -6 # R13 ] R12 LDRSH,        E17DC0F6 zz
[ R6 R5 ] R7 LDRSH,             E19570F6 zz
[ -R6 R5 ] R7 LDRSH,            E11570F6 zz
![ R6 R5 ] R7 LDRSH,            E1B570F6 zz
![ -R6 R5 ] R7 LDRSH,           E13570F6 zz
2 # [ R9 ] R3 LDRSH,            E0D930F2 zz

.( --- 8-bit signed LDR) CR
[ R0 ] R1 LDRSB,                E1D010D0 zz
5 # [ R1 ] R7 LDRSB,            E0D170D5 zz
-9 # [ R2 ] R1 LDRSB,           E05210D9 zz
R9 [ R3 ] R1 LDRSB,             E09310D9 zz
-R5 [ R4 ] R1 LDRSB,            E01410D5 zz
[ 2 # R3 ] R8 LDRSB,            E1D380D2 zz
[ -6 # R13 ] R12 LDRSB,         E15DC0D6 zz
![ 2 # R3 ] R8 LDRSB,           E1F380D2 zz
![ -6 # R13 ] R12 LDRSB,        E17DC0D6 zz
[ R6 R5 ] R7 LDRSB,             E19570D6 zz
[ -R6 R5 ] R7 LDRSB,            E11570D6 zz
![ R6 R5 ] R7 LDRSB,            E1B570D6 zz
![ -R6 R5 ] R7 LDRSB,           E13570D6 zz
2 # [ R9 ] R3 LDRSB,            E0D930D2 zz

.( --- 16-bit unsigned STR) CR
[ R0 ] R2 STRH,                 E1C020B0 zz
5 # [ R1 ] R7 STRH,             E0C170B5 zz
-9 # [ R2 ] R1 STRH,            E04210B9 zz
R3 [ R7 ] R1 STRH,              E08710B3 zz
-R5 [ R4 ] R1 STRH,             E00410B5 zz
[ 80 # R5 ] R2 STRH,            E1C528B0 zz
[ -8 # R6 ] R2 STRH,            E14620B8 zz
![ 80 # R7 ] R2 STRH,           E1E728B0 zz
![ -8 # R8 ] R2 STRH,           E16820B8 zz
[ R6 R5 ] R7 STRH,              E18570B6 zz
[ -R6 R5 ] R7 STRH,             E10570B6 zz
![ R6 R5 ] R7 STRH,             E1A570B6 zz
![ -R6 R5 ] R7 STRH,            E12570B6 zz
[ -R4 R7 ] R10 STRH,            E107A0B4 zz

.( --- Synonyms for hForth) CR
tos pushD,                      E52C9004 zz
R0 pushD,                       E52C0004 zz
R3 pushD,                       E52C3004 zz
fpc pushR,                      E52BA004 zz
R0 popR,                        E49B0004 zz
tos popD,                       E49C9004 zz
R1 popD,                        E49C1004 zz
NEXT,                           E49AF004 zz

.( --- =, syntax for MOV, MVN op-codes) CR
0 R0 =,                         E3A00000 zz
1 R1 =,                         E3A01001 zz
-1 R2 =,                        E3E02000 zz
0 # R4 MOV,                     E3A04000 zz
1 # R5 MOV,                     E3A05001 zz

\ ARM's assembler would generate a MVN op-code for the next one
\ but it's hard for us to do so.. we just generate a "can't express"
\ error.
\ -1 # R6 MOV,                  E3E06000 zz

\ ARM's assembler will always interchange MOV, MVN to get the value
\ you ask for into the register. This assembler takes you more
\ literally. The next instruction loads bit-invert of 0 == FFFF.FFFF
\ whereas ARM's assembler would use a MOV op-code and load 0
0 # R4 MVN,                     E3E04000 zz
\ The next instruction loads a bit-invert of 1 == FFFF.FFFE
\ whereas ARM's assembler would use a MOV op-code and load 1
1 # R5 MVN,                     E3E05001 zz
\ The next instruction causes a "can't express" again, because we
\ take the operand too literally. ARM's assembler would generate
\ MVN of 0, putting FFFF.FFFF into the register.
\ -1 # R6 MVN,                  E3E06000 zz
\ The solution to any confusion you feel about this is resolved by always
\ using the =, syntax to load literals.

FF R2 =,                        E3A020FF zz
FFFFFF00 R3 =,                  E3E030FF zz
3C0000 R2 =,                    E3A0270F zz
3 R10 =,                        E3A0A003 zz

\ The following two should both generate errors as the # is not wanted
\ FF # R1 =,
\ FFFF # R1 =,


\ TODO move MOV and MVN tests

.( --- =, syntax for Literal Pool accesses ) CR

0 LTORG-HEAD ! 20 LTORG

[ -4 # r15 ] r5 ldr,            E51F5004 zz \ example op-code

12345 r5 =,                     E51F5088 zz 12345 chklit
56789 r6 =,                     E51F6088 zz 56789 chklit
56789 r7 =,                     E51F708C zz 56789 chklit
ABCDE r8 =,                     E51F808C zz ABCDE chklit

\ now test the point where the pool becomes unreachable
F40 ALLOT
\ these will share with the first entry (the worst case) *until* the
\ literal pool is considered unreachable because the first entry is
\ unreachable. That should occur when the offset to the first entry reaches
\ 0xFFC. After that, we will not attempt to share entries but we *will* still
\ attempt to create entries in the current literal pool - which is possible
\ since the next free entry in the pool *is* reachable.
12345 r0 =,                     E51F0FD8 zz 12345 chklit
12345 r1 =,                     E51F1FDC zz
12345 r2 =,                     E51F2FE0 zz
12345 r3 =,                     E51F3FE4 zz
12345 r4 =,                     E51F4FE8 zz
12345 r5 =,                     E51F5FEC zz
12345 r6 =,                     E51F6FF0 zz
12345 r7 =,                     E51F7FF4 zz
12345 r8 =,                     E51F8FF8 zz
\ The next opcode contains an offset of FFC - it is the last value that can
\ share an entry in the literal pool
12345 r9 =,                     E51F9FFC zz 12345 chklit

\ The literal pool is no longer sharable so each use of it will allocate
\ a new entry. Each op-code moves xhere forward by 4 and the free entry
\ in the literal pool moves forward by 4, also. Therefore, unless we
\ take some other action, the offset in the op-code will remain constant
12345 r10 =,                    E51FAFF4 zz 12345 chklit
4 ALLOT
12345 r11 =,                    E51FBFF8 zz
4 ALLOT
12345 r12 =,                    E51FCFFC zz

\ That was the last reachable entry in the literal pool
\ TODO - want a recoverable error handler.
\ .( *** Expect a fatal error now - literal pool unreachable )
\ 4 ALLOT
\ 12345 r13 =,

LTORG-STATS
SYM-STATS

PREVIOUS
CR .( +++ End of armtest) CR
