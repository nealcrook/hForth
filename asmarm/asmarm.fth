\ ARM assembler
\ some parts and lots of ideas are thanks to hForth asm8086
\ 16-jan-1998 resume after a long break

\ $Id$
\ $Log$
\ Revision 1.1  1998/06/07 22:55:01  crook
\ Initial revision
\

\ Instruction groups TODO:
\ ========================
\ coprocessor data processing
\ coprocessor register transfers
\ coprocessor load and store

\ TODO: macros for access to internal (?forth) structure
\ TODO: synonyms for NOP etc.
\ TODO: has an environmental dependency on 32-bit word, should check for this.
\ TODO: support for Thumb mode
\ TODO: ability to express decimal hex and ascii easily
\ TODO: could do the whole thing as a parser/state-machine in which the
\ TODO: action of each token is to check a current state and move to a
\ TODO: next state whilst building part of the op-code.
\ TODO: pseudo-ops to emit words, bytes, strings 
\ TODO: user documentation
\ TODO: error-handling stance that allows stop or continue on error.

\ TODO: can use this assembler in 2 ways: as an assembler within an hForth
\ environment, or as a cross-assembler within a meta-compiling environment.
\ need to add a conditional to cover these cases to allow the :CODE stuff
\ to work in each case.

\ ******************* Environment
MARKER *ASMARM*
BASE @ HEX

\ Vector to a routine to emit the 32-bit value at tos at the current PC
VARIABLE 'ASM,32	: ASM,32 'ASM,32 @ EXECUTE ;
\ default action is that code is assembled for the local (host) system
\ and emitted in host code space using COMPILE,
' COMPILE, 'ASM,32 !

\ Vector to a routine to return the current value of "dot" - the point at
\ which (assembler) code is emitted
VARIABLE 'ASM.		: ASM. 'ASM. @ EXECUTE ;
\ default action is that code is emitted using COMPILE, so the PC is
\ given by HERE .. TODO: not really true: HERE points to data space
\ and I want something to emit in *code* space.. for hForth this would be
\ xhere
' HERE 'ASM. !

\ Vector to a routine to do a 32-bit store to an address in the target
\ image
VARIABLE 'ASM!		: ASM! 'ASM! @ EXECUTE ;
\ default action is to do a 32-bit store in host space
' ! 'ASM! !

\ Vector to a routine to do a 32-bit fetch from an address in the target
\ image
VARIABLE 'ASM@
\ default action is to do a 32-bit fetch in host space
' @ 'ASM@ !		: ASM@ 'ASM@ @ EXECUTE ;

\ TODO word for determining the host system.
: hForth 0 ;

\ TODO ram/rom@ if appropriate model

GET-CURRENT  GET-ORDER

\ In hForth, WORDLIST-NAME associates the name with the wid such that ORDER
\ can display it.
hForth INVERT [IF] : WORDLIST-NAME CONSTANT ; [THEN]

WORDLIST WORDLIST-NAME ASSEMBLER-WORDLIST
: ASSEMBLER GET-ORDER NIP ASSEMBLER-WORDLIST SWAP SET-ORDER ;

ALSO ASSEMBLER DEFINITIONS

\ ******************* Redefine standard words
\ the strings used for some standard words are required in the assembler
\ so provide aliases to make them accessible from within the assembler
: [[ POSTPONE [ ; IMMEDIATE    hForth [IF] COMPILE-ONLY [THEN]
: ]] ] ;
: ## # ;

\ ******************* create data structures
\ TODO Maybe re-assign the flag bits into more
\ TODO variables and give them names rather than accessing them off this-op
\ TODO combine the separate flags into a state nibble and add some words
\ TODO to check for legal state rather than doing all that bit twiddling.

VARIABLE oip \ opcode-in-progress
VARIABLE reg_fifo \ register FIFO - see register-handling description
VARIABLE adr_mod \ current addressing mode
VARIABLE this-op \ flags and register map
VARIABLE LTORG-HEAD \ literal pool control
0 LTORG-HEAD !	\ once only - when the assembler is loaded.

: newop
	E0000000 oip ! \ default condition => always
	80000 this-op ! \ guard bit protects against register underflow
	0 adr_mod ! ; \ TODO -- not currently used
	\ no need to init register FIFO

\ Format of the bits in this-op:
\ b31	- 1 => that an immediate is available. If b28 is set, the immediate
\	  has already been consumed. Otherwise it is still on the stack.
\ b30	- 1 => that [ has been encountered for a LDR/STR
\ b29	- 1 => that the [ is a ![ (b30 will also be set)
\ b28.	- 1 => that a shifter has been encountered and that bits 11:4 of oip
\	  hold a valid shifter
\ b27   - 1 => that a register list is being/has been defined
\ b26	- 1 => that ] has been encountered for a LDR/STR
\ b25   - 1 => that a -Rn (subtract register) was encountered
\ TODO have -Rn set b25 in iop directly.. keep the flag for error checking.
\ b24.	- when [ is encountered, set this bit if an immediate or a shifter
\         or a register are already available => post indexed addressing
\ b23   - set when an unresolved label is encountered.
\ b22
\ b21
\ b20:16 - register count for registers pushed onto the FIFO. See register
\          handling description below.
\ b15:0 - register map of quoted registers. bN is set when rN is quoted.
\         See register handling description below.

\ ******************* error handling
\ parse a string and generate a counted string in data space. The strings are
\ tightly packed but inefficient to extract (have to link through all of them
\ in turn). This is fine in this application, since they are only used for
\ error messages.
: msg"
	HERE [CHAR] " PARSE
        DUP CHAR+ ALLOT \ leave space for string plus byte count
	ROT \ from length HERE
	OVER OVER C! \ store length
	1+ SWAP \ from HERE+1 length
	MOVE ;


CREATE AsmMsg$
msg" ^^^ - ASMARM Error: "
msg" Wrong number of registers."
msg" Register write-back is only available in LDM/STM." \ 2
msg" -Rn encountered multiple times or at illegal place."
msg" Offset too large: >24 bits." \ 4
msg" Too many immediates."
msg" Multiple shifters." \ 6
msg" Immediate too large: >5 bits."
msg" Shifter with immediate and reg or >1 reg." \ 8
msg" Multiple shifters or unexpected arguments for RRX."
msg" 32-bit operand can't be expressed." \ 0A
msg" Can't define an LDM/STM register list here."
msg" LDM/STM register list was not in progress." \ 0C
msg" Illegal characters in LDM/STM register list."
msg" LDM/STM without register list." \ 0E
msg" Found [ after [ or { or ]."
msg" Unexpected tokens within [ ] during LDR/STR." \ 10
msg" Can't define a register list in LDR/STR word/byte."
msg" Explicit post-indexed write-back is illegal." \ 12
msg" Immediate too large: >12 bits."
msg" Can't define a shifter or register list in LDR/STR half-word/signed byte."
msg" Immediate too large: >8 bits."
msg" Bad addressing mode for SWAP." \ 16
msg" Failed to find [ ] in SWAP addressing mode."
msg" Cannot generate zero-size puddle for literal pool." \ 18
msg" Need a literal pool but none has been defined."
msg" Literal pool is full." \ 1A
msg" Fatal internal error - bad literal pool offset."
msg" Label number out of range." \ 1C
msg" Unresolved forward references table is full."
msg" Label value is already defined." \ 1E
msg" Unresolved labels - use DUMP-SYM-TABLE for details."
msg" Cannot use unresolved label as an immediate." \ 20
ALIGN

\ print the nth message from the array of error strings
: .AsmMsg$
	AsmMsg$ SWAP
	BEGIN
		DUP
	WHILE
		1- SWAP DUP C@ 1+ CHARS + SWAP
	REPEAT
	DROP DUP CHAR+ SWAP C@ TYPE ;

: huh
	CR SOURCE TYPE CR 0 .AsmMsg$ .AsmMsg$ newop ABORT ;

\ ******************* words for creating and handling registers
\ The act of quoting a register r0 - r15 is twofold. Firstly, it pushes the
\ number of the quoted register into a FIFO. Secondly, it sets an associated
\ bit in a bit-map at this-op. The bit-map is used for LDM/STM instructions
\ (where register ordering is irrelevant) the FIFO is used for everything else.
\
\ Since no instruction requires more than 4 registers, the FIFO is implemented
\ by shifting nibbles into a CELL (assumed to be a 32-bit value) at
\ this-op+CELL+CELL. Nibbles are inserted from the least-significant end.
\
\ A 5-bit field in this-op records the number of registers stored in the FIFO.
\ The 5-bit field is initialised to 0b01000. When a register is quoted the
\ value is blindly incremented and ANDed with 0b01111. This is fast and
\ prevents carry from corrupting other bits in this-op.
\
\ Registers are pulled off the top of the FIFO using 'oldreg'.
\ When a register is pulled, the register count is blindly decremented. The
\ maximum number of registers than can be pulled is 4 so the 0b01000 default
\ value will prevent an insufficient number of registers from causing borrow
\ in this-op. When the op-code is emitted, there is a check to see that the
\ final value in the field is 0b01000; that is the only test that the user
\ supplied the correct number of registers for the op. There are a few
\ error cases that would not be spotted by this scheme but it's pretty good
\ and it avoids lots of checking all over the place.
\
\ Note the special behaviour of LDM/STM -- when the register list is quoted it
\ has the effect of re-initialising the register count, so that the register
\ that is used as the index for the store is subsequently shown as the only
\ register present and can be accessed using oldreg.

\ run-time routine used by makereg makereg! makereg-
\ for a given register, push that register onto the FIFO and set the
\ associated bit in the register bitmap.
: reg ( u -- )
	1 OVER LSHIFT	\ reg-value reg-bit
	this-op @ 10000 + FFEFFFFF AND \ blindly increment but prevent carry
	OR this-op ! \ and set reg bit in map.. then store
	reg_fifo DUP @ 4 LSHIFT ROT OR SWAP ! ; \ push into register FIFO

\ A builder for registers.
: makereg ( u -- )
	CREATE , DOES> @ reg ;

\ A builder for registers with write-back. These are only available for
\ LDM/STM instructions. They have all the functionality of makereg with the
\ addition that they check this-op to see that a LDM/STM is in progress and
\ then set the W bit (bit 21) in oip
: makereg! ( u -- )
	CREATE , DOES> @ reg
	this-op @ 08000000 AND 0=
	IF 2 huh THEN oip DUP @ 200000 OR SWAP ! ;
\ TODO should this check that no other register had write-back specified?
\ ie that the bit in oip wasn't already set

\ A builder for registers that are subtracted. These are only available in
\ LDR/STR addressing modes. They have all the functionality of makereg with the
\ addition that they check this-op to see that this is the first -Rn to be
\ specified and that nothing else conflicting has gone before.
\ When we generate code we assume that the - was on the *correct* register
: makereg- ( u -- )
	CREATE , DOES> @ reg
	this-op @ 0E000000 AND
	IF 3 huh THEN this-op DUP @ 02000000 OR SWAP ! ;

0 makereg R0		0 makereg! R0!   	0 makereg- -R0  
1 makereg R1		1 makereg! R1!   	1 makereg- -R1  
2 makereg R2		2 makereg! R2!   	2 makereg- -R2  
3 makereg R3		3 makereg! R3!   	3 makereg- -R3  
4 makereg R4		4 makereg! R4!   	4 makereg- -R4  
5 makereg R5		5 makereg! R5!   	5 makereg- -R5  
6 makereg R6		6 makereg! R6!   	6 makereg- -R6  
7 makereg R7		7 makereg! R7!   	7 makereg- -R7  
8 makereg R8		8 makereg! R8!   	8 makereg- -R8  
9 makereg R9		9 makereg! R9!   	9 makereg- -R9  
A makereg R10		A makereg! R10!  	A makereg- -R10 
B makereg R11		B makereg! R11!  	B makereg- -R11 
C makereg R12		C makereg! R12!  	C makereg- -R12 
D makereg R13		D makereg! R13!  	D makereg- -R13 
E makereg R14		E makereg! R14!  	E makereg- -R14 
E makereg LR		E makereg! LR!   	E makereg- -LR  
F makereg R15					F makereg- -R15 
F makereg PC					F makereg- -PC  

\ use of r15 is UNPREDICTABLE for makereg! and therefore not defined

\ supply registers, oldest first. The parameter on the stack indicates
\ how far to shift the register field on the returned value (to put it in the
\ correct position to OR into the op-code). We can safely underflow as long as
\ we check, eventually.
: oldreg ( n1 -- n2 )
	this-op @ 10000 - DUP this-op !	\ decrement register count
	70000 AND E RSHIFT		\ where to grab the register from
	reg_fifo @ SWAP RSHIFT F AND SWAP LSHIFT ;

\ check that all registers were consumed by the op-code generation. LDM/STM
\ instructions use a trick to make sure the register count will be correct
\ here ( } clears down the register count at the end of a register list)
: noreg ( -- )
	this-op @ 1F0000 AND 80000 <> IF 1 huh THEN ;

\ ******************* create conditionals
\ quoting the conditional sets the correct bits in oip. By default oip
\ contains AL, so we clear that out first.
: makecc ( u -- )
	1C LSHIFT CREATE , DOES> @
	oip @ 0FFFFFFF AND 	\ clear out any existing condition
	OR oip ! ;

0 	makecc EQ	1 	makecc NE
2	makecc CS	2	makecc HS \ synonyms
3	makecc CC	3	makecc LO \ synonyms
4 	makecc MI	5 	makecc PL
6 	makecc VS	7 	makecc VC
8 	makecc HI	9 	makecc LS
A 	makecc GE	B 	makecc LT
C 	makecc GT	D 	makecc LE
E 	makecc AL	F 	makecc NV

\ ******************* track the addressing mode.
\ the addressing modes are:
\ 0 - register/register transfers. The default when a new instruction starts
\ 1 - data-processing
\ 2 - load/store word/unsigned byte (3 encoding types)
\ 3 - load/store halfword/signed byte (2 encoding types)
\ 4 - load/store multiple
\ 5 - co-processor
\
\ TODO not sure if this is useful .. it needs to remove complexity from
\ the fields in this-op

\ ******************* generate op-code
\ take part of the opcode and OR in the pre-build part to make the whole
\ thing. Check that all the registers have been used and clear down all the
\ flags for the opcode. Finally, emit the Dword for this instruction
: code-emit ( n -- )
	oip @ OR noreg newop ASM,32 ;

\ ******************* set condition codes
\ Multiply and some dp opcodes have variants that can set the condition
\ codes. Those variants call this word to set b20 in oip
: set-ccs
	oip DUP @ 100000 OR SWAP ! ;

\ ******************* initialisation
: init-asm
	newop ;

\ ******************* multiply instructions
\ TODO: want warning on illegal or unpredictable results
: mul3
	CREATE , DOES> @
		8 oldreg OR 0 oldreg OR 10 oldreg OR code-emit ;

: mul4a
	CREATE , DOES> @
		c oldreg OR 8 oldreg OR	0 oldreg OR 10 oldreg OR code-emit ;

: mul4b
	CREATE , DOES> @
		8 oldreg OR 0 oldreg OR	10 oldreg OR c oldreg OR code-emit ;

000090 mul3 MUL,		: MULS, set-ccs MUL, ;
200090 mul4a MLA,		: MLAS, set-ccs MLA, ;
800090 mul4b UMULL,		: UMULLS, set-ccs UMULL, ;
A00090 mul4b UMLAL,		: UMLALS, set-ccs UMLAL, ;
C00090 mul4b SMULL,		: SMULLS, set-ccs SMULL, ;
E00090 mul4b SMLAL,		: SMLALS, set-ccs SMLAL, ;

\ ******************* Software interrupt instruction
: SWI,  ( n -- )
	DUP FF000000 AND IF 4 huh ELSE
		F000000 OR code-emit
	THEN ;

\ ******************* Immediates
\ Immediates are used in operands for DP and Load/Store instructions. The
\ immediate operand is left on the stack, untouched. This word simply checks
\ and sets flags. The consumer of the operand is responsible for range
\ checking and coping with negative offsets.
: #
	this-op @ 80000000 AND IF 5 huh THEN
	this-op @   800000 AND IF 20 huh THEN
	this-op @ 80000000 OR this-op ! ; \ an immediate is now available

\ ******************* Shifter operands
\ The shifter operands LSL, LSR, ASR, ROR, RRX can be used in DP instructions
\ and in some LDR/STR instructions. The actions of the shifter operand are:
\ - check and consume its arguments
\ - generate bits 11:4 of the op-code in oip
\ - set the flag in this-op that says "bits 11:4 of a shifter are defined"
\
\ LSL, LSR, ASR, ROR are all handled in the same way. RRX is handled
\ differently because it has no arguments.
\
\ The I-bit (b25) of the op-code is a function of the shifter type, but its
\ polarity is inverted between DP op-codes and LDR/STR op-codes and so
\ it must be set down-stream (we don't know the op-code yet..).
\ For LDR/STR, I is calculated and merged in ldrstr. For DP, I is calculated
\ and merged in build-shft.
\
\ TODO Some things that downstream processing needs to check:
\ - some shifters allow 0-31, others allow 1-32 (coding for 0 means 32), need
\   to verify that the instructions are legal
\ - similarly, some shift values are illegal for ?? because it is used for
\   the RRX function
\ - any other checks?

: sh11-4
	CREATE , DOES> @ \ bits 6:5 of opcode
	\ expect no shifter currently defined, and *either* an immediate
	\ available *or* exactly one register available. 
	this-op @ DUP 10000000 AND IF 6 huh ELSE
		\ set the shifter-defined bit in case all turns out ok
		10000000 OR this-op !
		\ stack just holds shift op-code bits
		this-op @ 800F0000 AND \ look at immediate bit and reg count
		DUP 90000 = IF
			\ register shifter, exactly one register
			DROP
			\ set b4=> register shifter, get the register and
			\ merge in the op-code bits for LSL or whatever
			10 OR 8 oldreg OR oip @ OR oip !
		ELSE
			80080000 = IF
				\ immediate, no registers .. stack has
				\ <immediate> <shifter code>
				SWAP DUP FFFFFFE0 AND IF 7 huh ELSE
					7 LSHIFT OR oip @ OR oip !
				THEN
			ELSE 8 huh THEN
\ TODO or the error might be 	S" Shifter but neither immediate nor register" huh
		THEN
	THEN ;

00 sh11-4 LSL	20 sh11-4 LSR	40 sh11-4 ASR	60 sh11-4 ROR

\ Build bits 11-4 for RRX shift.
: RRX ( -- )
	\ expect no shifter currently defined, no registers and no immediate
	this-op @ DUP 90070000 AND IF \ TODO -- more rigorous check of # reg
		9 huh
	ELSE
		10000000 OR this-op ! \ show that a shifter is defined
		oip @ 60 OR oip ! \ make bits 11:4 of the oip indicate RRX
	THEN ;

\ ******************* Data-processing instructions
\ Attempt to convert a 32-bit immediate into an 8-bit immediate and 4-bit
\ shifter such that the number can be represented as imm >> 2*shft
\ this mode is available for dp operands, msr and some LDR/STR.
: 32to12 ( n -- n' -1 | 0)
	0 FFFFFF00	\ literal, index, initial shifter
	BEGIN
		DUP 3 PICK AND IF
			\ some bits still set so no match. Get ready for next
			DUP 2 RSHIFT 3FFFFFFF AND SWAP 1E LSHIFT OR
			SWAP 1+ SWAP
			OVER 10 =	\ true => failed to find 
		ELSE
			\ found a match. Stack holds num, index, shifter
			DROP
			SWAP OVER 10 SWAP - 2* RSHIFT SWAP 8 LSHIFT OR -1 EXIT
		THEN
	UNTIL
	\ number index shifter
	DROP DROP DROP 0 ;

\ Build a shifter-operand in bits 11:0 of oip or die in the attempt
\ legal combinations are (highest priority first)
\ -- b28 of this-op set (oip already has bits 11:4 built)
\ -- 24-bit immediate
\ -- register (a special case of shifter; LSL with a shift of 0) 

: build-shft ( n1 n2 | n2 -- ) \ n1 is immediate, n2 is op-code template
	this-op @ DUP 10000000 AND IF
		\ prebuilt shifter in 11:4 just need the register added in
		DROP oip DUP @ 0 oldreg OR SWAP !
	ELSE
		\ expect immediate or register
		80000000 AND IF
			\ immediate.. on the stack under the op-code template
			SWAP 32to12 \ convert it to offset:value format
			INVERT IF 0A huh THEN
			oip DUP @
			ROT OR \ merge the immediate into the opcode
			2000000 OR SWAP ! \ set I-bit to indicate immediate
		ELSE
			\ not immediate.. register?
			\ this is just a LSL with a shift of 0
			oip DUP @ 0 oldreg OR SWAP !
		THEN
	THEN ;

\ <shifter operand> <reg> {cond} <opcode>{s}
: dp1
	CREATE , DOES> @
	build-shft C oldreg OR code-emit ;

\ <shifter operand> <reg> {cond} <opcode>
: dp2
	CREATE , DOES> @
	build-shft 10 oldreg OR code-emit ;

\ <shifter operand> <reg> <reg> {cond} <opcode>{s}
: dp3
	CREATE , DOES> @
	build-shft 10 oldreg OR C oldreg OR code-emit ;

01A00000 dp1 MOV, 	: MOVS, set-ccs MOV, ;
01E00000 dp1 MVN,	: MVNS, set-ccs MVN, ;
01500000 dp2 CMP,
01700000 dp2 CMN,
01100000 dp2 TST,
01300000 dp2 TEQ,
00800000 dp3 ADD,	: ADDS, set-ccs ADD, ;
00A00000 dp3 ADC,	: ADCS, set-ccs ADC, ;
00400000 dp3 SUB,	: SUBS, set-ccs SUB, ;
00C00000 dp3 SBC,	: SBCS, set-ccs SBC, ;
00600000 dp3 RSB,	: RSBS, set-ccs RSB, ;
00E00000 dp3 RSC,	: RSCS, set-ccs RSC, ;
00000000 dp3 AND,	: ANDS, set-ccs AND, ;
00200000 dp3 EOR,	: EORS, set-ccs EOR, ;
01800000 dp3 ORR,	: ORRS, set-ccs ORR, ;
01C00000 dp3 BIC,	: BICS, set-ccs BIC, ;

\ ******************* Load/Store multiple
\ indicate start of register list. Check no other stuff in progress
\ and set flag to show that a register list is in progress.
\ TODO: other instructions should test that register-list-in-progress is
\ clear.
: {
	this-op @ F8000000 AND IF
		\ bits set => error because all those flags are mutually
		\ exclusive to our goal of defining a register list
		0B huh
	THEN
	this-op DUP @ 08000000 OR SWAP ! ;

\ start of register list; user-mode registers will be stored - set bit 22 in
\ the op-code to indicate this
: ^{ 
	oip DUP @ 400000 OR SWAP ! { ;

\ end of register list; check that a register list was in progress, build
\ part of the op-code from the information available and clear the register
\ count. This last action is a trick to make the next register quoted
\ (which should be the index for the LDM/STM) get stored properly in the FIFO.
: }
	this-op @ DUP 08000000 AND 0= IF 0C huh THEN
	F0000000 AND IF 0D huh THEN
	this-op @ FFE0FFFF AND 80000 OR DUP this-op ! \ re-init register count
	FFFF AND oip @ OR oip ! ; \ put register list into oip

\ The register write-back form (eg R5!) is only available in LDM/STM
\ instructions but is defined in the register-handling words section

\ All the op-codes have a similar form:
\ <register list> <reg> {cond} <opcode>
: ldmstm1
	CREATE , DOES> @ 10 oldreg OR \ add register to op template
	this-op @ 08000000 AND 0= IF 0E huh THEN
	\ syntax of register list itself is checked by }
	code-emit ;

\ these instructions have alternative names. The suffices of the fourth
\ column refer to increment/decrement before/after. Those of the last column
\ refer to full/empty ascending/descending (stack nomenclature)
08900000 DUP ldmstm1 LDMIA, ldmstm1 LDMFD,
08800000 DUP ldmstm1 STMIA, ldmstm1 STMEA,
09900000 DUP ldmstm1 LDMIB, ldmstm1 LDMED,
09800000 DUP ldmstm1 STMIB, ldmstm1 STMFA,
08100000 DUP ldmstm1 LDMDA, ldmstm1 LDMFA,
08000000 DUP ldmstm1 STMDA, ldmstm1 STMED,
09100000 DUP ldmstm1 LDMDB, ldmstm1 LDMEA,
09000000 DUP ldmstm1 STMDB, ldmstm1 STMFD,

\ ******************* Load/Store Word, half-word, byte
\ [ and ![ indicate that an addressing mode for a load/store instruction
\ is being used. At the time that the [ or ![ is encountered, there could
\ already be an immediate or shifter or register stored -- need to know this
\ because it differentiates pre and post indexing.
\ TODO: other instructions should test that LDR/STR-in-progress flags are
\ clear.
: [
	this-op @ 4C000000 AND IF
		0F huh \ [ or { or ] already encountered -> error
	THEN
	this-op @ 90000000 AND IF
		\ immediate or shifter are present so this is post indexing
		41000000
	ELSE
		this-op @ 000F0000 AND 90000 = IF
			\ 1 register already present so this is post indexing
			41000000
		ELSE
			40000000
		THEN
	THEN 
	this-op DUP @ ROT OR SWAP ! ;

: ![
\ TODO if shifter or immediate are already available it's a WARNING
\ - post-indexing *implies* write-back
	this-op DUP @ 20000000 OR SWAP ! [ ;

\ ] marks the end of stuff for LDR/STR addressing mode. The only legal things
\ after ] are the destination register and the op-code with conditional.
\ Several instruction formats use ] so the only thing
\ we can do here is check syntax
: ]
	this-op @ 4C000000 AND 40000000 <>
	\ check that [ has been encountered
	IF 10 huh THEN
	this-op DUP @ 04000000 OR SWAP ! ;

\ The negative register form (eg -R5) is only available in LDM/STM instructions
\ but is defined in the register-handling words section

\ Builders for the two forms of LDR/STR op-codes

\ <register list> <reg> {cond} <opcode> TODO
\ op-code form for 32-bit and unsigned byte loads and stores
: ldrstr
	CREATE , DOES> @
	this-op @ DUP 08000000 AND IF 11 huh ELSE
		900F0000 AND 000A0000 = IF
			\ no immediate or shifter and 2 registers => encode
			\ like an immediate of 0 with pre-indexing (could be
			\ post-indexing, but ARM assembler uses pre
			0 SWAP \ pretend the user set an immediate of 0
			this-op DUP @ 80000000 OR SWAP ! \ immediate pre-adr
		THEN
		\ use flags to generate P and W bits
		this-op @ 21000000 AND DUP 21000000 = IF 12 huh ELSE
			DUP 01000000 = IF
				DROP
				\ post-indexed, implicit write-back - opcode
				\ needs no modification
				ELSE 20000000 = IF
					01200000 OR \ pre-indexed writeback
				ELSE
					01000000 OR \ pre-indexed, no writeback
				THEN
			THEN
		THEN
		\ work out encoding type and build encoding-specific parts
		this-op @ 90000000 AND 90000000 = IF
			\ encoding 3 (scaled register offset/index)
			\ bits 11:4 are already formed in oip
			\ work out U bit and merge it in.. also set I bit
			this-op @ 02000000 AND
			IF 02000000 ELSE 02800000 THEN OR
			0 oldreg OR \ Rm
		ELSE
			this-op @ 80000000 AND IF
				\ encoding 1 (immediate, +/- 12-bit offset)
				SWAP DUP \ put op-code mask out of the way
				0< IF 0 ELSE 00800000 THEN SWAP \ U bit
				ABS DUP FFFFF000 AND IF 13 huh THEN
				\ mask U-bit immediate <- TOS
				OR OR \ add U bit and mask
			ELSE
				\ encoding 2 (register offset/index)
				\ work out U bit and merge it in.. also set I
				this-op @ 02000000 AND
				IF 02000000 ELSE 02800000 THEN OR
				0 oldreg OR \ Rm
			THEN
		THEN
		\ build common parts
		10 oldreg OR C oldreg OR this-op @ 20000000 AND
		IF 00200000 OR THEN \ ![ so set write-back bit
\ TODO check for illegal W, P combinations in op-code ?
\ TODO check for illegal register combinations?
		code-emit
	THEN
;

\ <register list> <reg> {cond} <opcode> TODO
\ op-code form for halfword and signed byte loads and stores.
: ldrstrhsb
	CREATE , DOES> @
	this-op @ DUP 18000000 AND IF 14 huh ELSE
		800F0000 AND 000A0000 = IF
			\ no immediate and 2 registers => encode like an
			\ immediate of 0 with pre-indexing (could be
			\ post-indexing, but ARM assembler uses pre
			0 SWAP \ pretend the user set an immediate of 0
			this-op DUP @ 80000000 OR SWAP ! \ immediate pre-adr
		THEN
		\ use flags to generate P bit
		this-op @ 01000000 AND IF 0 ELSE 01000000 THEN OR
		\ work out encoding type and build encoding-specific parts
		this-op @ 80000000 AND IF
			00400000 OR \ encoding 1 (immediate)
			SWAP DUP \ put op-code mask out of the way
			0< IF 0 ELSE 00800000 THEN SWAP \ U bit
			ABS DUP FFFFFF00 AND IF 15 huh THEN
			\ mask U-bit immediate <- TOS
			\ need to split the immediate into 2 nibbles
			\ 0F0 works-around pfe bug - pfe has a variable f0
			DUP F AND SWAP 0F0 AND 4 LSHIFT OR
			OR OR \ add U bit and mask
		ELSE
			\ encoding 2 (register)
			\ work out U bit and merge it in
			this-op @ 02000000 AND IF 0 ELSE 00800000 THEN OR
			0 oldreg OR \ Rm
		THEN
		\ build common parts
		10 oldreg OR C oldreg OR
		this-op @ 20000000 AND
		IF 00200000 OR THEN \ ![ so set write-back bit
\ TODO check for illegal W, P combinations in op-code
\ TODO check for illegal register combinations?
		code-emit
	THEN ;

04100000 ldrstr LDR,		04500000 ldrstr LDRB,
04300000 ldrstr LDRT,		04700000 ldrstr LDRBT,
04000000 ldrstr STR,		04400000 ldrstr STRB,
04200000 ldrstr STRT,		04600000 ldrstr STRBT,
001000B0 ldrstrhsb LDRH,	001000F0 ldrstrhsb LDRSH,
001000D0 ldrstrhsb LDRSB,	000000B0 ldrstrhsb STRH,
000000F0 ldrstrhsb STRSH,	000000D0 ldrstrhsb STRSB,

\ ******************* Semaphore instructions
: sem
	CREATE , DOES> @
	this-op @ DUP B9000000 AND
	IF 16 huh ELSE \ immediate, ![, shifter, reg list, -Rn are all bad 
		44000000 AND 44000000 = IF
			\ all looks OK so build it
			10 oldreg OR 0 oldreg OR C oldreg OR code-emit
		ELSE 17 huh THEN
	THEN ;

01000090 sem SWP,		01400090 sem SWPB,

\ ******************* Branch and Branch with Link
: bran
	CREATE , DOES> @
	\ either expect the destination address at tos or else the unresolved
	\ label bit to be set
	this-op @ 800000 AND IF
		0 \ unresolved label; set the offset to 0 for now
		\ - it will get back-patched when the label is resolved
	ELSE
		\ convert absolute destination address to 24-bit signed
		\ DWORD offset from . + 8
		SWAP ASM. - 8 - 2 RSHIFT DUP FF000000 AND
		DUP 3F000000 = SWAP 0= OR INVERT IF
			4 huh
		THEN
		FFFFFF AND
	THEN
	OR code-emit ;

0A000000 bran B,		0B000000 bran BL,

\ ******************* Branch and exchange instruction  set
: BX,
	0 oldreg 012FFF10 OR code-emit ;

\ ******************* Move immediate or register to status register
: sr-fields
	CREATE , DOES> @
	oip DUP @ OR SWAP ! ;

00480000 sr-fields SPSR_F		00080000 sr-fields CPSR_F
00410000 sr-fields SPSR_C		00010000 sr-fields CPSR_C
00450000 sr-fields SPSR_CF		00050000 sr-fields CPSR_CF
004F0000 sr-fields SPSR_CSXF		000F0000 sr-fields CPSR_CSXF
\ the last two are used for MRS,
00400000 sr-fields SPSR			00000000 sr-fields CPSR

\ TODO should really set a flag to show that a xPSR_xxxx occurred and
\ TODO then check for it and nothing else in MSR, and MRS, below.

: MSR,
	this-op @ 10000000 AND IF \ Encoding 1 (immediate)
		32to12 INVERT IF 0A huh THEN 0320F000 OR
	ELSE \ Encoding 2 (register)
		0 oldreg 0120F000 OR
	THEN code-emit ;

\ ******************* Move from status register
: MRS,
	C oldreg 010F0000 OR code-emit ;

\ ******************* Coprocessor instructions
\ TODO the real thing for these two builders
: cop-dp
	CREATE , DOES> @ code-emit ;

: cop-ldrstr
	CREATE , DOES> @ code-emit ;

0E000000 cop-dp CDP,
0E100010 cop-dp MRC,		0E000010 cop-dp MCR,
0C100000 cop-ldrstr LDC,	0C000000 cop-ldrstr STC,

\ ******************* Labels
\ Labels are stored in a symbol table, created by MK-SYM-TABLE. There are
\ two types of labels; locals and globals. Locals have the scope of a single
\ definition and globals have a scope that is user-defined.
\ The symbol table has three areas. One is a lookup table to get a local
\ label's value given its number. The second does the same job for global
\ labels. The third area is used to resolve forward references. When there
\ is (for example) a forward branch, the target is a label that has not been
\ defined. A reference to such a label is stored in the forward references
\ table. When the label is ultimately resolved, the structure of the table
\ allows the code to be back-patched to resolve the reference. As soon as
\ a label has been resolved, associated entries in the forward references
\ table are deleted (the space is available for reuse). Within the forward
\ references table, global labels are distinguished from local labels by
\ setting the msb of the label number of global labels. When the global
\ label number is used to index into the symbol table, the upper bit has to be
\ masked off.

\ take an address of the label field, and clear all the associated labels to
\ undefined. The undefined value is 0xDEADFEED which is *odd*, so is unlikely
\ to coincide with a real value. If it does coincide with a real value the
\ impact will be that the label with that value can never be resolved and
\ this will lead to an error
: clr-labels ( n -- )
	DUP CELL+ @ SWAP @ \ address-of-1st #labels
	CELLS OVER + SWAP DO DEADFEED I ! 1 CELLS +LOOP ;

\ clear forward ref table; each entry is 2 CELLs. The first holds the label
\ number and the second holds the address to be patched. The first is cleared
\ to FFFFFFFF when the entry is unused
: clr-unres ( n -- )
	DUP CELL+ @ SWAP @ \ address-of-1st #entries
	2 CELLS * OVER + SWAP DO FFFFFFFF I ! 2 CELLS +LOOP ;

\ create space for and initialise a symbol table. The symbol table is stored
\ in data space (created HERE using ALLOT) and supports n global labels,
\ n' local labels and n'' forward references. Add a pointer for each section of
\ the table in order to make the local/global handling code the same.
VARIABLE SYM-TABLE
: MK-SYM-TABLE ( n n' n'' -- )
	HERE SYM-TABLE !
	DUP , HERE 5 CELLS + , 2 CELLS * >R \ forward references
	DUP , HERE 3 CELLS + R@ + , CELLS R> + >R \ local labels
	DUP , HERE CELL+ R@ + , CELLS R> + ALLOT  \ global labels
	SYM-TABLE @ DUP DUP clr-unres 2 CELLS + clr-labels 4 CELLS + clr-labels
;

\ TODO: show high-water mark information for the symbol table
: SYM-TABLE-STATS
	SYM-TABLE @
	CR ." Symbol table has space for:"
	DUP @ CR U. ." Unresolved forward references"
	DUP 2 CELLS + @ CR U. ." Local labels"
	4 CELLS + @ CR U. ." Global labels" ;

\ TODO this needs to be defined in the normal word list so it can be found
\ when it is needed
: DUMP-SYM-TABLE
 CR ." TODO.."
;

\ create a word whose run-time action is to scan the forward ref area and
\ generate an error if any unresolved labels exist. Clear out the labels and
\ any unresolved labels.
: label-resolved
	CREATE , CELLS , DOES> DUP @ SWAP CELL+ @
	SYM-TABLE @ DUP ROT + clr-labels
	\ mask a-of-#forward-ref
	DUP CELL+ @ SWAP @ \ address-of-1st #entries
	2 CELLS * OVER + SWAP DO
		\ for each entry in the forward ref table.
		\ entry is label_number or FFFFFFFF (unused). tos holds a
		\ flag telling us what to look for. Search the whole table and
		\ abort if unexpected forward references exist
		I @ FFFFFFFF = INVERT IF
			\ unresolved label
			I @ 80000000 AND OVER = IF 1F huh THEN
		THEN
	2 CELLS +LOOP DROP ;

\ This must be used manually at the end of a program
4 80000000 label-resolved GLOBAL-RESOLVED

\ This is used automatically at the end of CODE and ;CODE definitions
2 00000000 label-resolved LOCAL-RESOLVED

\ is the label legal - n is label number, n' is address that holds the
\ number of labels. Legal label number is from 0 to number-1
: leglab ( n n' -- n n' )
	OVER 7FFFFFFF AND \ mask out msb that indicates global label
	0 ROT DUP >R @ \ n n 0 @n'
	WITHIN R> SWAP INVERT IF 1C huh THEN ;

\ create an entry in the unresolved table for label n. The patch address
\ for the label is the . (dot) address in target space.
: newunres ( n -- )
	SYM-TABLE @ DUP CELL+ @ SWAP @ \ n address-of-1st #entries 
	2 CELLS * OVER + SWAP DO \ n .. search unresolved labels table
		\ check this entry..
		I @ FFFFFFFF = IF
			\ empty entry so assign n to it
			I ! ASM. I CELL+ ." newunres:" .S ! UNLOOP EXIT
		THEN
	2 CELLS +LOOP 1D huh ;

\ create a word whose function is to attempt to resolve the value of label n.
\ If resolvable, n' is the resolved value. Otherwise, the unresolved bit is
\ set in this-op and an unresolved entry is created pointing to . (dot)
: label# \ runtime of created word: ( n -- n' |  )
	CREATE , CELLS , DOES> DUP @ ROT OR SWAP CELL+ @
	SYM-TABLE @ + leglab \ label a-of-number-of-labels
	CELL+ @ OVER 7FFFFFFF AND CELLS + @ \ label label-value
	DUP DEADFEED = IF 
		DROP newunres this-op @ 800000 OR this-op ! \ unresolved
	ELSE
		SWAP DROP \ resolved
	THEN ;

2 00000000 label# L#		4 80000000 label# G#

\ resolve any references to label n
: resolve ( n -- )
	SYM-TABLE @ \ label a-of-#forward-ref
	DUP CELL+ @ SWAP @ \ label address-of-1st #entries
	2 CELLS * OVER + SWAP DO
		\ for each entry in the forward references table...
		\ entry is label_number or FFFFFFFF (unused). tos holds the
		\ label number to look for. Resolve all the matches. To resolve
		\ look at the value stored at the target address. Expect the
		\ value to be a branch or an LDR pointing backwards to a
		\ literal pool entry. For a branch, work out the offset and
		\ OR it in. For an LDR, use the offset to calculate the
		\ address of the literal pool entry and patch that.
		I @ OVER = IF
			\ resolve this label
			DUP 80000000 AND IF I @ G# ELSE I @ L# THEN \ get value
			I CELL+ @ DUP ASM@
\ ." Resolve:" .S			\ label value patch-address cur-value
			0A000000 AND 0A000000 = IF
				\ it's a branch
				OVER OVER - 8 - 2 RSHIFT DUP FF000000 AND
				DUP 3F000000 = SWAP 0= OR INVERT IF
					4 huh
				THEN
				\ label value patch-address offset
				FFFFFF AND OVER ASM@ OR SWAP ASM! DROP
			ELSE
\ ." Literal ref" .S
				\ change patch address to address of literal
				DUP ASM@ 0FFF AND - 8 +
				ASM! \ and patch it
			THEN
			FFFFFFFF I ! \ it's resolved so clear the entry
		THEN
	2 CELLS +LOOP DROP ;

\ TODO don't know if the next word needs ASM@ ASM!
\ create word whose function is to create a label with name n' (range from 0
\ to maximum local label) with a value given by n. Look for unresolved labels
\ of this name (in the unresolved list) and resolve them
: label= \ runtime of created word: ( n n' -- )
	CREATE , CELLS , DOES> DUP @ ROT OR SWAP CELL+ @
	SYM-TABLE @ + leglab \ value label a-of-number-of-labels
	CELL+ @ OVER 7FFFFFFF AND CELLS + \ address where value will be stored
	DUP @ DEADFEED = IF
		ROT SWAP ! \ define value for label
		resolve
	ELSE
		1E huh
	THEN ;

2 00000000 label= L=			4 80000000 label= G=

\ create local label with name n (range from 0 to maximum local label) with
\ a value given by the current value of . (dot) Look for unresolved labels of
\ this name (in the unresolved list) and resolve them.
: L. ( n -- ) ASM. SWAP L= ;
: G. ( n -- ) ASM. SWAP G= ;

\ ******************* Literal Pool control
\ LTORG creates storage for a literal puddle of n entries. Many literal puddles
\ can be linked together to form the literal pool. Entries in the puddle are
\ initialised to 0 to prevent sharing of invalid literals.
\ The literal pool is in TARGET space so all accesses to it are indirected
\ through ASM@ and ASM!. The puddle is created at ASM. and onwards. From the
\ target processor's point of view, the pool is in *data* space, since a
\ d-stream read will access the pool in operation. However, from Forth's point
\ of view, this is *code* space, since it is accessed by an offset from the
\ current value of the PC and it is stored in-line with the i-stream storage.
\ In comparison with the literal pool that the ARM assembler would generate,
\ there are two differences; firstly, there is some overhead in each puddle:
\ a longword to store the number of literals in the puddle and another to store
\ the number of literals currently assigned. This is space wasted in the target
\ memory space. This could be avoided by separating the puddle into two
\ parts and allocating the counts in host space and the puddle itself in
\ target space. For a native assembler (target=host) this would be even more
\ wasteful though. The second difference is that these puddles are only
\ accessed by backward references; ARM assembler literal pools are accessed
\ by forward and backward references. This means that half the addressability
\ of the puddle is not exploited. It would be very tricky to fix this.
\ TODO since LTORG-HEAD is a TARGET space address, a value of 0 might
\ be reasonable.. add a check and error if an attempt is made to define
\ a puddle at address 0 since this would conflict with the magic end-marker.
\ Alternatively, use some non-aligned address  such as 1 as an indicator
\ of end-of-pool. Should really check that pools are defined on aligned
\ boundaries.
: LTORG ( n -- )
	DUP IF
		ASM.
		\ At start of puddle, emit a link to next (older) puddle
		\ (or 0 if this is the first puddle) and update the head
		\ pointer to point to this new puddle
		LTORG-HEAD @ ASM,32 LTORG-HEAD !
		0 ASM,32 \ number of literals used in this pool
		DUP ASM,32 \ number of literals in this pool
		1 DO 0 ASM,32 LOOP \ init each literal to 0
	ELSE 18 huh THEN ;

: LTORG-STATS
	CR ." Literal pool statistics"
	LTORG-HEAD @
	BEGIN
		DUP 0<>
	WHILE
		>R BASE @ 10 BASE ! CR ." Puddle at 0x" R@ U.
		0A BASE !
		R@ CELL+ DUP ASM@ ." : " U.
		." of " CELL+ ASM@ U. ." entries used."
		BASE ! R> ASM@ \ restore base and look at next puddle
	REPEAT
	DROP CR ." (End of literal pool)" CR ;

\ reserve an entry in the literal pool; return address of the entry. The
\ entry will have been set to 0 when the pool was created.
\ TODO: Caller must determine if the entry is reachable..
: RESLIT ( -- n )
	LTORG-HEAD @ DUP 0= IF 19 huh THEN
	CELL+ >R R@ ASM@ R@ CELL+ ASM@ = IF 1A huh THEN
	\ increment the literal count for this pool
	1 R@ ( +! ) SWAP OVER ASM@ + SWAP ASM!
	\ and return the address of the allocated entry
	R> DUP ASM@ 1+ CELLS + ;

\ Put a value n into the literal pool and return its address n'. The literal
\ may be new or shared. Error if literal pool is full
: NEWLIT ( n -- n' )
	LTORG-HEAD @
	BEGIN
		DUP >R 0<> \ real puddle? 
		\ assume that the puddle is reachable if the first entry
		\ is reachable (a conservative approximation)
		ASM. R@ - 1004 <
		AND
	WHILE	\ reachable puddle.. search it
		R@ SWAP R@ CELL+ ASM@ 3 + CELLS R@ + R> 3 CELLS + DO
			DUP I ASM@ = IF \ found sharable entry
				\ nxt-puddle-ptr literal 
				DROP DROP I UNLOOP EXIT
			THEN
		4 +LOOP	SWAP ASM@ \ n address-of-next-puddle
	REPEAT
	\ no match found in any reachable puddle so create new entry .. value
	\ is still at tos. This may result in a reachable entry being created
	\ in the current literal pool even though the earlier calculation
	\ concluded that the current literal pool was unreachable
	R> DROP RESLIT SWAP OVER ASM! ;

\ TODO: there's no range test in RESLIT so the "fatal internal error" here
\ is the escape from generating an illegal offset. Need to change the
\ message here or else move the test to RESLIT.
\ Create an offset for an LDR instruction for loading from a literal pool
\ earlier in memory; the offset is -ve, and the appropriate flag is
\ set in the op-in-progress later on. The op-code is going to go at . (dot),
\ the value at tos is the location to be referenced.
: ABS2OS ( a -- o/s )
	ASM. - 8 -
	\ value gets range-checked in ldrstr but we should already know
	\ that the literal is within reach so do a sanity test here
	DUP ABS FFFFF000 AND 0 <> IF 1B huh THEN ;

\ attempt to translate a line of the format: immediate Rn =,
\ this will map into one of:
\ MOV Rn,#immediate
\ MVN Rn,#immediate
\ LDR Rn,[R15,offset]
: =, ( n -- ) \ TODO check stack behaviour.. 
	this-op @ 800000 AND IF
		\ ." =, is unresolved forward reference"
		\ unresolved forward reference. This will have created an
		\ entry in the forward reference table, with a value that
		\ points to . (dot) Need to allocate a literal pool entry
		\ and emit an LDR here that points to the entry.
		\ When the forward reference is resolved it will use the
		\ offset in the LDR to locate and fix-up the literal
		\ pool value
		0 NEWLIT ABS2OS \ allocate a literal pool entry (0 for now)
		this-op DUP @ 80000000 OR SWAP ! \ set IMMEDIATE flag
		0 oldreg R15 reg LDR, \ see comments below..
	ELSE
		\ immediate or resolved value at tos
		DUP 32to12 IF \ can express using MOV
			\ ." =, is resolved & expressed with MOV" 
			DROP # MOV, \ fix up as tho MOV, with immediate
		ELSE DUP INVERT 32to12 IF \ can express using MVN,
			\ ." =, is resolved and expressed with MVN" 
			DROP INVERT # MVN, \ fix up as tho MVN, with immediate
		ELSE
			\ ." =, is resolved - requires literal pool entry"
			\ cannot express.. need to generate literal and use LDR
			NEWLIT ABS2OS \ leave immediate offset on stack
			this-op DUP @ 80000000 OR SWAP ! \ set IMMEDIATE flag
			\ want to do an LDR with PC-relative addressing
			\ need to put an R15 on reg_fifo underneath the
			\ existing register (there should be exactly 1)
			0 oldreg R15 reg
			LDR, \ by luck, the immediate is -VE by default
		THEN THEN
	THEN ;

\ ******************* Implementation-dependent end for CODE and ;CODE
: END-CODE
\ TODO pfe equivalent plus conditional
\    linkLast
    LOCAL-RESOLVED \ ensure all forward references are resolved
    PREVIOUS ;

\ ******************* Useful macros for hForth
4 CONSTANT CELLL
: tos R9 ;
: fpc R10 ;
: rsp R11 ;
: dsp R12 ;
\ usage of these is R4 pushD,
: pushD, 0 oldreg ![ CELLL NEGATE # dsp ] SWAP reg STR, ;
: pushR, 0 oldreg ![ CELLL NEGATE # rsp ] SWAP reg STR, ;
: popD,  0 oldreg CELLL # [ dsp ] SWAP reg LDR, ;
: popR,  0 oldreg CELLL # [ rsp ] SWAP reg LDR, ;

\ end a code definition
\ TODO compile-time conditional branch to udebug
: NEXT, CELLL # [ fpc ] PC LDR, ;

\ ******************* misc - debugging and testing words
: .this-op ( -- )
	this-op @ U. oip @ U. reg_fifo @ U. CR ;

\ compare last value emitted with value on stack
: zz ( n -- )
	ASM. 1 CELLS - @ 2DUP <> IF
		CR ." Generated " U. ." but expected " U.
		." . Final stack depth=" DEPTH .
		-1 ABORT" Failed"
	THEN DROP ;

\ print last op-code generated
: wot ( -- )
	ASM. 1 CELLS - @ U. CR ;

\ extract offset from op-code and check that the referenced literal
\ matches the value at tos
: chklit ( n -- )
	ASM. 1 CELLS - @
	DUP 800000 AND IF CR ." Error: expected a -ve offset" THEN
	FFF AND ASM. 1 CELLS - 8 + SWAP - @
	<> IF ." Error: literal mismatch" CR THEN ;


\ ******************* Standard words for using the assembler
FORTH-WORDLIST SET-CURRENT	\ add the following word in FORTH-WORDLIST

\   CODE	( '<spaces>name' -- )           \ TOOLS EXT
\		Skip leading space delimiters.	Parse name delimited by a
\		space. Create a definition for name, called a
\		'code definition,' with the execution semantics defined below.
\		Process subsequent characters in the parse area in an
\		implementation-defined manner, thus generating corresponding
\		machine code. Those characters typically represent source code
\		in a programming language, usually some form of assembly
\		language.  The process continues, refilling the input buffer
\		as needed, until an implementation-defined ending sequence is
\		processed.
\
\		name Execution:( i*x --- j*x )
\		Execute the machine code sequence that was generated
\		following CODE.
\ TODO check for hForth ROM model.. add alternative for meta-compilation?
\ TODO fix up usage of HERE/xhere/ ASM-HERE
\ [IF]
  : CODE ( "<spaces>name" -- )    \ TOOLS EXT
\ TODO pfe equivalent of head,
\      xhere ALIGNED DUP TOxhere   \ align code address
\      head,			  \ register a word in dictionary
      ALSO ASSEMBLER
      init-asm ;
\ [THEN]

\   ;CODE	Compilation: ( C: colon-sys -- )	\ TOOLS EXT
\		Interpretation: Interpretation semantics for this word
\				are undefined.
\		Append the run-time semantics below to the current definition.
\		End the current definition, allow it to be found in the
\		dictionary, and enter interpretation state, consuming
\		colon-sys. Process subsequent characters in the parse area in
\		an implementation-defined manner, thus generating corresponding
\		machine code. Those characters typically represent source code
\		in a programming language, usually some form of assembly
\		language.  The process continues, refilling the input buffer as
\		needed, until an implementation-defined ending sequence is
\		processed.
\
\		Run-time:( -- ) ( R: nest-sys -- )
\		Replace the execution semantics of the most recent definition
\		with the name execution semantics given below. Return control
\		to the calling definition specified by nest-sys. An ambiguous
\		condition exists if the most recen definition was not defined
\		with CREATE or a user-defined word that calls CREATE.
\
\		name Execution:( i*x --- j*x )
\		Perform the machine code sequence that was generated
\		following ;CODE.
: ;CODE
\ TODO pfe equivalent of bal and pipe
\    bal @ IF -22 THROW THEN
\    POSTPONE pipe POSTPONE [
    ALSO ASSEMBLER init-asm
    ; IMMEDIATE   hForth [IF] COMPILE-ONLY [THEN]

\ ******************* Tidy up

SET-ORDER  SET-CURRENT
\ TODO ram/rom! if appropriate model
BASE !

\ TODO: need to revisit and revise the usage of xhere and HERE. All data
\ structures should be at HERE, except the literal pool, which should be
\ at xhere because it is accessed by code. That means that we need a
\ mechanism for accessing all spaces.. OK for the ARM, but hard for
\ a segmented machine like the 8086.
