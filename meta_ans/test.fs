CR CR .( *** Test code ***)

hex
100 7ff cdata section code1
800 fff udata section udat1
1000 17ff idata section idat1
1800 1fff cdata section code2
2000 2fff udata section udat2
3800 3fff idata section idat2


udata variables
sections

\ TODO test with CDATA IDATA UDATA as active sections.


f-def
target
\ TODO BUG if the section is not cdata, the 2nd constant declaration fails
34 constant  foo
37 constant  bart
123 constant major
187 constant mayeor
abdec constant neal
89 constant bart          \ redefine


12345678 value fooby
abcd value nearly
ef01 value jumbo


variable v1
variable v2
variable v3

2variable v21
2variable v22
2variable v23

cvariable c1
cvariable c2
cvariable c3

\ to check that it aligns correctly..
variable v4


+asm
init-asm
10 10 30 MK-SYM-TABLE
0 LTORG-HEAD ! \ make sure no other puddle exists

\ 00 L# B, \ branch past literal pool
20 LTORG \ create the first puddle in the literal pool
\ 00 L.

-asm


CODE doVALUE	( -- x )
\		Run-time routine of VALUE. Return the value of VALUE word.
\		This is like an invocation of doCONST for a VARIABLE but
\		instead of returning the address of the variable, we return
\		the value of the variable -- in other words, there is another
\		level of indirection.
		tos pushD,
		[ R14 ] R0 LDR,
		[ R0 ] tos LDR,
		END-CODE COMPILE-ONLY
 
CODE doCONST	( -- x )
\		Run-time routine of CONSTANT and VARIABLE. When you quote a
\		constant or variable you execute its code, which consists of a
\		call to here, followed by an inline literal. The literal is a
\		constant (for a CONSTANT) or the address at which a VARIABLE's
\		value is stored. Although you come here as the result of a
\		native machine call, you never go back to the return address
\		-- you jump back up a level by continuing at the new fpc value.
\		For 8086, Z80 the inline literal is at the return address
\		stored on the top of the hardware stack. For the ARM, we came
\		here through a bl (branch-and-link) so the return address is
\		in r14
		tos pushD,
		[ R14 ] tos LDR,	\ inline address from calling point
		END-CODE COMPILE-ONLY

CODE doUSER	( -- a-addr )
\		Run-time routine of USER. Return address of data space.
\		This is like doCONST but a variable offset is added to the
\		result. By changing the value at AddruserP (which happens
\		on a taskswap) the whole set of user variables is switched
\		to the set for the new task.
		tos pushD,
		[ R14 ] tos LDR,
\		GetVaxAdr userP R0 =,  TODO need equivalent
		[ R0 ] R1 LDR,
		R1 TOS TOS ADD,
		END-CODE COMPILE-ONLY

CODE doLIT	( -- x )
\		Push an inline literal. The inline literal is at the current
\		value of the fpc, so put it onto the stack and point past it.
		tos pushD,
		CELLL # [ fpc ] tos LDR, \ get literal, inc to next forth word
		END-CODE COMPILE-ONLY

CODE doCREATE	( -- a-addr )
\		Run-time routine of CREATE. For CREATEd words with an
\		associated DOES>, get the address of the CREATEd word's data
\		space and execute the DOES> actions. For CREATEd word without
\		an associated DOES>, return the address of the CREATE'd word's
\		data space. A CREATEd word starts its execution through this
\		routine in exactly the same way as a colon definition uses
\		doLIST. In other words, we come here through a native machine
\		branch. For ARM, r14 holds the address of the next word at the
\		called point and fpc holds next word above that.
\
\		Structure of CREATEd word:
\		        | call-doCREATE | 0 or DOES> code addr | a-addr |
\
\		The DOES> address holds a native call to doLIST. This routine
\		doesn't alter the fpc. We never come back *here* so we never
\		need to preserve an address that would bring us back *here*. 
\
\		For ARM, r14 points to the "0 or DOES>" address. The DOES>
\
\		Example : myVARIABLE CREATE , DOES> \
\		56 myVARIABLE JIM
\		JIM \ stacks the address of the data cell that contains 56
\
\   : doCREATE    SWAP            \ switch BX and top of 8086 stack item
\		  DUP CELL+ @ SWAP @ ?DUP IF EXECUTE THEN ; COMPILE-ONLY
\
		tos pushD,
		CELLL # [ R14 ] R0 LDR,		\ 0 or DOES> address
		[ R14 ] tos LDR,		\ a-addr
		0 # R0 R0 ORRS,			\ set flags..
		R0 PC NE MOV,			\ a DOES> address.. go there
						\ (and never come back)
		END-CODE COMPILE-ONLY		\ no DOES> actions

CODE doTO	( x -- )
\		Run-time routine of TO. Store x at the address in the
\		following cell. The inline literal holds the address
\		to be modified.
		\ get the address to be modified and point past
		CELLL # [ fpc ] R0 LDR,
		\ update to new value from tos
		[ R0 ] tos STR,
		tos popD,
		END-CODE COMPILE-ONLY

CODE doLIST	( -- ) ( R: -- nest-sys )
\		Process colon list.
\		The first word of a definition (the xt for the word) is a
\		native machine-code instruction for the target machine. For
\		high-level definitions, that code is emitted by xt, and
\		performs a call to doLIST. doLIST executes the list of xt that
\		make up the definition. The final xt in the definition is EXIT.
\		The address of the first xt to be executed is passed to doLIST
\		in a target-specific way. Two examples:
\		Z80, 8086: native machine call, leaves the return address on
\		the hardware stack pointer, which is used for the data stack.
\		ARM: branch-and-link, so the return address is in r14,
\		not on the data stack.
\
		fpc pushR,			\ preserve forth PC
		R14 fpc MOV,			\ first xt of definition
		END-CODE COMPILE-ONLY

CODE doLOOP	( -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Run time routine for LOOP.
\
		R0 popR,			\ loop count
		1 # R0 R0 ADDS,
		01 L# VS B,			\ overflow -> end loop
		R0 pushR,			\ update loop count
		[ fpc ] fpc LDR,	\ get branch dest. to loop again
		END-CODE-EARLY
01 L.		CELLL # fpc fpc ADD,		\ ignore branch offset
		R0 popR,			\ NAC what's this dropping?? -- can just bump rsp rather than doing memory access
		END-CODE COMPILE-ONLY

CODE do+LOOP	( n -- ) ( R: loop-sys1 -- | loop-sys2 )
\		Run time routine for +LOOP.
\
		R0 popR,			\ loop count
		tos R0 R0 ADDS,
		tos popD,
		01 L# VS B,			\ overflow -> end loop
		R0 pushR,			\ update loop count
		[ fpc ] fpc LDR,		\ loop again
		END-CODE-EARLY
01 L.		CELLL # fpc fpc ADD,		\ ignore branch offset
		R0 popR,			\ NAC what's this dropping? -- can just bump rsp rather than doing memory access
		END-CODE COMPILE-ONLY

CODE branch	( -- )
\		Branch to an inline address.
		[ fpc ] fpc LDR,		\ get branch destination
		END-CODE COMPILE-ONLY		\ .. and go there

CODE 0branch	( flag -- )
\		Branch if flag is zero.
		tos tos tos ORRS,		\ test top of stack
\ ARM's cool conditional-execution saves pipeline-draining branches 
		CELLL # FPC FPC NE ADD,		\ don't branch, point past dest
		[ fpc ] fpc EQ LDR,		\ branch; get destination
		tos popD,			\ tidy up the stack
		END-CODE COMPILE-ONLY

F-DEF
CODE EXIT	( -- ) ( R: nest-sys -- )	\ CORE
\		Return control to the calling definition.
		fpc popR,			\ where call doLIST left it
		END-CODE COMPILE-ONLY

		
sections
		
HOST
CR ." Depth=" depth . ." at end of test.fs" CR
