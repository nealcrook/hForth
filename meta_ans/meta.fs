#! /usr/local/bin/gforth
\ Top-level for the meta-compiler

\ [ifdef] *meta*
\     *meta*
\ [then]

marker *meta*

s" compat.fs" included


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Definitions to control the flavour of compilation

base @ hex

\ Force high-level definitions where possible
\ TRUE CONSTANT META-HI
FALSE CONSTANT META-HI

\ Force unproven definitions where available
TRUE CONSTANT META-EXPERIMENTAL

\ Force code endings to branch through micro-debugger
FALSE CONSTANT MICRO-DEBUG

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Definitions to determine the target machine/mem-map/comm port/baud rate
\ TODO this all needs to be rationalised somewhat.
TRUE  CONSTANT TAR_EBSA110 \ Targets
FALSE CONSTANT TAR_EBSA285
FALSE CONSTANT TAR_COGENT

\ IO_DEFIO can take these values:
\ FF .. DEMON SWI
\ 00 .. COM0 (21285 internal UART)
\ 01 .. COM1 (16550)
\ 02 .. COM2 (16550)
FF  CONSTANT IO_DEFIO

\ IO_DEFBAUD can take these values:
\ 4    - 9600 baud
\ 5    - 19K2
\ 6    - 38K4
\ 7    - 56K
4     CONSTANT IO_DEFBAUD 

TRUE  CONSTANT MM_DEMON \ MEMMAP 
FALSE CONSTANT MM_BOOT
FALSE CONSTANT MM_PBLOADED

base !


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Load the target assembler and the cross-compiler

s" ../asmarm/asmarm.fth" included
s" cross.fs" included
s" define.fs" included


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Configure the assemblers vectors for target code generation

\ .. emit the 32-bit value at tos at the current target PC
:NONAME asa@ CDATA swap x-, asa! ;        'ASM,32 !

\ .. return the current value of "dot" - the point at which code is emitted
:NONAME asa@ CDATA x-HERE swap asa! ;     'ASM. !

\ .. do a 32-bit store to an address in the target image code space
                                    ' x-! 'ASM! !

\ .. do a 32-bit fetch from an address in the target image code space
                                    ' x-@ 'ASM@ !

CR .( Finished assembler setup) CR

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ TODO - temporary test files

\ s" test.fs" included
\ host .scope quit \ go interactive


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Now we're ready to load the source...

\ TODO hForth broken in half, temporarily.
\ TODO currently does a QUIT half-way through hforth.fth
s" hforth.fth" included
s" immediate.fs" included
\ s" hforth_b.fth" included

CR .( Finished loading hForth source) CR

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Print statistics and save final image to disk
ALSO ASSEMBLER
GLOBAL-RESOLVED \ check that global forward references are resolved.
SYM-STATS LTORG-STATS
PREVIOUS

.( Code pointer = 0x) _CODE U. 
.( , Name pointer = 0x) _NAME U.
.( , Space = 0x) _NAME _CODE - U.
s" hforth.bin" section2disk


false [if] TODO TODO TODO

    1. :noname                                 DONE
    2. add TDEF                                DONE
    3. revamp words to add TDEF part           DONE
    4. compile some code                       DONE
    5. compile some colon defn                 DONE
    6. revamp variables stuff for hForth
    7. compile/debug to 1st immediate
    8. immediate words
    9. environment queries
    10. target wordlist stuff?
    11. move all equates into a separate file, and partition into
        target dependent and independent

    PROBLEM: I thought I would not have *target in the target scope
    search order, but when I arrange things like this I have the problem
    that words defined interpretively (eg constants ) cannot be accessed.
    If I add *target I will have the problem that a named colon definition
    can be quoted interpretively, which ought to execute it but cannot.

    2 solutions:
    -- add a separate word list for words that can be referenced interpretively
    -- add *target and then add tSTATE-dependent DOES> actions for all
       target : and CODE definitions that prints an error.

    think the latter is a better solution. Does this problem represent a flaw
    in the cross-compiler words?   

    BUG: very fragile under error conditions. Not clear what the subsequent
    word order is. BAL is invariably left in a bad state.

    ISSUE: with searching address translation, how often is any section other
    than the active section used. Will everyone need to implement the same
    low-level HERE-p etc. that I have done?

    BUG: need to be able to find things created using EQU both in target
    interpret mode and in target compile mode. Also, probably, in
    host definitions? Not sure how that would work.
    
[then]
