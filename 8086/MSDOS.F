\
\ MSDOS.F
\ FILES and BLOCK words for MS-DOS
\
\ by Wonyong Koh
\
\ MSDOS.F can be loaded as below:
\
\	<< OPTIONAL.F
\	<< ASM8086.F
\	<< COREEXT.F
\	<< MSDOS.F
\
\ Then other files such as MULTI.F, HIOMULTI.F, etc. can be loaded as below:
\
\	BL PARSE MULTI.F    INCLUDED
\	BL PARSE HIOMULTI.F INCLUDED
\
\ In HF86EXE.EXE system image can be saved using SYSTEM-SAVED
\ or SAVE-SYSTEM-AS as below:
\
\	SAVE-SYSTEM-AS SAVE2.EXE
\
\ Don't forget to set up "'init-i/o" and "'boot" properly.
\
\ There is only one block buffer and only one file is assigned as BLOCK
\ in current implementation.
\
\ 1996. 3. 1.
\	DOS error code is offsetted by -512 to give 'ior'.
\ 1997. 5. 26.
\		Fix RESTORE-INPUT to restore BLK correctly.

CHAR " PARSE CPU" ENVIRONMENT? DROP
CHAR " PARSE 8086" COMPARE
[IF]
    CR .( BLOCK and FILE words for MS-DOS are for 8086 RAM and EXE models only.)
    ABORT
[THEN]

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE AND
[IF]
    CR .( BLOCK and FILE words for MS-DOS are for 8086 RAM and EXE models only.)
    ABORT
[THEN]

BASE @
GET-ORDER  GET-CURRENT

FORTH-WORDLIST SET-CURRENT
WORDLIST WORDLIST-NAME DOS-WORDLIST
DOS-WORDLIST SET-CURRENT
GET-ORDER DOS-WORDLIST SWAP 1+ SET-ORDER

DECIMAL
-512 CONSTANT iorOffset
VARIABLE MaxHandle	\ contains maximum DOS file handle
0 MaxHandle !		\ to be used to calculate UNUSED data space.

1024 CHARS CONSTANT 1K
0 VALUE updated 		\ true if block is updated
CREATE block-buffer 1K ALLOT	\ the only block buffer
50 VALUE def#blocks		\ default # of blocks for a new mapped file
-1 VALUE block-fid		\ BLOCK file id
-1 VALUE current-block#
NONSTANDARD-WORDLIST SET-CURRENT
50 VALUE #blocks		\ maximum # of blocks
DOS-WORDLIST SET-CURRENT

VARIABLE error-class	     0 error-class !
VARIABLE recommanded-action  0 recommanded-action !
VARIABLE error-locus	     0 error-locus !

: ?dupR>DropExit       ?DUP IF R> R> 2DROP EXIT THEN ;
: ?dupR>Drop2NipExit   ?DUP IF R> R> 2DROP NIP NIP EXIT THEN ;
: ?dupR>Drop4NipExit   ?DUP IF R> R> 2DROP NIP NIP NIP NIP EXIT THEN ;

HEX
CODE get-ior  ( -- ior )
    59 # AH MOV,
    BX BX XOR,
    21 INT,
    BH error-class ) MOV,
    BL recommanded-action ) MOV,
    CH error-locus ) MOV,
    AX BX MOV,
    iorOffset # BX ADD,
    NEXT,
END-CODE

CODE (open-file)  ( asciiz fam -- fileid ior )
    3D # AH MOV,
    BL AL MOV,
    DX POP,
    21 INT,
    AX PUSH,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

CODE (create-file)  ( asciiz -- fileid ior )
    3C # AH MOV,
    CX CX XOR,		\ CX = 0 ; normal read/write
    BX DX MOV,
    21 INT,
    AX PUSH,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

CODE (delete-file)  ( asciiz -- ior )
    41 # AH MOV,
    BX DX MOV,
    21 INT,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

CODE (reposition-file)	( ud fileid reposition_method -- ud ior )
    \ reposition_method;
    \	0 : offset from beginning of file
    \	1 : offset from present location
    \	2 : offset from end-of-file
    42 # AH MOV,
    BL AL MOV,		\ AL = reposition-method
    BX POP,		\ file handle
    CX POP,		\ CX:DX = offset
    DX POP,
    21 INT,
    AX PUSH,
    DX PUSH,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

CODE crlf=  ( char -- flag )
    BX AX MOV,
    -1 # BX MOV,
    0A # AL CMP,
    1 L# JZ,
    0D # AL CMP,
    1 L# JZ,
    BX INC,
1 L:
    NEXT,
END-CODE

\ PAD is constant in EXE model.
PAD BL PARSE /PAD ENVIRONMENT? DROP CHARS - CONSTANT S"buffer

\		Returns file input buffer address
\		Each text file has its own input buffer
\		  below buffer for S" below PAD.
\		In MS-DOS, a program can open up to 20 files.
\		  Thus fileid(=DOS handle) is normally 5 to 20.
\		DOS handle 0 : standard input (CON)
\			   1 : standard output (CON)
\			   2 : standard output for error message (CON)
\			   3 : standard serial interface (AUX)
\			   4 : standard printer (PRN)
\		<-/fileid6buffer/fileid5buffer/S"buffer/PAD/TIB||memTop
: input-buffer	( -- c_addr )
    SOURCE-ID
    ?DUP IF 1+ ?DUP IF \ source-id = fileid, text file source
		5 - [ BL PARSE /PAD ENVIRONMENT? DROP CHARS ] LITERAL
		* S"buffer SWAP - EXIT THEN THEN
    \ source-id = 0, user input device source
    \ source-id = -1, string source
    SOURCE DROP ;

FORTH-WORDLIST SET-CURRENT
\   UNUSED	( -- u )				\ CORE EXT
\		Return available data space in address units.
: UNUSED
    S"buffer
    MaxHandle @ 5 - [ BL PARSE /PAD ENVIRONMENT? DROP CHARS ] LITERAL * -
    HERE - ;	\ Available data space is HERE to assigned buffer addr
DOS-WORDLIST SET-CURRENT

CODE (file-status)  ( asciiz -- x ior )
    4300 # AX MOV,	\ get file attributes
    BX DX MOV,
    21 INT,
    CX PUSH,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

CODE (rename-file)  ( asciiz1 asciiz2 -- ior )
    56 # AH MOV,
    BX DX MOV,
    DS PUSH,
    ES POP,
    DI POP,
    21 INT,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

FORTH-WORDLIST SET-CURRENT

\   BLK 	( -- a_addr )				\ BLOCK
\		a_addr is the address of a cell containing 0 or the
\		mass-strorage block being interpreted. If BLK is 0, the input
\		source is not a block and can be identified by SOURCE-ID .
VARIABLE BLK  0 BLK !

\   BIN 	( fam1 -- fam2 )			\ FILE
\		Modify file access method to binary.
: BIN ; 	\ Do nothing for MS-DOS handle functions.

\   CLOSE-FILE	( fileid -- ior )			\ FILE
\		Close the file identified by fileid.
CODE CLOSE-FILE
    3E # AH MOV,	\ BX = file handle
    21 INT,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

\   OPEN-FILE	( c_addr u fam -- fileid ior )		\ FILE
\		Open a file with the name and file access method.
: OPEN-FILE
    >R asciiz R> (open-file)
    DUP 0= IF OVER MaxHandle @ MAX MaxHandle ! THEN ;

\   CREATE-FILE ( c_addr u fam -- fileid ior )		\ FILE
\		Create a file with the given name and the file access
\		method and return fileid.
: CREATE-FILE
    >R	2DUP			\ ca u ca u	  R: fam
    asciiz (create-file)	\ ca u fileid ior R: fam
    ?dupR>Drop2NipExit
    CLOSE-FILE DROP  R> OPEN-FILE ;

\   DELETE-FILE ( c_addr u -- ior )			\ FILE
\		Delete the named file.
: DELETE-FILE
    asciiz (delete-file) ;

\   FILE-POSITION ( fileid -- ud ior )			\ FILE
\		ud is the current file position for fileid.
: FILE-POSITION
    >R 0 0 R> 1 (reposition-file) ;

\   REPOSITION-FILE ( ud fileid -- ior )		\ FILE
\		Reposition the file to ud.
: REPOSITION-FILE
    0 (reposition-file) NIP NIP ;

\   FILE-SIZE	( fileid -- ud ior )			\ FILE
\		ud is the size of of fileid in characters.
: FILE-SIZE
    DUP >R			\ fid  R: fid
    FILE-POSITION		\ ud ior  R: fid
    ?dupR>DropExit			\ save current position
    0 0 R@ REPOSITION-FILE	\ ud ior  R: fid
    ?dupR>DropExit			\ reset file position
    0 0 R@ 2 (reposition-file)	\ ud ud' ior  R: fid
    ?dupR>Drop2NipExit			\ size = distance from end of file
    2SWAP R> REPOSITION-FILE ;

\   R/O 	( -- fam )				\ FILE
\		Put read-only method value on the stack.
0 CONSTANT R/O

\   W/O 	( -- fam )				\ FILE
\		Put write-only method value on the stack.
1 CONSTANT W/O

\   R/W 	( -- fam )				\ FILE
\		Put read/write method value on the stack.
2 CONSTANT R/W

\   READ-FILE	( c_addr u1 fileid -- u2 ior )		\ FILE
\		Read u1 consecutive characters to c_addr from the current
\		position of the file.
\		Results:
\		    u2=u1, ior=0  \ read with no exception
\		    u2<u1, ior=0  \ end-of-file
\		    u2=0,  ior=0  \ FILE-POSITION equals FILE-SIZE
\		    u2>=0, ior<>0 \ u2 is # chars read until exception occurs
CODE READ-FILE
    3F # AH MOV,
    CX POP,
    DX POP,
    21 INT,
    AX PUSH,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

\   READ-LINE	( c_addr u1 fileid -- u2 flag ior )	\ FILE
\		Read the next line from the file.
\		Results:
\		    x x nonzero \ Something bad and unexpected happened 		  l
\		    0 false 0	\ End-of-file; no characters were read
\		    0 true  0	\ A blank line was read
\	       0<u2<u1 true 0	\ The entire line was read
\		    u1 true 0	\ A partial line was read; the rest would
\				\ not fit in the buffer, and can be acquired
\				\ by additional calls to READ-LINE.
: READ-LINE
    >R OVER SWAP R@		\ ca ca u1 fid	R: fid
    READ-FILE			\ ca u2 ior	R: fid
    ?dupR>DropExit				\ exit on error
    DUP 0= IF NIP DUP DUP R> DROP EXIT THEN	\ 0 false 0, end-of-file
    DUP >R OVER + OVER		\ ca ca+u2 ca  R: fid u2
    DO	 I C@			\ ca char  R: fid u2 loop_index
	 DUP 09 ( TAB ) = IF BL I C! THEN
	 DUP 1A ( ctrl-Z ) = IF
	    DROP I UNLOOP R> DROP
	    SWAP -		\ ca'-ca (# chars before ctrl-Z)  R: fid
	    DUP 0= 0=		\ u -1|0  R: fid
	    R@ FILE-SIZE	\ u -1|0 ud ior  R: fid
	    ?dupR>Drop2NipExit
	    R> REPOSITION-FILE EXIT
	 THEN
	 crlf= IF I UNLOOP     \ ca ca'  R: fid u1
	    TUCK CHAR+ DUP C@ crlf=
	    IF CHAR+ THEN	\ ca' ca ca'+1|2  R: fid u1
	    OVER -		\ ca' ca line_length  R: fid u1
	    R> -		\ ca' ca #chars_to_roll_back  R: fid
	    S>D R> 1 (reposition-file)	\ ca' ca ud ior
	    DROP 2DROP		\ ca' ca ; adjust file position
	    - TRUE 0 EXIT
	 THEN
    LOOP			\ ca  R: fid u2
    DROP R> TRUE 0 R> DROP ;	\ line terminator not found, partial lile read

\   S"          Interpretation: ( 'ccc<">' -- c_addr u )        \ FILE
\		Compilation:	( 'ccc<">' -- )
\		Run-time:	( -- c_addr u )
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : S"
      STATE @ IF POSTPONE S" EXIT THEN          \ CORE word S"
      S"buffer DUP [CHAR] " PARSE DUP >R        \ S"buf S"buf c_addr u  R: u
      ROT SWAP CHARS MOVE R>
      ; IMMEDIATE
[THEN]
\ Define non-IMMEDIATE S" using special compilation action mechanism
\ Structure of words with special compilation action, CREATEd words and S",
\ for default compilation behavior
\	|compile_xt|name_ptr| execution_code |
\ Structure of dictionary in data segment
\	| xt | link | name |
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  :NONAME
      DROP		\ drop execution xt left for special compilation action
      POSTPONE S" ;
  code, 			\ store compilation xt
  ' S" xt>name DUP code,        \ store name pointer
  :NONAME
      [CHAR] " PARSE DUP >R     \ c_addr u  R: u
      CHARS S"buffer SWAP MOVE
      S"buffer R> ;
  SWAP				\ execution_S"_xt S"_name_addr
  DUP C@			\ get flags
  60 INVERT AND 		\ clear IMMEDIATE and COMPILE-ONLY flags
  80 OR 			\ set special compilation action flag
  OVER C!			\ store flags
  cell- cell- ! 		\ store new execution xt
[THEN]

\   SOURCE-ID	( -- 0|-1|fileid )			\ FILE
\		Returns input source identifier: 0 for user input device,
\		-1 for string (via EVALUATE), and fileid for text file.
\
\ INCLUDE-FILE and INCLUDED set SOURCE-ID to proper values.

\   WRITE-FILE	( c_addr u fileid -- ior )		\ FILE
\		Write u characters from c_addr u to the file.
HEX
CODE WRITE-FILE
    40 # AH MOV,
    CX POP,
    DX POP,
    21 INT,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

CREATE cr-lf 0D C, 0A C, ALIGN

\   WRITE-LINE	( c_addr u fileid -- ior )		\ FILE
\		Write u characters from c_addr followed by line terminator
\		to the file.
: WRITE-LINE
    DUP >R WRITE-FILE		\ ior  R: fid
    ?dupR>DropExit
    cr-lf 2 R> WRITE-FILE ;

\   RESIZE-FILE ( ud fileid -- ior )			\ FILE
\		Set the size of the file to ud.
: RESIZE-FILE
    DUP >R			\ ud1 fid  R: fid
    FILE-SIZE			\ ud1 ud2 ior  R: fid
    ?dupR>Drop4NipExit
    2OVER DNEGATE D+		\ ud1 ud2-ud1  R: fid
    NIP 0<	\ file_size < ud ?
    IF R@ FILE-SIZE		\ ud1 ud3 ior  R: fid
       ?dupR>Drop4NipExit
       2DUP R@ REPOSITION-FILE	\ ud1 ud3 ior  R: fid
       ?dupR>Drop4NipExit
       DNEGATE D+		\ ud1-ud3  R: fid
       BEGIN			\ u_low u_high	R: fid
	  ?DUP WHILE
	     0 8000 R@ WRITE-FILE	\ u1 u2 ior  R: fid
	     ?dupR>Drop2NipExit
	     0 8000 R@ WRITE-FILE	\ u1 u2 ior  R: fid
	     ?dupR>Drop2NipExit
	     1- 			\ u1 u2-1  R: fid
	  REPEAT			\ u1  R: fid
       0 SWAP R> WRITE-FILE EXIT
    THEN			\ ud1  R: fid
    R@ REPOSITION-FILE		\ ior  R: fid
    ?dupR>DropExit
    0 0 R> WRITE-FILE ; 	\ writing 0 byte truncates the file in MS-DOS.

\   FILE-STATUS ( c_addr u -- x ior )			\ FILE EXT
\		Return the status of the named file. If the file exists,
\		ior is 0. x contains implementation-defined information
\		about the file.
: FILE-STATUS
    asciiz (file-status) ;

\   FLUSH-FILE	( fileid -- ior )			\ FILE EXT
\		Attempt to force any buffered information written to the file
\		and update the directory.
CODE FLUSH-FILE
    45 # AH MOV,
    21 INT,
    1 L# JC,
    AX BX MOV,
    3E # AH MOV,
    21 INT,
    1 L# JC,
    BX BX XOR,
    NEXT,
1 L:
    ' get-ior # JMP,
END-CODE

\   RENAME-FILE ( c_addr1 u1 c_addr2 u2 -- ior )	\ FILE
\		Rename the file named bye c_addr1 u1 to the name c_addr2 u2.
: RENAME-FILE
    \ another asciiz buffer after PAD
    PAD [ BL PARSE /PAD ENVIRONMENT? DROP CHARS ] LITERAL +
    DUP >R  SWAP 2DUP + 0 SWAP C! CHARS MOVE
    asciiz R>  (rename-file) ;

\   SAVE-BUFFERS ( -- ) 				\ BLOCK
\		Transfer the contents of each UPDATEd block buffer to mass
\		storage. Mark all buffers as unmodified.
: SAVE-BUFFERS
    updated IF
      current-block# 1K UM* block-fid REPOSITION-FILE THROW
      block-buffer 1K  block-fid WRITE-FILE THROW
      block-fid FLUSH-FILE
      0 TO updated
    THEN ;

\   BUFFER	( u -- a_addr ) 			\ BLOCK
\		a_addr is the address of the first character of the block
\		buffer assigned to block u. The contents of the block are
\		unspecified.
\		If block u is already in a block buffer, a_addr is the address
\		    of that block buffer.
\		If block u is not already in memory and there is an unassigned
\		    block buffer, a_addr is the address of that block buffer.
\		If block u is not already in memory and there are no
\		    unassigned block buffer, unassign a block. If the block
\		    in that buffer has been UPDATEd, transfer the block to
\		    mass storage. a_addr is the address of that block buffer.
\		At the conclusion of the operateion, the block buffer pointed
\		    to by a_addr is the current block buffer and is assigned
\		    to u.
: BUFFER
    SAVE-BUFFERS
    TO current-block#  block-buffer ;

DECIMAL
\   BLOCK	( u -- a_addr ) 			\ BLOCK
\		a_addr is the address of the first character of the block
\		buffer assigned to mass-storage block u.
\		If block u is already in a block buffer, a_addr is the address
\		    of the block.
\		If block u is not already in memory and there is an unassigned
\		    block buffer, transfer block u from mass storage to an
\		    unassigned block buffer. a_addr is the address of that
\		    block buffer.
\		If block u is not already in memory and there are no
\		    unassigned block buffer, unassign a block. If the block
\		    in that buffer has been UPDATEd, transfer the block to
\		    mass storage and transfer block u from mass storage into
\		    that buffer. a_addr is the address of that block buffer.
\		At the conclusion of the operateion, the block buffer pointed
\		    to by a_addr is the current block buffer and is assigned
\		    to u.
: BLOCK
    DUP current-block# = IF DROP block-buffer EXIT THEN
    DUP BUFFER DROP
    1K UM* block-fid REPOSITION-FILE THROW
    block-buffer 1K block-fid READ-FILE THROW
       1K = 0= IF ." Unexpected end of BLOCK file."
		  -33 THROW  THEN	\ block read exception
    block-buffer ;

\   SAVE-INPUT	( -- xn ... x1 n )			\ CORE EXT
\		Implementated as ( -- c_addr u >in source_id blk@ 5 )
\		x1 through xn describe the current state of the input source
\		specification for later use by RESTORE-INPUT .
: SAVE-INPUT
    SOURCE  >IN @  SOURCE-ID  BLK @  5 ;

\   RESTORE-INPUT ( xn ... x1 n -- flag )		\ CORE EXT
\		Attempt to restore the input specification to the state
\		described by x1 through xn. flag is true if the input
\		source specification cannot be so restored.
: RESTORE-INPUT
    DUP 5 = IF DROP  DUP IF BLOCK THEN
	       BLK ! TO SOURCE-ID >IN ! sourceVar 2!
	       FALSE EXIT
    THEN  0 DO DROP LOOP  TRUE ;

\   EMPTY-BUFFERS ( -- )				\ BLOCK EXT
\		Unassign all block buffers. Do not transfer the contents of
\		any UPDATEd block buffer to mass storage.
: EMPTY-BUFFERS
    -1 TO current-block#
    0 TO updated ;

\   EVALUATE	( i*x c-addr u -- j*x ) 		\ CORE, BLOCK
\		Evaluate the string. Save the input source specification.
\		Store -1 in SOURCE-ID. Store 0 in BLK.
: EVALUATE   0 BLK !  EVALUATE ;

\   FLUSH	( -- )					\ BLOCK
\		Perform the function of SAVE-BUFFERS, then unassign all block
\		buffers.
: FLUSH     SAVE-BUFFERS EMPTY-BUFFERS ;

\   LOAD	( i*x u -- j*x )			\ BLOCK
\		Save the current input-source specification. Store u in BLK
\		(thus making block u the input source and setting the input
\		source buffer to encompass its contents), set >IN to 0, and
\		interpret. When the parse area is exhausted, restore the
\		prior input source specification. Other stack effects are due
\		to the words LOADed.
: LOAD
    SAVE-INPUT
    DUP BEGIN ?DUP WHILE 1- ROT >R REPEAT  >R
    DUP BLK !  BLOCK 1K
    sourceVar 2!  0 >IN !
    interpret
    R> DUP BEGIN ?DUP WHILE 1- R> ROT ROT REPEAT
    RESTORE-INPUT
    IF ." Input source specification was not properly restored."
	-37 THROW	\ file I/O exception
    THEN ;

\   SOURCE	( -- c_addr u ) 			\ CORE
\
: SOURCE
    BLK @ ?DUP IF BLOCK 1K EXIT THEN
    SOURCE ;				\ old SOURCE

\   UPDATE	( -- )					\ BLOCK
\		Mark the current block buffer as modified.
: UPDATE
    current-block# -1 = IF
	." There is no current block buffer."
	-35 THROW  THEN 	\ invalid block number
    TRUE TO updated ;

\   SCR 	( -- a_addr )				\ BLOCK EXT
\		a_addr is the address of a cell containing the block number
\		of the block most recently LISTed.
VARIABLE SCR  0 SCR !

\   LIST	( u -- )				\ BLOCK EXT
\		Display block u in an implementation-defined format.
\		Store u in SCR.
DECIMAL
: LIST
    DUP SCR !  BLOCK  BASE @ DECIMAL SWAP
    16 0 DO  CR I 2 .R SPACE
	     64  2DUP TYPE + LOOP  CR DROP
    BASE ! ;

\   THRU	( i*x u1 u2 -- j*x )			\ BLOCK EXT
\		LOAD the mass storage blocks numbered u1 through u2 in
\		sequence. Other stack effects are due to the words LOADed.
: THRU
    1+ SWAP DO I LOAD LOOP ;

\   INCLUDE-FILE ( i*x fileid -- j*x )			\ FILE
\		Remove fileid, save the current input source specification
\		including current value of SOURCE-ID. Store fileid in
\		SOURCE-ID . Make the file specified by fileid the input
\		source. Store 0 in BLK . Repeat read a line, fill the input
\		buffer, set >IN 0 and interpret until the end of the file.
\
\		Each text file has its own input buffer below PAD.
\		In MS-DOS, fileid is normally 5 to 20.
DECIMAL
: INCLUDE-FILE
    SAVE-INPUT
    DUP BEGIN ?DUP WHILE 1- ROT >R REPEAT  >R
    TO SOURCE-ID  input-buffer >R
    BEGIN
       R@ DUP [ BL PARSE /PAD ENVIRONMENT? DROP CHARS ] LITERAL
       SOURCE-ID		\ ca ca u1 fileid
       READ-LINE		\ ca u2 flag ior
       THROW
    WHILE
       sourceVar 2!  0 >IN !
       interpret
    REPEAT 2DROP  R> DROP
    R> DUP BEGIN ?DUP WHILE 1- R> ROT ROT REPEAT
    RESTORE-INPUT
    IF ." Input source specification was not properly restored."
	-37 THROW	\ file I/O exception
    THEN ;

\   INCLUDED	( i*x c_addr u -- j*x ) 		\ FILE
\		Open the named file and do INCLUDE-FILE .
: INCLUDED
    R/O OPEN-FILE THROW
    DUP >R INCLUDE-FILE
    R> CLOSE-FILE THROW ;

NONSTANDARD-WORDLIST SET-CURRENT

\ for convenience, not to use in Standard program
: INCLUDE  ( i*x 'filename<space>' -- j*x )
    BL PARSE INCLUDED ;

FORTH-WORDLIST SET-CURRENT

\   REFILL	( -- flag )			\ CORE EXT, BLOCK EXT, FILE EXT
\		Extend the execution semantics of REFILL for block and file
\		input.
\		When the input source is a block, make the next block the input
\		    source and current input buffer by adding one to the value
\		    of BLK and setting >IN to 0. Return true if the new value
\		    of BLK is a valid block number, otherwise false.
\		On file input attempt to read the next line from the text-input file.
\		    If sucessful, make the result the current input buffer, set
\		    >IN to 0, and return true.
: REFILL
    BLK @ IF 1+ DUP BLK ! BLOCK block-buffer 1K sourceVar 2!  0 >IN ! TRUE
	     EXIT THEN
    SOURCE-ID -1 = IF 0 EXIT THEN
    SOURCE-ID 0= IF REFILL EXIT THEN	\ old REFILL
    input-buffer
    DUP [ BL PARSE /PAD ENVIRONMENT? DROP CHARS ] LITERAL
    SOURCE-ID		     \ ca ca u1 fileid
    READ-LINE		     \ ca u2 flag ior
    IF 2DROP DROP FALSE EXIT THEN
    IF sourceVar 2!  0 >IN ! TRUE EXIT THEN
    2DROP FALSE ;

\   \		( 'ccc<eol>' -- )                       \ CORE EXT, BLOCK EXT
\		Extend the semantics of '\' for block.
\		If BLK contains 0, parse and discard the remainder of the parse
\		    area; otherwise parse and discard the portion of the parse
\		    area corresponding to the remainder of the current line.
DECIMAL
: \   BLK @ IF	>IN @ 63 + -64 AND
      ELSE  SOURCE NIP
      THEN  >IN !  ; IMMEDIATE

\   (		( 'ccc<)>' -- )                         \ CORE, FILE
\		Extend the semantics of '(' for file.
\		Skip until ')' or end-of-file.
: (
    BEGIN
       [CHAR] ) PARSE 2DROP
       SOURCE NIP >IN @ XOR IF EXIT THEN \ ')' is if source is not fully parsed
       SOURCE 1- CHARS + C@ [CHAR] ) = IF EXIT THEN
       REFILL 0=
    UNTIL ; IMMEDIATE

\   [ELSE]	( *<spaces>name...* - ) 	\ TOOLS EXT
\		Skipping leading spaces, parse and discard words from the
\		parse area, including nested [IF] ... [THEN] and [IF] ...
\		[ELSE] ... [THEN], until the word [THEN] has been parsed
\		and discared.
: [ELSE]  ( -- )
   1 BEGIN					\ level
     BEGIN  PARSE-WORD	DUP  WHILE		\ level c-addr len
       2DUP  S" [IF]"  COMPARE 0= IF            \ level c-addr len
	 2DROP 1+				\ level'
       ELSE					\ level c-addr len
	 2DUP  S" [ELSE]"  COMPARE 0= IF        \ level c-addr len
	   2DROP 1- DUP IF 1+ THEN		\ level'
	 ELSE					\ level c-addr len
	   S" [THEN]"  COMPARE 0= IF            \ level
	     1- 				\ level'
	   THEN
	 THEN
       THEN ?DUP 0=  IF EXIT THEN		\ level'
     REPEAT  2DROP				\ level
   REFILL 0= UNTIL				\ level
   DROP ;  IMMEDIATE

\   [IF]	( flag | flag *<spaces>name...* -- )	\ TOOLS EXT
\		If flag is true, do nothing. Otherwise, Skipping leading
\		spaces, parse and discard words from the parse area,
\		including nested [IF] ... [THEN] and [IF] ... [ELSE] ...
\		[THEN], until either the word [ELSE] or [THEN] has been
\		parsed and discared.
: [IF]	( flag -- )				\ TOOLS EXT
   0= IF POSTPONE [ELSE] THEN ;  IMMEDIATE

HEX
\   TIME&DATE	( -- +n1 +n2 +n3 +n4 +n5 +n6 )		\ FACILITY EXT
\		Return the current time and date. +n1 is the second {0...59},
\		+n2 is the minute {0...59}, +n3is the hour {0...23}, +n4 is
\		the day {1...31} +n5 is the month {1...12}, and +n6 is the
\		year(e.g., 1991).
CODE TIME&DATE
    BX PUSH,
    BX BX XOR,
    2C # AH MOV,
    21 INT,
    DH BL MOV,
    BX PUSH,		\ second
    CL BL MOV,
    BX PUSH,		\ minute
    CH BL MOV,
    BX PUSH,		\ hour
    2A # AH MOV,
    21 INT,
    DL BL MOV,
    BX PUSH,		\ day
    DH BL MOV,
    BX PUSH,		\ month
    CX BX MOV,		\ year
    NEXT,
END-CODE

DOS-WORDLIST SET-CURRENT

HERE CHAR " PARSE insufficient disk space" HERE pack" TO HERE           \ 27h 39
HERE CHAR " PARSE cannot complete file operation (out of input)" HERE pack" TO HERE
HERE CHAR " PARSE code page mismatch" HERE pack" TO HERE
HERE CHAR " PARSE sharing buffer overflow" HERE pack" TO HERE
HERE CHAR " PARSE FCB unavailable" HERE pack" TO HERE
HERE CHAR " PARSE disk change invalid" HERE pack" TO HERE
HERE CHAR " PARSE lock violation" HERE pack" TO HERE
HERE CHAR " PARSE sharing violation" HERE pack" TO HERE
HERE CHAR " PARSE general failure" HERE pack" TO HERE
HERE CHAR " PARSE read fault" HERE pack" TO HERE
HERE CHAR " PARSE write fault" HERE pack" TO HERE
HERE CHAR " PARSE printer out of paper" HERE pack" TO HERE
HERE CHAR " PARSE sector not found" HERE pack" TO HERE
HERE CHAR " PARSE unknown media type (non-DOS disk)" HERE pack" TO HERE
HERE CHAR " PARSE seek error" HERE pack" TO HERE
HERE CHAR " PARSE bad request structure length" HERE pack" TO HERE
HERE CHAR " PARSE data error (CRC)" HERE pack" TO HERE
HERE CHAR " PARSE unknown command" HERE pack" TO HERE
HERE CHAR " PARSE drive not ready" HERE pack" TO HERE
HERE CHAR " PARSE unknown unit" HERE pack" TO HERE
HERE CHAR " PARSE disk write-protected" HERE pack" TO HERE
HERE CHAR " PARSE no more files" HERE pack" TO HERE
HERE CHAR " PARSE not same device" HERE pack" TO HERE
HERE CHAR " PARSE attempted to remove current directory" HERE pack" TO HERE
HERE CHAR " PARSE invalid drive" HERE pack" TO HERE
HERE CHAR " PARSE reserved" HERE pack" TO HERE
HERE CHAR " PARSE data invalid" HERE pack" TO HERE
HERE CHAR " PARSE access code invalid" HERE pack" TO HERE
HERE CHAR " PARSE format invalid" HERE pack" TO HERE
HERE CHAR " PARSE environment invalid (usually >32K in length)" HERE pack" TO HERE
HERE CHAR " PARSE memory block address invalid" HERE pack" TO HERE
HERE CHAR " PARSE insufficient memory" HERE pack" TO HERE
HERE CHAR " PARSE memory control block destroyed" HERE pack" TO HERE
HERE CHAR " PARSE invalid handle" HERE pack" TO HERE
HERE CHAR " PARSE access denied" HERE pack" TO HERE
HERE CHAR " PARSE too many open files (no handles available)" HERE pack" TO HERE
HERE CHAR " PARSE path not found" HERE pack" TO HERE
HERE CHAR " PARSE file not found" HERE pack" TO HERE
HERE CHAR " PARSE function number invalid" HERE pack" TO HERE
HERE CHAR " PARSE no error" HERE pack" TO HERE                          \ 0

CREATE DOSErrorMsgTbl
    , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , ,

FORTH-WORDLIST SET-CURRENT

DECIMAL
: QUIT
    BEGIN
      rp0 rp!  0 BLK !	0 TO SOURCE-ID	0 TO bal  POSTPONE [
      BEGIN CR REFILL DROP SPACE		\ REFILL returns always true
	    ['] interpret CATCH ?DUP 0=
      WHILE STATE @ 0= IF .prompt THEN
      REPEAT
      DUP -1 XOR IF					\ ABORT
      DUP -2 = IF SPACE abort"msg 2@ TYPE    ELSE       \ ABORT"
      SPACE errWord 2@ TYPE
      SPACE [CHAR] ? EMIT SPACE
      DUP [ 1 iorOffset + ] LITERAL
	  [ 40 iorOffset + ] LITERAL
		 WITHIN IF iorOffset - CELLS DOSErrorMsgTbl +
			 @ COUNT TYPE	     ELSE	\ DOS error
      DUP -1 -58 WITHIN IF ." Exeption # " . ELSE       \ undefined exeption
      CELLS THROWMsgTbl + @ COUNT TYPE	     THEN THEN THEN THEN
      sp0 sp!
    AGAIN ;

: BYE	block-fid FLUSH-FILE  BYE ;

NONSTANDARD-WORDLIST SET-CURRENT

: MAPPED-TO-BLOCK  ( c_addr u -- )
    -1 TO block-fid
    2DUP R/W OPEN-FILE	?DUP IF
      NIP	\ drop invalid fileid
      DUP [ 2 iorOffset + ] LITERAL
      <> IF	\ not 'file not found error', cannot map block to BLOCKS.BLK
	-1 TO block-fid
	." Cannot map BLOCK to " ROT ROT TYPE [CHAR] . EMIT
	THROW THEN
      DROP  ." Create " 2DUP TYPE ."  for BLOCK"
      2DUP R/W CREATE-FILE THROW
      HERE 1K BL FILL
      def#blocks 0 DO DUP HERE 1K ROT WRITE-FILE THROW	LOOP
      DUP FLUSH-FILE THROW
    THEN
    DUP FILE-SIZE THROW
    1K UM/MOD TO #blocks DROP	\ store file-size/1K in #blocks
    TO block-fid 2DROP ;

BL PARSE BLOCKS.BLK MAPPED-TO-BLOCK
\ new boot word, jump into new QUIT

HEX
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : DOSCommand>PAD
      80 PAD OVER C@ 1+ CHARS MOVE ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : DOSCommand>PAD
      CS@ 10 -	\ PSP segment
      80 2DUP LC@ 1+ 0 DO 2DUP LC@ PAD I + C! CHAR+ LOOP 2DROP ;
[THEN]

: newboot ( -- )
    0 MaxHandle !	\ to be used to calculate UNUSED data space.
    DOSCommand>PAD
    hi	S" BLOCKS.BLK" MAPPED-TO-BLOCK  QUIT ;

' newboot TO 'boot

CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  DOS-WORDLIST SET-CURRENT
  HEX
  CREATE EXEHead
      5A4D ,		\ file ID for .EXE file
      0 , 0 ,		\ * file size (remainder and # of 512B pages)
      0 , 20 ,		\ no relocatable item, head is 20h paragraphs (512B)
      2000 ,		\ 128 KB needs to be allocated
      0FFFF ,		\ max paragraphs to allocate
      0 , 0 ,		\ stack relative segment and initial stack pointer
      0 ,		\ checksum
      06 , 0 ,		\ initial relative CS:IP
      1C ,		\ offset in bytes of the relocation pointer table
      0 ,		\ overlay number, 0 for main program
      0 , 0 ,		\ Total is 32 bytes.

  \   xWrite-file  ( code_space_addr u fileid -- ior )
  \		  Write u characters from c_addr u to the file.
  CODE xWrite-file
      40 # AH MOV,
      CX POP,
      DX POP,
      CS DI MOV,
      DS PUSH,
      DI DS MOV,
      21 INT,
      DS POP,
      1 L# JC,
      BX BX XOR,
      NEXT,
  1 L:
      ' get-ior # JMP,
  END-CODE

  NONSTANDARD-WORDLIST SET-CURRENT

  : SYSTEM-SAVED  ( c-addr u -- )
      W/O CREATE-FILE THROW ( fileid ) >R
      #order DUP @ #order0 SWAP 1+ CELLS MOVE	\ adjust default search order
      HERE 0F + 4 RSHIFT	\ data_paragraphs
      1000			\ data_paragraphs code_paragraphs
      + 0 20 UM/MOD OVER IF 1+ THEN	\ add 1 if partial page
      1+				\ one head page
				\ mod16 #pages
      EXEHead 4 + !		\ mod16
      4 LSHIFT	EXEHead 2 + !
      HERE 200 0 FILL  EXEHead HERE 20 MOVE
      HERE 200 R@ WRITE-FILE THROW
      0    8000 R@ xWrite-file THROW
      8000 8000 R@ xWrite-file THROW
      0 HERE 0F + 0FFF0 AND R@ WRITE-FILE THROW
      R> CLOSE-FILE THROW ;

  : SAVE-SYSTEM-AS  ( 'name' -- )
      BL PARSE SYSTEM-SAVED ;
[THEN]

envQList SET-CURRENT
-1 CONSTANT BLOCK
-1 CONSTANT BLOCK-EXT
-1 CONSTANT FILE
-1 CONSTANT FILE-EXT

SET-CURRENT  SET-ORDER
BASE !

QUIT

<< CON
