\ $Id: compare.fth,v 1.2 1998/09/05 12:07:57 crook Exp $
\ requires: hmeta.fth
\ hForth meta-compiler project
\
\ Words for:
\ - loading a pre-build image into memory
\ - saving a meta-compiled image to disk
\ - comparing two images in memory and reporting where they differ

HEX

\ name of file to compare against
: pre-built S" prebuilt/hf110dm.aif" ;
\ the newly meta-compiled image is referred to as meta-built


VARIABLE myfile \ save file handle
CREATE PB-IMAGE 10000 ALLOT \ reserve space to load a pre-build image

\ check to see if file operation completed successfully. If it didn't, fail
\ ungracefully.
: file-ok
	DUP 0= IF DROP ELSE CR ." File operation failed with error code " .
	ABORT THEN ;

\ load a pre-build image into memory.
: PBIMAGE-RD
	pre-built R/O BIN OPEN-FILE file-ok myfile !
	\ first 80 bytes are the aif header, which we don't want
	PB-IMAGE 80 myfile @ READ-FILE file-ok DROP 
	\ now read the image - trash the aif header
	PB-IMAGE 10000 myfile @ READ-FILE file-ok
	10000 = IF ."  64Kbytes read successfully" CR ELSE
	ABORT"  image file for comparison was too small" THEN
	myfile @ CLOSE-FILE file-ok ;


\ dump a line in dword + ASCII format
: dline ( address -- )
	BASE @ HEX SWAP CR DUP >R
	0 <# BL HOLD # # # # # # # # #> TYPE
	R@ R@ 10 + SWAP DO
		I @ 0 <# BL HOLD # # # # # # # # #> TYPE
	4 +LOOP
	R@ R> 10 + SWAP DO
		I C@ 20 7F WITHIN IF I C@ ELSE [CHAR] .
		THEN EMIT
	LOOP
	BASE ! ;


\ when images mis-compare, show the context
: .context ( address -- )
	3 + FFFFFFFC AND \ round up to dword
	DUP 10 - dline dline ;

\ The meta-compiled image is stored in host memory at TARGET-IMAGE. Do a
\ compare.
: cmp-inc
	CR ." Compare incrementing: "
	10000 0 DO
		TARGET-IMAGE I + C@
		PB-IMAGE I + C@
		<> IF
			\ not equal so error
			." Miscompare at byte offset " I U. CR
			CR ." Prebuild image:" PB-IMAGE I + .context CR
			CR ." This image:" TARGET-IMAGE I + .context
			UNLOOP EXIT
		THEN
	LOOP ;

: cmp-dec
	CR ." Compare decrementing: "
	0 FFFF DO \ decrementing loops aren't symmetrical w/incrementing ones
		TARGET-IMAGE I + C@
		PB-IMAGE I + C@
		<> IF
			\ not equal so error
			." Miscompare at byte offset " I U. CR
			CR ." Prebuild image:" PB-IMAGE I + .context CR
			CR ." This image:" TARGET-IMAGE I + .context
			UNLOOP EXIT
		THEN
	-1 +LOOP ;

: IMAGE-COMPARE	cmp-inc CR cmp-dec ;		

\ save the meta-compiled image to disk
: IMAGE-WR
	meta-built R/W BIN CREATE-FILE
	file-ok myfile !
	\ store 64K in the file
	TARGET-IMAGE 10000 myfile @ WRITE-FILE file-ok
	myfile @ CLOSE-FILE file-ok ;
