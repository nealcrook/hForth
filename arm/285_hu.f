\ $Id$
\ $Log$
\
\ ebsa-285 hForth utility words
\ these utilities expect optional.f to be loaded; they require
\ definitions of HEX MARKER SAVE-SYSTEM ROM RAM
\
\ This file will ultimately contain words for 5 sets of functions
\ - LED toggling
\ - PCI setup and control
\ - Misc. functions (eg timer control)
\ - MMU initialisation
\ - Flash reprogramming
\
\ TODO in Flash routines
\      - verify checksums in images as part of directory list
\      - check that checksum gets written correctly


\ TODO - HEX may not be defined
\      - MARKER doesn't seem to work at the moment
DECIMAL
: HEX 16 BASE ! ;
: MARKER CREATE DOES> ;


HEX
MARKER STARTOF-UTILS


: .U2 ( n -- )
 \ print a single number as a 2-digit unsigned number
 S>D <# # # #> TYPE
;


: FREEBIT ( n -- n )
 \ take a value that represents a bit-map in which a set bit
 \ means a resource is in use. Find the first free resource.
 \ bit 0 is resource 0, bit 1 is resource 1 etc.
 \ I feel as tho' there must be a better way..
 0 SWAP
 BEGIN
   DUP 1 AND 1 =
 WHILE
   1 RSHIFT SWAP 1+ SWAP
 REPEAT
 DROP
;


\ ---------------------------------------------------------------
\ Words for using the Flash ROM
\ ---------------------------------------------------------------

MARKER STARTOF-FLASH

: FLB2A ( n -- a)
 \ convert Flash block number (0-F) to an absolute base address
 12 LSHIFT \ 256Kbyte blocks
 41000000 +
;


: FLHDR ( n -- T|F )
 \ given a Flash block number, return a flag showing whether
 \ the block has a valid Flash header. This is determined by
 \ looking for a defined magic byte pattern
 FLB2A 4 + @ FFFFFF00 AND
 00AA5500 =
;


: FLINIMAGE ( n -- n )
 \ take a Flash block number and return the number of the image
 \ in which it is used. Return FF if not found in any image
 FF SWAP \ put the default return value under the block number
 10 0 DO
   I FLHDR IF
     1 OVER LSHIFT \ bit mask of the block we're searching for 
     I FLB2A 8 + @ \ fetch a valid map
     AND IF
       \ The block is used in the image whose header is in
       \ block I. Extract the image number from block I
       \ and replace the FF with this number
       SWAP DROP I FLB2A 4 + C@ SWAP
     THEN
   THEN
 LOOP DROP \ drop the block number
;


: FLINBLOCK ( n -- n )
 \ take a Flash image number and return the number of the block
 \ that contains the header for that image. Return FF if image
 \ number not found in any image header
 FF SWAP \ put the default return value under the image number
 10 0 DO
   I FLHDR IF
     I FLB2A 4 + C@ OVER = IF
       \ found the block containing the image header whose image
       \ number we are looking for. Shove the block number
       \ on the stack in place of the FF
       SWAP DROP I SWAP
     THEN
   THEN
 LOOP DROP \ drop the image number
;


: FLBUSED ( -- n )
\ return a bitmap showing the blocks in use
 0 \ accumulate the bitmap here
 10 0 DO
   I FLHDR IF
     I FLB2A 8 + @ OR
   THEN
 LOOP
 3 OR \ force blocks 0, 1 to overcome pass1 PBL code bug
;
   

: FLIUSED ( -- n )
\ return a bitmap showing the images in use
 0 \ accumulate the bitmap here
 10 0 DO
   I FLHDR IF
     I FLB2A 4 + @ F AND 1 SWAP LSHIFT OR
   THEN
 LOOP
 3 OR \ force images 0, 1 to overcome pass1 PBL code bug
;


: .FLI-LIST ( -- )
 \ list the images in Flash
 CR BASE @
 10 0 DO \ for each of 16 Flash blocks
   I FLHDR
   IF
     I FLB2A \ describe the image whose header starts here
     DECIMAL ." Image " DUP 4 + C@ .U2 \ image number
     BL EMIT 22 EMIT DUP 24 + DUP 10 - DO
       I C@ EMIT
     LOOP 22 EMIT \ image name
     ."  Length " DUP 10 + @ U. ." bytes, Map 0x"
     HEX 8 + @ S>D <# # # # #  # # # # #> TYPE CR
   THEN
 LOOP
 BASE ! \ restore the base to its value when we started all this
;

: .FLB-LIST ( -- )
 \ list the blocks in Flash
 CR BASE @ DECIMAL
 10 0 DO \ for each of 16 Flash blocks
   DECIMAL ." Block " I .U2 \ block number
   I FLINIMAGE DUP FF = IF
     ."  (Unused)  " DROP
   ELSE
     ."  (Image " .U2 ." )"
   THEN
   HEX I FLB2A DUP 8 + SWAP DO \ print first 8 bytes from the block
     ."  0x" I C@ S>D <# # # #> TYPE
   LOOP CR
 LOOP
 BASE ! \ restore the base to its value when we started all this
;


: FLSTAT ( -- n)
 \ return flash status bytes
 70707070 41000000 !
 41000000 @
;


: FLSTAT0 ( -- )
 \ clear flash status
 50505050 41000000 !
;


: FLBUSY ( -- )
 \ wait while any flash byte is indicating that it is busy
 BEGIN
   FLSTAT
   80808080 AND 80808080 =
 UNTIL
;


: FLB-DEL ( n -- T|F )
 \ delete block n, return status flag
 DUP 2 < ABORT" Fatal: ignored request to delete block 0 or 1"
 DUP F > ABORT" Fatal: ignored request to delete non-existent block"
 FLB2A DUP
 20202020 SWAP ! \ erase setup
 D0D0D0D0 SWAP ! \ erase confirm
 FLBUSY FLSTAT 28282828 AND 0= FLSTAT0
;


: FLI-DEL ( n -- T|F )
 \ delete image n, return status flag
 \ since the first block contains the image header, there is no need
 \ to delete any more than this. FALSE status always means erase
 \ failed - attempt to delete a non-existent image number will ABORT
 DUP 2 < ABORT" Fatal: ignored request to delete images 0 or 1"
 FLINBLOCK
 DUP FF = ABORT" Fatal: ignored attempt to delete non-existent image"
 FLB-DEL
;


: FLPROG ( a n -- T|F )
 \ program 256Kbyte block, n, with data from address a
 \ return success/failure status flag
 FLB2A SWAP OVER - \ flash-a offset-from-flash-a-to-src
 SWAP DUP 40000 + SWAP DO \ loop for each Flash DWORD address
   FFFFFFFF 41000000 ! \ reset flash array
   I OVER + @ \ get source data
   40404040 I ! \ prime flash for write
   I ! \ write data to flash
   FLBUSY
 4 +LOOP DROP \ drop offset
 \ get status and clear any error
 FLSTAT 28282828 AND 0= FLSTAT0
 FFFFFFFF 41000000 ! \ reset array to be super-safe
;


: QCSUM ( n1 n2 -- n)
\ compute longword 2s complement checksum starting at n1, of n2 longwords
 0 SWAP \ start_addr sum #longwords
 0 DO
   OVER I 2 LSHIFT + \ calculate address
   @ + \ add its value in
 LOOP
 SWAP DROP NEGATE
;


: FL-FORTH ( " name " )
 \ put current value of hForth back into Flash
 \ name is the name for the new image. New image number and
 \ block are chosen automatically.
 CR SAVE-SYSTEM
 \ need a Flash header. We can steal this from the block we were
 \ loaded from. Our image starts at 100C0 in SDRAM so there is a
 \ gap ready for the header
 40012000 @ INVERT F AND \ Flash image selected by the switch
 FLINBLOCK FLB2A 10000 C0 MOVE \ copy the header from that image
 \ fix up the header..
 \ set the checksum to look like an erased Flash location (we
 \ can fix it up in Flash after the rest of the block has been
 \ programmed)
 FFFFFFFF 1000C !
 \ clear the name field and fill in the user-supplied name
 10024 10014 DO BL I C! LOOP
 BL PARSE \ c-addr u
 DUP 0= ABORT" Fatal: supply a name for the new image"
 DUP 16 > ABORT" Fatal: maximum length of name is 16 chars"
 10014 SWAP MOVE \ move the name into the header
 \ find a free image number to use and put it into the header
 FLIUSED FREEBIT
 DUP 10 > ABORT" Fatal: all 16 image numbers are in use"
 DUP 10004 C!
 ." Using image number 0x" .U2 CR
 \ find a free block to use and update the header's map
 FLBUSED FREEBIT
 DUP 10 > ABORT" Fatal: all 16 Flash blocks are in use"
 DUP 1 SWAP LSHIFT 10008 !
 DUP ." Using Flash block 0x" .U2 CR
 DUP FLB-DEL 0= ABORT" Fatal: Flash delete block failed" 
 \ flash block number is on the stack
 DUP 10000 SWAP FLPROG
 0= ABORT" Fatal: Flash programming failed"
 \ now go back and calculate the checksum
 FLB2A \ block was on tos
 -1 OVER 4000 QCSUM NEGATE -
 SWAP C + \ point to c'sum location in Flash header
 40404040 OVER !
 ! \ program in the checksum
 FLBUSY FFFFFFFF 41000000 ! \ reset the Flash array
;


\ ---------------------------------------------------------------
\ Words for using memory-management and other CP15 stuff.
\ ---------------------------------------------------------------

MARKER STARTOF-MM

: BUILDFLATMAP ( n -- )
\ build a set of level-1 page tables at address n for 1-1 mapping of the whole
\ 32-bit address space. The coarsest way that we can divide up the memory (to
\ minimise the size of the page tables) is to define SECTIONS. Each section
\ has a 12-bit base address and 20-bit offset; therefore each section is
\ 1MByte in size. There are 4096 sections and each page-table entry occupies 
\ 4 bytes. Therefore, the page tables occupy 16Kbytes.
\
\ The address map is divided up like this:
\ Address range         # sections  Properties
\ 8000.0000 - FFFF.FFFF 800H        non-cached, buffered (PCI space)
\ 1000.0000 - 7FFF.FFFF 700H        non-cached, non-buffered (21285 stuff)
\ 0000.0000 - 0FFF.FFFF 100H        cached, buffered (SDRAM)
\
\ If you wished to refine this map, it might be worth making the Flash ROM
\ cacheable, but this would complicate the Flash programming stuff
\
\ all the domains are set to 0. All the permissions are set to the most
\ permissive.
 
 0 \ base address that this section will map
 100 0 DO \ SDRAM section
   DUP >R
   C0E OR \ cacheable, bufferable
   OVER !
   4 + R> 100000 + \ increment table address and base address
 LOOP
 
 700 0 DO \ 21285 section
   DUP >R
   C02 OR \ non-cacheable, non-bufferable
   OVER !
   4 + R> 100000 +
 LOOP

 800 0 DO \ PCI section
   DUP >R
   C06 OR \ non-cacheable, bufferable
   OVER !
   4 + R> 100000 +
 LOOP
 DROP DROP
;

: INITMMU ( n -- )
\ build page tables at address n and enable the MMU
 DUP
 TTB! BUILDFLATMAP
 TLBFLUSH \ in case they are still valid
 3 DAC! \ domain 0 accessible to all
 MMUON
;

: SPEEDUP ( -- )
\ start up mmu and clock switching
 MMUPTAB \ build-time constant tells us where to build the tables
 INITMMU CKSWON
;

: SLOWDOWN ( -- )
\ turn off the MMU and flush the caches
 MMUOFF CKSWOFF IFLUSH DFLUSH

\ end

HAND

