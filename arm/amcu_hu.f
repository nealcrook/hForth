\ $Id: amcu_hu.f,v 1.1 1999/10/31 17:09:52 crook Exp $
\ $Log: amcu_hu.f,v $
\ Revision 1.1  1999/10/31 17:09:52  crook
\ AMCU utility words -- only partially working.
\
\
\ eb-amcu hForth utility words
\ these utilities expect optional.f to be loaded; they require
\ definitions of HEX MARKER SAVE-SYSTEM ROM RAM
\
\ This file will ultimately contain words for 5 sets of functions
\ - LED toggling
\ - SPI setup and control
\ - Misc. functions (eg timer control)
\ - Flash reprogramming
\
\ TODO in Flash routines
\      - port to AMCU..
\      - verify checksums in images as part of directory list
\      - check that checksum gets written correctly
\      - header source in FL-FORTH relies on being in X-Bus mode

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


: .CONFIG
 \ TODO
 \ report various interesting facts about the board
 \ - ram size
 \ - clock speed
 \ - flash arrangement and timing
 \ - amcu version
;


\ ---------------------------------------------------------------
\ Words for using the Flash ROM
\ ---------------------------------------------------------------

MARKER STARTOF-FLASH

VARIABLE FL-BASE
VARIABLE FL-ERASE
VARIABLE FL-PROG

: FLB2A ( n -- a)
 \ convert Flash block number (0-F) to an absolute base address
 10 LSHIFT \ 64Kbyte blocks
 FL-BASE @ +
;

: FLHDR ( n -- T|F )
 \ given a Flash block number, return a flag showing whether
 \ the block has a valid Flash trailer. This is determined by
 \ looking for a defined magic byte pattern at a given place near
 \ the end of the block
 FLB2A [ 10000 20 - ] LITERAL + @ FFFFFF00 AND
 00AA5500 =
;

: FLINIMAGE ( n -- n )
 \ take a Flash block number and return the number of the image
 \ in which it is used. Return FF if not found in any image
 FF SWAP \ put the default return value under the block number
 10 0 DO
   I FLHDR IF
     1 OVER LSHIFT \ bit mask of the block we're searching for 
     I FLB2A [ 10000 1C - ] LITERAL + @ \ fetch a valid map
     AND IF
       \ The block is used in the image whose header is in
       \ block I. Extract the image number from block I
       \ and replace the FF with this number
       SWAP DROP I FLB2A [ 10000 20 - ] LITERAL + C@ SWAP
     THEN
   THEN
 LOOP DROP \ drop the block number
;

: FLINBLOCK ( n -- n )
 \ take a Flash image number and return the number of the block
 \ that contains the trailer for that image. Return FF if image
 \ number not found in any image trailer
 FF SWAP \ put the default return value under the image number
 10 0 DO
   I FLHDR IF
     I FLB2A [ 10000 20 - ] LITERAL + C@ OVER = IF
       \ found the block containing the image trailer whose image
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
     I FLB2A [ 10000 1C - ] LITERAL + @ OR
   THEN
 LOOP
;
   
: FLIUSED ( -- n )
\ return a bitmap showing the images in use
 0 \ accumulate the bitmap here
 10 0 DO
   I FLHDR IF
     I FLB2A [ 10000 20 - ] LITERAL + @ F AND 1 SWAP LSHIFT OR
   THEN
 LOOP
;

: .FLI-LIST ( -- )
 \ list the images in Flash
 CR BASE @
 10 0 DO \ for each of 16 Flash blocks
   I FLHDR
   IF
     I FLB2A [ 10000 20 - ] LITERAL + \ describe the image whose trailer is here
     DECIMAL ." Image " DUP C@ .U2 \ image number
     BL EMIT 22 EMIT DUP 18 + DUP 10 - DO
       I C@ EMIT
     LOOP 22 EMIT \ image name
     ."  Map 0x" 4 + @
     HEX S>D <# # # # #  # # # # #> TYPE CR
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

: FL-GOODSTAT ( n-d n-a -- T|F )
 \ after an operation involving address n-a and data n-d, wait
 \ while flash is indicating that it is busy then return a flag
 \ indicating whether the operation completed successfully. This
 \ algorithm replicates the one shown in Figure 5
 \ of the the AMD data sheet.
 BEGIN \ wait for it to go non-busy
   DUP @ \ get status
   DUP 80 AND 3 PICK 80 AND = IF
     \ non-busy, data matches
     DROP 2DROP TRUE EXIT
   THEN
 20 AND UNTIL \ loop until bit5 is set
 @ \ re-read status (consume address) check bit7
 SWAP 80 AND =
 \ need to reset the array at this point if there was an error
 F0 FL-BASE @ ! 
;

: FL-ERASE16 ( a -- T|F )
  AA FL-BASE @ AAA + W!
  55 FL-BASE @ 555 + W!
  80 FL-BASE @ AAA + W!
  AA FL-BASE @ AAA + W!
  55 FL-BASE @ 555 + W!
  30 OVER W!
  FF \ expected read data from that address after the erase
  SWAP FL-GOODSTAT
;

: FL-ERASE8 ( a -- T|F)
  AA FL-BASE @ AAA + C!
  55 FL-BASE @ 555 + C!
  80 FL-BASE @ AAA + C!
  AA FL-BASE @ AAA + C!
  55 FL-BASE @ 555 + C!
  30 OVER C!
  FF \ expected read data from that address after the erase
  SWAP FL-GOODSTAT
;

: FLB-DEL ( n -- T|F )
 \ delete block n, return status flag
 DUP F > ABORT" Fatal: ignored request to delete non-existent block"
 DUP 0= IF
   \ sector 0 is actually 4 small chunks, but we hide this fact
   0000 FL-BASE @ + FL-ERASE @ EXECUTE
   4000 FL-BASE @ + FL-ERASE @ EXECUTE
   6000 FL-BASE @ + FL-ERASE @ EXECUTE
   8000 FL-BASE @ + FL-ERASE @ EXECUTE
   AND AND AND
 ELSE
   FLB2A FL-ERASE @ EXECUTE
 THEN
;

: FLI-DEL ( n -- T|F )
 \ delete image n, return status flag
 \ since the last block contains the image trailer, there is no need
 \ to delete any more than this. FALSE status always means erase
 \ failed - attempt to delete a non-existent image number will ABORT
 FLINBLOCK
 DUP FF = ABORT" Fatal: ignored attempt to delete non-existent image"
 FLB-DEL
;

: FL-PROG8 ( n-d n-a -- T|F )
  \ program 16-bit data n-d into address n-a of 8-bit Flash. Wait until
  \ it has completed then return a status flag
  FL-BASE @
  AAA OVER + AA SWAP C! 
  555 OVER + 55 SWAP C!
  AAA + A0 SWAP C!
  2DUP SWAP FF AND SWAP 2DUP C! \ low byte
  FL-GOODSTAT >R
  FL-BASE @
  AAA OVER + AA SWAP C! 
  555 OVER + 55 SWAP C!
  AAA + A0 SWAP C!
  SWAP 8 RSHIFT SWAP 1+ 2DUP C!
  FL-GOODSTAT R> AND
;

: FL-PROG16
  \ program 16-bit data n-d into address n-a of 16-bit Flash. Wait until
  \ it has completed then return a status flag
  FL-BASE @
  AAA OVER + AA SWAP W!
  555 OVER + 55 SWAP W!
  AAA + A0 SWAP W!
  2DUP W!
  FL-GOODSTAT
;

: FL-LOW ( -- )
 \ set the flash base address to be the low Flash, work out the
 \ width and set the vectors for Flash erase and program routines
 C000001C @ 1 AND IF
   \ high Flash is 16-bit
   ['] FL-PROG8 FL-PROG !
   ['] FL-ERASE8 FL-ERASE !
 ELSE
   ['] FL-PROG16 FL-PROG !
   ['] FL-ERASE16 FL-ERASE !
 THEN
 \ reset Flash and set base address
 7F400000 F0 OVER ! FL-BASE !
;

: FL-HIGH ( -- )
 \ set the flash base address to be the high Flash, work out the
 \ width and set the vectors for Flash erase and program routines
 C000001C @ 1 AND IF
   \ high Flash is 16-bit
   ['] FL-PROG16 FL-PROG !
   ['] FL-ERASE16 FL-ERASE !
 ELSE
   ['] FL-PROG8 FL-PROG !
   ['] FL-ERASE8 FL-ERASE !
 THEN
 \ reset Flash and set base address
 7FC00000 F0 OVER ! FL-BASE !
;

: FLB-PROG ( a n -- T|F )
 \ program 64Kbyte sector, n, with data from address a
 \ return success/failure status flag
 FLB2A SWAP OVER - \ flash-a offset-from-flash-a-to-src
 SWAP DUP 10000 + SWAP DO \ loop for each Flash address
   I OVER + W@ I \ .. 16-bit-data address
   FL-PROG @ EXECUTE
   \ if bad status then tidy up and leave
   \ TODO test this...
   0= IF UNLOOP DROP FALSE EXIT THEN
 2 +LOOP DROP \ drop offset
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
 \ need a Flash trailer. We can steal this from the block we were
 \ loaded from and bolt it onto the end of the image being executed
 88000058 @ INVERT 0F AND \ Flash image selected by the switch
 \ append the trailer from that image to this image
 FLINBLOCK FLB2A [ 10000 20 - ] LITERAL 20 MOVE
 \ fix up the header..
 \ set the checksum to look like an erased Flash location (we
 \ can fix it up in Flash after the rest of the block has been
 \ programmed)
 FFFFFFFF [ 10000 20 - 1C + ] LITERAL !
 \ clear the name field and fill in the user-supplied name
 [ 10000 20 - 18 + ] LITERAL [ 10000 20 - 8 + ] LITERAL DO BL I C! LOOP
 BL PARSE \ c-addr u
 DUP 0= ABORT" Fatal: supply a name for the new image"
 DUP 16 > ABORT" Fatal: maximum length of name is 16 chars"
 [ 10000 20 - 8 + ] LITERAL SWAP MOVE \ move the name into the header
 \ find a free image number to use and put it into the header
 FLIUSED FREEBIT
 DUP 10 > ABORT" Fatal: all 16 image numbers are in use"
 DUP [ 10000 20 - 0 + ] LITERAL C!
 ." Using image number 0x" .U2 CR
 \ find a free block to use and update the header's map
 FLBUSED FREEBIT
 DUP 10 > ABORT" Fatal: all 16 Flash blocks are in use"
 DUP 1 SWAP LSHIFT 10008 !
 DUP ." Using Flash block 0x" .U2 CR
 DUP FLB-DEL 0= ABORT" Fatal: Flash delete block failed" 
 \ flash block number is on the stack
 DUP 0 SWAP FLB-PROG
 0= ABORT" Fatal: Flash programming failed"
 \ now go back and calculate the checksum
 FLB2A \ block was on tos
 -1 OVER 4000 QCSUM NEGATE -
 OVER [ 10000 20 - 1C + ] LITERAL + \ lo address
 OVER FFFF AND \ lo data
 SWAP FL-PROG @ EXECUTE >R
 10 RSHIFT \ hi data
 SWAP [ 10000 20 - 1C + 2 + ] LITERAL + \ hi address
 FL-PROG @ EXECUTE R> AND
;

\ end
HAND
.( amcu_hu.f loaded.) CR


