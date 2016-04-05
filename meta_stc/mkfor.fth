\ create a file defining values for forward references. This can be read
\ the next time the compiler is run, resolving forward references.
S" forward.fth" R/W CREATE-FILE
[IF] .( Unable to create file forward.fth) ABORT [THEN]
VALUE FILE-ID 
S" \ Forward definitions" FILE-ID WRITE-LINE DROP
BASE @ HEX
CREATE DQUOTE CHAR " COMPILE,
' doDO       S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT doDO" FILE-ID WRITE-LINE 2DROP
' doCONST    S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT doCONST" FILE-ID WRITE-LINE 2DROP
' doVALUE    S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT doVALUE" FILE-ID WRITE-LINE 2DROP
' doLIST     S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT doLIST" FILE-ID WRITE-LINE 2DROP
' pipe       S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT pipe " FILE-ID WRITE-LINE 2DROP
' UNLOOP     S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT UNLOOP" FILE-ID WRITE-LINE 2DROP
' doS"       S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT doS" FILE-ID WRITE-FILE
DQUOTE 1 FILE-ID WRITE-LINE DROP 2DROP
' abort"msg  S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT abort" FILE-ID WRITE-FILE
DQUOTE 1 FILE-ID WRITE-FILE S" msg" FILE-ID WRITE-LINE 2DROP 2DROP
' TYPE       S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT TYPE" FILE-ID WRITE-LINE 2DROP
' COMPILE,   S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT COMPILE," FILE-ID WRITE-LINE 2DROP
' ROT        S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT ROT" FILE-ID WRITE-LINE 2DROP
' 2!         S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT 2!" FILE-ID WRITE-LINE 2DROP
' 2DROP      S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT 2DROP" FILE-ID WRITE-LINE 2DROP
' THROW      S>D <# # # # # # # # # #> FILE-ID WRITE-FILE S"  hCONSTANT THROW" FILE-ID WRITE-LINE 2DROP

FILE-ID CLOSE-FILE DROP
BASE !
