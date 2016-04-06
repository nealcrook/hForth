\
\ SAVE.F
\
\ SAVE.EXE is built from HF86EXE.EXE by loading Forth sources
\   in the following order.
\
\       << OPTIONAL.F
\       << ASM8086.F
\       << COREEXT.F
\       << MSDOS.F
\       BL PARSE SAVE.F INCLUDED

GET-CURRENT
NONSTANDARD-WORDLIST SET-CURRENT

: <<   CR ." Do NOT use '<<'."
       CR ." Use 'BL PARSE filename INCLUDED' instead of '<< filename'."
       ABORT ; IMMEDIATE

SET-CURRENT
BL PARSE MULTI.F INCLUDED
SAVE-SYSTEM-AS SAVE.EXE
