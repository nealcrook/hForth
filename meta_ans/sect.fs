\ TODO - turn this into a sections and addressing test suite
\ include SAVE-SECTIONS <create new section> RESTORE-SECTIONS

\ demo sections
HOST
base @ hex
INTERPRETER

idata 10 3f     section i-sect
     30 1ff     section i-sect2
variables
udata 10b0 20ff section u-sect
          20 5f section u-sect2
cdata  100 14f  section c-data
       1a0 1af  section c-data2	  

HOST
base !
TARGET