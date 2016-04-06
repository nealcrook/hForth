\
\ Multitasker Demo Program
\
\ This demo is very crude, however, it shows hForth's multitasking ability.
\
\ Reserve enough space for data and return stack for each task.
\ Data stack and return stack seems to be at lease 50 CELLS deep for MS-DOS.
\
\ 1995. 5. 1.
\ By Wonyong Koh

BASE @ DECIMAL

0 50 CELLS 50 CELLS HAT TASK0
0 50 CELLS 50 CELLS HAT TASK1
0 50 CELLS 50 CELLS HAT TASK2
0 50 CELLS 50 CELLS HAT TASK3
TASK0 BUILD
TASK1 BUILD
TASK2 BUILD
TASK3 BUILD
:NONAME TASK0 ACTIVATE BEGIN 1000 0 DO PAUSE LOOP [CHAR] 0 EMIT AGAIN ; EXECUTE
:NONAME TASK1 ACTIVATE BEGIN 2000 0 DO PAUSE LOOP [CHAR] 1 EMIT AGAIN ; EXECUTE
:NONAME TASK2 ACTIVATE BEGIN 4000 0 DO PAUSE LOOP [CHAR] 2 EMIT AGAIN ; EXECUTE
:NONAME TASK3 ACTIVATE BEGIN 8000 0 DO PAUSE LOOP [CHAR] 3 EMIT AGAIN ; EXECUTE
TASK0 SLEEP
TASK1 SLEEP
TASK2 SLEEP
TASK3 SLEEP

BASE !
CR .( Try 'TASK1 AWAKE  TASK2 AWAKE' and wait for a while.)
CR .( Try 'TASK2 SLEEP' and wait for a while and try 'TASK1 SLEEP'.)
CR .( You will get the idea.)

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
