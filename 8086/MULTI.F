\
\ hForth multitasker
\
\ Originally written by Bill Muench.
\ Adapted to hForth by Wonyong Koh
\
\ Usage:
\   HAT  ( user_size ds_size rs_size "<spaces>name" -- )
\	 Run-time: ( -- tid )
\	Create a new task.
\   BUILD  ( tid -- )
\	Initialize and link new task into PAUSE chain.
\   ACTIVATE  ( tid -- )
\	Activate the task identified by tid. ACTIVATE must be used
\	only in definition. The code following ACTIVATE must not
\	EXIT. In other words it must be infinite loop like QUIT.
\   .TASKS  ( -- )
\	Display tasks list in status-follower chain.
\   SLEEP  ( tid -- )
\	Sleep another task.
\   AWAKE  ( tid -- )
\	Awake another task.
\   PAUSE  ( -- )
\	Stop current task and transfer control to the task of which
\	'status' USER variable is stored in 'follower' USER variable
\	of current task.
\   STOP  ( -- )
\	Sleep current task.
\
\ 1997. 2. 28.
\	Facelift to be used with other CPUs.
\ 1995. 11. 3.
\	Fix ACTIVATE. sp@ should return a value not larger than sp0.

BASE @ HEX
GET-CURRENT
NONSTANDARD-WORDLIST SET-CURRENT

\ Structure of a task created by HAT
\ userP points follower.
\ //userP//return_stack//data_stack
\ //user_area/user1/taskName/throwFrame/stackTop/status/follower/sp0/rp0

\ 'PAUSE' and 'wake' are defined in assembler source.

\   PAUSE	( -- )
\		Stop current task and transfer control to the task of which
\		'status' USER variable is stored in 'follower' USER variable
\		of current task.
\ CHAR " PARSE systemID" ENVIRONMENT? DROP
\ CHAR " PARSE hForth 8086 ROM Model" COMPARE 0=
\ CHAR " PARSE systemID" ENVIRONMENT? DROP
\ CHAR " PARSE hForth 8086 RAM Model" COMPARE 0= OR
\ [IF]
\   : PAUSE	rp@ sp@ stackTop !  follower @ >R ; COMPILE-ONLY
\ [THEN]
\ CHAR " PARSE systemID" ENVIRONMENT? DROP
\ CHAR " PARSE hForth 8086 EXE Model" COMPARE 0=
\ [IF]
\   : PAUSE	rp@ sp@ stackTop !  follower @ code@ >R ; COMPILE-ONLY
\ [THEN]

\   wake	( -- )
\		Wake current task.
\ CHAR " PARSE systemID" ENVIRONMENT? DROP
\ CHAR " PARSE hForth 8086 ROM Model" COMPARE 0=
\ CHAR " PARSE systemID" ENVIRONMENT? DROP
\ CHAR " PARSE hForth 8086 RAM Model" COMPARE 0= OR
\ [IF]
\   : wake	R> userP !	  \ userP points 'follower' of current task
\		stackTop @ sp!		\ set data stack
\		rp! ; COMPILE-ONLY	\ set return stack
\ [THEN]
\ CHAR " PARSE systemID" ENVIRONMENT? DROP
\ CHAR " PARSE hForth 8086 EXE Model" COMPARE 0= OR
\ [IF]
\   : wake	R> CELL+ code@ userP !	\ userP points 'follower' of current task
\		stackTop @ sp!		\ set data stack
\		rp! ; COMPILE-ONLY	\ set return stack
\ [THEN]

\   STOP	( -- )
\		Sleep current task.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : STOP   ['] branch status ! PAUSE ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : STOP   ['] branch status @ code! PAUSE ;
[THEN]

\   's          ( tid a-addr -- a-addr' )
\		Index another task's USER variable
: 's
    userP @ -  SWAP	\ offset tid
    @  + ;

\   SLEEP	( tid -- )
\		Sleep another task.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : SLEEP   status 's  ['] branch  SWAP ! ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : SLEEP   status 's @  ['] branch  SWAP code! ;
[THEN]

\   AWAKE	( tid -- )
\		Awake another task.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : AWAKE   status 's  ['] wake  SWAP ! ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : AWAKE   status 's @  ['] wake  SWAP code! ;
[THEN]

\   HAT 	( user_size ds_size rs_size "<spaces>name" -- )
\		Run-time: ( -- tid )
\		Create a new task.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
[IF]
  : HAT
      RAM/ROM@ >R RAM
      CREATE HERE >R		\ user_size ds_size rs_size  R: tid
      0 ,	( reserve space for userP pointer)
      ALLOT	( Use 'HERE OVER ALLOT SWAP 0AA FILL')
		( to see how deep return stack grows.)
      ALIGN HERE cell- >R	\ user_size ds_size	 R: tid rp0
      ALLOT	( Use 'HERE OVER ALLOT SWAP 055 FILL')
		( to see how deep data stack grows.)
      ALIGN HERE cell- >R	\ user_size	     R: tid rp0 sp0
      ALLOT ALIGN
      [ 6 ( minimul USER variables) CELLS ] LITERAL ALLOT
      HERE cell-		\ user_pointer	     R: tid rp0 sp0
      R> , R> , ( store sp0 and rp0  )
      R@ !	( store userP pointer)
      lastName R> taskName 's ! \ store task name in new task's 'taskName'
      R> RAM/ROM! ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0=
[IF]
  : HAT
      CREATE HERE >R		\ user_size ds_size rs_size  R: tid
      0 ,	( reserve space for userP pointer)
      ALLOT	( Use 'HERE OVER ALLOT SWAP 0AA FILL')
		( to see how deep return stack grows.)
      ALIGN HERE cell- >R	\ user_size ds_size	 R: tid rp0
      ALLOT	( Use 'HERE OVER ALLOT SWAP 055 FILL')
		( to see how deep data stack grows.)
      ALIGN HERE cell- >R	\ user_size	     R: tid rp0 sp0
      ALLOT ALIGN
      [ 6 ( minimul USER variables) CELLS ] LITERAL ALLOT
      HERE cell-		\ user_pointer	     R: tid rp0 sp0
      R> , R> , ( store sp0 and rp0  )
      R@ !	( store userP pointer)
      lastName R> taskName 's ! ; \ store task name in new task's 'taskName'
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : HAT
      CREATE HERE >R		\ user_size ds_size rs_size  R: tid
      0 ,	( reserve space for userP pointer)
      ALLOT	( Use 'HERE OVER ALLOT SWAP 0AA FILL')
		( to see how deep return stack grows.)
      ALIGN HERE cell- >R	\ user_size ds_size	 R: tid rp0
      ALLOT	( Use 'HERE OVER ALLOT SWAP 055 FILL')
		( to see how deep data stack grows.)
      ALIGN HERE cell- >R	\ user_size	     R: tid rp0 sp0
      ALLOT ALIGN
      [ 4 ( minimul USER variables less 'status' and 'follower') CELLS ]
      LITERAL ALLOT
      xhere ALIGNED DUP CELL+ CELL+ TO xhere
      DUP ,		( store 'status' code-address)
      CELL+ ,		( store 'follower' code-address)
      HERE cell-		\ user_pointer	     R: tid rp0 sp0
      DUP COMPILE,	( store 'userP' pointer in code space)
      R> , R> , ( store sp0 and rp0  )
      R@ !	( store userP pointer)
      lastName R> taskName 's ! ; \ store task name in new task's 'taskName'
[THEN]

\   BUILD	( tid -- )
\		Initialize and link new task into PAUSE chain.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : BUILD
      DUP SLEEP 		\ sleep new task
      follower @ OVER		\ current task's 'follwer'
      follower 's !             \ store it in new task's 'follower'
      status 's follower ! ;    \ store new 'status' in current 'follower'
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : BUILD
      DUP SLEEP 		\ sleep new task
      follower @ code@ OVER
      follower 's @ code!       \ store current task's 'follwer' in new one
      status 's @ follower @ code! ; \ new 'status' in current task's follower
[THEN]

\   ACTIVATE	( tid -- )
\		Activate the task identified by tid. ACTIVATE must be used
\		only in definition. The code following ACTIVATE must not
\		EXIT. In other words it must be infinite loop like QUIT.
: ACTIVATE
    DUP @ CELL+ 2@ cell-	\ top of stack is in BX register
    SWAP			\ tid sp0 rp0
    R> OVER !			\ save entry at rp
    OVER !			\ save rp at sp
    OVER stackTop 's !          \ save sp in stackTop
    AWAKE ; COMPILE-ONLY

\   .TASKS  ( -- )
\	Display tasks list in status-follower chain.
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE ROM Model" COMPARE 0=
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE RAM Model" COMPARE 0= OR
[IF]
  : .TASKS
      follower			\ current task's follower
      BEGIN
	CR DUP [ taskName follower - ] LITERAL + @ .name
	DUP cell- @ ['] wake = IF ." awaked " ELSE  ." sleeping " THEN
	@ CELL+ 		\ next task's follower
      DUP follower =
      UNTIL DROP CR ;
[THEN]
CHAR " PARSE model" ENVIRONMENT? DROP
CHAR " PARSE EXE Model" COMPARE 0=
[IF]
  : .TASKS
      follower				\ current task's follower
      BEGIN
	CR DUP [ taskName follower - ] LITERAL + @ .name
	DUP @ cell- code@ ['] wake = IF ." awaked " ELSE  ." sleeping " THEN
	@ code@ CELL+ CELL+ code@	\ next task's follower
      DUP follower =
      UNTIL DROP CR ;
[THEN]


SET-CURRENT
BASE !

CHAR " PARSE FILE" ENVIRONMENT?
[IF]
  0= [IF] << CON [THEN]
[ELSE] << CON
[THEN]
