어셈블리 소스를 크게 다듬었습니다. 덧붙임말로 설명을 더했습니다.
환경 변수 'systemID'를 'CPU'와 'Model'로 나누었습니다. 환경 변수를
사용한 조건부 번역 방법을 써서 대부분의 hForth 바탕본을 고치지 않고
다른 CPU용 hForth에서 쓸 수 있습니다 (COREEXT.F와 DOUBLE.F를
보십시오). Turbo assembler뿐 아니라 마이크로소프트사의 MASM 6.11에서도
에러없이 어셈블이 되도록 어셈블리 바탕본을 고쳤습니다.

제어 구조 낱말을 모두 고쳤습니다. 제어 흐름 더미(control-flow stack)를
이제 자료 더미에서 아래처럼 자료 더미에서 각각 두 개의 값으로
표현합니다.

Control-flow stack item     Representation (parameter and type)
-----------------------    -------------------------------------
     dest                    control-flow destination      0
     orig                    control-flow origin           1
     of-sys                  OF origin                     2
     case-sys                x (any value)                 3
     do-sys                  ?DO origin           DO destination
     colon-sys               xt of current definition     -1

이제 "BEGIN IF AGAIN THEN" 같은 잘못도 쉽게 잡을 수 있습니다. 그리고
자료 흐름 더미의 값을 종류를 논리합(OR) 연산한 결과가 1인지를 확인해서
dest와 orig에 대해서만 적용할 수 있는 CS-ROLL과 CS-PICK를 부를 수
있는지 없는지를 확인할 수 있습니다. 이제 hForth가 잡지 못하는 제어
구조의 잘못은 없으리라고 생각합니다.

RESTORE-INPUT을 비롯한 몇 가지 벌레를 잡았습니다.


Changes from 0.9.7

1997. 5. 26.
      MSDOS.F: Fix RESTORE-INPUT to restore BLK correctly.
1997. 2. 28.
      Facelift to be used with other CPUs.
1997. 2. 19.
      Split environmental variable systemID into CPU and Model.
1997. 2. 6.
      Add Neal Crook's microdebugger and comments on assembly definitions.
1997. 1. 25.
      Add $THROWMSG macro and revise accordingly.
1997. 1. 18.
      Remove 'NullString' from assembly source.
1996. 12. 18.
      Revise 'head,'.
1996. 12. 14.
      DOSEXEC.F: Revise (DOSEXEC) in MSDOS.F.
1996. 12. 6.
      OPTIONAL.F: Fix 'compiles>' for colon-sys.
1996. 11. 29.
      OPTIONAL.F: Remove PICK which was added in assembly source.
      OPTIONAL.F: Revise CASE, ENDCASE, OF, ENDOF, RETRY for
		control-flow stack.
      OPTIONAL.F: Revise '.xt' due to the removal of 'do."' and change
		of 'doS"'.
1996. 12. 3.
      Revise PICK to catch stack underflow.
1996. 11. 29.
      Implement control-flow stack on data stack. Control-flow stack
	      item consists of two data stack items, one for value
	      and one for the type of control-flow stack item.
      control-flow stack item	data stack representation
	      dest	      control-flow_destination	      0
	      orig	      control-flow_origin	      1
	      of-sys	      OF_origin 		      2
	      case-sys	      x (any value)		      3
	      do-sys	      ?DO_origin	 DO_destination
	      colon-sys       xt_of_current_definition	     -1
      Add PICK.
      'bal' is now the depth of control-flow stack.
      Drop 'lastXT'.
      Introduce 'notNONAME?'
      Add 'bal+' and 'bal-'. Drop  'orig+', 'orig-', 'dest+', 'dest-',
	      'dosys+', and 'dosys-'.
      Revise ':NONAME', ':', ';', 'linkLast', 'head,', RECURSE, 'DOES>',
	      CONSTANT, CREATE, VALUE, VARIABLE, and QUIT.
	      This change makes RECURSE work properly in ':NONAME ... ;'
	      and '... DOES> ... ;'.
      Revise 'rake', AGAIN, AHEAD, IF, THEN, +LOOP, BEGIN, DO, ELSE, LOOP,
	      UNTIL, and WHILE.
      Revise SLITERAL, '."', 'doS"' to allow a string larger than
	      max char size.
      Revise $INSTR and remove 'do."'.
      Revise 'pack"'.
      ASM8086.F: Revise ';CODE' for control-flow stack.
      COREEXT.F: Provide CODE definition of ROLL.
      COREEXT.F: Revise '?DO' for control-flow stack.
      COREEXT.F: Revise 'C"' to catch exception -24 'parsed string overflow'.
1996. 8. 17.
      HF86EXE.ASM: Revise MAX-UD.
1996. 8. 10.
      HF86EXE.ASM: Replace 'COMPILE,' with 'code,' in the definition of
		'compileCREATE'.
1996. 7. 19.
      DOUBLE.F: Fix 'M+'. Thanks M. Edward Borasky.
1996. 6. 19.
      Fix '/STRING'.
1996. 4. 15.
      ASM8086.F: ';CODE' is fixed. END-CODE is changed.
1996. 3. 1.
      MSDOS.F: Shift value of 'ior' of FILE words to system THROW code area,
		i.e. -4096..-256 .
      MSDOS.F: Remove subtle bug in 'SAVE-SYSTEM-AS'.

Changes from 0.9.6

1996. 2. 10.
      Revise FM/MOD and SM/REM to catch result-out-of-range error in
	      '80000. 2 FM/MOD'.
1996. 1. 19.
      Rename 'x,' to 'code,'; 'x@' to 'code@'; 'x!' to 'code!';
	      'xb@' to 'codeB@' and 'xb!' to 'codeB!'.
1996. 1. 7
      Rename non-Standard 'parse-word' to PARSE-WORD.
1995. 12. 2
      Drop '?doLIST' and revise 'optiCOMPILE,'.
1995. 11. 28
      Drop 'LIT,:' all together.
      Return CELLS to non-IMMEDIATE definition.

Changes from 0.9.5

1995. 11. 27.
    In ASM8086.F
    ';CODE' is redefined following the change of 'DOES>' and 'doCREATE'.
1995. 11. 25.
    Add RETRY described by Dr. Astle in Forth Dimensions 17(4), 19-21 (1995).
1995. 11. 25.
    Make 'lastXT' VALUE word.
1995. 11. 24.
    For RAM model only
	Replace 'xhere' with HERE.
	Revise doVALUE, VALUE, TO, and all $VALUE macros.
1995. 11. 23.
    Revise doCREATE, CREATE, pipe, DOES>, and >BODY.
    'pipe' is no longer processor-dependent.
1995. 11. 17.
    Move ERASE to ASM8086.F.
