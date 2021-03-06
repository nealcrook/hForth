
hForth 0.9.9 판입니다. hForth는 ANS Forth 표준을 따르는 작은 포스
시스템입니다. hForth는 무료 프로그램(free software)입니다. 이것을
개인적으로나 상업적으로나 어떤 목적에 사용해도 좋습니다. 단, 이
프로그램만을 팔아서는 안되고 (hForth를 써서 만든 프로그램을 파는
것은 제가 상관하지 않고 거기에 hForth 꾸러미가 들어 있어도
좋습니다.) hForth라는 이름을 쓰려면 꾸러미의 파일들이 본래 상태로
있어야 합니다. 가능한 한 hForth를 널리 퍼뜨려주십시오.

hForth에는 어셈블리 바탕본이 3 개 들어 있습니다. HF86ROM.COM은
ANSEF86.COM처럼 RAM과 ROM가 다른 주소에 있는 시스템에서 쓰이도록
만들었습니다. HF86RAM.COM은 HF86RAM을 고쳐 RAM만을 쓰는 시스템
용으로 만들었습니다. HF86EXE.EXE는 8086에서 코드와 자료를 다른
세그먼트에 넣고 여러 낱말을 기계어로 정의했습니다. HF86EXE.EXE는
이렇게 해서 HF86ROM.COM과 HF86RAM.COM에 비해 두 배의 메모리를 쓸
수 있고 속도도 두 배 이상 빨라졌습니다. 각 어셈블리어 바탕본의 첫
머리에 각 바탕본들이 어떻게 다른지를 적어 두었습니다. 한두 개의
어셈블리어 낱말를 더한 것 말고는 똑같은 어셈블리어 낱말들을써서
HF86ROM.ASM으로부터 HF86RAM을 만들었습니다. 기계어로 정의한
낱말만을 바꾸어서 Z80에도 hForth를 심었습니다.

이 꾸러미에는 아래 파일들이 들어 있습니다.

HF86ROM.ASM  IBM-PC 용 hForth 8086 ROM 모델의 TASM 바탕본.
HF86RAM.ASM  IBM-PC 용 hForth 8086 RAM 모델의 TASM 바탕본.
HF86EXE.ASM  IBM-PC 용 hForth 8086 EXE 모델의 TASM 바탕본.
HF86ROM.COM  hForth 8086 ROM 모델의 실행 파일.
HF86RAM.COM  hForth 8086 RAM 모델의 실행 파일.
HF86EXE.EXE  hForth 8086 EXE 모델의 실행 파일.
SAVE.EXE     OPTIONAL.F와 ASM8086.F와 COREEXT.F와 MSDOS.F와
	     MULTI.F를 더한 HF86EXE.EXE.
SAVE1.EXE    HIOMULTI.F를 더한 SAVE.EXE.
SAVE2.EXE    HIOMULT2.F를 더한 SAVE.EXE.
HTURTLE.EXE  우리말 거북 그림 통역기.
ETURTLE.EXE  영문 거북 그림 통역기.
OPTIONAL.F   선택 낱말(OPTIONAL wordset words)들의 포스 바탕본.
ASM8086.F    8086 어셈블러 포스 바탕본.
ASMTEST.F    8086 어셈블러를 시험하기 위한 포스 바탕본.
COREEXT.F    ANS 표준 포스 프로그램에 쓰지 말아야 할 낡은 낱말을 뺀
	     (OPTIONAL.F에 빠진) 다른 모든 CORE EXT 낱말의 포스 바탕본.
MULTI.F      Bill Muench의 다중작업기(multitasker)의 포스 바탕본.
MULDEMO.F    다중 작업기를 시험하기 위한 간단한 포스 바탕본.
MSDOS.F      DOS 함수를 써서 정의한 FILE 낱말과 BLOCK 낱말들.
DOSEXEC.F    hForth에서 도스 프로그램을 부르는 낱말의 정의.
SAVE.F	     SAVE.EXE를 만드는 포스 바탕본.
DOUBLE.F     완전한 DOUBLE과 DOUBLE EXT 낱말의 정의.
HIOMULTI.F   다중작업기를 쓰는 한글입출력 포스 바탕본.
HIOMULT2.F   글꼴을 바꿀 수 있는 한글입출력 포스 바탕본.
ENG.FNT      HIOMULT2.F에서 쓰는 영문 글꼴.
HAN.FNT      HIOMULT2.F에서 쓰는 한글 글꼴.
CLOCK.F      SAVE2.EXE에서 쓸 수 있는 다중 작업기 시계
STACK.F      SAVE2.EXE에서 화면에 더미의 내용을 보이는 프로그램.
TURTLE.F     우리말 거북 그림 통역기 포스 바탕본.
HTURTLE.GLO  거북 그림 낱말 풀이.
SIO.F	     직렬 통신 프로그램. 굳은모 제어의 예.
LOG.F	     화면 출력을 HFORTH.LOG에 갈무리하는 프로그램.
DISCP.F      M. Edward Borasky의 Dijkstra guarded command control
	     structures.
MEMORY.F     MEMORY ALLOCATION 낱말의 정의. Gordon Chlarlton의
	     MEMORY.FTH를 고친 것.
MEMORY.FTH   Gordon Charlton의 본래 파일.
DEBUGGER.ANS ANS 표준 포스 낱말로 만든 Joerg Plewe의 벌레잡개.
HFORTH.HTM   Forth Dimensions에 실렸던 hForth 소개글
README.KOR   이 파일 (조합형 한글).
README.KS    이 파일 (완성형 한글).

각 실행 파일들은 아래 방법으로 만들었습니다.

>TASM /ml HF86ROM 또는 HF86RAM 또는 HF86EXE
>TLINK /t HF86ROM 또는 TLINK /t HF86RAM 또는 TLINK HF86EXE

꾸러미 안의 SAVE.EXE는 HF86EXE.EXE를 시작한 다음 아래처럼 해서
마련한 것입니다.

    << OPTIONAL.F
    << ASM8086.F
    << MSDOS.F
    BL PARSE SAVE.F INCLUDED		\ 또는 INCLUDE SAVE.F

시작하면 바로 그래픽 화면에 영문 알파벳과 한글을 보이는 SAVE2.EXE는
SAVE.EXE를 시작한 다음, 아래처럼 해서 마련한 것입니다.

    BL PARSE HIOMULT2.F INCLUDED	\ 또는 INCLUDE HIOMULT2.F
    SAVE-SYSTEM-AS  SAVE2.EXE

바로 위에서 보신 것처럼 이제 '<<'를 쓰지 않고 표준 포스의 FILE
낱말인 INCLUDED나 비표준 낱말 INCLUDE를 써서 포스 바탕본을 읽어 들일
수 있습니다. MSDOS.F를 올리신 후에는 '<<'를 쓰지 마십시오.

앤시 표준 포스의 CORE 낱말들은 모두 어셈블리어 바탕본에
들어있습니다. 완전한 TOOLS 낱말과 SEARCH ORDER 낱말과 SEARCH ORDER
EXTENSION 낱말들과 다른 쓸모있는 낱말들은 OPTIONAL.F에
들어있습니다. 어셈블리어로 정의하는 것이 나은 CORE EXTENSION
낱말들은 COREEXT.F에 들어있습니다.

'init-i/o와 'boot를 써서 정해진 일을 하는 응용 프로그램을 만들 수
있습니다. SAVE-SYSTEM이나 SAVE-SYSTEM-AS로 갈무리한 프로그램이 처음
시켜질 때 'init-i/o가 시켜지고 나서 'boot가 시켜집니다. 'init-i/o는
예외 처리를 하는 THROW도 사용합니다. 에러가 발생했다면 입출력의
방향을 제대로 돌려 놓아야만 글쇠판에서 입력을 받을 수 있을
것입니다. HIOMULTI.F와 HIOMULT2.F에서 'init-i/o는 텍스트 화면에
출력하고 영문 알파벳 입력만 받을 것인지 그래픽 화면에 출력하고 한글
입력도 받을 것인지를 정하는 NEW-SET-I/O를 가리킵니다. HIOMULTI.F와
HIOMULT2.F에서 'boot는 인사말을 보이고 도스 명령행에서 프로그램을
뺀 나머지 부분을 PAD로 옮기고 포스 통역기를 시작합니다. 명령행을
그래픽 화면에 보이는 프로그램을 HIOMULT2.F의 NEW-hi를 고쳐서
아래처럼 만들고 시켜 보십시오.

    C:\HF>SAVE2

    hForth 8086 EXE Model v0.9.9 by Wonyong Koh, 1997
    ALL noncommercial and commercial uses are granted.
    Please send comment, bug report and suggestions to:
      wykoh@pado.krict.re.kr or 82-42-861-4245 (FAX)
    의견과 제안과 비평을 하이텔 wykoh로 보내 주십시오.

    HEX
    : SAMPLE
	CS@ 10 -  \ PSP segment
	80 2DUP LC@ 1+ 0 DO 2DUP LC@ PAD I + C! CHAR+ LOOP 2DROP
	HGRAPHIC
	PAD COUNT TYPE CR CR
	." 아무 글쇠나 누르십시오." KEY BYE ;
    ' SAMPLE TO 'boot
    SAVE-SYSTEM-AS BYE.EXE
    BYE
    C:\HF>BYE 11 22 33

HIOMULTI.F와 HIOMULT2.F에서 다중작업기를 실제 문제에 어떻게
적용하는지 볼 수 있습니다. 포스 통역기가 글쇠 입력을 기다리는 동안에
화면이 위로 말리기 때문에 그래픽 화면 출력이 텍스트 화면 출력만큼
빠릅니다.

LOG.F에 들어 있는 LOGON, LOGOFF 낱말을 써서 화면 출력을 텍스트 파일
HFORTH.LOG에 갈무리할 수 있습니다. 글쇠판에 만든 낱말들을 갈무리
해서 나중에 쉽게 포스 바탕본을 만들 수 있습니다. DOSEXEC.F의 낱말을
써서 도스 실행 파일을 hForth에서 부를 수 있습니다. hForth를 빠져
나가지 않고 Q Editor나 U Editor를 불러 포스 바탕본을 편집하고
편집기를 빠져 나와서 포스 바탕본을 올리고 낱말들을 시험할 수
있습니다. 이렇게 할 때는 포스 바탕본의 맨 앞에 'MARKER 지우기'을
넣어서 나중에 낱말 '지우기'를 시켜서 더한 낱말들을 쉽게 지울 수 있게
하는 것이 좋을 것입니다. LOG.F와 DOSEXEC.F 파일 앞부분의 낱말 사용
설명을 참조하십시오.

거북 그림(Turtle Graphics) 통역기는 이제 쓸만한 상태가 되었습니다.

hForth는 1990 년에 Bill Muench와 Dr. C. H. Ting이 발표한
eForth를바탕으로 만들어서 본래의 eForth의 특징들을 그대로
살렸습니다. 아래는 8086 eForth 바탕본에서 따 온 것입니다.

    > 각각의 마이크로프로세서에 맞춘 몇 안되는 CODE 낱말들과 모든
    >	  마이크로프로세서에 공통인 고급 (high level) 낱말들로
    > 이루어져 있습니다.
    > 원시코드는 MASM 어셈블러용입니다.
    > 직접 꿴 (direct threaded) 방법을 씁니다.
    > 사전의 코드와 이름이 메모리에 따로 자리합니다.
    > 입출력은 가리킴 낱말을 통하고 주 컴퓨터(host computer)를
    >	  단말기와 파일 입출력에 이용합니다.
    > 제안된 미국 표준 포스(ANS Forth)의 방향을 따랐습니다.
    > 어떤 마이크로프로세서에 맞추어 최적화하기가 쉽습니다.

이것들은 그대로 hForth의 성질입니다. 그리고 hForth 는 앤시 표준
포스의 방향만을 따르는 것이 아니라 앤시 표준 포스의 요구 조건을
모두 만족하는 앤시 표준 포스 시스템입니다.

여러 분의 의견, 제안, 비평을 들려 주십시오. 도움을 주실 수 있다면
더욱 좋습니다.

1996. 6. 2.

고원용
인터넷: wykoh@pado.krict.re.kr
하이텔: wykoh
