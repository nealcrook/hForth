\ compat.fs - compatibility words
\
\ Intention here is to add any toolbox words that are not ANS Forth
\ and to declare any environmental dependencies

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Environmental depencencies
\
\ ALIGN (and probably other stuff relies on 2's complement arithmetic on
\ the host.

\ in Gforth allows all white space..
: PARSE SWORD ;

\ TODO if the host system does not define it...
\ COMPILE-ONLY
\ : COMPILE-ONLY ;

\ TODO if the host system does not define it..
\ ALIAS

\ : ALIAS CREATE , DOES> @ EXECUTE ;

\ TODO.. make a portable version of VOCABULARY
\ if the host system doesn't have it
\ use Vocabulary. Equivalent in ANS Forth is:
\ : Vocabulary
\   wordlist create ,
\ does> @ >r get-order nip r> swap set-order ;
\ in hForth WORDLIST-NAME associates a string with a WID such that ORDER
\ can display it, so want to use that here.

\ TODO sometimes have this..
: cell- [ 1 cells ] literal - ;
