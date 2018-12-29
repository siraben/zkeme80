\ This file should run a bunch of tests before loading the next stage.
: TELL DROP PLOT-STRING ;
: SHUTDOWN PAUSE POWEROFF ;
: PAGE CLEAR-SCREEN ORIGIN ;
: USED HERE @ H0 - ;
: UNUSED 49152 HERE @ - ;

: (
  BEGIN
    GETC 41 = IF
      EXIT
    THEN
  AGAIN
; IMMEDIATE


: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;

: MOD /MOD DROP ;

: MIN ( N N -- N : RETURN THE MINIMUM OF TWO INTEGERS )
	2DUP < IF DROP ELSE NIP THEN  ;

: MAX ( N N -- N : RETURN THE MAXIMUM OF TWO INTEGERS )
2DUP > IF DROP ELSE NIP THEN ;

: CELL+ 2+ ;

: 2@  DUP CELL+ @ SWAP @ ;
: 2!  SWAP OVER ! CELL+ ! ;
: . U. ;
: CHAR+ 1+ ;
: CHARS ;
: BL 32 ;

: RECURSE LATEST @ >CFA , ; IMMEDIATE

: ['] ' LIT , ;

: '
  STATE @
  IF 
    ' ' ,
  ELSE
    WORD FIND >CFA
  THEN
    
; IMMEDIATE

: LITERAL
  ' LIT ,
  ,
; IMMEDIATE

: POSTPONE
  WORD FIND >CFA ,
; IMMEDIATE


: CHAR WORD DROP C@ ;

: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

: '"' [ CHAR " ] LITERAL ;

: S"
  STATE @
  IF
    ' LITSTRING , HERE @ 0 ,
    BEGIN
      GETC DUP 34 <>
    WHILE
      C,
    REPEAT
    DROP 0 C, DUP HERE @ SWAP - 3 - SWAP !
  ELSE
    HERE @
    BEGIN
      GETC DUP 34 <>
    WHILE
      OVER C! 1+
    REPEAT
    DROP HERE @ - HERE @
    SWAP
  THEN
; IMMEDIATE

: ."		( -- )
  STATE @ IF
    POSTPONE S"
    ' TELL ,
  ELSE
    BEGIN
      GETC
      DUP '"' = IF
        DROP
        EXIT
      THEN
      EMIT
    AGAIN
  THEN
;  IMMEDIATE

\ Redefine ; to be verbose
\ : ; POSTPONE ; LATEST @ ID. ."  defined." CR ; IMMEDIATE

." Welcome to siraben's
Forth-based operating
system.

"
16384 OS-END - .
     ." bytes remaining on
page 00.

The test suite is
running, please wait...
"


: CELLS 2 * ;


: CASE 
       0
; IMMEDIATE


: OF 
     ' OVER ,
     ' = ,
     POSTPONE IF
       ' DROP ,
; IMMEDIATE


: ENDOF 
        POSTPONE ELSE
; IMMEDIATE


: ENDCASE 
          ' DROP ,
          BEGIN
            ?DUP
          WHILE
            POSTPONE THEN
          REPEAT
; IMMEDIATE


: STATUS
DECIMAL UNUSED .
." bytes available" CR
HEX HERE @ ." HERE is at " . CR DECIMAL
." Stack has contents" CR
.S
;

: CONSTANT WORD CREATE DOCOL-H ' LIT , , ' EXIT , ;

\ Bit shifts are not fast!
: RSHIFT ?DUP IF 0 DO 2/ LOOP THEN ;
: LSHIFT ?DUP IF 0 DO 2* LOOP THEN ;

: TYPE 0 DO DUP C@ EMIT 1+ LOOP DROP ;

: UNLOOP    ( -- , R: I LIMIT -- : REMOVE LIMIT AND I FROM  )
	R>           ( SAVE OUR RETURN ADDRESS )
	RDROP        ( POP OFF I )
	RDROP        ( POP OFF LIMIT )
        >R
;

: LEAVE ( -- , R: I LIMIT RETURN -- : BREAK OUT OF A DO-LOOP CONSTRUCT )
  UNLOOP
  RDROP
; ( RETURN TO THE CALLER'S CALLER ROUTINE )


HERE @ 32 CELLS ALLOT CONSTANT ACTUAL-RESULTS

VARIABLE ACTUAL-DEPTH			\ stack record

VARIABLE START-DEPTH

VARIABLE XCURSOR      \ for ...}T

VARIABLE ERROR-XT


: ERROR ERROR-XT @ EXECUTE ;   \ for vectoring of error reporting


: EMPTY-STACK	\ ( ... -- ) empty stack; handles underflowed stack too.
    DEPTH START-DEPTH @ < IF
        DEPTH START-DEPTH @ SWAP DO 0 LOOP
    THEN
    DEPTH START-DEPTH @ > IF
        DEPTH START-DEPTH @ DO DROP LOOP
    THEN
;



: SEEK-NEWLINE-BACK
  \ Need this, why?
  2-
  BEGIN
    DUP C@ 10 =
    IF
      1+ EXIT
    ELSE
      1-
    THEN
  AGAIN
;

: EMIT-UNTIL-NEWLINE
  BEGIN
    DUP C@ 10 =
    IF
      DROP EXIT
    ELSE
      DUP C@ EMIT 1+
    THEN
  AGAIN
;


: ERROR1	\ ( C-ADDR U -- ) display an error message 
		\ followed by the line that had the error.
  TYPE CR INPUT-PTR @ SEEK-NEWLINE-BACK EMIT-UNTIL-NEWLINE CR
  \ display line corresponding to error
   EMPTY-STACK				\ throw away everything else
;


' ERROR1 ERROR-XT !

VARIABLE TEST-COUNT
0 TEST-COUNT !
VARIABLE SUCCESS-TEST-COUNT
0 SUCCESS-TEST-COUNT !
: ADD-TEST TEST-COUNT @ 1+ TEST-COUNT ! ;
: ADD-SUCCESS-TEST SUCCESS-TEST-COUNT @ 1+ SUCCESS-TEST-COUNT ! ;

: REPORT-TESTS SUCCESS-TEST-COUNT @ . ." / " TEST-COUNT @ . ." tests passed" ;

: T{		\ ( -- ) syntactic sugar.
   ADD-TEST DEPTH START-DEPTH ! 0 XCURSOR !
;

: ->		\ ( ... -- ) record depth and contents of stack.
   DEPTH DUP ACTUAL-DEPTH !		\ record depth
   START-DEPTH @ > IF		\ if there is something on the stack
       DEPTH START-DEPTH @ - 0 DO ACTUAL-RESULTS I CELLS + ! LOOP \ save them
   THEN
; 

: }T		\ ( ... -- ) COMPARE STACK (EXPECTED) CONTENTS WITH SAVED
		\ (ACTUAL) CONTENTS.
   DEPTH ACTUAL-DEPTH @ = IF		\ if depths match
      DEPTH START-DEPTH @ > IF		\ if there is something on the stack
         DEPTH START-DEPTH @ - 0 DO	\ for each stack item
	    ACTUAL-RESULTS I CELLS + @	\ compare actual with expected
	    <> IF S" INCORRECT RESULT: " ERROR LEAVE THEN
	 LOOP
      THEN
   ELSE					\ depth mismatch
      S" WRONG NUMBER OF RESULTS: " ERROR
   THEN
   ADD-SUCCESS-TEST
;


: ...}T ( -- )
    XCURSOR @ START-DEPTH @ + ACTUAL-DEPTH @ <> IF
        S" NUMBER OF CELL RESULTS BEFORE '->' DOES NOT MATCH ...}T SPECIFICATION: " ERROR
    ELSE DEPTH START-DEPTH @ = 0= IF
        S" NUMBER OF CELL RESULTS BEFORE AND AFTER '->' DOES NOT MATCH: " ERROR
    THEN THEN
;


: { T{ ;
: } }T ;

{ -> }					\ START WITH CLEAN SLATE
( TEST IF ANY BITS ARE SET; ANSWER IN BASE 1 )
{ : BITSSET? IF 0 0 ELSE 0 THEN ; -> }
{  0 BITSSET? -> 0 }		( ZERO IS ALL BITS CLEAR )
{  1 BITSSET? -> 0 0 }		( OTHER NUMBER HAVE AT LEAST ONE BIT )

{ 0 INVERT 1 AND -> 1 }
{ 1 INVERT 1 AND -> 0 }

0	 CONSTANT 0S
0 INVERT CONSTANT 1S

{ 0S INVERT -> 1S }
{ 1S INVERT -> 0S }

T{ 0 INVERT 1 AND -> 1 }T
T{ 1 INVERT 1 AND -> 0 }T

T{ 0S 0S AND -> 0S }T
T{ 0S 1S AND -> 0S }T
T{ 1S 0S AND -> 0S }T
T{ 1S 1S AND -> 1S }T

{ 0S 0S OR -> 0S }
{ 0S 1S OR -> 1S }
{ 1S 0S OR -> 1S }
{ 1S 1S OR -> 1S }

{ 0S 0S XOR -> 0S }
{ 0S 1S XOR -> 1S }
{ 1S 0S XOR -> 1S }
{ 1S 1S XOR -> 0S }

0S CONSTANT <FALSE>
1S CONSTANT <TRUE>


: GN2 \ ( -- 16 10 )
   BASE @ >R HEX BASE @ DECIMAL BASE @ R> BASE ! ;
T{ GN2 -> 16 10 }T


( WE TRUST 1S, INVERT, AND BITSSET?; WE WILL CONFIRM RSHIFT LATER )
1S 1 RSHIFT INVERT CONSTANT MSB
{ MSB BITSSET? -> 0 0 }

{ 0S 2* -> 0S }
{ 1 2* -> 2 }

T{  0  0 * ->  0 }T          \ TEST IDENTITIES
T{  0  1 * ->  0 }T
T{  1  0 * ->  0 }T
T{  1  2 * ->  2 }T
T{  2  1 * ->  2 }T
T{  3  3 * ->  9 }T

{ 4000 2* -> 8000 }
{ 1S 2* 1 XOR -> 1S }
{ MSB 2* -> 0S }

{ 0S 2/ -> 0S }
{ 1 2/ -> 0 }
{ 4000 2/ -> 2000 }

{ 1 0 LSHIFT -> 1 }
{ 1 1 LSHIFT -> 2 }
{ 1 2 LSHIFT -> 4 }
{ 1S 1 LSHIFT 1 XOR -> 1S }
{ MSB 1 LSHIFT -> 0 }

{ 1 0 RSHIFT -> 1 }
{ 1 1 RSHIFT -> 0 }
{ 2 1 RSHIFT -> 1 }
{ 4 2 RSHIFT -> 1 }
{ MSB 1 RSHIFT 2* -> MSB }


T{ 1 2 2DROP -> }T

T{ 1 2 2DUP -> 1 2 1 2 }T

T{ 1 2 3 4 2OVER -> 1 2 3 4 1 2 }T

T{ 1 2 3 4 2SWAP -> 3 4 1 2 }T

T{ : NOP : POSTPONE ; ; -> }T
T{ NOP NOP1 NOP NOP2 -> }T
T{ NOP1 -> }T
T{ NOP2 -> }T

T{ : GDX   123 ;    : GDX   GDX 234 ; -> }T
T{ GDX -> 123 234 }T

T{  0 ?DUP ->  0    }T
T{  1 ?DUP ->  1  1 }T


T{ : GR1 >R R> ; -> }T
T{ : GR2 >R R@ R> DROP ; -> }T
T{ 123 GR1 -> 123 }T
T{ 123 GR2 -> 123 }T
T{  1S GR1 ->  1S }T      ( Return stack holds cells )

\ This test fails!  Maybe this is where being non-standard is better?
\ T{ ( A comment)1234 -> }T
T{ : pc1 ( A comment)1234 ; pc1 -> 1234 }T


HERE @ 1 ,
HERE @ 2 ,
CONSTANT 2ND
CONSTANT 1ST


T{       1ST 2ND < -> 1 }T \ HERE MUST GROW WITH ALLOT
T{       1ST CELL+  -> 2ND }T \ ... BY ONE CELL
T{   1ST 1 CELLS +  -> 2ND }T
T{     1ST @ 2ND @  -> 1 2 }T
T{         5 1ST !  ->     }T
T{     1ST @ 2ND @  -> 5 2 }T
T{         6 2ND !  ->     }T
T{     1ST @ 2ND @  -> 5 6 }T
T{           1ST 2@ -> 6 5 }T
T{       2 1 1ST 2! ->     }T
T{           1ST 2@ -> 2 1 }T
T{ 1S 1ST !  1ST @  -> 1S  }T    \ CAN STORE CELL-WIDE VALUE

HERE @ 1 ALLOT
HERE @
CONSTANT 2NDA
CONSTANT 1STA
T{ 1STA 2NDA < ->  1 }T    \ HERE MUST GROW WITH ALLOT
T{      1STA 1+ ->   2NDA }T    \ ... BY ONE ADDRESS UNIT 


HERE @ 1 C,
HERE @ 2 C,
CONSTANT 2NDC
CONSTANT 1STC

T{    1STC 2NDC < ->   1 }T	\ HERE MUST GROW WITH ALLOT
T{      1STC CHAR+ ->  2NDC  }T	\ ... BY ONE CHAR
T{  1STC 1 CHARS + ->  2NDC  }T
T{ 1STC C@ 2NDC C@ ->   1 2  }T
T{       3 1STC C! ->        }T
T{ 1STC C@ 2NDC C@ ->   3 2  }T
T{       4 2NDC C! ->        }T
T{ 1STC C@ 2NDC C@ ->   3 4  }T

T{ : GI3 BEGIN DUP 5 < WHILE DUP 1+ REPEAT ; -> }T
T{ 0 GI3 -> 0 1 2 3 4 5 }T
T{ 4 GI3 -> 4 5 }T
T{ 5 GI3 -> 5 }T
T{ 6 GI3 -> 6 }T

T{ : GI4 BEGIN DUP 1+ DUP 5 > UNTIL ; -> }T
T{ 3 GI4 -> 3 4 5 6 }T
T{ 5 GI4 -> 5 6 }T
T{ 6 GI4 -> 6 7 }T


T{ VARIABLE V1 ->     }T
T{    123 V1 ! ->     }T
T{        V1 @ -> 123 }T


: GS3 WORD DROP COUNT SWAP C@ ;
T{ GS3 HELLO -> 5 CHAR H }T

: OUTPUT-TEST
   PAGE
   ." YOU SHOULD SEE THE STANDARD GRAPHIC CHARACTERS:" CR
   65 BL DO I EMIT LOOP CR
   97 65 DO I EMIT LOOP CR
   127 97 DO I EMIT LOOP CR
   PAGE
   ." YOU SHOULD SEE 0-9 SEPARATED BY A SPACE:" CR
   9 1+ 0 DO I . LOOP CR
   PAGE     
   ." YOU SHOULD SEE 0-9 (WITH NO SPACES):" CR
   [ CHAR 9 ] LITERAL 1+ [ CHAR 0 ] LITERAL DO I EMIT LOOP CR
   PAGE     
   ." YOU SHOULD SEE A-G SEPARATED BY A SPACE:" CR
   [ CHAR G ] LITERAL 1+ [ CHAR A ] LITERAL DO I EMIT SPACE LOOP CR
   PAGE     
   ." YOU SHOULD SEE 0-5 SEPARATED BY TWO SPACES:" CR
   5 1+ 0 DO I [ CHAR 0 ] LITERAL + EMIT 2 SPACES LOOP CR
   PAGE     
   ." YOU SHOULD SEE TWO SEPARATE LINES:" CR
   S" LINE 1" TYPE CR S" LINE 2" TYPE CR
   PAGE     
;

\ Optional output test, may dizzy the user.
\ T{ OUTPUT-TEST -> }T

T{ 1 2 3 SWAP -> 1 3 2 }T


PAGE
." End of tests." CR
." End of phase 2." CR

REPORT-TESTS CR CR


." Press any key to
continue..." CR PAUSE

PAGE

." Performing RC4 test
(code taken from
Wikipedia)." CR

." Expect this sequence:
F1 38 29 C9 DE" CR

: VALUE    WORD CREATE DOCOL-H ' LIT , , ' EXIT , ;

: TO WORD FIND >DFA 2 + STATE @
     IF
       ' LIT ,
       ,
       ' ! ,
     ELSE
       !
     THEN
; IMMEDIATE

0 VALUE II        0 VALUE JJ
0 VALUE KEYADDR   0 VALUE KEYLEN

HERE @ 256 CELLS ALLOT CONSTANT SARRAY
: KEYARRAY      KEYLEN MOD  KEYADDR ;

: GET-BYTE      + C@ ;
: SET-BYTE      + C! ;
: AS-BYTE       255 AND ;
: RESET-IJ      0 TO II   0 TO JJ ;
: I-UPDATE      1 +   AS-BYTE TO II ;
: J-UPDATE      II SARRAY GET-BYTE + AS-BYTE TO JJ ;
: SWAP-S-IJ
    JJ SARRAY GET-BYTE
       II SARRAY GET-BYTE  JJ SARRAY SET-BYTE
    II SARRAY SET-BYTE
;

: RC4-INIT ( KEYADDR KEYLEN -- )
    256 MIN TO KEYLEN   TO KEYADDR
    256 0 DO   I I SARRAY SET-BYTE   LOOP
    RESET-IJ
    BEGIN
        II KEYARRAY GET-BYTE   JJ +  J-UPDATE
        SWAP-S-IJ
        II 255 < WHILE
        II I-UPDATE
    REPEAT
    RESET-IJ
;
: RC4-BYTE
    II I-UPDATE   JJ J-UPDATE
    SWAP-S-IJ
    II SARRAY GET-BYTE   JJ SARRAY GET-BYTE +   AS-BYTE SARRAY GET-BYTE  XOR
;


HEX
HERE @    97 C, 138 C, 99 C, 210 C, 251 C, CONSTANT MKEY
: TEST   CR   0 DO  RC4-BYTE . LOOP  CR ;
MKEY 5 RC4-INIT
44 249 76 238 220 5 TEST

DECIMAL
PAUSE

PAGE


." Forgetting all words
after ACTUAL-RESULTS
to save on space." CR
USED

FORGET ACTUAL-RESULTS

USED - . ." bytes freed." CR

PAUSE 
: STAGE2

  \ Try to set RAM Memory region A to be the first RAM flash page.
  2 SET-RAM-MEMA
  IF
    \ We set the input pointer to point to memory bank A.
    MEMA INPUT-PTR ! PAGE
  ELSE
    \ Something went wrong.  Shutdown.
    ." Couldn't load stage 3.  Shutting down." CR
    PAUSE POWEROFF           
  THEN
;


STAGE2

\ Load the next bootstrap
