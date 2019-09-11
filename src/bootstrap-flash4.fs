: TEST-SUITE-START
." The test suite is
running, please wait..." CR
;

: GREETING
." Welcome to the test
suite"  
;

: PRESS-TO-CONTINUE
." Press any key to
continue..." PAUSE CR
;

\ end of bootstrap definitions

PAGE GREETING CR CR TEST-SUITE-START PAGE

\ Any word defined from this point on to the end of this stage.  will
\ be forgotten.
HERE 32 CELLS ALLOT CONSTANT ACTUAL-RESULTS

VARIABLE ACTUAL-DEPTH \ stack record

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


: ERROR1	\ ( c-addr u -- ) display an error message
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
: ADD-TEST 1 TEST-COUNT +! ;
: ADD-SUCCESS-TEST 1 SUCCESS-TEST-COUNT +! ;

: REPORT-TESTS PAGE SUCCESS-TEST-COUNT @ . ." / " TEST-COUNT @ . ;

: T{		\ ( -- ) syntactic sugar.
   ADD-TEST DEPTH START-DEPTH ! 0 XCURSOR !
;

: ->		\ ( ... -- ) record depth and contents of stack.
   DEPTH DUP ACTUAL-DEPTH !		\ record depth
   START-DEPTH @ > IF		\ if there is something on the stack
       DEPTH START-DEPTH @ - 0 DO ACTUAL-RESULTS I CELLS + ! LOOP \ save them
   THEN
;
: CLEAR-TITLE 0 0 MAX-COL 5 RECT-AND ;

: UPDATE-TEST-STATUS ORIGIN CLEAR-TITLE REPORT-TESTS ;

: }T		\ ( ... -- ) compare stack (expected) contents with saved
                \ (actual) contents.
   DEPTH ACTUAL-DEPTH @ = IF		\ if depths match
      DEPTH START-DEPTH @ > IF		\ if there is something on the stack
         DEPTH START-DEPTH @ - 0 DO	\ for each stack item
            ACTUAL-RESULTS I CELLS + @	\ compare actual with expected
            <> IF S" INCORRECT RESULT: " ERROR UPDATE-TEST-STATUS LEAVE THEN
         LOOP
      THEN
   ELSE					\ depth mismatch
      S" WRONG NUMBER OF RESULTS: " ERROR UPDATE-TEST-STATUS EXIT
   THEN
   \ The test was good.
   ADD-SUCCESS-TEST UPDATE-TEST-STATUS
;


: ...}T ( -- )
    XCURSOR @ START-DEPTH @ + ACTUAL-DEPTH @ <> IF
        S" NUMBER OF CELL RESULTS BEFORE '->' DOES NOT MATCH ...}T SPECIFICATION: " ERROR
    ELSE DEPTH START-DEPTH @ = 0= IF
        S" NUMBER OF CELL RESULTS BEFORE AND AFTER '->' DOES NOT MATCH: " ERROR
    THEN THEN
;


\ start with clean slate
T{ -> }T
( test if any bits are set; answer in base 1 )
T{ : BITSSET? IF 0 0 ELSE 0 THEN ; -> }T
T{  0 BITSSET? -> 0 }T    ( zero is all bits clear )
T{  1 BITSSET? -> 0 0 }T  ( other number have at least one bit )

T{ 0 INVERT 1 AND -> 1 }T
T{ 1 INVERT 1 AND -> 0 }T

0	 CONSTANT 0S
0 INVERT CONSTANT 1S

T{ 0S INVERT -> 1S }T
T{ 1S INVERT -> 0S }T

T{ 0S 0S AND -> 0S }T
T{ 0S 1S AND -> 0S }T
T{ 1S 0S AND -> 0S }T
T{ 1S 1S AND -> 1S }T

T{ 0S 0S OR -> 0S }T
T{ 0S 1S OR -> 1S }T
T{ 1S 0S OR -> 1S }T
T{ 1S 1S OR -> 1S }T

T{ 0S 0S XOR -> 0S }T
T{ 0S 1S XOR -> 1S }T
T{ 1S 0S XOR -> 1S }T
T{ 1S 1S XOR -> 0S }T

0S CONSTANT <FALSE>
1 CONSTANT <TRUE>

T{ TRUE -> <TRUE> }T

: GN2 \ ( -- 16 10 )
   BASE @ >R HEX BASE @ DECIMAL BASE @ R> BASE ! ;
T{ GN2 -> 16 10 }T


( we trust 1s, invert, and bitsset?; we will confirm rshift later )
1S 1 RSHIFT INVERT CONSTANT MSB
T{ MSB BITSSET? -> 0 0 }T

T{ 0S 2* -> 0S }T
T{ 1 2* -> 2 }T

T{ 0 0 * -> 0 }T  \ Test identities
T{ 0 1 * -> 0 }T
T{ 1 0 * -> 0 }T
T{ 1 2 * -> 2 }T
T{ 2 1 * -> 2 }T
T{ 3 3 * -> 9 }T


T{  5  0 - -> 5 }T
T{ 10  3 - -> 7 }T

T{ 4000 2* -> 8000 }T
T{ 1S 2* 1 XOR -> 1S }T
T{ MSB 2* -> 0S }T

T{ 3 1- -> 2 }T
T{ 3 2+ -> 5 }T
T{ 3 2- -> 1 }T

T{ : GC1 [CHAR] X     ; -> }T
T{ : GC2 [CHAR] HELLO ; -> }T
T{ GC1 -> 88 }T
T{ GC2 -> 72 }T

T{ : GC3 [ GC1 ] LITERAL ; -> }T
T{ GC3 -> 88 }T


T{ : GT1 123 ;   ->     }T
T{ ' GT1 EXECUTE -> 123 }T

T{ : GT2 ['] GT1 ; IMMEDIATE -> }T
T{ GT2 EXECUTE -> 123 }T


: TMOD /MOD DROP ;
: T/   /MOD SWAP DROP ;

T{ 0 1 / -> 0 1 T/ }T
T{ 1 1 / -> 1 1 T/ }T
T{ 2 1 / -> 2 1 T/ }T
T{ 2 2 / -> 2 2 T/ }T
T{ 7 3 / -> 7 3 T/ }T

T{ 0 1 MOD -> 0 1 TMOD }T
T{ 1 1 MOD -> 1 1 TMOD }T
T{ 2 1 MOD -> 2 1 TMOD }T

T{ 0 0= -> 1 }T
T{ 1 0= -> 0 }T
T{ 2 0= -> 0 }T

T{ 0 0  = -> <TRUE>  }T
T{ 0 0 >= -> <TRUE>  }T
T{ 0 0 <= -> <TRUE>  }T

T{ 0 1  = -> <FALSE> }T
T{ 0 1 >= -> <FALSE> }T
T{ 0 1 <= -> <TRUE> }T

T{ 1 0  = -> <FALSE> }T
T{ 1 0 >= -> <TRUE> }T
T{ 1 0 <= -> <FALSE> }T

T{ 1 1  = -> <TRUE>  }T
T{ 1 1 >= -> <TRUE>  }T
T{ 1 1 <= -> <TRUE>  }T

T{   0 1 10 WITHIN -> <FALSE> }T
T{   1 1 10 WITHIN -> <TRUE> }T
T{   4 0 10 WITHIN -> <TRUE> }T
T{  10 0 10 WITHIN -> <TRUE> }T
T{  11 0 10 WITHIN -> <FALSE> }T

T{ 0 1 DEPTH -> 0 1 2 }T
T{   0 DEPTH -> 0 1   }T
T{     DEPTH -> 0     }T

T{ 0S 2/ -> 0S }T
T{ 1 2/ -> 0 }T
T{ 4000 2/ -> 2000 }T

T{ 1 0 LSHIFT -> 1 }T
T{ 1 1 LSHIFT -> 2 }T
T{ 1 2 LSHIFT -> 4 }T
T{ 1S 1 LSHIFT 1 XOR -> 1S }T
T{ MSB 1 LSHIFT -> 0 }T

T{ 1 0 RSHIFT -> 1 }T
T{ 1 1 RSHIFT -> 0 }T
T{ 2 1 RSHIFT -> 1 }T
T{ 4 2 RSHIFT -> 1 }T
T{ MSB 1 RSHIFT 2* -> MSB }T

\ Stack word tests.
T{ 0       ?DUP  -> 0            }T
T{ 1       ?DUP  -> 1 1          }T
T{ 1 2     2DROP ->              }T
T{ 1 2     2DUP  -> 1 2 1 2      }T
T{ 1 2 3 4 2OVER -> 1 2 3 4 1 2  }T
T{ 1 2 3 4 2SWAP -> 3 4 1 2      }T
T{ 1 2 3   ROT   -> 2 3 1        }T
T{ 2 3 1   -ROT  -> 1 2 3        }T
T{ 1 2     SWAP  -> 2 1          }T
T{ 1 2     OVER  -> 1 2 1        }T
T{ 1 2 0   PICK  -> 1 2 DUP      }T
T{ 1 2 1   PICK  -> 1 2 OVER     }T
T{ 1 2     NIP   -> 2            }T
T{ 1 2     TUCK  -> 2 1 2        }T

\ Return stack tests.
T{ 1 2 >R >R RDROP R>       -> 2 }T
T{ 1 2 3 >R >R >R 2RDROP R> -> 3 }T

T{ : GD1 DO I LOOP ; -> }T
T{ 4 1 GD1 -> 1 2 3 }T

T{ : GD3 DO 1 0 DO J LOOP LOOP ; -> }T
T{ 4 1 GD3 -> 1 2 3 }T

T{ : GD5 123 SWAP 0 DO
     I 4 > IF DROP 234 LEAVE THEN
   LOOP ; -> }T
T{ 1 GD5 -> 123 }T
T{ 5 GD5 -> 123 }T
T{ 6 GD5 -> 234 }T

T{ : GD6 ( PAT: {0 0},{0 0}{1 0}{1 1},{0 0}{1 0}{1 1}{2 0}{2 1}{2 2} )
      0 SWAP 0 DO
         I 1+ 0 DO
           I J + 3 = IF I UNLOOP I UNLOOP EXIT THEN 1+
         LOOP
      LOOP ; -> }T
T{ 1 GD6 -> 1 }T
T{ 2 GD6 -> 3 }T
T{ 3 GD6 -> 4 1 2 }T


: CS1 CASE 1 OF 111 ENDOF
   2 OF 222 ENDOF
   3 OF 333 ENDOF
   >R 999 R>
   ENDCASE
;

T{ 1 CS1 -> 111 }T
T{ 2 CS1 -> 222 }T
T{ 3 CS1 -> 333 }T
T{ 4 CS1 -> 999 }T

: CS2 >R CASE
   1 OF CASE R@ 1 OF 100 ENDOF
                2 OF 200 ENDOF
                >R 300 R>
        ENDCASE
     ENDOF
   2 OF CASE R@ 1 OF 99 ENDOF
                >R 199 R>
        ENDCASE
     ENDOF
     >R 299 R>
   ENDCASE R> DROP ;

T{ 1 1 CS2 ->  100 }T
T{ 1 2 CS2 ->  200 }T
T{ 1 3 CS2 -> 300 }T
T{ 2 1 CS2 ->  99 }T
T{ 2 2 CS2 -> 199 }T
T{ 0 2 CS2 ->  299 }T

T{ : NOP : POSTPONE ; ; -> }T
T{ NOP NOP1 NOP NOP2 -> }T
T{ NOP1 -> }T
T{ NOP2 -> }T

T{ : GDX   123 ;    : GDX   GDX 234 ; -> }T
T{ GDX -> 123 234 }T


T{ : GR1 >R R> ; -> }T
T{ : GR2 >R R@ R> DROP ; -> }T
T{ 123 GR1 -> 123 }T
T{ 123 GR2 -> 123 }T
T{  1S GR1 ->  1S }T      ( Return stack holds cells )

\ 2>R is semantically equivalent to SWAP >R >R
T{ 1 2 2>R 2R> -> 1 2 SWAP >R >R R> R> SWAP }T


\ This test fails!  Maybe this is where being non-standard is better?
\ T{ ( A comment)1234 -> }T
T{ : PC1 ( A comment)1234 ; PC1 -> 1234 }T

HERE 1 ,
HERE 2 ,
CONSTANT 2ND
CONSTANT 1ST

T{       1ST 2ND < -> 1    }T \ HERE MUST GROW WITH ALLOT
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
T{ 1S 1ST !  1ST @  -> 1S  }T  \ CAN STORE CELL-WIDE VALUE


T{
BEGIN-STRUCTURE POINT     \ -- a-addr 0 ; -- lenp
   FIELD: P.X             \ -- a-addr cell
   FIELD: P.Y             \ -- a-addr cell*2
END-STRUCTURE
-> }T

HERE POINT ALLOT CONSTANT MY-POINT

T{ 3 MY-POINT P.X ! -> }T
T{ 5 MY-POINT P.Y ! -> }T

T{ MY-POINT P.X @ -> 3 }T
T{ MY-POINT P.Y @ -> 5 }T

HERE 1 ALLOT
HERE
CONSTANT 2NDA
CONSTANT 1STA
T{ 1STA 2NDA <  ->      1 }T         \ HERE MUST GROW WITH ALLOT
T{      1STA 1+ ->   2NDA }T    \ ... BY ONE ADDRESS UNIT


HERE 1 C,
HERE 2 C,
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
T{   111 V1 +! ->     }T
T{        V1 @ -> 234 }T
T{   111 V1 -! ->     }T
T{        V1 @ -> 123 }T

: GS3 WORD DROP COUNT SWAP C@ ;
T{ GS3 HELLO -> 5 CHAR H }T

\ : OUTPUT-TEST
\    PAGE
\    ." YOU SHOULD SEE THE STANDARD GRAPHIC CHARACTERS:" CR
\    65 BL DO I EMIT LOOP CR
\    97 65 DO I EMIT LOOP CR
\    127 97 DO I EMIT LOOP CR
\    PAGE
\    ." YOU SHOULD SEE 0-9 SEPARATED BY A SPACE:" CR
\    9 1+ 0 DO I . LOOP CR
\    PAGE
\    ." YOU SHOULD SEE 0-9 (WITH NO SPACES):" CR
\    [ CHAR 9 ] LITERAL 1+ [ CHAR 0 ] LITERAL DO I EMIT LOOP CR
\    PAGE
\    ." YOU SHOULD SEE A-G SEPARATED BY A SPACE:" CR
\    [ CHAR G ] LITERAL 1+ [ CHAR A ] LITERAL DO I EMIT SPACE LOOP CR
\    PAGE
\    ." YOU SHOULD SEE 0-5 SEPARATED BY TWO SPACES:" CR
\    5 1+ 0 DO I [ CHAR 0 ] LITERAL + EMIT 2 SPACES LOOP CR
\    PAGE
\    ." YOU SHOULD SEE TWO SEPARATE LINES:" CR
\    S" LINE 1" TYPE CR S" LINE 2" TYPE CR
\    PAGE
\ ;

\ Optional output test, may dizzy the user.
\ T{ OUTPUT-TEST -> }T

\ Test exceptions.
: T1 9 ;
: C1 1 2 3 ['] T1 CATCH ;
T{ C1 -> 1 2 3 9 0 }T    \ no throw executed

: T2 8 0 THROW ;
: C2 1 2 ['] T2 CATCH ;
T{ C2 -> 1 2 8 0 }T    \ 0 throw does nothing

: T3 7 8 9 99 THROW ;
: C3 1 2 ['] T3 CATCH ;
T{ C3 -> 1 2 99 }T    \ restores stack to catch depth

: T5 2DROP 2DROP 9999 THROW ;
: C5 1 2 3 4 ['] T5 CATCH           \ test depth restored correctly
   DEPTH >R DROP 2DROP 2DROP R> ;    \ after stack has been emptied
T{ C5 -> 5 }T

REPORT-TESTS CR CR

PRESS-TO-CONTINUE

PAGE

: RC4-TEST-MSG
." Performing RC4 test
(code taken from
Wikipedia)." CR

." Expect this sequence:
F1 38 29 C9 DE" CR
;

RC4-TEST-MSG

0 VALUE II        0 VALUE JJ
0 VALUE KEYADDR   0 VALUE KEYLEN

HERE 256 CELLS ALLOT CONSTANT SARRAY
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

: RC4-INIT ( keyaddr keylen -- )
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
HERE    97 C, 138 C, 99 C, 210 C, 251 C, CONSTANT MKEY
: TEST   CR   0 DO  RC4-BYTE . LOOP  CR ;
MKEY 5 RC4-INIT
44 249 76 238 220 5 TEST

DECIMAL CR PRESS-TO-CONTINUE PAGE

." Unloading test suite
words to save on space" CR CR
USED

FORGET TEST-SUITE-START

USED - . ." bytes freed." CR

PRESS-TO-CONTINUE

MENU-DEMO
