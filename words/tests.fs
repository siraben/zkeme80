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

PAGE
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

PAGE

: OUTPUT-TEST
   ." YOU SHOULD SEE THE STANDARD GRAPHIC CHARACTERS:" CR
   65 BL DO I EMIT LOOP CR
   97 65 DO I EMIT LOOP CR
   127 97 DO I EMIT LOOP CRbr
   PAUSE PAGE
   ." YOU SHOULD SEE 0-9 SEPARATED BY A SPACE:" CR
   9 1+ 0 DO I . LOOP CR
   PAUSE PAGE     
   ." YOU SHOULD SEE 0-9 (WITH NO SPACES):" CR
   [ CHAR 9 ] LITERAL 1+ [ CHAR 0 ] LITERAL DO I EMIT LOOP CR
   PAUSE PAGE     
   ." YOU SHOULD SEE A-G SEPARATED BY A SPACE:" CR
   [ CHAR G ] LITERAL 1+ [ CHAR A ] LITERAL DO I EMIT SPACE LOOP CR
   PAUSE PAGE     
   ." YOU SHOULD SEE 0-5 SEPARATED BY TWO SPACES:" CR
   5 1+ 0 DO I [ CHAR 0 ] LITERAL + EMIT 2 SPACES LOOP CR
   PAUSE PAGE     
   ." YOU SHOULD SEE TWO SEPARATE LINES:" CR
   S" LINE 1" TYPE CR S" LINE 2" TYPE CR
   PAUSE PAGE     
;

T{ OUTPUT-TEST -> }T
