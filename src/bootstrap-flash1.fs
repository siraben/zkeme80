\ We define the rest of Forth.

\ Possibly test interrupts later.
\ : BAR FOO 200 SET-INTERRUPT ;
\ BAR

\ Parse the next word as binary number.
: %B
  0
  BEGIN
    GETC DUP NUM? NOT
    IF
      DROP STATE @ IF ' LIT ,  , THEN EXIT
    ELSE
      '0' - SWAP 2* +
    THEN
  AGAIN
; IMMEDIATE


HERE
%B 00000000 C,
%B 00000000 C,
%B 00100100 C,
%B 01000010 C,
%B 01000010 C,
%B 01000010 C,
%B 01000010 C,
%B 00100100 C,
CONSTANT ZKEME80-LOGO-00

HERE
%B 00000000 C,
%B 00011000 C,
%B 00100100 C,
%B 00100100 C,
%B 00011000 C,
%B 00100100 C,
%B 00100100 C,
%B 00011000 C,
CONSTANT ZKEME80-LOGO-01

HERE
%B 00000000 C,
%B 00011000 C,
%B 00100100 C,
%B 00100100 C,
%B 00100100 C,
%B 00100100 C,
%B 00011000 C,
%B 00000000 C,
CONSTANT ZKEME80-LOGO-11

HERE
%B 00000000 C,
%B 00000000 C,
%B 01111100 C,
%B 00001000 C,
%B 00010000 C,
%B 00100000 C,
%B 01111100 C,
%B 00000000 C,
CONSTANT ZKEME80-LOGO-10

HERE
%B 00000000 C,
%B 00000000 C,
%B 00111100 C,
%B 00111100 C,
%B 00111100 C,
%B 00111100 C,
%B 00000000 C,
%B 00000000 C,
CONSTANT LOADING-DOT

38 VALUE ZKEME80-LOGO-STARTX
27 VALUE ZKEME80-LOGO-STARTY

30 VALUE DOT-X
45 VALUE DOT-Y

: DRAW-LOGO ( addr x y -- ) 8 -ROT PUT-SPRITE-XOR ;

: DRAW-LOADING-DOT LOADING-DOT DOT-X DOT-Y DRAW-LOGO 8 +TO DOT-X ;

: ZKEME80-LOGO
  ZKEME80-LOGO-STARTX 5 - ZKEME80-LOGO-STARTY 9 -
  AT-XY ." zkeme80"
  ZKEME80-LOGO-00 ZKEME80-LOGO-STARTX     ZKEME80-LOGO-STARTY     DRAW-LOGO
  ZKEME80-LOGO-01 ZKEME80-LOGO-STARTX 8 + ZKEME80-LOGO-STARTY     DRAW-LOGO
  ZKEME80-LOGO-10 ZKEME80-LOGO-STARTX     ZKEME80-LOGO-STARTY 8 + DRAW-LOGO
  ZKEME80-LOGO-11 ZKEME80-LOGO-STARTX 8 + ZKEME80-LOGO-STARTY 8 + DRAW-LOGO
  ZKEME80-LOGO-STARTX  ZKEME80-LOGO-STARTY 16 16
  RECT-XOR
;

ZKEME80-LOGO
DRAW-LOADING-DOT

: LOAD-MODULE ( page -- ) MAP-FLASH 0= IF 10 THROW THEN MEMA CSTRING-SOURCE ;
: LOAD-TEST-SUITE MODULE-TESTS LOAD-MODULE ;
VARIABLE WORKBENCH-XT
: LOAD-SHELL WORKBENCH-XT @ EXECUTE ;

\ Bit shifts are not fast!

: LSHIFT ?DUP IF 0 DO 2* LOOP THEN ;

: UNLOOP    ( -- , r: i limit -- : remove limit and i from  )
        R>           ( save our return address )
        RDROP        ( pop off i )
        RDROP        ( pop off limit )
        >R
;

: BEGIN-STRUCTURE  \ -- addr 0 ; -- size
   CREATE
     HERE 0 0 ,    \ mark stack, lay dummy
   DOES> @         \ -- rec-len
;

: +FIELD           \ n <"name"> -- ; Exec: addr -- 'addr
   CREATE OVER , +
   DOES> @ +
;

: FIELD:           ( n1 "name" -- n2 ; addr1 -- addr2 )
  1 CELLS +FIELD
;

: END-STRUCTURE    \ addr n --
  SWAP !
;                  \ set len

\ Non-standard for now.
\ Display n defined words.

: WORDS ( n -- )
  LATEST @ SWAP 0 DO
    ?DUP IF
      DUP ?HIDDEN NOT IF
        DUP ID. SPACE
      THEN
      @
    ELSE
      LEAVE
    THEN
  LOOP
  DROP
;

\ Returns the number of words defined.

: NUMBER-OF-WORDS ( -- n )
  0 HERE ! LATEST @
  BEGIN
    ?DUP
  WHILE
    DUP ?HIDDEN NOT
    IF 1 HERE +! THEN
    @
  REPEAT
  HERE @
;

: STATUS
DECIMAL UNUSED .
." bytes available" CR
HEX HERE ." HERE is at " . CR DECIMAL
." Stack has contents" CR
.S
;


1 CONSTANT RIGHT
2 CONSTANT LEFT
\ Scan group $FE emits code 4 for the physical up key and 3 for
\ down (keyboard.scm gs-keygroup1); y=0 is the top menu row.
4 CONSTANT UP
3 CONSTANT DOWN
