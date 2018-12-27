: TELL DROP PLOT-STRING ;
: SHUTDOWN PAUSE POWEROFF ;
: PAGE CLEAR-SCREEN ORIGIN ;
: USED HERE @ H0 - ;


: . U. ;

: '
  STATE @
  IF 
    ' ' ,
  ELSE
    WORD FIND >CFA
  THEN
  
    
; IMMEDIATE

: (
  BEGIN
    GETC 41 = IF
      EXIT
    THEN
  AGAIN
; IMMEDIATE

: LITERAL
  ' LIT ,
  ,
; IMMEDIATE

: [POSTPONE]
  WORD FIND >CFA ,
; IMMEDIATE



: CHAR WORD DROP C@ ;

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
    [POSTPONE] S"
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

: ; [POSTPONE] ; LATEST @ ID. ."  defined." CR ; IMMEDIATE

: AWAIT ." Press a key to continue." ;

PAGE
." Welcome to siraben's
Forth-based operating
system.

"
16384 OS-END - . ." bytes remaining on
page 00.

"

PAGE

: CELLS 2 * ;

: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;


: CFA>
  LATEST @
  BEGIN
    ?DUP
  WHILE
    2DUP SWAP
    < IF
      NIP
      EXIT
    THEN
    @		( follow link pointer back )
  REPEAT
  DROP		( restore stack )
  0		( sorry, nothing found )
;


: CASE IMMEDIATE
       0
       ;


: OF IMMEDIATE
     ' OVER ,
     ' = ,
     [POSTPONE] IF
       ' DROP ,
;


: ENDOF IMMEDIATE
        [POSTPONE] ELSE
;


: ENDCASE IMMEDIATE
          ' DROP ,
          BEGIN
            ?DUP
          WHILE
            [POSTPONE] THEN
          REPEAT
;



\ Debugging info.
: REPORT IMMEDIATE
         ." Report"
         PAUSE
         
;


\ : SEE WORD FIND U. ;


PAGE

AWAIT PAGE
: STATUS
DECIMAL USED . ." bytes have been
used" CR
HEX HERE @ ." HERE is at " . CR DECIMAL ;
STATUS

: CONSTANT WORD CREATE DOCOL-H ' LIT , , ' EXIT , ;



HERE @ 32 CELLS ALLOT NIP CONSTANT ACTUAL-RESULTS
\ : ARRAY WORD CREATE DOCOL-H ' LIT , CELLS ALLOT , ;

VARIABLE ACTUAL-DEPTH			\ stack record

\ 32 ARRAY ACTUAL-RESULTS
\ 
VARIABLE START-DEPTH

VARIABLE XCURSOR      \ for ...}T

VARIABLE ERROR-XT


PAGE 


: ERROR ERROR-XT @ EXECUTE ;   \ for vectoring of error reporting


: EMPTY-STACK	\ ( ... -- ) empty stack; handles underflowed stack too.
    DEPTH START-DEPTH @ < IF
        DEPTH START-DEPTH @ SWAP DO 0 LOOP
    THEN
    DEPTH START-DEPTH @ > IF
        DEPTH START-DEPTH @ DO DROP LOOP
    THEN
;



: TYPE 0 DO DUP C@ EMIT 1+ LOOP DROP ;



: (UNLOOP)    ( -- , R: I LIMIT -- : REMOVE LIMIT AND I FROM  )
	R>           ( SAVE OUR RETURN ADDRESS )
	RDROP        ( POP OFF I )
	RDROP        ( POP OFF LIMIT )
        >R
;

: LEAVE ( -- , R: I LIMIT RETURN -- : BREAK OUT OF A DO-LOOP CONSTRUCT )
	(UNLOOP)
RDROP ; ( RETURN TO THE CALLER'S CALLER ROUTINE )


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

: T{		\ ( -- ) syntactic sugar.
  DEPTH START-DEPTH ! 0 XCURSOR !
;


PAGE



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

.S CR

\ Do all the tests!
{ 0 0 AND -> 0 }
{ 0 1 AND -> 0 }
{ 1 0 AND -> 0 }
{ 1 1 AND -> 1 }

{ 0 INVERT 1 AND -> 1 }
{ 1 INVERT 1 AND -> 0 }

0	 CONSTANT 0S
0 INVERT CONSTANT 1S

{ 0S INVERT -> 1S }
{ 1S INVERT -> 0S }

{ 0S 0S AND -> 0S }
{ 0S 1S AND -> 0S }
{ 1S 0S AND -> 0S }
{ 1S 1S AND -> 1S }

{ 0S 0S OR -> 0S }
{ 0S 1S OR -> 1S }
{ 1S 0S OR -> 1S }
{ 1S 1S OR -> 1S }

{ 0S 0S XOR -> 0S }
{ 0S 1S XOR -> 1S }
{ 1S 0S XOR -> 1S }
{ 1S 1S XOR -> 0S }


." End of tests." CR
." End of phase 2."
PAUSE

SHUTDOWN

\ Check how many bytes are left by running the following Scheme program:
\ (begin (load "smiley-os.scm") (make-rom "forth.rom"))
