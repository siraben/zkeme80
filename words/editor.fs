\ An editor.  Since we're using a lot of memory we're going to assign
\ it to use AppBackupScreen.

: INVTXT ;

: TITLE ( -- )
  0 0 CHAR-AT-XY
  INVTXT
  ." --------EDITOR----------"
  INVTXT
;

\ : TYPE ( c-addr u -- )
\   0 DO DUP C@ DUP 13 = IF [CHAR] n THEN EMIT 1+ LOOP DROP
\ ;


( Display the text area )
: TXTAREA ( addr -- addr )
  DUP
  0 1 CHAR-AT-XY
  240 TYPE
;

( addr -- addr )
24 CONSTANT LINE-SIZE
LINE-SIZE 5  * CONSTANT PAGE-SIZE
: NEXT-LINE  LINE-SIZE + ;
: PREV-LINE  LINE-SIZE - ;
: NEXT-PAGE  PAGE-SIZE + ;
: PREV-PAGE  PAGE-SIZE - ;

: FRAME PAGE TITLE TXTAREA ;

: CHAR- ( linum -- charnum )
  16 *
;

: MARK-LINE ( addr linum -- addr linum )
  2DUP
  DUP 1+ 0 CHAR-AT-XY
  CHAR- +  16 INVTXT TYPE INVTXT
;


: MODE-FRAME ( addr linum -- addr linum )
  OVER FRAME DROP
  MARK-LINE
;

( addr count char -- )
: FILL -ROT 0 DO 2DUP C! 1+ LOOP 2DROP ;

: CL-LINE ( addr linum -- addr linum )
  2DUP CHAR- +  16  32 FILL
;

: DELETE-TITLE ( -- )
  ORIGIN
  INVTXT
  ." --------DELETE----------"
  INVTXT
;

: DELETE-MODE ( addr -- addr )
  0
  BEGIN
    MODE-FRAME DELETE-TITLE
    KEY DUP 10 <>
  WHILE
    CASE
      3 OF DUP 0=  IF DROP 6 ELSE 1- THEN ENDOF
      4 OF DUP 6 = IF DROP 0 ELSE 1+ THEN ENDOF
      9 OF CL-LINE PAGE ENDOF
    ENDCASE
  REPEAT
  2DROP
;



: EDITOR ( addr -- addr )
  BEGIN
    FRAME
    KEY DUP 5 <>
  WHILE
    CASE
      3  OF PREV-LINE   ENDOF
      4  OF NEXT-LINE   ENDOF
      1  OF NEXT-PAGE   ENDOF
      2  OF PREV-PAGE   ENDOF
      10 OF DELETE-MODE ENDOF
    ENDCASE
  REPEAT
  DROP
;


INPUT-PTR @ EDITOR
