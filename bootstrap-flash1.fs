: TELL DROP PLOT-STRING ;
: SHUTDOWN PAUSE POWEROFF ;
: PAGE CLEAR-SCREEN ORIGIN ;
: USED HERE @ H0 - ;

: . U. ;

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

: S" IMMEDIATE STATE @ IF ' LITSTRING , HERE @ 0 , BEGIN GETC DUP 34 <>
WHILE C, REPEAT DROP 0 C, DUP HERE @ SWAP - 3 - SWAP ! ELSE HERE @
BEGIN GETC DUP 34 <> WHILE OVER C! 1+ REPEAT DROP HERE @ - HERE @
SWAP THEN ;

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

: AWAIT ." Press a key to continue." PAUSE ;

PAGE
." Welcome to siraben's
Forth-based operating
system.

"
16384 OS-END - . ." bytes remaining on
page 00.

"

AWAIT PAGE

: CELLS 2 * ;
: SAY-DEF LATEST @ ID. ."  defined" CR ;
SAY-DEF

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

SAY-DEF
: CASE IMMEDIATE
       0
       ;

SAY-DEF
: OF IMMEDIATE
     ' OVER ,
     ' = ,
     [POSTPONE] IF
       ' DROP ,
;

SAY-DEF
: ENDOF IMMEDIATE
        [POSTPONE] ELSE
;

SAY-DEF
: ENDCASE IMMEDIATE
          ' DROP ,
          BEGIN
            ?DUP
          WHILE
            [POSTPONE] THEN
          REPEAT
;

SAY-DEF

\ Debugging info.
: REPORT IMMEDIATE
         ." Report"
         PAUSE
         
;

SAY-DEF


: SEE

        WORD FIND       ( find the dictionary entry to decompile )

        ( Now we search again, looking for the next word in the dictionary.  This gives us
          the length of the word that we will be decompiling.  Well, mostly it does. )
        HERE @          ( address of the end of the last compiled word )
        LATEST @        ( word last curr )
        BEGIN
                2 PICK          ( word last curr word )
                OVER            ( word last curr word curr )
                <>              ( word last curr word<>curr? )
        WHILE                   ( word last curr )
                NIP             ( word curr )
                DUP @           ( word curr prev which becomes: word last curr )
        REPEAT


        DROP            ( at this point, the stack is: start-of-word end-of-word )
        SWAP            ( end-of-word start-of-word )

        ( begin the definition with : NAME [IMMEDIATE] )
         58 EMIT SPACE DUP ID. SPACE
        DUP ?IMMEDIATE IF ." IMMEDIATE " THEN

        >DFA            ( get the data address, ie. points after DOCOL | end-of-word start-of-data )

        ( now we start decompiling until we hit the end of the word )
        BEGIN           ( end start )
                \ PAUSE
                2DUP >
        WHILE
                DUP @           ( end start codeword )

                CASE
                ' LIT OF                ( is it LIT ? )
                        2+ DUP @                ( get next word which is the integer constant )
                        .                       ( and print it )
                ENDOF
                ' LITSTRING OF             ( is it LITSTRING ? )
                         83 EMIT  34 EMIT SPACE ( print S"<space> )
                        2+ DUP @                ( get the length )
                        SWAP 2+ SWAP            ( end start+2 length )
                        2DUP TELL               ( print the string )
                         34 EMIT SPACE               ( finish the string with a final quote )
                        +                       ( end start+4+len, aligned )
                        1+                        ( because we're about to add 4 below )
                ENDOF
                ' 0BRANCH OF            ( is it 0BRANCH ? )
                        ." 0BRANCH ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF
                ' BRANCH OF             ( is it BRANCH ? )
                        ." BRANCH ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF
                ' JUMP OF             ( is it BRANCH ? )
                        ." JUMP ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF

                ' 0JUMP OF             ( is it BRANCH ? )
                        ." 0JUMP ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF                
                ' ' OF                  ( is it ' TICK ? )
                         39 EMIT SPACE
                        2+ DUP @                ( get the next codeword )
                        CFA>                    ( and force it to be printed as a dictionary entry )
                        ID. SPACE
                ENDOF
                ' EXIT OF               ( is it EXIT? )
                        ( We expect the last word to be EXIT, and if it is then we don't print it
                          because EXIT is normally implied by ;.  EXIT can also appear in the middle
                          of words, and then it needs to be printed. )
                        2DUP                    ( end start end start )
                        2+                      ( end start end start+4 )
                        <> IF                   ( end start | we're not at the end )
                                ." EXIT "
                        THEN
                ENDOF
                                        ( default case: )
                        DUP                     ( in the default case we always need to DUP before using )
                        CFA>                    ( look up the codeword to get the dictionary entry )
                        ID. SPACE               ( and print it )
                ENDCASE

                2+              ( end start+2 )
        REPEAT

         59 EMIT CR

        2DROP           ( restore stack )
;


\ : SEE WORD FIND U. ;
SAY-DEF
: AWAIT ." Press a key to continue." PAUSE ;


PAUSE PAGE

SEE ABORT

AWAIT PAGE

DEC USED . ." bytes have been
used" CR
HEX HERE @ ." HERE is at " . CR

." End of stage 2." CR
AWAIT SHUTDOWN

\ Check how many bytes are left by running the following Scheme program:
\ (begin (load "smiley-os.scm") (make-rom "forth.rom"))
