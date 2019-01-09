." Stage 4 loaded." CR

HERE 24 CELLS ALLOT CONSTANT PAD
: PAD-CLEAR PAD 48 0 DO DUP 0 SWAP C! 1+ LOOP DROP ;

STATUS
PAUSE PAGE
\ ." A simple REPL is coming
\ up."
: SEE

        WORD FIND       ( find the dictionary entry to decompile )

        ( Now we search again, looking for the next word in the dictionary.  This gives us
          the length of the word that we will be decompiling.  Well, mostly it does. )
        HERE            ( address of the end of the last compiled word )
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
        [CHAR] : EMIT SPACE DUP ID. SPACE
        DUP ?IMMEDIATE >R

        >DFA            ( get the data address, ie. points after DOCOL | end-of-word start-of-data )

        ( now we start decompiling until we hit the end of the word )
        BEGIN           ( end start )
                \ PAUSE PAGE
                2DUP >
        WHILE
                DUP @           ( end start codeword )

                CASE
                ['] LIT OF                ( is it LIT ? )
                        2+ DUP @                ( get next word which is the integer constant )
                        .                       ( and print it )
                ENDOF
                ['] LITSTRING OF             ( is it LITSTRING ? )
                        [CHAR] S EMIT
                        [CHAR] " EMIT
                        SPACE ( print S"<space> )
                        2+ DUP @                ( get the length )
                        SWAP 2+ SWAP            ( end start+2 length )
                        2DUP TELL               ( print the string )
                        [CHAR] " EMIT SPACE               ( finish the string with a final quote )
                        +                       ( end start+4+len, aligned )
                        1+                        ( because we're about to add 4 below )
                ENDOF
                ['] 0BRANCH OF            ( is it 0BRANCH ? )
                        ." 0BRANCH ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF
                ['] BRANCH OF             ( is it BRANCH ? )
                        ." BRANCH ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF
                ['] JUMP OF             ( is it BRANCH ? )
                        ." JUMP ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF

                ['] 0JUMP OF             ( is it BRANCH ? )
                        ." 0JUMP ( "
                        2+ DUP @                ( print the offset )
                        .
                        ." ) "
                ENDOF                
                ['] (') OF                  ( is it ' TICK ? )
                        ." [']" SPACE
                        2+ DUP @                ( get the next codeword )
                        CFA>                    ( and force it to be printed as a dictionary entry )
                        ID. SPACE
                ENDOF
                ['] EXIT OF               ( is it EXIT? )
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

        [CHAR] ; EMIT

        R> IF ."  IMMEDIATE " THEN
        CR

        2DROP           ( restore stack )
;
PAGE

  
\ PAUSE PAGE

\ PAD-CLEAR
\ PAD 48 EXPECT
\ : KEY-LOOP BEGIN KEY PAGE U. AGAIN ;

\ PAD INPUT-PTR !

SEE ."

PAUSE PAGE

\ KEY-LOOP

SHUTDOWN


