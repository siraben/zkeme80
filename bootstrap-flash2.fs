: LOAD-STAGE3

  \ Try to set RAM Memory region A to be the first RAM flash page.
  3 SET-RAM-MEMA
  IF
    \ We set the input pointer to point to memory bank A.
    MEMA INPUT-PTR ! PAGE
  ELSE
    \ Something went wrong.  Shutdown.
    ." Couldn't load stage 4.  Shutting down." CR
    PAUSE POWEROFF           
  THEN
;


: STAGE2-LOADED ." Bootstrap stage 3 loaded" ;

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

: LAST-10-WORDS
." Here's last 10 defined
words" CR
;

: STAGE2-END ." End of stage 3." ;

: STAGE2-MAIN
  STAGE2-LOADED CR
  LAST-10-WORDS
  10 WORDS
  PAUSE PAGE STATUS
  STAGE2-END
  PAUSE
  LOAD-STAGE3
  
;

STAGE2-MAIN
