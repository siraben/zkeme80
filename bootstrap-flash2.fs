." Bootstrap stage 3 loaded" CR

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

." Here's last 10 defined
words" CR


10 WORDS

PAUSE PAGE
STATUS

." End of stage 3."

PAUSE

: STAGE3

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

STAGE3
