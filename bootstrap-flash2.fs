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

PAUSE 
POWEROFF
