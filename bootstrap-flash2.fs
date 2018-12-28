." Bootstrap stage 3 loaded"
: WORDS
  LATEST @
  BEGIN
    ?DUP
  WHILE
    DUP ?HIDDEN NOT IF
      DUP ID.
      SPACE
      KEY PAGE 9 = IF EXIT THEN
    THEN
    @
  REPEAT
  CR
;
CR
." Press any key to see
the list of defined
words and press to
advance"

PAUSE PAGE
WORDS
STATUS

PAUSE 
POWEROFF
