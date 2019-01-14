\ This should be the shell.

\ We should handle modal key input, so that the user can type 0-9, A-Z
\ etc.  The state transition diagram for this is shown in the file
\ "key.dot".

\ Dummy word to mark start of word definitions.
: SHELL-START ;

54 CONSTANT 2ND-KEY
48 CONSTANT ALPHA-KEY

: SHELL-KEY KEY ;
: SHELL-TITLE
ORIGIN
." No REPL yet, but here's
a key demo.

Press ENTER to quit." CR CR
;
2 CONSTANT SHELL-ERROR
1 CONSTANT SHELL-OK
0 CONSTANT SHELL-EXIT

1 VALUE SHELL-STATUS

: SHELL-OK? SHELL-STATUS 1 = ;

: ?EXIT-IF-ENTER DUP 9 = IF SHELL-EXIT TO SHELL-STATUS THEN ;

: SHELL-READ-KEY KEY ?EXIT-IF-ENTER ." You pressed: " . CR CR ." Stack: " .S ;
: SHELL-TICK SHELL-TITLE SHELL-READ-KEY ;
: SHELL-LOOP BEGIN SHELL-OK? WHILE SHELL-TICK REPEAT ;
  
PAGE

SHELL-LOOP

FORGET SHELL-START
MENU-DEMO
