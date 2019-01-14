\ This should be the shell.

\ We should handle modal key input, so that the user can type 0-9, A-Z
\ etc.

\ State transition diagram for switching key modes:
(
digraph "Keyboard transition table" {
  normal
  2nd [shape=circle]
  alpha [shape=circle]
  alpha-lock [shape=circle]
  alpha-2nd [shape=circle]
  normal -> 2nd [label="2nd"];
  alpha -> alpha-2nd [label="2nd"];
  normal -> alpha [label="alpha"];
  2nd -> alpha-lock [label="alpha"];
  alpha-lock -> normal [label="clear"];
  2nd -> normal [label="clear"];
  alpha-2nd -> normal [label="clear"];

  normal -> normal [label="other"];
  alpha -> normal [label="other"];
  2nd -> normal [label="other"];
  alpha-lock -> alpha-lock [label="other"];  
}
)
)
54 CONSTANT 2ND-KEY
48 CONSTANT ALPHA-KEY

: SHELL-KEY KEY ;
: SHELL-TICK SHELL-KEY ORIGIN . ;
: SHELL-LOOP BEGIN SHELL-TICK AGAIN ;
  
PAGE

SHELL-LOOP
PAUSE SHUTDOWN
