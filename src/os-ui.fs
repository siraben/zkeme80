\ Shared 96x64 chrome. Glyphs are five pixels high; keep a blank row between.
: OS-NUM ( u -- ) BASE @ >R DECIMAL U. R> BASE ! ;
: OS-PRINTABLE ( c -- c )
  DUP 32 < OVER 126 > OR IF DROP 46 THEN ;
: OS-LABEL ( addr len -- )
  ?DUP IF 0 DO DUP I + C@ OS-PRINTABLE EMIT LOOP THEN DROP ;
: OS-TITLE ( addr len -- )
  PAGE 3 1 AT-XY OS-LABEL 0 0 96 8 RECT-XOR ;
: OS-FOOT ( -- ) 0 55 96 1 RECT-OR 2 58 AT-XY ;
: OS-SELECT ( y -- ) 1- 2 SWAP 91 7 RECT-XOR ;
: OS-HEX-DIGIT ( u -- ) DUP 9 > IF 7 + THEN 48 + EMIT ;
: OS-BYTE ( u -- ) DUP 16 / OS-HEX-DIGIT 15 AND OS-HEX-DIGIT ;
