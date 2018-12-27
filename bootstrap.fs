: SHUTDOWN PAUSE POWEROFF ;

: (
    BEGIN
      GETC 41 = IF
        EXIT
      THEN
    AGAIN
; IMMEDIATE  

: WORDS
  LATEST @
  BEGIN
    KEY DROP ?DUP
  WHILE
    DUP ?HIDDEN NOT
    IF
      DUP ID. SPACE
    THEN
    @
  REPEAT
  CR
;

: LITERAL 
	' LIT ,		\ compile LIT
	,		\ compile the literal itself (from the stack)
; IMMEDIATE

: [POSTPONE]
  WORD FIND >CFA ,
; IMMEDIATE

: CHAR WORD DROP C@ ;

: '"' [ CHAR " ] LITERAL ;

: C,
	HERE @ C!	( store the character in the compiled image )
	1 HERE +!	( increment HERE pointer by 1 byte )
;

: S"		( -- addr len )
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			GETC 		( get next character of the string )
			DUP '"' <>
		WHILE
			C,		( copy character )
		REPEAT
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		2-		( subtract 4 [because we measured from the start of the length word] )
		SWAP !		( and back-fill the length location )
		
	ELSE		( immediate mode )
		HERE @		( get the start address of the temporary space )
		BEGIN
			KEY
			DUP '"' <>
		WHILE
			OVER C!		( save next character )
			1+		( increment address )
		REPEAT
		DROP		( drop the final " character )
		HERE @ -	( calculate the length )
		HERE @		( push the start address )
		SWAP 		( addr len )
	THEN
;  IMMEDIATE

: STARS 0 DO STAR LOOP ;

: ."		( -- )
	STATE @ IF	( compiling? )
		[POSTPONE] S"	( read the string, and compile LITSTRING, etc. )
		' PLOT-STRING ,	( compile the final TELL )
	ELSE
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		BEGIN
			KEY
			DUP '"' = IF
				DROP	( drop the double quote character )
				EXIT	( return from this function )
			THEN
			EMIT
		AGAIN
	THEN
;  IMMEDIATE

: ." 		( -- )
	STATE @ IF	( compiling? )
		[POSTPONE] S"	( read the string, and compile LITSTRING, etc. )
		' PLOT-STRING ,	( compile the final TELL )
	ELSE
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		BEGIN
			GETC
			DUP '"' = IF
				DROP	( drop the double quote character )
				EXIT	( return from this function )
			THEN
			EMIT
		AGAIN
	THEN
; IMMEDIATE

ORIGIN
CLEAR-SCREEN

." Welcome to siraben's
Forth-based operating
system for the TI-84+
calculator!"

CR CR
." Press any key to
shutdown..."

SHUTDOWN
