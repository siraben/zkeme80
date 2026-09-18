\ Resident ABI and scoped bank access. Cells and execution tokens are 16 bit.
1 CONSTANT OS-ABI
16384 CONSTANT STORAGE-SIZE
8 CONSTANT STORAGE-PAGE

\ Callback and source must live outside the banked window (4000-7fff).
\ Return the callback's CATCH status, restoring the original selector.
: WITH-PAGE ( xt page -- ior )
  DUP 64 U< 0= IF 2DROP 10 EXIT THEN
  BANK@ >R (BANK!) CATCH R> (BANK!)
;

\ Brackets can leave STATE at zero inside an unfinished definition.
\ Inspect all entries since the checkpoint, including those below later words.
: (EVAL-PARTIAL?) ( old-latest -- flag )
  LATEST @ BEGIN 2DUP <> OVER 0 <> AND WHILE
    DUP ?UNFINISHED IF 2DROP 1 EXIT THEN @
  REPEAT 2DROP 0
;
: (EVAL-ROLLBACK) ( old-latest old-here old-flags -- )
  >R DP ! DUP LATEST ! 2+ R> SWAP C! CLEAR-FIND-CACHE
;
\ Compilation must stay inside its entry definition, even across [ and ].
: (EVAL-CLEANUP) ( ior old-latest old-here old-state old-flags -- ior )
  >R
  DUP STATE @ <> OVER R@ 32 AND OR IF
    3 PICK LATEST @ <> OR
    R@ 32 AND LATEST @ ?UNFINISHED <> OR
  ELSE 3 PICK (EVAL-PARTIAL?) OR THEN
  IF
    >R >R >R DUP 0= IF DROP 22 THEN R> R> R> 1
  ELSE 3 PICK 0 <> OVER R@ 32 AND OR 0 <> AND THEN
  IF
    R@ SWAP >R (EVAL-ROLLBACK) R>
  ELSE >R 2DROP R> THEN
  STATE ! R> DROP
;

\ Nested zero-terminated input, preserving source, radix, and compiler state.
\ Keep the complete evaluation frame on the return stack for nested calls.
\ Reject unfinished definitions and discard their partial dictionary entries.
: EVALUATE0 ( zaddr -- ior )
  INPUT-PTR @ >R BASE @ >R
  CP-DP @ >R CP-LATEST @ >R CP-LOOPS @ >R CP-BODY @ >R
  \ Reserve only active DO contexts, retaining nested evaluation frames.
  CP-LOOPS @ 4 * DUP RP@ SWAP - RP!
  CP-CONTEXTS RP@ 2 PICK CMOVE >R
  LATEST @ 2+ C@ >R
  STATE @ >R HERE >R LATEST @ >R
  DUP BEGIN DUP C@ WHILE 1+ REPEAT OVER -
  ['] EVALUATE CATCH DUP IF >R 2DROP R> THEN
  R> R> R> R> (EVAL-CLEANUP)
  R> DUP RP@ CP-CONTEXTS ROT CMOVE RP@ + RP!
  R> CP-BODY ! R> CP-LOOPS ! R> CP-LATEST ! R> CP-DP !
  R> BASE ! R> INPUT-PTR !
;

VARIABLE IO-OFF
VARIABLE IO-ADDR
VARIABLE IO-LEN
: IO-ARGS ( offset addr len -- ) IO-LEN ! IO-ADDR ! IO-OFF ! ;
: IO-VALID? ( -- flag )
  STORAGE-SIZE IO-OFF @ U< IF 0 EXIT THEN
  STORAGE-SIZE IO-OFF @ - IO-LEN @ U< IF 0 EXIT THEN
  IO-ADDR @ 33792 U< IF 0 EXIT THEN
  65535 IO-ADDR @ - IO-LEN @ U< IF 0 EXIT THEN 1
;
: (STORAGE-READ)
  IO-OFF @ MEMA + IO-ADDR @ IO-LEN @ CMOVE
;
: STORAGE-READ ( offset addr len -- ior )
  IO-ARGS IO-VALID? NOT IF 10 EXIT THEN
  ['] (STORAGE-READ) STORAGE-PAGE WITH-PAGE
;
\ Check the entire destination before programming any byte. A zero length
\ write does nothing; flash is only programmed into erased space.
: (STORAGE-WRITE)
  IO-LEN @ 0= IF EXIT THEN
  IO-LEN @ 0 DO
    IO-OFF @ MEMA + I + C@ 255 <> IF 11 THROW THEN
  LOOP
  IO-LEN @ 0 DO
    IO-ADDR @ I + C@ IO-OFF @ MEMA + I + (FLASH-C!)
    IO-ADDR @ I + C@ IO-OFF @ MEMA + I + C@ <>
    IF 12 THROW THEN
  LOOP
;
: STORAGE-WRITE ( addr offset len -- ior )
  >R SWAP R> IO-ARGS IO-VALID? NOT IF 10 EXIT THEN
  ['] (STORAGE-WRITE) STORAGE-PAGE WITH-PAGE
;

: (STORAGE-BLANK?) IO-OFF @ MEMA + IO-LEN @ FF? ;
: STORAGE-BLANK? ( offset len -- flag ior )
  IO-LEN ! IO-OFF !
  STORAGE-SIZE IO-OFF @ U< IF 0 10 EXIT THEN
  STORAGE-SIZE IO-OFF @ - IO-LEN @ U< IF 0 10 EXIT THEN
  ['] (STORAGE-BLANK?) STORAGE-PAGE WITH-PAGE
;
