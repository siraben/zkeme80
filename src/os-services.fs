\ Resident ABI and scoped bank access. Cells and execution tokens are 16 bit.
1 CONSTANT OS-ABI
16384 CONSTANT STORAGE-SIZE
8 CONSTANT STORAGE-PAGE

\ Callback and source must live outside the banked window (4000-7fff).
\ Return the callback's CATCH status, restoring the original selector.
: WITH-PAGE ( xt page -- ior )
  DUP 63 > IF 2DROP 10 EXIT THEN
  BANK@ >R (BANK!) CATCH R> (BANK!)
;

\ Keep the complete evaluation frame on the return stack for nested calls.
\ A source must finish in the compilation state in which it started.
: (EVAL-CLEANUP) ( ior old-latest old-here old-state -- ior )
  DUP STATE @ <> IF
    >R >R >R DUP 0= IF DROP 22 THEN R> R> R>
  THEN
  >R
  2 PICK IF
    R@ 0= STATE @ 0 <> AND IF DP ! LATEST ! ELSE 2DROP THEN
  ELSE 2DROP THEN
  R> STATE !
;

\ Nested zero-terminated input, preserving source, radix, and compiler state.
\ Reject unfinished definitions and discard their partial dictionary entries.
: EVALUATE0 ( zaddr -- ior )
  INPUT-PTR @ >R BASE @ >R
  STATE @ >R HERE >R LATEST @ >R
  INPUT-PTR ! ['] INTERPRET CATCH
  ?DUP IF THEN
  R> R> R> (EVAL-CLEANUP)
  R> BASE ! R> INPUT-PTR !
;

VARIABLE IO-OFF
VARIABLE IO-ADDR
VARIABLE IO-LEN
: IO-ARGS ( offset addr len -- ) IO-LEN ! IO-ADDR ! IO-OFF ! ;
: IO-VALID? ( -- flag )
  IO-OFF @ STORAGE-SIZE > IF 0 EXIT THEN
  IO-LEN @ STORAGE-SIZE IO-OFF @ - > IF 0 EXIT THEN
  IO-ADDR @ 33792 < IF 0 EXIT THEN
  IO-LEN @ 65535 IO-ADDR @ - > IF 0 EXIT THEN 1
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
  IO-OFF @ STORAGE-SIZE > IF 0 10 EXIT THEN
  IO-LEN @ STORAGE-SIZE IO-OFF @ - > IF 0 10 EXIT THEN
  ['] (STORAGE-BLANK?) STORAGE-PAGE WITH-PAGE
;
