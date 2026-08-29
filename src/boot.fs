CLEAR-SCREEN ORIGIN
\ This is the first file to be loaded.  Just go straight to the first
\ bootstrap file!
: STAGE1
  \ Map the first bootstrap Flash page into memory bank A.
  1 SET-FLASH-MEMA
  IF
    \ We set the input pointer to point to memory bank A.
    MEMA INPUT-PTR !
  ELSE
    \ Something went wrong.  Shutdown.
    \ Print "ERR"
    ." ERR 9999"
    PAUSE POWEROFF           
  THEN
;

STAGE1
