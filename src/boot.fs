CLEAR-SCREEN ORIGIN
\ This is the first file to be loaded.  Just go straight to the first
\ bootstrap file!
: STAGE1
  \ Map flash page 1 into memory bank A.
  1 MAP-FLASH
  IF
    \ We set the input pointer to point to memory bank A.
    MEMA CSTRING-SOURCE
  ELSE
    \ Something went wrong.  Shutdown.
    \ Print "ERR"
    ." ERR 9999"
    PAUSE POWEROFF           
  THEN
;

STAGE1
