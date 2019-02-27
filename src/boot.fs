CLEAR-SCREEN ORIGIN
\ This is the first file to be loaded.  Just go straight to the first
\ bootstrap file!
: STAGE1
  \ Try to set RAM Memory region A to be the first RAM flash page.
  1 SET-RAM-MEMA
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
