CLEAR-SCREEN ORIGIN
\ This is the first file to be loaded.  Just go straight to the first
\ bootstrap file!
: STAGE1 1 SET-RAM-MEMA
         IF
           16384 INPUT-PTR !
         ELSE
           \ Print "ERR"
           69 EMIT 82 EMIT 82 EMIT 58 EMIT SPACE
           9999 U.
           PAUSE POWEROFF           
         THEN
;

STAGE1
