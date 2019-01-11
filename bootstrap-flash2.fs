: LOAD-STAGE3
  3 SET-RAM-MEMA
  IF
    MEMA INPUT-PTR ! PAGE
  ELSE
    ." Couldn't load stage 4.  Shutting down." CR
    PAUSE POWEROFF           
  THEN
;

: STAGE2-LOADED ." Bootstrap stage 3 loaded" ;
: STAGE2-END ." End of stage 3." ;

: STAGE2-MAIN
  PAGE
  STAGE2-LOADED CR
  STAGE2-END
  LOAD-STAGE3
;

STAGE2-MAIN
