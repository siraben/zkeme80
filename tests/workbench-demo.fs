\ Demo data is loaded only into the recorder's disposable ROM.
VARIABLE DEMO-READY
VARIABLE HITS
0 DEMO-READY ! 0 HITS !
: DEMO-SEED
  S" Welcome to the  Forth workbench.Named objects   share one store.Text, source anddata live here. Load SRC/GREET  with ENTER.     Open workspace; type GREET.     BYE brings back the desktop.    "
  S" README" 1 FS-PUT THROW
  S" : GREET 1 HITS +! STAR STAR STAR ;"
  S" SRC/GREET" 2 FS-PUT THROW
  S" Next: edit source, compact the journal, and give jobs private stacks."
  S" NOTES/PLAN" 1 FS-PUT THROW
;
DEMO-SEED
12345 DEMO-READY !
MENU-DEMO
