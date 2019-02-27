all:
	echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
	tilem2 -r zkeme80.rom

build:
	echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile

upgrade:
	echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
	mktiupgrade -k 0A.key --device TI-84+ zkeme80.rom zkeme80.8xu 00 01 02 03 3C
