all:
	echo '(begin (load "zkeme80.scm") (make-rom+map "zkeme80.rom" "zkeme80.ram-labelmap.json"))' | guile
	tilem2 -r zkeme80.rom

build:
	echo '(begin (load "zkeme80.scm") (make-rom+map "zkeme80.rom" "zkeme80.ram-labelmap.json"))' | guile

upgrade:
	echo '(begin (load "zkeme80.scm") (make-rom+map "zkeme80.rom" "zkeme80.ram-labelmap.json"))' | guile
	mktiupgrade -k 0A.key --device TI-84+ zkeme80.rom zkeme80.8xu 00 01 02 03 3C
