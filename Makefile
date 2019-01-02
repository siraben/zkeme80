all:
	rm -f zkeme80.rom
	echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
	tilem2 -r zkeme80.rom

build:
	rm -f zkeme80.rom
	echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
