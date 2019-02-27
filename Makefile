build:
	echo '(load "build.scm")' | guile

all:
	echo '(load "build.scm")' | guile
	tilem2 -r zkeme80.rom

upgrade:
	echo '(load "build.scm")' | guile
	mktiupgrade -k 0A.key --device TI-84+ zkeme80.rom zkeme80.8xu 00 01 02 03 3C
