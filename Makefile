all:
	rm -f forth.rom
	echo '(begin (load "smiley-os.scm") (make-rom "forth.rom"))' | guile
	tilem2 -r forth.rom