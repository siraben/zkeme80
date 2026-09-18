GUILE_BUILD = (use-modules (ice-9 format)) (load "zkeme80.scm") ((module-ref (current-module) (quote make-rom+map)) "zkeme80.rom" "zkeme80.ram-labelmap.json")
ROM_PAGES = 00 01 02 03 04 05 3C

.PHONY: all build upgrade

all: build
	tilem2 -r zkeme80.rom

build:
	guile --no-auto-compile -c '$(GUILE_BUILD)'

upgrade: build
	mktiupgrade -k ../0A.key --device TI-84+ zkeme80.rom zkeme80.8xu $(ROM_PAGES)
