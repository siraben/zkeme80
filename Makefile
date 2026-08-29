ROM_PAGES = 00 01 02 03 04 05 3C

.PHONY: all build test upgrade

build:
	cd src && make build && cp zkeme80.rom zkeme80.ram-labelmap.json ../

all: build
	tilem2 -r zkeme80.rom

test: build
	cd src && guile --no-auto-compile ../tests/assembler-test.scm
	python3 -m unittest discover -s tests -p '*_test.py' -v
	python3 -m unittest discover -s re -p 'test_*.py' -v

upgrade: build
	mktiupgrade -k 0A.key --device TI-84+ zkeme80.rom zkeme80.8xu $(ROM_PAGES)
