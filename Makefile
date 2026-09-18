.PHONY: build all upgrade test-build test-emulator

build:
	$(MAKE) -C src build
	cp src/zkeme80.rom src/zkeme80.ram-labelmap.json .

all: build
	tilem2 -r zkeme80.rom

upgrade:
	$(MAKE) -C src upgrade
	cp src/zkeme80.8xu .

test-build:
	python3 tests/test-build.py

# Requires a TilEm build with --headless and --macro; set TILEM to its path.
test-emulator: build
	python3 tests/master-kernel.py
