build:
	cd src && make build && cp zkeme80.rom ../

all:
	cd src && make build && cp zkeme80.rom ../
	tilem2 -r zkeme80.rom

upgrade:
	cd src && make build
	mktiupgrade -k 0A.key --device TI-84+ zkeme80.rom zkeme80.8xu 00 01 02 03 3C
	cp zkeme80.8xu ../
