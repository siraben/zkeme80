ROM_PAGES = 00 01 02 03 04 05 06 3C
PYTHON ?= python3
TILEM_HEADLESS ?= tilem2
BOOTSTRAP_RAM = build/bootstrap.ram
BOOTSTRAP_IMAGE = build/bootstrap.zkbi
BOOTSTRAP_STREAM = build/bootstrap.zbs
PRECOMPILED_ROM = zkeme80-precompiled.rom
PRECOMPILED_LABELMAP = zkeme80-precompiled.ram-labelmap.json

.PHONY: all build test upgrade bootstrap-capture bootstrap-pack \
	bootstrap-image bootstrap-verify bootstrap-self-test bootstrap-stream \
	precompiled-rom precompiled-smoke precompiled-upgrade

build:
	cd src && make build && cp zkeme80.rom zkeme80.ram-labelmap.json ../

all: build
	tilem2 -r zkeme80.rom

test: build
	cd src && guile --no-auto-compile ../tests/assembler-test.scm
	python3 -m unittest discover -s tests -p '*_test.py' -v
	python3 -m unittest discover -s re -p 'test_*.py' -v
	$(PYTHON) tools/bootstrap_image.py self-test

upgrade: build
	mktiupgrade -k 0A.key --device TI-84+ zkeme80.rom zkeme80.8xu $(ROM_PAGES)

bootstrap-capture: build
	@command -v "$(TILEM_HEADLESS)" >/dev/null || { \
		echo "tilem-headless not found; set TILEM_HEADLESS=/path/to/tilem2" >&2; \
		exit 1; \
	}
	mkdir -p build
	$(TILEM_HEADLESS) --headless --rom zkeme80.rom --model ti84p \
		--normal-speed --reset --macro re/macros/capture-bootstrap.macro

bootstrap-pack: build
	@test -f "$(BOOTSTRAP_RAM)" || { \
		echo "missing $(BOOTSTRAP_RAM); run make bootstrap-capture first" >&2; \
		exit 1; \
	}
	$(PYTHON) tools/bootstrap_image.py pack "$(BOOTSTRAP_RAM)" \
		"$(BOOTSTRAP_IMAGE)" --dump-layout physical

bootstrap-image: bootstrap-capture
	$(PYTHON) tools/bootstrap_image.py pack "$(BOOTSTRAP_RAM)" \
		"$(BOOTSTRAP_IMAGE)" --dump-layout physical
	$(PYTHON) tools/bootstrap_image.py verify "$(BOOTSTRAP_IMAGE)"

bootstrap-verify: build
	$(PYTHON) tools/bootstrap_image.py verify "$(BOOTSTRAP_IMAGE)"

bootstrap-self-test:
	$(PYTHON) tools/bootstrap_image.py self-test

precompiled-rom: bootstrap-image
	ZKEME80_BOOTSTRAP_IMAGE=../$(BOOTSTRAP_IMAGE) $(MAKE) -C src build
	cp src/zkeme80.rom "$(PRECOMPILED_ROM)"
	cp src/zkeme80.ram-labelmap.json "$(PRECOMPILED_LABELMAP)"
	$(PYTHON) tools/bootstrap_image.py verify "$(BOOTSTRAP_IMAGE)" \
		--rom zkeme80.rom

precompiled-smoke: precompiled-rom
	@command -v "$(TILEM_HEADLESS)" >/dev/null || { \
		echo "tilem-headless not found; set TILEM_HEADLESS=/path/to/tilem2" >&2; \
		exit 1; \
	}
	$(TILEM_HEADLESS) --headless --rom "$(PRECOMPILED_ROM)" --model ti84p \
		--normal-speed --reset --macro re/macros/capture-precompiled.macro
	$(PYTHON) tools/bootstrap_image.py verify-ram "$(BOOTSTRAP_IMAGE)" \
		build/precompiled.ram --dump-layout physical

precompiled-upgrade: precompiled-rom
	mktiupgrade -k 0A.key --device TI-84+ "$(PRECOMPILED_ROM)" \
		zkeme80-precompiled.8xu $(ROM_PAGES)

bootstrap-stream:
	mkdir -p build
	$(PYTHON) tools/bootstrap_stream.py build --codec lzss \
		--output "$(BOOTSTRAP_STREAM)"
