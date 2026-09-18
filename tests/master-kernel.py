#!/usr/bin/env python3
"""Run master's ANS suite and allocation regressions on disposable TilEm ROMs.

Requires an extended TilEm with --headless/--macro and an X display. Generated
macros, screenshots, logs, and logical RAM dumps remain in --output.
"""

import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
PAGE = 16384


def run_emulator(args, output, name, rom, macro):
    image = output / f"{name}.rom"
    image.write_bytes(rom)
    state = output / f"{name}.sav"
    state.write_text("MODEL = ti84p\n")
    script = output / f"{name}.macro"
    script.write_text(macro)
    result = subprocess.run(
        [args.emulator, "--headless", "--full-speed", "--rom", str(image),
         "--state-file", str(state), "--reset", "--macro", str(script)],
        cwd=ROOT, env={**os.environ, "DISPLAY": args.display}, text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=180,
    )
    (output / f"{name}.log").write_text(result.stdout)
    if result.returncode:
        raise AssertionError(f"emulator failed; inspect {output}/{name}.log")


def inject_checks(rom):
    """Replace only master's final page-1 menu action; use erased page 6."""
    if len(rom) != 64 * PAGE or rom[6 * PAGE:7 * PAGE] != b"\xff" * PAGE:
        raise ValueError("expected master's 1MiB layout with erased page 6")
    core = rom[PAGE:2 * PAGE].split(b"\0", 1)[0]
    trailer = b"\nMENU-DEMO\n"
    if not core.endswith(trailer):
        raise ValueError("bootstrap page 1 has no final MENU-DEMO")

    # Read NEXT from H0's native constant stub, then emit a test-only bank
    # inspector: PUSH BC; IN A,(6); LD B,0; LD C,A; JP NEXT. No new OS API.
    marker = b"\x02H0\0\xc5\x01"
    if rom[:PAGE].count(marker) != 1:
        raise ValueError("missing or ambiguous H0 constant stub")
    offset = rom.index(marker) + len(marker) + 2
    if rom[offset] != 0xC3:
        raise ValueError("unexpected H0 constant epilogue")
    code = b"\xc5\xdb\x06\x06\x00\x4f\xc3" + rom[offset + 1:offset + 3]
    source = "HERE " + " ".join(f"{byte} C," for byte in code)
    source += " CONSTANT TEST-BANK-CODE\n: TEST-BANK@ TEST-BANK-CODE EXECUTE ;\n"
    source += (ROOT / "tests/master-memory.fs").read_text()
    core = core[:-len(trailer)] + (b"\n: START-CHECKS 6 MAP-FLASH DROP "
                                   b"MEMA CSTRING-SOURCE ; START-CHECKS\n")
    image = bytearray(rom)
    for page, payload in ((1, core), (6, source.encode("ascii"))):
        if len(payload) >= PAGE or b"\0" in payload:
            raise ValueError("test source must fit one terminated flash page")
        image[page * PAGE:(page + 1) * PAGE] = (payload + b"\0").ljust(PAGE, b"\xff")
    return bytes(image)


def check_memory(ram, labelmap):
    labels = {entry["name"]: entry["addr"] for entry in labelmap["labels"]}

    def cell(address):
        if not 0x8000 <= address < 0xDFFF:
            raise AssertionError(f"invalid test data address {address:#x}")
        offset = address - 0x8000
        return int.from_bytes(ram[offset:offset + 2], "little")

    wanted = {"CHECKS", "FAILURES", "DONE", "ERRORS"}
    fields = {}
    seen = set()
    header = cell(labels["var-latest"])
    while 0x8000 <= header < 0xE000 and header not in seen:
        seen.add(header)
        offset = header - 0x8000
        length = ram[offset + 2] & 31
        name = ram[offset + 3:offset + 3 + length].decode("ascii")
        if name in wanted:
            fields[name] = header + 4 + length + 3  # CREATE/VARIABLE's DOVAR body
        header = cell(header)
    if fields.keys() != wanted or cell(fields["DONE"]) != 51966:
        raise AssertionError(f"kernel tests did not complete: {fields}")
    failures = cell(fields["FAILURES"])
    errors = [tuple(cell(fields["ERRORS"] + index * 6 + part * 2) for part in range(3))
              for index in range(min(failures, 80))]
    checks = cell(fields["CHECKS"])
    if failures or checks < 100:
        raise AssertionError(f"{failures}/{checks} failed (check, actual, expected): {errors}")
    print(f"Kernel boundaries: {checks} assertions passed", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "src/zkeme80.rom")
    parser.add_argument("--labelmap", type=Path, default=ROOT / "src/zkeme80.ram-labelmap.json")
    parser.add_argument("--emulator", default=os.environ.get("TILEM", "tilem2"))
    parser.add_argument("--display", default=os.environ.get("DISPLAY", ":99"))
    parser.add_argument("--output", type=Path)
    parser.add_argument("--only", choices=("suite", "boundaries"))
    args = parser.parse_args()
    output = (args.output or Path(tempfile.mkdtemp(prefix="zkeme80-master-"))).resolve()
    output.mkdir(parents=True, exist_ok=True)
    rom = args.rom.read_bytes()
    labelmap = json.loads(args.labelmap.read_text())
    (output / "labels.json").write_text(json.dumps(labelmap))
    if args.only != "boundaries":
        macro = (ROOT / "debug/macros/run-test-suite.macro").read_text()
        macro = macro.replace("debug/macros/", str(output) + "/")
        run_emulator(args, output, "suite", rom, macro)
        subprocess.run(
            [sys.executable, str(ROOT / "debug/verify_test_ram.py"),
             str(output / "SUITE_RAM"), str(output / "labels.json"),
             "--before", str(output / "BEFORE_SUITE_RAM"),
             "--after", str(output / "AFTER_SUITE_RAM")], check=True,
        )
    if args.only != "suite":
        run_emulator(args, output, "boundaries", inject_checks(rom),
                     f"key ON\nwait 15s\nscreenshot {output}/boundaries.png\n"
                     f"memdump {output}/boundaries.ram ram-logical\n")
        check_memory((output / "boundaries.ram").read_bytes(), labelmap)
    print(f"Emulator evidence: {output}")


if __name__ == "__main__":
    main()
