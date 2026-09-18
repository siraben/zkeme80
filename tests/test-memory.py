#!/usr/bin/env python3
"""Verify dictionary allocation bounds on a disposable ROM in headless TilEm."""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile

from emulator import inject, variable

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "src/zkeme80.rom")
    parser.add_argument("--emulator", default=os.environ.get("TILEM", "tilem2"))
    parser.add_argument("--display", default=os.environ.get("DISPLAY", ":99"))
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    output = (args.output or Path(tempfile.mkdtemp(prefix="zkeme80-memory-"))).resolve()
    output.mkdir(parents=True, exist_ok=True)
    rom = args.rom.read_bytes()
    source = (ROOT / "tests/os-memory.fs").read_text()
    source += '\nPAGE MEM-FAILURES @ . ." failures / " MEM-CHECKS @ . CR\n'
    source += ': MEM-HOLD BEGIN AGAIN ; MEM-HOLD\n'
    image, state, macro = (output / name for name in ("memory.rom", "memory.sav", "memory.macro"))
    image.write_bytes(inject(rom, source))
    state.write_text("MODEL = ti84p\n")
    macro.write_text(f"key ON\nwait 8s\nscreenshot {output}/memory.png\n"
                     f"memdump {output}/memory.ram ram-logical\n")
    process = subprocess.run(
        [args.emulator, "--headless", "--full-speed", "--rom", str(image),
         "--state-file", str(state), "--reset", "--macro", str(macro)],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
        timeout=60, env={**os.environ, "DISPLAY": args.display},
    )
    (output / "memory.log").write_text(process.stdout)
    assert process.returncode == 0, f"emulator failed: {output}/memory.log"
    ram = (output / "memory.ram").read_bytes()
    assert variable(ram, rom, "MEM-DONE") == 51966, f"test did not finish: {output}"
    checks, failures = variable(ram, rom, "MEM-CHECKS"), variable(ram, rom, "MEM-FAILURES")
    assert checks >= 40 and failures == 0, (checks, failures, output)
    print(f"Memory bounds: {checks} target assertions passed")
    print(f"Emulator artifacts: {output}")


if __name__ == "__main__":
    main()
