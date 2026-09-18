#!/usr/bin/env python3
"""Run core services and interactive compiler regressions on private ROM copies.

Run inside the emulator's dependency environment, for example:
  nix develop --command python3 tests/test-core.py --emulator /path/to/tilem2
"""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile

from emulator import inject, kernel_constant, variable

ROOT = Path(__file__).resolve().parents[1]


def run(args, output, name, source, commands):
    image = output / f"{name}.rom"
    image.write_bytes(inject(args.rom.read_bytes(), source))
    macro = output / f"{name}.macro"
    macro.write_text(commands)
    state = output / f"{name}.sav"
    state.write_text("MODEL = ti84p\n")
    process = subprocess.run(
        [args.emulator, "--headless", "--full-speed", "--rom", str(image),
         "--state-file", str(state), "--reset", "--macro", str(macro)],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
        timeout=90, env={**os.environ, "DISPLAY": args.display},
    )
    (output / f"{name}.log").write_text(process.stdout)
    assert process.returncode == 0, f"emulator failed; inspect {output / (name + '.log')}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "src/zkeme80.rom")
    parser.add_argument("--emulator", default=os.environ.get("TILEM", "tilem2"))
    parser.add_argument("--display", default=os.environ.get("DISPLAY", ":99"))
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    output = (args.output or Path(tempfile.mkdtemp(prefix="zkeme80-core-"))).resolve()
    output.mkdir(parents=True, exist_ok=True)
    rom = args.rom.read_bytes()
    source = (ROOT / "tests/os-core.fs").read_text()
    source += '\nPAGE CORE-FAILURES @ . ." failures / " CORE-CHECKS @ . CR\n'
    source += ': CORE-HOLD BEGIN AGAIN ; CORE-HOLD\n'
    run(args, output, "core", source,
        f"key ON\nwait 8s\nscreenshot {output}/core.png\n"
        f"memdump {output}/core.ram ram-logical\n")
    ram = (output / "core.ram").read_bytes()
    assert variable(ram, rom, "CORE-DONE") == 51966, f"core tests did not finish: {output}"
    checks, failures = variable(ram, rom, "CORE-CHECKS"), variable(ram, rom, "CORE-FAILURES")
    assert checks >= 40 and failures == 0, (checks, failures, output)
    print(f"Core services: {checks} target assertions passed", flush=True)

    source = "VARIABLE CORE-VALUE 0 CORE-VALUE ! : MARK 1 CORE-VALUE +! ; MENU-DEMO"
    commands = f"""set key_hold 0.08s
set key_delay 0.04s
key ON
wait 8s
key ENTER
wait 0.2s
scanstring ": PERSIST"
key ENTER
wait 0.2s
memdump {output}/compiling.ram ram-logical
scanstring "MARK ;"
key ENTER
wait 0.2s
scanstring "PERSIST"
key ENTER
wait 0.2s
memdump {output}/defined.ram ram-logical
scanstring ": BROKEN"
key ENTER
scanstring "NOPE"
key ENTER
wait 0.2s
scanstring "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
key ENTER
wait 0.2s
screenshot {output}/overflow.png
scanstring "PERSIST"
key ENTER
wait 0.2s
memdump {output}/recovered.ram ram-logical
scanstring "BYE"
key ENTER
wait 0.2s
key ENTER
wait 0.2s
scanstring "PERSIST"
key ENTER
wait 0.2s
screenshot {output}/persisted.png
memdump {output}/persisted.ram ram-logical
"""
    run(args, output, "workspace", source, commands)
    compiling = (output / "compiling.ram").read_bytes()
    state_offset = kernel_constant(rom, "STATE") - 0x8000
    assert int.from_bytes(compiling[state_offset:state_offset + 2], "little") == 1
    for name, expected in (("defined", 1), ("recovered", 2), ("persisted", 3)):
        ram = (output / f"{name}.ram").read_bytes()
        assert variable(ram, rom, "CORE-VALUE") == expected, (name, output)
        assert int.from_bytes(ram[state_offset:state_offset + 2], "little") == 0
    print("Multiline compilation, parser error recovery, and BYE/reenter persistence passed")
    print(f"Emulator artifacts: {output}")


if __name__ == "__main__":
    main()
