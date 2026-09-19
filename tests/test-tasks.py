#!/usr/bin/env python3
"""Run scheduler regressions and menu/shell idle checks on a private ROM copy."""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parents[1]
from emulator import inject, variable


def run(args, directory, stem, source, commands):
    rom = directory / f"{stem}.rom"
    state = directory / f"{stem}.sav"
    macro = directory / f"{stem}.macro"
    rom.write_bytes(inject(args.rom.read_bytes(), source))
    state.write_text("MODEL = ti84p\n")
    macro.write_text(commands)
    subprocess.run(
        [str(args.emulator), "--headless", "--full-speed", "--rom", str(rom),
         "--state-file", str(state), "--reset", "--macro", str(macro)],
        cwd=ROOT, check=True, timeout=120,
        env={**os.environ, "DISPLAY": args.display},
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "src/zkeme80.rom")
    parser.add_argument("--emulator", type=Path,
                        default=Path(os.environ.get("TILEM", "tilem2")))
    parser.add_argument("--display", default=os.environ.get("DISPLAY", ":99"))
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    directory = (args.output or Path(tempfile.mkdtemp(prefix="zkeme80-tasks-"))).resolve()
    directory.mkdir(parents=True, exist_ok=True)
    def value(ram, name):
        return variable(ram, args.rom.read_bytes(), name)
    source = (ROOT / "tests/os-tasks-standalone.fs").read_text()
    source += '\nPAGE TASK-FAILURES @ . ." failures / " TASK-TESTS @ . CR\n'
    source += ': TASK-HOLD BEGIN AGAIN ; TASK-HOLD\n'
    run(args, directory, "suite", source,
        f"key ON\nwait 20s\nscreenshot {directory}/suite.png\n"
        f"memdump {directory}/suite.ram ram-logical\n")
    ram = (directory / "suite.ram").read_bytes()
    failures, total = value(ram, "TASK-FAILURES"), value(ram, "TASK-TESTS")
    assert total == 167 and failures == 0, (failures, total, directory)
    print(f"Scheduler: {total} target assertions passed")

    commands = f"""set key_hold 0.25s
set key_delay 0.1s
key ON
wait 20s
memdump {directory}/menu-before.ram ram-logical
wait 1s
memdump {directory}/menu-after.ram ram-logical
key ENTER
wait 1s
memdump {directory}/shell-before.ram ram-logical
wait 1s
memdump {directory}/shell-after.ram ram-logical
scanstring "STAR"
key ENTER
wait 0.5s
screenshot {directory}/shell-idle.png
scanstring "BYE"
key ENTER
wait 0.5s
screenshot {directory}/desktop-return.png
memdump {directory}/returned.ram ram-logical
"""
    run(args, directory, "idle", "TASK-INIT TASK-DEMO DROP MENU-DEMO", commands)
    for context in ("menu", "shell"):
        before_ram = (directory / f"{context}-before.ram").read_bytes()
        after_ram = (directory / f"{context}-after.ram").read_bytes()
        before = value(before_ram, "TASK-COUNT")
        after = value(after_ram, "TASK-COUNT")
        assert before != after, (context, before, after)
        if context == "shell":
            assert value(before_ram, "SHELL-RUNNING") == 1
            assert value(after_ram, "SHELL-RUNNING") == 1
        print(f"{context} idle: counter advanced {before} -> {after}")
    returned = (directory / "returned.ram").read_bytes()
    assert value(returned, "SHELL-RUNNING") == 0, "BYE did not return to desktop"
    print(f"Screenshots and RAM snapshots: {directory}")


if __name__ == "__main__":
    main()
