#!/usr/bin/env python3
"""Exercise desktop navigation, object previews, task controls and bank restoration.

Uses a disposable ROM with test data and a cooperative bank-checking observer.
Screenshots, a macro, and RAM snapshots are retained in --output or /tmp.
"""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile

from emulator import inject, variable

ROOT = Path(__file__).resolve().parents[1]

SOURCE = r'''
VARIABLE DESKTOP-BANK
VARIABLE DESKTOP-BANK-FAILS
VARIABLE DESKTOP-JOB-STATE
VARIABLE DESKTOP-FS-IOR
VARIABLE DESKTOP-READY
BANK@ DESKTOP-BANK !
0 DESKTOP-BANK-FAILS ! 0 DESKTOP-JOB-STATE !
HERE 200 ALLOT CONSTANT DESKTOP-DATA
: DESKTOP-SEED 200 0 DO 65 DESKTOP-DATA I + C! LOOP ;
DESKTOP-SEED
DESKTOP-DATA 200 S" NOTE" 1 FS-PUT DESKTOP-FS-IOR !
: DESKTOP-OBSERVE
  DROP BANK@ DESKTOP-BANK @ <> IF 1 DESKTOP-BANK-FAILS +! THEN
  OS-TASK-ID @ TASK-INFO 2DROP DESKTOP-JOB-STATE !
;
TASK-INIT ' DESKTOP-OBSERVE 0 TASK-NEW DROP
12345 DESKTOP-READY !
MENU-DEMO
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "src/zkeme80.rom")
    parser.add_argument("--emulator", default=os.environ.get("TILEM", "tilem2"))
    parser.add_argument("--display", default=os.environ.get("DISPLAY", ":99"))
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    output = (args.output or Path(tempfile.mkdtemp(prefix="zkeme80-desktop-"))).resolve()
    output.mkdir(parents=True, exist_ok=True)
    rom = inject(args.rom.read_bytes(), SOURCE)
    image, state, macro = (output / name for name in ("desktop.rom", "desktop.sav", "desktop.macro"))
    image.write_bytes(rom)
    state.write_text("MODEL = ti84p\n")
    commands = ["set key_hold 0.25s", "set key_delay 0.1s", "key ON", "wait 15s"]

    def keys(*names):
        commands.extend(f"key {name}" for name in names)

    def snapshot(name):
        commands.extend(["wait 1s", f"screenshot {output}/{name}.png",
                         f"memdump {output}/{name}.ram ram-logical"])

    snapshot("desktop")
    keys("DOWN", "ENTER")
    snapshot("files")
    keys("ENTER")
    snapshot("file-preview")
    keys("RIGHT")
    snapshot("file-next")
    keys("CLEAR", "CLEAR", "DOWN", "ENTER")
    snapshot("tasks")
    keys("RIGHT")
    snapshot("task-demo")
    keys("ENTER")
    snapshot("task-paused")
    snapshot("task-still-paused")
    keys("ENTER")
    snapshot("task-resumed")
    keys("CLEAR", "DOWN", "ENTER")
    snapshot("pages")
    keys("RIGHT")
    snapshot("page-next")
    keys("CLEAR", "DOWN", "ENTER")
    snapshot("services")
    keys("RIGHT")
    snapshot("services-next")
    keys("LEFT", "CLEAR", "UP")
    snapshot("returned")
    keys("DOWN", "DOWN")
    snapshot("suite-selected")
    macro.write_text("\n".join(commands) + "\n")
    process = subprocess.run(
        [args.emulator, "--headless", "--full-speed", "--rom", str(image),
         "--state-file", str(state), "--reset", "--macro", str(macro)],
        cwd=ROOT, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        timeout=150, env={**os.environ, "DISPLAY": args.display},
    )
    (output / "desktop.log").write_text(process.stdout)
    assert process.returncode == 0, (process.returncode, process.stdout, output)

    def value(snapshot_name, name):
        return variable((output / f"{snapshot_name}.ram").read_bytes(), rom, name)

    assert value("desktop", "DESKTOP-READY") == 12345, "boot/test setup did not finish"
    assert value("desktop", "DESKTOP-FS-IOR") == 0, "could not seed preview object"
    expected = {
        "desktop": ("OS-CHOICE", 0), "files": ("OS-CHOICE", 1),
        "file-preview": ("OS-FILE-OFFSET", 0), "file-next": ("OS-FILE-OFFSET", 128),
        "tasks": ("OS-CHOICE", 2), "task-demo": ("OS-TASK-ID", 2),
        "task-paused": ("DESKTOP-JOB-STATE", 2),
        "task-resumed": ("DESKTOP-JOB-STATE", 1),
        "pages": ("OS-CHOICE", 3), "page-next": ("OS-PAGE-NO", 1),
        "services": ("OS-CHOICE", 4), "services-next": ("OS-SERVICE-FIRST", 6),
        "returned": ("OS-CHOICE", 3),
        "suite-selected": ("OS-CHOICE", 5),
    }
    for name, (field, expected_value) in expected.items():
        observed = value(name, field)
        assert observed == expected_value, (name, field, expected_value, observed, output)
        assert value(name, "DESKTOP-BANK-FAILS") == 0, (name, "bank mapping leaked")
    paused = value("task-paused", "TASK-COUNT")
    assert paused == value("task-still-paused", "TASK-COUNT"), "paused task ran"
    assert paused != value("task-resumed", "TASK-COUNT"), "resumed task did not run"
    assert value("returned", "OS-SERVICE-FIRST") == 0, "service previous-page failed"
    print("Desktop: files/preview, task create/pause/resume, pages, services and return passed")
    print("Bank mapping stayed restored at every scheduling point")
    print(f"Screenshots and RAM snapshots: {output}")


if __name__ == "__main__":
    main()
