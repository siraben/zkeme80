#!/usr/bin/env python3
"""Record real TilEm LCD walkthroughs using a disposable, seeded ROM.

Requires the headless TilEm fork, ImageMagick, and an X display. Boot frames
are trimmed; LCD frames otherwise remain in order at their recorded timing.
"""

import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile

from emulator import inject, variable

ROOT = Path(__file__).resolve().parents[1]
FIXTURE = ROOT / "tests/workbench-demo.fs"


def run(command, **kwargs):
    return subprocess.run(command, check=True, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, **kwargs).stdout


def trim_and_scale(source, home, destination):
    # TilEm's GIF clock can differ from macro time. Match the actual home LCD
    # pixels so trimming cannot silently cut off an interaction. Thresholding
    # is only for matching: output retains the original captured LCD colors.
    signature = run(["magick", str(home), "-threshold", "50%", "-format", "%#",
                     "info:"]).strip()
    frames = [line.split() for line in run(
        ["magick", str(source), "-coalesce", "-threshold", "50%", "-format",
         "%T %#\n", "info:"]).splitlines()]
    first = next(index for index, (_, frame_signature) in enumerate(frames)
                 if frame_signature == signature)
    command = ["magick", str(source), "-coalesce"]
    if first:
        command += ["-delete", f"0-{first - 1}"]
    command += ["-sample", "400%",
                "-loop", "0", "-layers", "Optimize", str(destination)]
    run(command)
    return round(sum(int(delay) for delay, _ in frames[first:]) / 100, 2)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "src/zkeme80.rom")
    parser.add_argument("--emulator", default=os.environ.get("TILEM", "tilem2"))
    parser.add_argument("--display", default=os.environ.get("DISPLAY", ":99"))
    parser.add_argument("--output", type=Path)
    parser.add_argument("--assets", type=Path, default=ROOT / "docs")
    args = parser.parse_args()
    output = (args.output or Path(tempfile.mkdtemp(prefix="zkeme80-demos-"))).resolve()
    assets = args.assets.resolve()
    output.mkdir(parents=True, exist_ok=True)
    assets.mkdir(parents=True, exist_ok=True)
    rom = inject(args.rom.read_bytes(), FIXTURE.read_text())
    metadata = {}

    for name in ("workbench-workspace", "workbench-system"):
        image, state, macro, raw = (output / f"{name}.{suffix}"
                                    for suffix in ("rom", "sav", "macro", "gif"))
        image.write_bytes(rom)
        state.write_text("MODEL = ti84p\n")
        # Leave enough release time for a full inspector redraw before the
        # next edge, including when another emulator is sharing the host.
        commands = ["set key_hold 0.25s", "set key_delay 0.3s", "key ON", "wait 15s"]

        def keys(*names):
            commands.extend(f"key {key}" for key in names)

        def pause(seconds=1.5):
            commands.append(f"wait {seconds}s")

        def snapshot(label):
            commands.append(f"screenshot {output}/{name}-{label}.png")
            commands.append(f"memdump {output}/{name}-{label}.ram ram-logical")

        snapshot("home")
        pause()
        if name == "workbench-workspace":
            keys("DOWN", "ENTER")
            pause(2)
            snapshot("files")
            keys("ENTER")
            pause(2)
            snapshot("preview")
            keys("RIGHT")
            pause(2)
            snapshot("next")
            keys("CLEAR", "DOWN")
            pause(1)
            keys("ENTER")
            pause(2)
            snapshot("source")
            keys("ENTER")
            pause(1.5)
            snapshot("loaded")
            keys("CLEAR", "CLEAR", "UP", "ENTER")
            pause(1)
            commands.append('scanstring "GREET"')
            pause(0.5)
            keys("ENTER")
            pause(2)
            snapshot("workspace")
            commands.append('scanstring "BYE"')
            keys("ENTER")
            pause(2)
        else:
            keys("DOWN", "DOWN", "ENTER")
            pause(1)
            keys("RIGHT")
            pause(2)
            snapshot("task-ready")
            keys("ENTER")
            pause(2)
            snapshot("task-paused")
            keys("ENTER")
            pause(1.5)
            snapshot("task-resumed")
            keys("CLEAR", "DOWN", "ENTER")
            pause(2)
            snapshot("pages")
            keys("RIGHT")
            pause(2)
            snapshot("page-next")
            keys("ENTER")
            pause(2)
            snapshot("page-hex")
            keys("CLEAR", "DOWN", "ENTER")
            pause(2)
            snapshot("services")
            keys("RIGHT")
            pause(2)
            snapshot("services-next")
            keys("CLEAR")
            pause(1)
        snapshot("end")
        macro.write_text("\n".join(commands) + "\n")
        log = run([args.emulator, "--headless", "--full-speed", "--rom", str(image),
                   "--state-file", str(state), "--reset", "--macro", str(macro),
                   "--headless-record", str(raw)],
                  cwd=ROOT, timeout=180, env={**os.environ, "DISPLAY": args.display})
        (output / f"{name}.log").write_text(log)

        def value(label, field):
            return variable((output / f"{name}-{label}.ram").read_bytes(), rom, field)

        assert value("home", "DEMO-READY") == 12345, "fixture/boot did not finish"
        assert value("home", "OS-CHOICE") == 0
        assert value("end", "DEMO-READY") == 12345
        if name == "workbench-workspace":
            assert value("next", "OS-FILE-OFFSET") > 0, "preview did not advance"
            assert value("source", "OS-FILE-NO") == 1
            assert value("loaded", "OS-NOTICE") == 3, "source was not loaded"
            assert value("workspace", "HITS") == 1, "workspace did not execute GREET"
            assert value("end", "OS-CHOICE") == 0, "BYE did not return to desktop"
            run(["magick", str(output / f"{name}-home.png"), "-sample", "400%",
                 str(assets / "workbench-cover.png")])
        else:
            assert value("task-ready", "OS-CHOICE") == 2
            assert value("task-paused", "TASK-COUNT") > 0
            assert value("task-resumed", "TASK-COUNT") > value("task-paused", "TASK-COUNT")
            assert value("pages", "OS-CHOICE") == 3
            assert value("page-next", "OS-PAGE-NO") == 1
            assert value("page-hex", "OS-PAGE-MODE") != 0
            assert value("services-next", "OS-SERVICE-FIRST") == 6
            assert value("services", "OS-CHOICE") == 4
        target = assets / f"{name}.gif"
        metadata[name] = {"seconds": trim_and_scale(
            raw, output / f"{name}-home.png", target),
                          "bytes": target.stat().st_size}
        print(f"Recorded {target}: {metadata[name]}")
    (output / "recordings.json").write_text(json.dumps(metadata, indent=2) + "\n")
    print(f"Source GIFs, macros, screenshots, and RAM snapshots: {output}")


if __name__ == "__main__":
    main()
