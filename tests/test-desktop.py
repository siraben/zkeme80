#!/usr/bin/env python3
"""Exercise the desktop in TilEm, including boundary states and LCD rendering.

Each scenario uses a disposable ROM. Replay macros, screenshots, expected pixel
models, and RAM snapshots are retained in --output (or a temporary directory).
"""

import argparse
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile

from emulator import inject, kernel_constant, variable

ROOT = Path(__file__).resolve().parents[1]

SETUP = r'''
VARIABLE DESKTOP-BANK
VARIABLE DESKTOP-BANK-FAILS
VARIABLE DESKTOP-JOB-STATE
VARIABLE DESKTOP-FS-IOR
VARIABLE DESKTOP-READY
VARIABLE DESKTOP-LOADED
0 DESKTOP-LOADED !
BANK@ DESKTOP-BANK !
0 DESKTOP-BANK-FAILS ! 0 DESKTOP-JOB-STATE ! 0 DESKTOP-FS-IOR !
: DESKTOP-OBSERVE
  DROP BANK@ DESKTOP-BANK @ <> IF 1 DESKTOP-BANK-FAILS +! THEN
  OS-TASK-ID @ TASK-INFO 2DROP DESKTOP-JOB-STATE !
;
TASK-INIT ' DESKTOP-OBSERVE 0 TASK-NEW DROP
65535 1 TASK-RECORD 8 + !
'''
SEED = r'''
HERE 200 ALLOT CONSTANT DESKTOP-DATA
: DESKTOP-SEED 200 0 DO 65 I 26 MOD + DESKTOP-DATA I + C! LOOP ;
DESKTOP-SEED
DESKTOP-DATA 200 S" NOTE" 1 FS-PUT DESKTOP-FS-IOR +!
DESKTOP-DATA 0 S" EMPTY" 1 FS-PUT DESKTOP-FS-IOR +!
: DESKTOP-SOURCE S" 17 DESKTOP-LOADED !" ;
DESKTOP-SOURCE S" SOURCE" 2 FS-PUT DESKTOP-FS-IOR +!
DESKTOP-DATA 16 S" BINARY" 3 FS-PUT DESKTOP-FS-IOR +!
DESKTOP-DATA 20 S" FOUR" 1 FS-PUT DESKTOP-FS-IOR +!
: DESKTOP-BAD-SOURCE S" UNKNOWN-DESKTOP-WORD" ;
DESKTOP-BAD-SOURCE S" FIVE" 2 FS-PUT DESKTOP-FS-IOR +!
HERE 4 ALLOT CONSTANT DESKTOP-NAME
83 DESKTOP-NAME C! 1 DESKTOP-NAME 1+ C!
88 DESKTOP-NAME 2+ C! 255 DESKTOP-NAME 3 + C!
DESKTOP-DATA 28 DESKTOP-NAME 4 1 FS-PUT DESKTOP-FS-IOR +!
'''
START = "\n12345 DESKTOP-READY !\nMENU-DEMO\n"


class Scenario:
    def __init__(self, args, output, name, source):
        self.args = args
        self.output = output / name
        self.output.mkdir(parents=True, exist_ok=True)
        self.rom = inject(args.rom.read_bytes(), source)
        self.commands = ["set key_hold 0.25s", "set key_delay 0.1s", "key ON", "wait 20s"]
        self.expected = {}

    def keys(self, *names):
        self.commands.extend(f"key {name}" for name in names)

    def snapshot(self, name, **expected):
        self.commands.extend(["wait 1s", f"screenshot {self.output}/{name}.png",
                              f"memdump {self.output}/{name}.ram ram-logical"])
        self.expected[name] = expected

    def run(self):
        image, state, macro = (self.output / name for name in
                               ("desktop.rom", "desktop.sav", "desktop.macro"))
        image.write_bytes(self.rom)
        state.write_text("MODEL = ti84p\n")
        macro.write_text("\n".join(self.commands) + "\n")
        process = subprocess.run(
            [self.args.emulator, "--headless", "--full-speed", "--rom", str(image),
             "--state-file", str(state), "--reset", "--macro", str(macro)],
            cwd=ROOT, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            timeout=240, env={**os.environ, "DISPLAY": self.args.display},
        )
        (self.output / "desktop.log").write_text(process.stdout)
        assert process.returncode == 0, (process.returncode, process.stdout, self.output)
        for name, fields in self.expected.items():
            assert self.value(name, "DESKTOP-READY") == 12345, (name, "setup did not finish")
            assert self.value(name, "DESKTOP-FS-IOR") == 0, (name, "could not seed objects")
            assert self.value(name, "DESKTOP-BANK-FAILS") == 0, (name, "bank mapping leaked")
            for field, wanted in fields.items():
                observed = self.value(name, field.replace("_", "-"))
                assert observed == wanted, (name, field, wanted, observed, self.output)

    def value(self, name, field):
        return variable((self.output / f"{name}.ram").read_bytes(), self.rom, field)



class ScreenModel:
    """Compare selected LCD regions against glyphs from the real kernel font."""

    def __init__(self):
        path = ROOT / "tests/verify-shell-screen.py"
        spec = importlib.util.spec_from_file_location("shell_screen", path)
        self.renderer = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.renderer)
        self.font = self.renderer.load_font(ROOT / "src/font.scm")
        self.pixels = [[False] * 96 for _ in range(64)]

    def text(self, x, y, value):
        self.renderer.draw_text(self.pixels, self.font, x, y, value)
        return self

    def invert(self, x, y, width, height):
        for row in range(y, y + height):
            for col in range(x, x + width):
                self.pixels[row][col] = not self.pixels[row][col]
        return self

    def frame(self, title, footer):
        self.text(3, 1, title).invert(0, 0, 96, 8)
        self.invert(0, 55, 96, 1).text(2, 58, footer)
        return self

    def compare(self, scenario, name, regions=((0, 0, 96, 64),)):
        expected = scenario.output / f"{name}-expected.pbm"
        self.renderer.write_pbm(expected, self.pixels)
        actual = subprocess.run(
            ["magick", str(scenario.output / f"{name}.png"), "-colorspace", "Gray",
             "-threshold", "50%", "-depth", "8", "gray:-"],
            check=True, stdout=subprocess.PIPE,
        ).stdout
        assert len(actual) == 96 * 64, (name, "expected native 96x64 LCD capture")
        differences = []
        for x, y, width, height in regions:
            for row in range(y, y + height):
                for col in range(x, x + width):
                    if (actual[row * 96 + col] == 0) != self.pixels[row][col]:
                        differences.append((col, row))
        assert not differences, (name, len(differences), "LCD pixels differ", differences[:12], expected)
        print(f"{scenario.output.name}/{name}: exact LCD region match")


def empty_scenario(args, output):
    scenario = Scenario(args, output, "empty", SETUP + "9 OS-FILE-NO !\n" + START)
    scenario.snapshot("desktop", OS_CHOICE=0)
    scenario.keys("UP")
    scenario.snapshot("home-first", OS_CHOICE=0)
    scenario.keys("DOWN", "ENTER")
    scenario.snapshot("files", OS_FILE_NO=0)
    scenario.keys("DOWN", "UP", "ENTER")
    scenario.snapshot("files-still-empty", OS_FILE_NO=0, OS_FILE_OFFSET=0)
    scenario.keys("CLEAR", *("DOWN",) * 8)
    scenario.snapshot("home-last", OS_CHOICE=6)
    scenario.run()
    return scenario


def populated_scenario(args, output):
    scenario = Scenario(args, output, "populated", SETUP + SEED + START)
    scenario.snapshot("desktop", OS_CHOICE=0)
    scenario.keys("DOWN", "ENTER")
    scenario.snapshot("files", OS_CHOICE=1, OS_FILE_NO=0)
    scenario.keys("UP", "ENTER", "LEFT")
    scenario.snapshot("file-preview", OS_FILE_OFFSET=0)
    scenario.keys("RIGHT")
    scenario.snapshot("file-next", OS_FILE_OFFSET=96)
    scenario.keys("RIGHT", "RIGHT")
    scenario.snapshot("file-last", OS_FILE_OFFSET=192)
    scenario.keys("LEFT", "LEFT", "LEFT")
    scenario.snapshot("file-first", OS_FILE_OFFSET=0)
    scenario.keys("CLEAR", "DOWN", "ENTER", "RIGHT")
    scenario.snapshot("empty-preview", OS_FILE_OFFSET=0)
    scenario.keys("CLEAR", "DOWN", "ENTER", "ENTER")
    scenario.snapshot("source-loaded", OS_NOTICE=3, DESKTOP_LOADED=17)
    scenario.keys("CLEAR", "DOWN", "DOWN", "DOWN", "ENTER", "ENTER")
    scenario.snapshot("source-error", OS_NOTICE=4, DESKTOP_LOADED=17)
    scenario.keys("CLEAR", *("DOWN",) * 8)
    scenario.snapshot("file-list-last", OS_FILE_NO=6)
    scenario.keys(*("UP",) * 8)
    scenario.snapshot("file-list-first", OS_FILE_NO=0)
    scenario.keys("CLEAR", "DOWN", "ENTER")
    scenario.snapshot("tasks", OS_CHOICE=2)
    scenario.keys("UP", "RIGHT")
    scenario.snapshot("task-demo", OS_TASK_ID=2, DESKTOP_JOB_STATE=1)
    scenario.keys("ENTER")
    scenario.snapshot("task-paused", DESKTOP_JOB_STATE=2)
    scenario.snapshot("task-still-paused", DESKTOP_JOB_STATE=2)
    scenario.keys("ENTER")
    scenario.snapshot("task-resumed", DESKTOP_JOB_STATE=1)
    scenario.keys("RIGHT", "RIGHT", "RIGHT")
    scenario.snapshot("task-full", OS_TASK_ID=4, OS_NOTICE=1)
    scenario.keys("DOWN")
    scenario.snapshot("task-last", OS_TASK_ID=4)
    scenario.keys(*("UP",) * 5)
    scenario.snapshot("task-first", OS_TASK_ID=1)
    scenario.keys("CLEAR", "DOWN", "ENTER", "LEFT")
    scenario.snapshot("pages", OS_CHOICE=3, OS_PAGE_NO=0)
    scenario.keys("RIGHT")
    scenario.snapshot("page-next", OS_PAGE_NO=1)
    scenario.keys(*("RIGHT",) * 65)
    scenario.snapshot("page-last", OS_PAGE_NO=63)
    scenario.keys("DOWN", "ENTER")
    scenario.snapshot("page-last-hex", OS_PAGE_NO=63, OS_PAGE_MODE=1)
    scenario.keys("ENTER")
    scenario.keys(*("LEFT",) * 65)
    scenario.snapshot("page-first", OS_PAGE_NO=0)
    scenario.keys("UP", "ENTER")
    scenario.snapshot("page-first-hex", OS_PAGE_NO=0, OS_PAGE_MODE=1)
    scenario.keys("ENTER")
    scenario.keys("CLEAR", "DOWN", "ENTER", "LEFT")
    scenario.snapshot("services", OS_CHOICE=4, OS_SERVICE_FIRST=0)
    scenario.keys("RIGHT")
    scenario.snapshot("services-next", OS_SERVICE_FIRST=6)
    scenario.keys("RIGHT", "RIGHT", "RIGHT")
    scenario.snapshot("services-last", OS_SERVICE_FIRST=12)
    scenario.keys("LEFT", "LEFT", "LEFT")
    scenario.snapshot("services-first", OS_SERVICE_FIRST=0)
    scenario.keys("CLEAR", "UP")
    scenario.snapshot("returned", OS_CHOICE=3)
    scenario.keys("DOWN", "DOWN")
    scenario.snapshot("suite-selected", OS_CHOICE=5)
    scenario.run()
    paused = scenario.value("task-paused", "TASK-COUNT")
    assert paused == scenario.value("task-still-paused", "TASK-COUNT"), "paused task ran"
    assert paused != scenario.value("task-resumed", "TASK-COUNT"), "resumed task did not run"
    return scenario




def lifecycle_scenario(args, output):
    scenario = Scenario(args, output, "lifecycle", SETUP + START)
    scenario.keys("DOWN", "DOWN", "ENTER", "RIGHT", "LEFT")
    scenario.snapshot("stopped", OS_TASK_ID=2, DESKTOP_JOB_STATE=3)
    scenario.snapshot("still-stopped", DESKTOP_JOB_STATE=3)
    scenario.keys("ENTER")
    scenario.snapshot("restarted", DESKTOP_JOB_STATE=1)
    scenario.keys("DEL")
    scenario.snapshot("freed", OS_TASK_ID=2, DESKTOP_JOB_STATE=0)
    scenario.keys("ENTER")
    scenario.snapshot("free-stays-free", DESKTOP_JOB_STATE=0)
    scenario.keys("RIGHT")
    scenario.snapshot("slot-reused", OS_TASK_ID=2, DESKTOP_JOB_STATE=1)
    scenario.run()
    assert scenario.value("stopped", "TASK-COUNT") == scenario.value("still-stopped", "TASK-COUNT")
    assert scenario.value("restarted", "TASK-COUNT") != scenario.value("stopped", "TASK-COUNT")
    return scenario


def check_pixels(empty, populated):
    labels = ("Forth workspace", "Files", "Tasks", "Pages / memory",
              "System calls", "Test suite", "Power off")
    for scenario, name, choice in ((empty, "desktop", 0), (empty, "home-last", 6),
                                   (populated, "returned", 3),
                                   (populated, "suite-selected", 5)):
        model = ScreenModel().frame("ZKEME80 / WORKBENCH", "^v select ENT open")
        first = max(0, choice - 4)
        for row in range(5):
            index = first + row
            y = 20 + row * 7
            model.text(4, y, f"{index + 1} ").text(15, y, labels[index])
            if index == choice:
                model.invert(2, y - 1, 91, 7)
        # Free dictionary bytes change with the injected fixture. Check all
        # other pixels, including blank gaps, scrolling rows and footer.
        model.compare(scenario, name, ((0, 0, 96, 9), (0, 17, 96, 47)))

    for name in ("files", "files-still-empty"):
        (ScreenModel().frame("FILES", "^v ENT view CLR back")
         .text(2, 11, "0 objects 16 free")
         .text(8, 27, "No saved objects").text(8, 38, "Save with FS-PUT")
         .compare(empty, name))

    objects = (("T", "NOTE"), ("T", "EMPTY"), ("F", "SOURCE"),
               ("B", "BINARY"), ("T", "FOUR"), ("F", "FIVE"), ("T", "S.X."))
    for name, selected in (("files", 0), ("file-list-last", 6), ("file-list-first", 0)):
        model = ScreenModel().frame("FILES", "^v ENT view CLR back")
        model.text(2, 11, "7 objects 9 free")
        first = selected // 4 * 4
        for row, (kind, label) in enumerate(objects[first:first + 4]):
            y = 20 + row * 8
            model.text(4, y, kind).text(14, y, label)
            if first + row == selected:
                model.invert(2, y - 1, 91, 7)
        model.text(2, 50, f"{selected + 1} / 7 ")
        model.compare(populated, name)

    content = bytes(65 + index % 26 for index in range(200))
    for name, offset in (("file-preview", 0), ("file-next", 96),
                         ("file-last", 192), ("file-first", 0)):
        model = ScreenModel().frame("NOTE", "<> page CLR back")
        for index, byte in enumerate(content[offset:offset + 96]):
            model.text(2 + index % 16 * 5, 12 + index // 16 * 7, chr(byte))
        model.compare(populated, name)
    (ScreenModel().frame("EMPTY", "<> page CLR back")
     .text(2, 12, "Empty object").compare(populated, "empty-preview"))

    for name, footer in (("source-loaded", "Loaded / CLR back"),
                         ("source-error", "Load error / CLR back")):
        ScreenModel().frame("SOURCE" if name == "source-loaded" else "FIVE", footer).compare(
            populated, name, ((0, 0, 96, 8), (0, 54, 96, 10)))

    calls = ("PAGE", "EMIT", "KEYC", "UNUSED", "YIELD", "FS-LIST", "FS-GET",
             "FS-PUT", "FS-DELETE", "FS-LOAD", "WITH-PAGE", "TASK-NEW",
             "TASK-PAUSE", "TASK-RUN", "TASK-STOP")
    for name, first in (("services", 0), ("services-next", 6),
                        ("services-last", 12), ("services-first", 0)):
        model = ScreenModel().frame("SYSTEM CALLS / V1", "<> browse CLR back")
        for row, call in enumerate(calls[first:first + 6]):
            y = 12 + row * 7
            model.text(3, y, f"{first + row} ").text(16, y, call)
        model.compare(populated, name)

    page_names = {0: "kernel", 2: "RAM init", 8: "objects"}
    for module, label in (("CORE", "core"), ("STORAGE", "storage"),
                          ("DESKTOP", "desktop"), ("WORKBENCH", "forth"),
                          ("TESTS", "tests")):
        page_names[kernel_constant(populated.rom, f"MODULE-{module}")] = label
    used = set(page_names) | set(range(56, 64))
    for name, selected in (("pages", 0), ("page-next", 1),
                           ("page-last", 63), ("page-first", 0)):
        model = ScreenModel().frame("MEMORY", "<> page ENT map/hex")
        for page in range(64):
            x, y = 3 + page % 8 * 6, 12 + page // 8 * 5
            width, height = (4, 3) if page in used else (1, 1)
            model.invert(x, y, width, height)
            if page == selected:
                model.invert(x - 1, y - 1, 6, 5)
        label = page_names.get(selected, "system" if selected >= 56 else "unused")
        model.text(54, 12, f"Page {selected} ").text(54, 22, label)
        model.compare(populated, name, ((0, 0, 96, 9), (0, 9, 52, 45),
                                       (52, 9, 44, 23), (0, 54, 96, 10)))

    for name, page in (("page-first-hex", 0), ("page-last-hex", 63)):
        model = ScreenModel().frame("MEMORY", "<> page ENT map/hex")
        data = populated.rom[page * 16384:page * 16384 + 24]
        for row in range(6):
            text = f"{row * 4:02X}: " + "".join(f"{byte:02X} " for byte in data[row * 4:row * 4 + 4])
            model.text(3, 12 + row * 7, text)
        model.compare(populated, name)

    # Scheduler attempts vary with emulator timing. Verify the stable task
    # names, selected-row inverse pixels, and user feedback separately.
    for name, selected, states, notice in (
            ("task-paused", 2, ("ready", "paused", "free", "free"), "Error 0 >new <stop"),
            ("task-full", 4, ("ready", "ready", "ready", "ready"), "Task table full"),
            ("tasks", 1, ("ready", "free", "free", "free"), "Error 65535 >new <stop")):
        model = ScreenModel().frame("TASKS / STEPS", "^v ENT run/pause CLR")
        for row, state in enumerate(states):
            y = 13 + row * 8
            model.text(4, y, f"{row + 1} ").text(15, y, state)
            if row + 1 == selected:
                model.invert(2, y - 1, 91, 7)
        model.text(2, 47, notice)
        model.compare(populated, name, ((0, 0, 96, 9), (0, 10, 65, 34), (0, 45, 96, 19)))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "src/zkeme80.rom")
    parser.add_argument("--emulator", default=os.environ.get("TILEM", "tilem2"))
    parser.add_argument("--display", default=os.environ.get("DISPLAY", ":99"))
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    output = (args.output or Path(tempfile.mkdtemp(prefix="zkeme80-desktop-"))).resolve()
    output.mkdir(parents=True, exist_ok=True)
    empty = empty_scenario(args, output)
    populated = populated_scenario(args, output)
    check_pixels(empty, populated)
    lifecycle_scenario(args, output)
    print("Desktop: empty/multiple files, preview bounds, source load/error, task lifecycle,")
    print("full task table, page 0/63 bounds, service first/final pages and home bounds passed")
    print("Bank mapping stayed restored at every scheduling point")
    print(f"Screenshots, pixel models and RAM snapshots: {output}")


if __name__ == "__main__":
    main()
