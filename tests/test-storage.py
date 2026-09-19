#!/usr/bin/env python3
"""Exercise the resident filesystem and a cold reboot using a disposable ROM.

Run after make build, inside nix develop if required by the emulator binary:
  python3 tests/test-storage.py --emulator /path/to/tilem2 --full
Only temporary ROM copies are changed. --output preserves screenshots/dumps.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

from emulator import inject, variable

ROOT = Path(__file__).resolve().parents[1]
PAGE = 16384


def run(emulator: str, rom: bytes, source: str, output: Path, name: str) -> tuple[bytes, bytes]:
    image = output / f"{name}.rom"
    image.write_bytes(inject(rom, source))
    ram = output / f"{name}.ram"
    flash = output / f"{name}-flash.rom"
    macro = output / f"{name}.macro"
    macro.write_text(
        f"key ON\nwait 15s\nscreenshot {output / (name + '.png')}\n"
        f"memdump {ram} ram-logical\nmemdump {flash} rom\n", encoding="ascii"
    )
    process = subprocess.run(
        [emulator, "--headless", "--full-speed", "--rom", str(image), "--state-file",
         str(output / f"{name}.sav"), "--reset", "--macro", str(macro)],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=90,
    )
    (output / f"{name}.log").write_text(process.stdout)
    if process.returncode or not ram.exists() or not flash.exists():
        raise AssertionError(f"emulator failed: {process.stdout}")
    memory, persisted = ram.read_bytes(), flash.read_bytes()
    if variable(memory, persisted, "FS-TEST-DONE") != 51966:
        raise AssertionError(f"{name}: test did not reach completion; inspect {output / (name + '.png')}")
    failures = variable(memory, persisted, "FS-TEST-FAILURES")
    checks = variable(memory, persisted, "FS-TEST-CHECKS")
    if failures:
        raise AssertionError(f"{name}: {failures}/{checks} Forth checks failed")
    if checks < 10:
        raise AssertionError(f"{name}: too few checks ({checks})")
    print(f"{name}: {checks} Forth checks passed")
    return memory, persisted


def decode(rom: bytes) -> dict[bytes, tuple[int, bytes]]:
    objects: dict[bytes, tuple[int, bytes]] = {}
    for slot in range(16):
        start = 8 * PAGE + slot * 1024
        record = rom[start : start + 1024]
        if record[0] != 90:
            continue
        version, kind, namelen = record[1:4]
        length = int.from_bytes(record[4:6], "little")
        assert version == 1 and kind <= 3 and 1 <= namelen <= 16 and length <= 992
        checksum = (sum(record[2:6]) + sum(record[8 : 32 + length])) & 65535
        assert checksum == int.from_bytes(record[6:8], "little"), f"checksum, slot {slot}"
        name = record[8 : 8 + namelen]
        if kind:
            objects[name] = kind, record[32 : 32 + length]
        else:
            objects.pop(name, None)
    return objects


DONE = '\nVARIABLE FS-TEST-DONE 51966 FS-TEST-DONE !\nPAGE FS-TEST-FAILURES @ . FS-TEST-CHECKS @ .\n: HOLD BEGIN AGAIN ; HOLD\n'


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, default=ROOT / "zkeme80.rom")
    parser.add_argument("--emulator", default=os.environ.get("TILEM", "tilem2"))
    parser.add_argument("--output", type=Path)
    parser.add_argument("--full", action="store_true", help="also exhaust the journal")
    args = parser.parse_args()
    if not shutil.which(args.emulator):
        parser.error(f"emulator not found: {args.emulator}")
    temporary = None if args.output else tempfile.TemporaryDirectory(prefix="zkeme80-storage-")
    output = (args.output or Path(temporary.name)).resolve()
    output.mkdir(parents=True, exist_ok=True)
    try:
        rom = args.rom.read_bytes()
        if rom[8 * PAGE : 9 * PAGE] != b"\xff" * PAGE:
            raise AssertionError("input ROM page 8 must be erased; pass a fresh build")
        source = (ROOT / "tests/os-storage.fs").read_text()
        if args.full:
            source += "\nFS-STORAGE-FULL-TEST\n"
        _, persisted = run(args.emulator, rom, source + DONE, output, "storage")
        expected = {b"TEST/B": (3, b"NEXT"), b"TEST/EMPTY": (1, b""),
                    b"TEST/SOURCE": (2, b"123 FS-TEST-VALUE !")}
        if args.full:
            expected[b"TEST/FULL"] = (1, b"")
        assert decode(persisted) == expected, "persisted journal differs from expected objects"
        assert persisted[8 * PAGE + 14336] == 255, "partial record must remain uncommitted"
        assert persisted[8 * PAGE + 14337] == 90, "interrupted record fixture missing"
        reboot = f"""
VARIABLE FS-TEST-FAILURES VARIABLE FS-TEST-CHECKS VARIABLE FS-TEST-VALUE
0 FS-TEST-FAILURES ! 0 FS-TEST-CHECKS ! 0 FS-TEST-VALUE !
: FS-ASSERT 1 FS-TEST-CHECKS +! 0= IF 1 FS-TEST-FAILURES +! THEN ;
FS-COUNT {len(expected)} = FS-ASSERT
FS-FREE {0 if args.full else 9} = FS-ASSERT
S" TEST/B" FS-GET 0= FS-ASSERT 3 = FS-ASSERT 4 = FS-ASSERT C@ 78 = FS-ASSERT
S" TEST/A" FS-GET 43 = FS-ASSERT DROP 2DROP
S" TEST/EMPTY" FS-GET 0= FS-ASSERT 1 = FS-ASSERT 0= FS-ASSERT DROP
S" TEST/SOURCE" FS-LOAD 0= FS-ASSERT
FS-TEST-VALUE @ 123 = FS-ASSERT
"""
        # Keep the pristine boot chain: persisted also contains the first
        # run's injected test program, which must not execute on cold boot.
        reboot_rom = bytearray(rom)
        reboot_rom[8 * PAGE : 9 * PAGE] = persisted[8 * PAGE : 9 * PAGE]
        run(args.emulator, bytes(reboot_rom), reboot + DONE, output, "reboot")
        # A committed payload with one changed bit must produce a checked
        # read error, while unrelated source and deletion records still work.
        corrupted = bytearray(reboot_rom)
        for slot in range(16):
            start = 8 * PAGE + slot * 1024
            if corrupted[start + 8 : start + 14] == b"TEST/B":
                corrupted[start + 32] ^= 1
                break
        else:
            raise AssertionError("missing checksum corruption fixture")
        checksum = f"""
VARIABLE FS-TEST-FAILURES VARIABLE FS-TEST-CHECKS VARIABLE FS-TEST-VALUE
0 FS-TEST-FAILURES ! 0 FS-TEST-CHECKS ! 0 FS-TEST-VALUE !
: FS-ASSERT 1 FS-TEST-CHECKS +! 0= IF 1 FS-TEST-FAILURES +! THEN ;
FS-COUNT {len(expected)} = FS-ASSERT
FS-FREE {0 if args.full else 9} = FS-ASSERT
S" TEST/B" FS-GET 44 = FS-ASSERT 0= FS-ASSERT 0= FS-ASSERT 0= FS-ASSERT
S" TEST/A" FS-GET 43 = FS-ASSERT 0= FS-ASSERT 0= FS-ASSERT 0= FS-ASSERT
S" TEST/SOURCE" FS-LOAD 0= FS-ASSERT
FS-TEST-VALUE @ 123 = FS-ASSERT
"""
        run(args.emulator, bytes(corrupted), checksum + DONE, output, "checksum")
        print("flash format, interrupted writes, cold boot, and corrupt-payload handling passed")
        if args.output:
            print(f"emulator artifacts: {output}")
    except Exception:
        # Keep failures inspectable even when no output directory was requested.
        if temporary:
            diagnostic = Path(tempfile.mkdtemp(prefix="zkeme80-storage-failed-"))
            shutil.copytree(output, diagnostic, dirs_exist_ok=True)
            print(f"failure artifacts: {diagnostic}")
        raise
    finally:
        if temporary:
            temporary.cleanup()


if __name__ == "__main__":
    main()
