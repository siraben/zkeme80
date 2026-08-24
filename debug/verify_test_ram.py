#!/usr/bin/env python3
"""Verify the terminal test-suite counters in a logical TilEm RAM dump."""

import argparse
import json
from pathlib import Path


RAM_BASE = 0x8000
RAM_LIMIT = 0xC000


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("ram", type=Path)
    parser.add_argument("labelmap", type=Path)
    parser.add_argument("--expected", type=int, default=288)
    parser.add_argument("--before", type=Path)
    parser.add_argument("--after", type=Path)
    args = parser.parse_args()

    memory = args.ram.read_bytes()
    labels = json.loads(args.labelmap.read_text())["labels"]
    latest_cell = next(item["addr"] for item in labels
                       if item["name"] == "var-latest")

    def u16(address: int) -> int:
        offset = address - RAM_BASE
        if offset < 0 or offset + 1 >= len(memory):
            raise ValueError(f"address outside logical RAM dump: {address:#06x}")
        return memory[offset] | memory[offset + 1] << 8

    wanted = {"TEST-COUNT", "SUCCESS-TEST-COUNT", "FAILED-COUNT"}
    values = {}
    saw_hold = False
    header = u16(latest_cell)
    for _ in range(1024):
        if not RAM_BASE <= header < RAM_LIMIT:
            break
        offset = header - RAM_BASE
        name_length = memory[offset + 2] & 0x1F
        name = memory[offset + 3:offset + 3 + name_length].decode("ascii")
        xt = header + 4 + name_length
        if name in wanted:
            values[name] = u16(xt + 3)  # DOVAR's data cell
        saw_hold |= name == "HOLD-RESULTS"
        header = u16(header)

    expected = {
        "TEST-COUNT": args.expected,
        "SUCCESS-TEST-COUNT": args.expected,
        "FAILED-COUNT": 0,
    }
    if values != expected or not saw_hold:
        print(f"FAIL: counters={values!r}, HOLD-RESULTS={saw_hold}")
        return 1

    if bool(args.before) != bool(args.after):
        raise ValueError("--before and --after must be supplied together")
    if args.before:
        before = args.before.read_bytes()
        after = args.after.read_bytes()

        def cell_bytes(dump: bytes, address: int) -> bytes:
            offset = address - RAM_BASE
            if offset < 0 or offset + 2 > len(dump):
                raise ValueError(
                    f"logical RAM dump does not contain cell at {address:#06x}"
                )
            return dump[offset : offset + 2]

        for name in ("var-dp", "var-latest"):
            address = next(item["addr"] for item in labels if item["name"] == name)
            if cell_bytes(before, address) != cell_bytes(after, address):
                print(f"FAIL: suite unload did not restore {name}")
                return 1

    unload = ", DP/LATEST restored" if args.before else ""
    print(
        f"PASS: {args.expected}/{args.expected}, 0 failures, "
        f"completion waiter present{unload}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
