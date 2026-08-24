#!/usr/bin/env python3
"""Verify cursor-editing RAM states captured by shell-editing.macro."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def read_u16(data: bytes, offset: int) -> int:
    return data[offset] | data[offset + 1] << 8


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--actual-dir", type=Path, default=Path("/tmp"))
    parser.add_argument(
        "--labelmap", type=Path, default=Path("src/zkeme80.ram-labelmap.json")
    )
    args = parser.parse_args()

    labelmap = json.loads(args.labelmap.read_text())
    labels = {entry["name"]: entry["addr"] for entry in labelmap["labels"]}
    ram_base = labelmap["ram_range"]["start"]

    def offset(name: str) -> int:
        return labels[name] - ram_base

    def forth_variables(data: bytes, wanted: set[str]) -> dict[str, int]:
        values: dict[str, int] = {}
        header = read_u16(data, offset("var-latest"))
        for _ in range(1024):
            if not ram_base <= header < 0xC000:
                break
            header_offset = header - ram_base
            name_length = data[header_offset + 2] & 0x1F
            name = data[
                header_offset + 3 : header_offset + 3 + name_length
            ].decode("ascii")
            if name in wanted:
                xt = header + 4 + name_length
                values[name] = read_u16(data, xt + 3 - ram_base)
            header = read_u16(data, header_offset)
        if set(values) != wanted:
            raise RuntimeError(f"missing transient variables: {wanted - set(values)}")
        return values

    cases = {
        "history-up": (b"DEPTH .", 7, 7, 121),
        "history-down": (b"DRAFT", 5, 5, 123),
        "overwrite": (b"STAR", 4, 3, 124),
        "delete": (b"STAR", 4, 2, 124),
        "rights": (b"STAR", 4, 4, 124),
        "shift-left": (b"STAR", 4, 3, 124),
        "shift-right": (b"STAR", 4, 4, 124),
        "boundaries": (b"STAR", 4, 4, 124),
        "wrap": (b"A" * 22, 22, 22, 106),
        "wrap-left": (b"A" * 22, 22, 21, 106),
        "wrap-backspace": (b"A" * 21, 21, 20, 107),
        "full": (b"A" * 128, 128, 128, 0),
        "full-recall": (b"A" * 128, 128, 128, 0),
    }
    canary_dump = (args.actual_dir / "zkeme80-shell-edit-canary.ram").read_bytes()
    canary_address = offset("prompt-space-canary")
    if labels["prompt-space-canary"] != labels["prompt-space"] + 128:
        raise RuntimeError("prompt-space canary is not immediately after PBUF")
    if canary_dump[canary_address] != 7:
        raise RuntimeError(
            f"canary setup failed: expected 7, got {canary_dump[canary_address]}"
        )
    for name, (expected_text, length, edit_delta, expected_count) in cases.items():
        path = args.actual_dir / f"zkeme80-shell-edit-{name}.ram"
        data = path.read_bytes()
        start = read_u16(data, offset("expect-ptr-initial"))
        end = read_u16(data, offset("expect-ptr"))
        edit = read_u16(data, offset("expect-edit-ptr"))
        count = read_u16(data, offset("expect-count"))
        text_offset = start - ram_base
        text = data[text_offset : text_offset + len(expected_text)]

        expected = (
            expected_text,
            start + length,
            start + edit_delta,
            expected_count,
        )
        actual = (text, end, edit, count)
        if actual != expected:
            raise RuntimeError(f"{name}: expected {expected!r}, got {actual!r}")
        if name == "full" and data[text_offset + length] != 7:
            raise RuntimeError(
                f"full: addr+u canary changed to {data[text_offset + length]}"
            )
        value = expected_text.decode("ascii")
        print(f"{name}: {value}, edit={edit_delta}, remaining={count}")

    before = (args.actual_dir / "zkeme80-shell-edit-before-compile-error.ram").read_bytes()
    after = (args.actual_dir / "zkeme80-shell-edit-after-compile-error.ram").read_bytes()
    for variable in ("var-dp", "var-latest"):
        position = offset(variable)
        if before[position : position + 2] != after[position : position + 2]:
            raise RuntimeError(f"compile rollback changed {variable}")
    if read_u16(after, offset("var-state")) != 0:
        raise RuntimeError("compile error left STATE nonzero")
    if read_u16(after, offset("loop-compile-depth")) != 0:
        raise RuntimeError("compile error left loop context active")
    print("compile error: DP/LATEST rolled back, STATE and loop context cleared")

    before = (args.actual_dir / "zkeme80-shell-edit-before-multiline.ram").read_bytes()
    middle = (args.actual_dir / "zkeme80-shell-edit-mid-multiline.ram").read_bytes()
    after = (args.actual_dir / "zkeme80-shell-edit-after-multiline.ram").read_bytes()
    before_dp = read_u16(before, offset("var-dp"))
    before_latest = read_u16(before, offset("var-latest"))
    if read_u16(middle, offset("var-state")) != 1:
        raise RuntimeError("multiline definition did not preserve compile STATE")
    if read_u16(middle, offset("compile-start-dp")) != before_dp:
        raise RuntimeError("multiline definition lost its DP transaction marker")
    if read_u16(middle, offset("compile-start-latest")) != before_latest:
        raise RuntimeError("multiline definition lost its LATEST transaction marker")
    if read_u16(after, offset("var-state")) != 0:
        raise RuntimeError("multiline semicolon did not restore interpretation STATE")
    if read_u16(after, offset("compile-start-dp")) != 0:
        raise RuntimeError("multiline semicolon did not commit its transaction")
    if read_u16(after, offset("var-dp")) == before_dp:
        raise RuntimeError("multiline definition did not advance DP")
    if read_u16(after, offset("var-latest")) == before_latest:
        raise RuntimeError("multiline definition did not update LATEST")
    print("multiline definition: STATE/context preserved across REFILL and committed")

    history_names = {
        "SHELL-HISTORY-HEAD",
        "SHELL-HISTORY-COUNT",
        "SHELL-HISTORY-DATA-HEAD",
        "SHELL-HISTORY-DATA-USED",
    }
    history_cases = {
        "512": {
            "SHELL-HISTORY-HEAD": 0,
            "SHELL-HISTORY-COUNT": 512,
            "SHELL-HISTORY-DATA-HEAD": 512,
            "SHELL-HISTORY-DATA-USED": 512,
        },
        "4096": {
            "SHELL-HISTORY-HEAD": 32,
            "SHELL-HISTORY-COUNT": 32,
            "SHELL-HISTORY-DATA-HEAD": 0,
            "SHELL-HISTORY-DATA-USED": 4096,
        },
        "evict": {
            "SHELL-HISTORY-HEAD": 33,
            "SHELL-HISTORY-COUNT": 32,
            "SHELL-HISTORY-DATA-HEAD": 1,
            "SHELL-HISTORY-DATA-USED": 3969,
        },
    }
    for case, expected in history_cases.items():
        data = (
            args.actual_dir / f"zkeme80-shell-edit-history-{case}.ram"
        ).read_bytes()
        actual = forth_variables(data, history_names)
        if actual != expected:
            raise RuntimeError(f"history {case}: expected {expected}, got {actual}")
        print(f"history {case}: {actual}")

    before_shell = (
        args.actual_dir / "zkeme80-shell-edit-before-shell.ram"
    ).read_bytes()
    shell_loaded = (
        args.actual_dir / "zkeme80-shell-edit-shell-loaded.ram"
    ).read_bytes()
    after_shell = (args.actual_dir / "zkeme80-shell-edit-after-shell.ram").read_bytes()
    for variable in (
        "var-dp",
        "var-latest",
        "var-current-input-device",
        "var-current-error-handler",
        "var-current-eof-handler",
        "var-edit-history",
    ):
        position = offset(variable)
        if before_shell[position : position + 2] != after_shell[position : position + 2]:
            raise RuntimeError(f"shell exit did not restore {variable}")
    loaded_dp = read_u16(shell_loaded, offset("var-dp"))
    if not loaded_dp < 0xC000:
        raise RuntimeError(f"shell transient dictionary crossed RAM limit: {loaded_dp:#06x}")
    print(
        "shell lifecycle: DP/LATEST and input/editor vectors restored; "
        f"{0xC000 - loaded_dp} transient bytes remain"
    )


if __name__ == "__main__":
    main()
