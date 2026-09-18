#!/usr/bin/env python3
"""Verify every printable host key through TilEm into zkeme80's ROM tables."""

from __future__ import annotations

import argparse
from configparser import ConfigParser
import json
from pathlib import Path
import re


TI84P_KEYS = tuple((
    "Down Left Right Up - - - - Enter Add Sub Mul Div Power Clear - "
    "Chs 3 6 9 RParen Tan Vars - DecPnt 2 5 8 LParen Cos Prgm Stat "
    "0 1 4 7 Comma Sin Apps Graphvar On Store Ln Log Square Recip Math Alpha "
    "Graph Trace Zoom Window YEqu 2nd Mode Del - - - - - - - -"
).split())

HOST_KEYS = (
    "space exclam quotedbl numbersign dollar percent ampersand apostrophe "
    "parenleft parenright asterisk plus comma minus period slash "
    "0 1 2 3 4 5 6 7 8 9 colon semicolon less equal greater question at "
    "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z "
    "bracketleft backslash bracketright asciicircum underscore grave "
    "a b c d e f g h i j k l m n o p q r s t u v w x y z "
    "braceleft bar braceright asciitilde"
).split()

CONTROL_KEYS = {
    "Return": 10,
    "BackSpace": 8,
    "Delete": 8,
    "Left": 2,
    "Right": 6,
    "Up": 16,
    "Down": 14,
}

ALIAS_KEYS = {
    **{f"KP_{digit}": ord(digit) for digit in "0123456789"},
    "KP_Add": ord("+"),
    "KP_Subtract": ord("-"),
    "KP_Multiply": ord("*"),
    "KP_Divide": ord("/"),
    "KP_Decimal": ord("."),
    "KP_Up": 16,
    "KP_Down": 14,
    "KP_Left": 2,
    "KP_Right": 6,
    "KP_Delete": 8,
    "KP_Enter": 10,
    "ISO_Enter": 10,
}

# These bindings intentionally affect calculator/editor state without yielding
# a cooked input byte.  Keeping them explicit makes every configured host key
# part of the audit instead of silently ignoring extras.
NONCHAR_KEYS = {
    "Tab": "2nd",
    "KP_Tab": "2nd",
    "ISO_Left_Tab": "2nd",
    "Escape": "Clear",
    "F12": "On",
}


def parse_tilem_keys(path: Path) -> tuple[str, ...]:
    source = path.read_text(encoding="utf-8")
    match = re.search(r"xp_keynames\[64\]\s*=\s*\{(.*?)\};", source, re.S)
    if not match:
        raise ValueError(f"could not find xp_keynames[64] in {path}")
    return tuple(
        token[1:-1] if token.startswith('"') else "-"
        for token in re.findall(r'"[^"]*"|\b0\b', match.group(1))
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--bindings", type=Path, default=Path("debug/tilem-keybindings.ini")
    )
    parser.add_argument("--rom", type=Path, default=Path("src/zkeme80.rom"))
    parser.add_argument(
        "--labelmap", type=Path, default=Path("src/zkeme80.ram-labelmap.json")
    )
    parser.add_argument(
        "--tilem-source",
        type=Path,
        help="optional TilEm checkout; verifies emu/xp/xp_subcore.c too",
    )
    args = parser.parse_args()

    if args.tilem_source:
        source_keys = parse_tilem_keys(args.tilem_source / "emu/xp/xp_subcore.c")
        if source_keys != TI84P_KEYS:
            raise RuntimeError("TilEm TI-84+ key-name table differs from verifier")

    config = ConfigParser(interpolation=None)
    config.optionxform = str
    with args.bindings.open(encoding="utf-8") as stream:
        config.read_file(stream)
    bindings = config["ti84p"]

    labels = {
        item["name"]: item["addr"]
        for item in json.loads(args.labelmap.read_text(encoding="utf-8"))["labels"]
    }
    rom = args.rom.read_bytes()
    alpha = rom[labels["char-lookup-table"] : labels["char-lookup-table"] + 128]
    numeric = rom[
        labels["numeric-char-lookup-table"]
        : labels["numeric-char-lookup-table"] + 128
    ]

    scanner = b"".join(
        rom[labels[f"gs-keygroup{group}"] : labels[f"gs-keygroup{group}"] + 8]
        for group in range(1, 8)
    )
    raw_by_name = {
        name: scanner[index]
        for index, name in enumerate(TI84P_KEYS[: len(scanner)])
        if name != "-" and scanner[index]
    }

    def translate(binding: str) -> int:
        shifted = False
        result = 0
        for name in (part.strip() for part in binding.split(",")):
            if name not in raw_by_name:
                raise RuntimeError(f"unknown TilEm TI-84+ key token: {name!r}")
            raw = raw_by_name[name]
            if raw == 54:
                shifted = True
                continue
            result = (numeric if shifted else alpha)[raw]
            shifted = False
        return result

    errors = []
    for codepoint, host_key in zip(range(32, 127), HOST_KEYS, strict=True):
        binding = bindings.get(host_key)
        actual = translate(binding) if binding else None
        expected = ord(chr(codepoint).upper()) if 97 <= codepoint <= 122 else codepoint
        if actual != expected:
            errors.append(
                f"{chr(codepoint)!r} ({host_key}): binding={binding!r}, "
                f"result={actual!r}, expected={expected}"
            )

    for host_key, expected in CONTROL_KEYS.items():
        binding = bindings.get(host_key)
        actual = translate(binding) if binding else None
        if actual != expected:
            errors.append(
                f"control {host_key}: binding={binding!r}, result={actual!r}, expected={expected}"
            )

    for host_key, expected in ALIAS_KEYS.items():
        binding = bindings.get(host_key)
        actual = translate(binding) if binding else None
        if actual != expected:
            errors.append(
                f"alias {host_key}: binding={binding!r}, result={actual!r}, expected={expected}"
            )

    for host_key, expected_binding in NONCHAR_KEYS.items():
        binding = bindings.get(host_key)
        actual = translate(binding) if binding and host_key != "F12" else 0
        if binding != expected_binding or actual != 0:
            errors.append(
                f"non-character {host_key}: binding={binding!r}, result={actual!r}, "
                f"expected binding={expected_binding!r}, result=0"
            )

    audited = set(HOST_KEYS) | set(CONTROL_KEYS) | set(ALIAS_KEYS) | set(NONCHAR_KEYS)
    extras = set(bindings) - audited
    missing = audited - set(bindings)
    if extras:
        errors.append(f"configured host keys lack an audit contract: {sorted(extras)!r}")
    if missing:
        errors.append(f"audited host keys lack bindings: {sorted(missing)!r}")

    if errors:
        raise RuntimeError("keybinding verification failed:\n  " + "\n  ".join(errors))

    source_note = " and local TilEm source" if args.tilem_source else ""
    print(
        f"PASS: all {len(audited)} host bindings (95 printable, editor/keypad, "
        f"and intentional non-character controls) map through configured "
        f"TI key names and zkeme80's assembled tables{source_note} "
        f"(letters normalize to uppercase)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
