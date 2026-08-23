#!/usr/bin/env python3
"""Render expected shell states and compare them with emulator screenshots."""

from __future__ import annotations

import argparse
import re
import subprocess
import tempfile
from pathlib import Path


WIDTH = 96
HEIGHT = 64
CURSOR_ROWS = (0b11110000, 0b10010000, 0b10010000, 0b10010000, 0b11110000)


def load_font(path: Path) -> list[tuple[int, tuple[int, ...]]]:
    lines = path.read_text(encoding="utf-8").splitlines()
    start = next(i for i, line in enumerate(lines) if "(label kernel-font)" in line)
    width_pattern = re.compile(r"^\s*\(db \((\d+)\)\)")
    row_pattern = re.compile(r"^\s*\(db \(#b([01]{8})\s*\)\)")
    glyphs: list[tuple[int, tuple[int, ...]]] = []
    i = start + 1

    while i < len(lines):
        width_match = width_pattern.match(lines[i])
        if not width_match:
            i += 1
            continue

        rows: list[int] = []
        j = i + 1
        while j < len(lines) and len(rows) < 5:
            row_match = row_pattern.match(lines[j])
            if row_match:
                rows.append(int(row_match.group(1), 2))
            elif width_pattern.match(lines[j]):
                break
            j += 1

        if len(rows) != 5:
            raise ValueError(f"incomplete glyph at {path}:{i + 1}")
        glyphs.append((int(width_match.group(1)), tuple(rows)))
        i = j

    if len(glyphs) < 96:
        raise ValueError(f"expected at least 96 glyphs, found {len(glyphs)}")
    return glyphs


def draw_text(
    pixels: list[list[bool]],
    font: list[tuple[int, tuple[int, ...]]],
    x: int,
    y: int,
    text: str,
) -> int:
    for character in text:
        codepoint = ord(character)
        if not 32 <= codepoint <= 255:
            raise ValueError(f"unsupported character {character!r}")
        width, rows = font[codepoint - 32]
        for dy, row in enumerate(rows):
            for dx in range(8):
                if row & (0x80 >> dx) and x + dx < WIDTH and y + dy < HEIGHT:
                    pixels[y + dy][x + dx] = True
        x += width
    return x


def draw_cursor(pixels: list[list[bool]], x: int, y: int) -> None:
    for dy, row in enumerate(CURSOR_ROWS):
        for dx in range(8):
            if row & (0x80 >> dx):
                pixels[y + dy][x + dx] = True


def render_state(
    font: list[tuple[int, tuple[int, ...]]],
    input_text: str,
    result: bool,
    after_help: bool,
) -> list[list[bool]]:
    pixels = [[False] * WIDTH for _ in range(HEIGHT)]

    if after_help:
        lines = (
            "Forth shell",
            "ENTER runs input.",
            "2ND shifts one key.",
            "DEL backspaces.",
            "BYE opens the menu.",
            " ok",
        )
        for row, line in enumerate(lines):
            draw_text(pixels, font, 0, row * 6, line)
        input_row = 36
    else:
        draw_text(pixels, font, 0, 0, "zkeme80 Forth")
        draw_text(pixels, font, 0, 6, "Type HELP for help.")
        draw_text(pixels, font, 0, 18, " ok")
        input_row = 24

    cursor_x = draw_text(pixels, font, 0, input_row, f"> {input_text}")

    if result:
        draw_text(pixels, font, 0, input_row + 6, "* ok")
        input_row += 12
        cursor_x = draw_text(pixels, font, 0, input_row, "> ")

    draw_cursor(pixels, cursor_x, input_row)
    return pixels


def render_error_state(
    font: list[tuple[int, tuple[int, ...]]],
) -> list[list[bool]]:
    pixels = [[False] * WIDTH for _ in range(HEIGHT)]
    lines = (
        "2ND shifts one key.",
        "DEL backspaces.",
        "BYE opens the menu.",
        " ok",
        "> STAR",
        "* ok",
        "> NOPE",
        "Error 1 at NOPE",
    )
    for row, line in enumerate(lines):
        draw_text(pixels, font, 0, row * 6, line)
    cursor_x = draw_text(pixels, font, 0, 48, "> ")
    draw_cursor(pixels, cursor_x, 48)
    return pixels


def write_pbm(path: Path, pixels: list[list[bool]]) -> None:
    packed = bytearray()
    for row in pixels:
        for offset in range(0, WIDTH, 8):
            byte = 0
            for bit, value in enumerate(row[offset : offset + 8]):
                if value:
                    byte |= 0x80 >> bit
            packed.append(byte)
    path.write_bytes(f"P4\n{WIDTH} {HEIGHT}\n".encode("ascii") + packed)


def compare_state(expected: Path, actual: Path, thresholded: Path) -> None:
    subprocess.run(
        [
            "magick",
            str(actual),
            "-colorspace",
            "Gray",
            "-threshold",
            "50%",
            str(thresholded),
        ],
        check=True,
    )
    result = subprocess.run(
        ["magick", "compare", "-metric", "AE", str(expected), str(thresholded), "null:"],
        text=True,
        capture_output=True,
    )
    difference = result.stderr.strip()
    absolute_error = difference.split()[0] if difference else ""
    if result.returncode != 0 or absolute_error != "0":
        raise RuntimeError(
            f"{actual.name} differs from the model by {difference} pixels"
        )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--font", type=Path, default=Path("src/font.scm"))
    parser.add_argument("--actual-dir", type=Path, default=Path("/tmp"))
    parser.add_argument("--expected-dir", type=Path)
    args = parser.parse_args()

    font = load_font(args.font)
    states = {
        "start": render_state(font, "", False, False),
        "replace": render_state(font, "S", False, True),
        "input": render_state(font, "STAR", False, True),
        "result": render_state(font, "STAR", True, True),
        "error": render_error_state(font),
    }

    with tempfile.TemporaryDirectory(prefix="zkeme80-shell-model-") as temporary:
        output_dir = args.expected_dir or Path(temporary)
        output_dir.mkdir(parents=True, exist_ok=True)
        for name, pixels in states.items():
            expected = output_dir / f"{name}.pbm"
            thresholded = output_dir / f"{name}-actual.pbm"
            actual = args.actual_dir / f"zkeme80-shell-model-{name}.png"
            write_pbm(expected, pixels)
            compare_state(expected, actual, thresholded)
            print(f"{name}: exact 96x64 pixel match")


if __name__ == "__main__":
    main()
