#!/usr/bin/env python3
"""Measure a small, Z80-friendly LZSS format on zkeme80 Forth sources.

The format groups eight tokens under one LSB-first flag byte.  A set flag is
followed by one literal byte.  A clear flag is followed by a two-byte match:
12 bits of (distance - 1), then four bits of (length - 3).  Matches are 3..18
bytes and the maximum window is 4096 bytes.
"""

from __future__ import annotations

import argparse
from collections import defaultdict, deque
from pathlib import Path


DEFAULT_SOURCES = (
    "src/boot.fs",
    "src/bootstrap-flash1.fs",
    "src/bootstrap-flash2.fs",
    "src/bootstrap-flash3.fs",
    "src/bootstrap-flash4.fs",
    "src/bootstrap-flash5.fs",
)


def compress(data: bytes, window: int = 4096) -> bytes:
    if not 1 <= window <= 4096:
        raise ValueError("window must be between 1 and 4096 bytes")

    positions: dict[bytes, deque[int]] = defaultdict(deque)
    output = bytearray()
    cursor = 0

    def remember(pos: int) -> None:
        if pos + 3 <= len(data):
            positions[data[pos : pos + 3]].append(pos)

    while cursor < len(data):
        flag_at = len(output)
        output.append(0)
        flags = 0

        for bit in range(8):
            if cursor >= len(data):
                break

            best_pos = -1
            best_len = 0
            key = data[cursor : cursor + 3]
            candidates = positions.get(key)
            if len(key) == 3 and candidates:
                cutoff = cursor - window
                while candidates and candidates[0] < cutoff:
                    candidates.popleft()
                for pos in reversed(candidates):
                    length = 3
                    limit = min(18, len(data) - cursor)
                    while length < limit and data[pos + length] == data[cursor + length]:
                        length += 1
                    if length > best_len:
                        best_pos, best_len = pos, length
                        if length == limit:
                            break

            if best_len >= 3:
                distance = cursor - best_pos
                encoded_distance = distance - 1
                output.append(encoded_distance & 0xFF)
                output.append(((best_len - 3) << 4) | (encoded_distance >> 8))
                for pos in range(cursor, cursor + best_len):
                    remember(pos)
                cursor += best_len
            else:
                flags |= 1 << bit
                output.append(data[cursor])
                remember(cursor)
                cursor += 1

        output[flag_at] = flags

    return bytes(output)


def decompress(data: bytes) -> bytes:
    output = bytearray()
    cursor = 0
    while cursor < len(data):
        flags = data[cursor]
        cursor += 1
        for bit in range(8):
            if cursor >= len(data):
                break
            if flags & (1 << bit):
                output.append(data[cursor])
                cursor += 1
            else:
                if cursor + 1 >= len(data):
                    raise ValueError("truncated LZSS match")
                low, high = data[cursor], data[cursor + 1]
                cursor += 2
                distance = (((high & 0x0F) << 8) | low) + 1
                length = (high >> 4) + 3
                if distance > len(output):
                    raise ValueError("LZSS match precedes output")
                for _ in range(length):
                    output.append(output[-distance])
    return bytes(output)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", default=DEFAULT_SOURCES)
    parser.add_argument("--window", type=int, default=4096)
    args = parser.parse_args()

    total_raw = 0
    total_packed = 0
    for name in args.paths:
        raw = Path(name).read_bytes()
        packed = compress(raw, args.window)
        if decompress(packed) != raw:
            raise RuntimeError(f"round-trip failed for {name}")
        total_raw += len(raw)
        total_packed += len(packed)
        print(f"{name:32} {len(raw):6} -> {len(packed):6}  {len(packed) / len(raw):6.1%}")

    print(f"{'total (separate streams)':32} {total_raw:6} -> {total_packed:6}  "
          f"{total_packed / total_raw:6.1%}")


if __name__ == "__main__":
    main()
