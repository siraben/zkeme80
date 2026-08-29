#!/usr/bin/env python3
"""Build and inspect byte-exact, Forth-aware bootstrap stream archives.

This is deliberately an offline format experiment.  It does not change the
interpreter: decoding an archive reconstructs the original source bytes, while
the record tags leave room for a future token-aware input device.
"""

from __future__ import annotations

import argparse
from collections import Counter
from dataclasses import dataclass
from enum import IntEnum
from pathlib import Path
import struct
import sys
import zlib

try:
    from .analyze_forth_compression import compress as lzss_compress
    from .analyze_forth_compression import decompress as lzss_decompress
except ImportError:  # Direct execution from tools/.
    from analyze_forth_compression import compress as lzss_compress
    from analyze_forth_compression import decompress as lzss_decompress


DEFAULT_SOURCES = (
    "src/boot.fs",
    "src/bootstrap-flash1.fs",
    "src/bootstrap-flash2.fs",
    "src/bootstrap-flash3.fs",
    "src/bootstrap-flash4.fs",
    "src/bootstrap-flash5.fs",
)

MAGIC = b"ZKBS"
VERSION = 1
CODEC_TOKENS = 0
CODEC_LZSS = 1


class Kind(IntEnum):
    END = 0
    WORD = 1
    WORD_REF = 2
    WHITESPACE = 3
    LINE_COMMENT = 4
    PAREN_COMMENT = 5
    QUOTED = 6


@dataclass(frozen=True)
class Token:
    kind: Kind
    data: bytes


@dataclass(frozen=True)
class Stage:
    name: str
    data: bytes


@dataclass(frozen=True)
class DecodedArchive:
    codec: int
    window: int
    dictionary: tuple[bytes, ...]
    stages: tuple[Stage, ...]


@dataclass(frozen=True)
class StageSizes:
    name: str
    raw: int
    tokens: int
    lzss: int


# These are exactly the delimiters consumed by WORD in src/forth.scm.
FORTH_WHITESPACE = frozenset(b" \t\n")


def _is_space(value: int) -> bool:
    return value in FORTH_WHITESPACE


def lex_forth(data: bytes) -> list[Token]:
    """Split Forth source into semantically tagged, byte-exact records."""

    result: list[Token] = []
    cursor = 0
    while cursor < len(data):
        start = cursor
        if _is_space(data[cursor]):
            cursor += 1
            while cursor < len(data) and _is_space(data[cursor]):
                cursor += 1
            result.append(Token(Kind.WHITESPACE, data[start:cursor]))
            continue

        cursor += 1
        while cursor < len(data) and not _is_space(data[cursor]):
            cursor += 1
        word = data[start:cursor]

        if word == b"\\":
            while cursor < len(data) and data[cursor] != 10:
                cursor += 1
            result.append(Token(Kind.LINE_COMMENT, data[start:cursor]))
        elif word == b"(":
            close = data.find(b")", cursor)
            cursor = len(data) if close < 0 else close + 1
            result.append(Token(Kind.PAREN_COMMENT, data[start:cursor]))
        elif word in (b'S"', b'."'):
            close = data.find(b'"', cursor)
            cursor = len(data) if close < 0 else close + 1
            result.append(Token(Kind.QUOTED, data[start:cursor]))
        else:
            result.append(Token(Kind.WORD, word))

    if b"".join(token.data for token in result) != data:
        raise AssertionError("lexer did not preserve input")
    return result


def reconstruct_tokens(tokens: list[Token] | tuple[Token, ...]) -> bytes:
    return b"".join(token.data for token in tokens)


def encode_uleb(value: int) -> bytes:
    if value < 0:
        raise ValueError("ULEB128 cannot encode a negative value")
    output = bytearray()
    while True:
        byte = value & 0x7F
        value >>= 7
        output.append(byte | (0x80 if value else 0))
        if not value:
            return bytes(output)


class _Reader:
    def __init__(self, data: bytes):
        self.data = data
        self.cursor = 0

    def byte(self) -> int:
        if self.cursor >= len(self.data):
            raise ValueError("truncated bootstrap stream")
        value = self.data[self.cursor]
        self.cursor += 1
        return value

    def take(self, length: int) -> bytes:
        end = self.cursor + length
        if end > len(self.data):
            raise ValueError("truncated bootstrap stream")
        value = self.data[self.cursor:end]
        self.cursor = end
        return value

    def uleb(self) -> int:
        value = 0
        for shift in range(0, 64, 7):
            byte = self.byte()
            value |= (byte & 0x7F) << shift
            if not byte & 0x80:
                if encode_uleb(value)[-1] != byte or len(encode_uleb(value)) != shift // 7 + 1:
                    raise ValueError("non-canonical ULEB128")
                return value
        raise ValueError("ULEB128 value is too large")

    def done(self) -> bool:
        return self.cursor == len(self.data)


def build_dictionary(token_lists: list[list[Token]]) -> tuple[bytes, ...]:
    """Choose profitable repeated words in a stable, content-only order."""

    counts = Counter(
        token.data
        for tokens in token_lists
        for token in tokens
        if token.kind == Kind.WORD
    )
    dictionary: list[bytes] = []
    for word in sorted(word for word, count in counts.items() if count >= 2):
        index = len(dictionary)
        literal_size = 1 + len(encode_uleb(len(word))) + len(word)
        reference_size = 1 + len(encode_uleb(index))
        entry_size = len(encode_uleb(len(word))) + len(word)
        if counts[word] * (literal_size - reference_size) > entry_size:
            dictionary.append(word)
    return tuple(dictionary)


def encode_token_stream(tokens: list[Token], dictionary: tuple[bytes, ...]) -> bytes:
    indices = {word: index for index, word in enumerate(dictionary)}
    output = bytearray()
    for token in tokens:
        if token.kind == Kind.WORD and token.data in indices:
            output.append(Kind.WORD_REF)
            output += encode_uleb(indices[token.data])
        else:
            if token.kind in (Kind.END, Kind.WORD_REF):
                raise ValueError("reserved token kind in input")
            output.append(token.kind)
            output += encode_uleb(len(token.data))
            output += token.data
    output.append(Kind.END)
    return bytes(output)


def decode_token_stream(
    data: bytes, dictionary: tuple[bytes, ...], expected_count: int | None = None
) -> list[Token]:
    reader = _Reader(data)
    result: list[Token] = []
    while True:
        try:
            kind = Kind(reader.byte())
        except ValueError as error:
            if str(error).startswith("truncated"):
                raise
            raise ValueError("unknown bootstrap token kind") from error
        if kind == Kind.END:
            break
        if kind == Kind.WORD_REF:
            index = reader.uleb()
            if index >= len(dictionary):
                raise ValueError("bootstrap word reference is out of range")
            result.append(Token(Kind.WORD, dictionary[index]))
            continue
        if kind not in (
            Kind.WORD,
            Kind.WHITESPACE,
            Kind.LINE_COMMENT,
            Kind.PAREN_COMMENT,
            Kind.QUOTED,
        ):
            raise ValueError("invalid inline bootstrap token kind")
        result.append(Token(kind, reader.take(reader.uleb())))
    if not reader.done():
        raise ValueError("trailing bytes after bootstrap token stream")
    if expected_count is not None and len(result) != expected_count:
        raise ValueError("bootstrap token count does not match header")
    return result


def _stage_name_bytes(name: str) -> bytes:
    encoded = name.encode("utf-8")
    if not encoded or b"/" in encoded or b"\\" in encoded:
        raise ValueError("stage names must be non-empty basenames")
    return encoded


def build_archive(stages: list[Stage], codec: int = CODEC_LZSS, window: int = 4096) -> bytes:
    if codec not in (CODEC_TOKENS, CODEC_LZSS):
        raise ValueError("unknown bootstrap stream codec")
    if codec == CODEC_LZSS and not 1 <= window <= 4096:
        raise ValueError("window must be between 1 and 4096 bytes")
    if codec == CODEC_TOKENS:
        window = 0

    names = [_stage_name_bytes(stage.name) for stage in stages]
    if len(set(names)) != len(names):
        raise ValueError("stage names must be unique")
    token_lists = [lex_forth(stage.data) for stage in stages]
    dictionary = build_dictionary(token_lists)

    output = bytearray(MAGIC)
    output += bytes((VERSION, codec))
    output += struct.pack("<H", window)
    output += encode_uleb(len(dictionary))
    for word in dictionary:
        output += encode_uleb(len(word)) + word
    output += encode_uleb(len(stages))

    for stage, name, tokens in zip(stages, names, token_lists):
        token_stream = encode_token_stream(tokens, dictionary)
        stored = lzss_compress(token_stream, window) if codec == CODEC_LZSS else token_stream
        output += encode_uleb(len(name)) + name
        output += encode_uleb(len(stage.data))
        output += encode_uleb(len(tokens))
        output += encode_uleb(len(token_stream))
        output += encode_uleb(len(stored))
        output += struct.pack("<I", zlib.crc32(stage.data))
        output += stored

    archive = bytes(output)
    decoded = parse_archive(archive)
    if decoded.stages != tuple(stages):
        raise AssertionError("bootstrap archive did not round-trip")
    return archive


def parse_archive(data: bytes) -> DecodedArchive:
    reader = _Reader(data)
    if reader.take(len(MAGIC)) != MAGIC:
        raise ValueError("not a bootstrap stream archive")
    version = reader.byte()
    if version != VERSION:
        raise ValueError(f"unsupported bootstrap stream version {version}")
    codec = reader.byte()
    if codec not in (CODEC_TOKENS, CODEC_LZSS):
        raise ValueError("unknown bootstrap stream codec")
    window = struct.unpack("<H", reader.take(2))[0]
    if (codec == CODEC_TOKENS and window != 0) or (
        codec == CODEC_LZSS and not 1 <= window <= 4096
    ):
        raise ValueError("invalid bootstrap stream window")

    dictionary = tuple(reader.take(reader.uleb()) for _ in range(reader.uleb()))
    if len(set(dictionary)) != len(dictionary) or any(not word for word in dictionary):
        raise ValueError("invalid bootstrap stream dictionary")

    stages: list[Stage] = []
    names: set[str] = set()
    for _ in range(reader.uleb()):
        try:
            name = reader.take(reader.uleb()).decode("utf-8")
        except UnicodeDecodeError as error:
            raise ValueError("stage name is not UTF-8") from error
        _stage_name_bytes(name)
        if name in names:
            raise ValueError("duplicate bootstrap stage name")
        names.add(name)
        raw_length = reader.uleb()
        token_count = reader.uleb()
        token_length = reader.uleb()
        stored_length = reader.uleb()
        checksum = struct.unpack("<I", reader.take(4))[0]
        stored = reader.take(stored_length)
        token_stream = lzss_decompress(stored, window) if codec == CODEC_LZSS else stored
        if len(token_stream) != token_length:
            raise ValueError("bootstrap token stream length does not match header")
        raw = reconstruct_tokens(decode_token_stream(token_stream, dictionary, token_count))
        if len(raw) != raw_length:
            raise ValueError("bootstrap source length does not match header")
        if zlib.crc32(raw) != checksum:
            raise ValueError("bootstrap source checksum does not match header")
        stages.append(Stage(name, raw))
    if not reader.done():
        raise ValueError("trailing bytes after bootstrap archive")
    return DecodedArchive(codec, window, dictionary, tuple(stages))


def load_sources(paths: list[str] | tuple[str, ...]) -> list[Stage]:
    stages = [Stage(Path(path).name, Path(path).read_bytes()) for path in paths]
    if len({stage.name for stage in stages}) != len(stages):
        raise ValueError("input paths must have unique basenames")
    return stages


def measure(stages: list[Stage], window: int) -> tuple[list[StageSizes], int, int]:
    token_lists = [lex_forth(stage.data) for stage in stages]
    dictionary = build_dictionary(token_lists)
    rows = []
    for stage, tokens in zip(stages, token_lists):
        token_stream = encode_token_stream(tokens, dictionary)
        packed = lzss_compress(token_stream, window)
        decoded = decode_token_stream(lzss_decompress(packed, window), dictionary)
        if reconstruct_tokens(decoded) != stage.data:
            raise AssertionError("reported bootstrap stream did not round-trip")
        rows.append(StageSizes(stage.name, len(stage.data), len(token_stream), len(packed)))
    dictionary_size = sum(len(encode_uleb(len(word))) + len(word) for word in dictionary)
    return rows, len(dictionary), dictionary_size


def print_report(stages: list[Stage], window: int) -> None:
    rows, dictionary_count, dictionary_size = measure(stages, window)
    print(f"{'stage':28} {'source':>8} {'tokens':>8} {'lzss':>8} {'lzss/src':>9}")
    for row in rows:
        ratio = row.lzss / row.raw if row.raw else 0
        print(f"{row.name:28} {row.raw:8} {row.tokens:8} {row.lzss:8} {ratio:9.1%}")
    raw = sum(row.raw for row in rows)
    tokens = sum(row.tokens for row in rows)
    packed = sum(row.lzss for row in rows)
    ratio = packed / raw if raw else 0
    print(f"{'stage payload total':28} {raw:8} {tokens:8} {packed:8} {ratio:9.1%}")
    token_archive = len(build_archive(stages, CODEC_TOKENS))
    lzss_archive = len(build_archive(stages, CODEC_LZSS, window))
    print(f"dictionary: {dictionary_count} words, {dictionary_size} encoded bytes")
    print(f"archive total: tokens={token_archive} bytes, lzss={lzss_archive} bytes")


def _codec(value: str) -> int:
    return {"tokens": CODEC_TOKENS, "lzss": CODEC_LZSS}[value]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    report = subparsers.add_parser("report", help="show per-stage and archive sizes")
    report.add_argument("paths", nargs="*", default=DEFAULT_SOURCES)
    report.add_argument("--window", type=int, default=4096)

    build = subparsers.add_parser("build", help="build a byte-exact bootstrap archive")
    build.add_argument("paths", nargs="*", default=DEFAULT_SOURCES)
    build.add_argument("--output", required=True, type=Path)
    build.add_argument("--codec", choices=("tokens", "lzss"), default="lzss")
    build.add_argument("--window", type=int, default=4096)

    verify = subparsers.add_parser("verify", help="validate and optionally compare an archive")
    verify.add_argument("archive", type=Path)
    verify.add_argument("paths", nargs="*")

    args = parser.parse_args(argv)
    if args.command == "report":
        print_report(load_sources(args.paths), args.window)
        return 0
    if args.command == "build":
        stages = load_sources(args.paths)
        archive = build_archive(stages, _codec(args.codec), args.window)
        args.output.write_bytes(archive)
        print_report(stages, args.window)
        print(f"wrote {len(archive)} bytes to {args.output}")
        return 0

    archive = parse_archive(args.archive.read_bytes())
    if args.paths:
        expected = tuple(load_sources(args.paths))
        if archive.stages != expected:
            print("archive contents differ from supplied sources", file=sys.stderr)
            return 1
    print(
        f"verified {args.archive}: {len(archive.stages)} stages, "
        f"{sum(len(stage.data) for stage in archive.stages)} source bytes"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
