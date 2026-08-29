#!/usr/bin/env python3
"""Pack a deterministic post-bootstrap Forth dictionary image.

The input is a RAM dump captured after zkeme80 has finished interpreting its
text bootstrap.  The output contains the live H0..DP dictionary slice, enough
metadata to reject an incompatible kernel, checksums for both the header and
payload, and the captured menu entry CFA.  The opt-in precompiled ROM embeds
this image on flash page 6; the plain-text bootstrap remains the fallback.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import struct
import sys
import tempfile
import zlib
from dataclasses import asdict, dataclass, replace
from pathlib import Path


MAGIC = b"ZKEME80B"
VERSION = 2
MODEL = b"TI84"
RAM_PAGE_SIZE = 0x4000
RAM_SELECTOR = 0x81
CPU_PAGE_BASE = 0x8000
CPU_PAGE_LIMIT = 0xC000
FLAG_DICTIONARY_SLICE = 1 << 0
HEADER = struct.Struct("<8sHHI4sB3x8I32s32s32sII")
ENTRY_WORD = b"MENU-DEMO"

DEFAULT_SOURCES = (
    "src/boot.fs",
    "src/bootstrap-flash1.fs",
    "src/bootstrap-flash2.fs",
    "src/bootstrap-flash3.fs",
    "src/bootstrap-flash4.fs",
    "src/bootstrap-flash5.fs",
)


@dataclass(frozen=True)
class ImageHeader:
    flags: int
    ram_selector: int
    load_address: int
    payload_size: int
    dp_address: int
    dp_value: int
    latest_address: int
    latest_value: int
    h0_value: int
    payload_crc32: int
    kernel_sha256: bytes
    sources_sha256: bytes
    payload_sha256: bytes
    entry_cfa: int
    header_crc32: int = 0

    def encode(self, *, zero_checksum: bool = False) -> bytes:
        return HEADER.pack(
            MAGIC,
            VERSION,
            HEADER.size,
            self.flags,
            MODEL,
            self.ram_selector,
            self.load_address,
            self.payload_size,
            self.dp_address,
            self.dp_value,
            self.latest_address,
            self.latest_value,
            self.h0_value,
            self.payload_crc32,
            self.kernel_sha256,
            self.sources_sha256,
            self.payload_sha256,
            0 if zero_checksum else self.header_crc32,
            self.entry_cfa,
        )

    def with_checksum(self) -> "ImageHeader":
        checksum = zlib.crc32(self.encode(zero_checksum=True)) & 0xFFFFFFFF
        values = asdict(self)
        values["header_crc32"] = checksum
        return ImageHeader(**values)


def sha256(data: bytes) -> bytes:
    return hashlib.sha256(data).digest()


def source_digest(paths: list[Path]) -> bytes:
    """Hash ordered, length-delimited contents without host-dependent paths."""
    digest = hashlib.sha256(b"zkeme80 bootstrap sources v1\0")
    digest.update(struct.pack("<I", len(paths)))
    for path in paths:
        data = path.read_bytes()
        digest.update(struct.pack("<Q", len(data)))
        digest.update(data)
    return digest.digest()


def load_labels(path: Path) -> dict[str, int]:
    document = json.loads(path.read_text(encoding="utf-8"))
    return {entry["name"]: entry["addr"] for entry in document["labels"]}


def read_u16(data: bytes, offset: int, name: str) -> int:
    if not 0 <= offset <= len(data) - 2:
        raise ValueError(f"{name} lies outside the selected RAM page")
    return int.from_bytes(data[offset : offset + 2], "little")


def find_word_cfa(page: bytes, latest: int, h0: int, dp: int, name: bytes) -> int:
    """Find NAME in the captured linked dictionary and return its CFA."""
    nfa = latest
    visited: set[int] = set()
    while h0 <= nfa < dp:
        if nfa in visited:
            raise ValueError("captured dictionary link cycle")
        visited.add(nfa)
        offset = nfa - CPU_PAGE_BASE
        if offset < 0 or offset + 3 > len(page):
            raise ValueError("captured dictionary header lies outside RAM page")
        link = read_u16(page, offset, "dictionary link")
        length = page[offset + 2] & 0x1F
        name_start = offset + 3
        name_end = name_start + length
        if name_end >= len(page) or page[name_end] != 0:
            raise ValueError("captured dictionary name is not NUL-terminated")
        if nfa + 4 + length >= dp:
            raise ValueError("captured dictionary name exceeds DP")
        if page[name_start:name_end] == name:
            return nfa + 4 + length
        if link >= nfa:
            raise ValueError("captured dictionary links are not descending")
        nfa = link
    raise ValueError(f"captured dictionary is missing entry word {name!r}")


def select_ram_page(dump: bytes, layout: str, selector: int) -> bytes:
    if layout == "logical":
        if len(dump) < RAM_PAGE_SIZE:
            raise ValueError("logical RAM dump is shorter than 16 KiB")
        return dump[:RAM_PAGE_SIZE]
    if layout == "physical":
        page = selector & 7
        start = page * RAM_PAGE_SIZE
        end = start + RAM_PAGE_SIZE
        if len(dump) < end:
            raise ValueError(
                f"physical RAM dump is too short for selector 0x{selector:02x}"
            )
        return dump[start:end]
    raise ValueError(f"unknown RAM dump layout {layout!r}")


def make_image(
    dump: bytes,
    layout: str,
    labels: dict[str, int],
    rom: bytes,
    sources: list[Path],
) -> bytes:
    for name in ("var-state", "var-dp", "var-latest", "dp-start"):
        if name not in labels:
            raise ValueError(f"labelmap is missing required label {name!r}")
    if len(rom) < RAM_PAGE_SIZE:
        raise ValueError("ROM is shorter than page 0")

    page = select_ram_page(dump, layout, RAM_SELECTOR)
    state_address = labels["var-state"]
    dp_address = labels["var-dp"]
    latest_address = labels["var-latest"]
    h0_value = labels["dp-start"]
    for name, address in (
        ("var-state", state_address),
        ("var-dp", dp_address),
        ("var-latest", latest_address),
        ("dp-start", h0_value),
    ):
        if not CPU_PAGE_BASE <= address < CPU_PAGE_LIMIT:
            raise ValueError(f"{name} is outside RAM page 0x81: 0x{address:04x}")

    dp_value = read_u16(page, dp_address - CPU_PAGE_BASE, "DP")
    latest_value = read_u16(page, latest_address - CPU_PAGE_BASE, "LATEST")
    state_value = read_u16(page, state_address - CPU_PAGE_BASE, "STATE")
    if state_value != 0:
        raise ValueError(
            f"capture is not in interpretation state (STATE={state_value})"
        )
    if not h0_value <= dp_value <= CPU_PAGE_LIMIT:
        raise ValueError(f"captured DP is outside the dictionary: 0x{dp_value:04x}")
    if not h0_value <= latest_value < dp_value:
        raise ValueError(
            f"captured LATEST is outside the compiled dictionary: 0x{latest_value:04x}"
        )

    load_address = h0_value
    start = load_address - CPU_PAGE_BASE
    end = dp_value - CPU_PAGE_BASE
    payload = page[start:end]
    entry_cfa = find_word_cfa(page, latest_value, h0_value, dp_value, ENTRY_WORD)
    header = ImageHeader(
        flags=FLAG_DICTIONARY_SLICE,
        ram_selector=RAM_SELECTOR,
        load_address=load_address,
        payload_size=len(payload),
        dp_address=dp_address,
        dp_value=dp_value,
        latest_address=latest_address,
        latest_value=latest_value,
        h0_value=h0_value,
        payload_crc32=zlib.crc32(payload) & 0xFFFFFFFF,
        kernel_sha256=sha256(rom[:RAM_PAGE_SIZE]),
        sources_sha256=source_digest(sources),
        payload_sha256=sha256(payload),
        entry_cfa=entry_cfa,
    ).with_checksum()
    return header.encode() + payload


def decode_image(image: bytes) -> tuple[ImageHeader, bytes]:
    if len(image) < HEADER.size:
        raise ValueError("truncated bootstrap-image header")
    fields = HEADER.unpack_from(image)
    magic, version, header_size, flags, model, selector = fields[:6]
    if magic != MAGIC:
        raise ValueError(f"bad bootstrap-image magic {magic!r}")
    if version != VERSION:
        raise ValueError(f"unsupported bootstrap-image version {version}")
    if header_size != HEADER.size:
        raise ValueError(f"unsupported bootstrap-image header size {header_size}")
    if model != MODEL:
        raise ValueError(f"bootstrap image targets an unsupported model {model!r}")
    if flags != FLAG_DICTIONARY_SLICE:
        raise ValueError(f"unsupported bootstrap-image flags 0x{flags:08x}")
    if selector != RAM_SELECTOR:
        raise ValueError(f"unsupported bootstrap-image RAM selector 0x{selector:02x}")
    ints = fields[6:14]
    hashes = fields[14:17]
    header_crc32, entry_cfa = fields[17:19]
    header = ImageHeader(
        flags, selector, *ints, *hashes, entry_cfa, header_crc32
    )
    expected_header_crc = zlib.crc32(header.encode(zero_checksum=True)) & 0xFFFFFFFF
    if header_crc32 != expected_header_crc:
        raise ValueError("bootstrap-image header checksum mismatch")
    payload = image[HEADER.size :]
    if len(payload) != header.payload_size:
        raise ValueError(
            f"payload length mismatch: header={header.payload_size}, file={len(payload)}"
        )
    if zlib.crc32(payload) & 0xFFFFFFFF != header.payload_crc32:
        raise ValueError("bootstrap-image payload CRC32 mismatch")
    if sha256(payload) != header.payload_sha256:
        raise ValueError("bootstrap-image payload SHA-256 mismatch")
    if header.load_address != header.h0_value:
        raise ValueError("bootstrap-image payload does not begin at H0")
    if header.load_address + header.payload_size != header.dp_value:
        raise ValueError("bootstrap-image payload does not end at captured DP")
    if not (
        CPU_PAGE_BASE
        <= header.h0_value
        <= header.latest_value
        < header.dp_value
        <= CPU_PAGE_LIMIT
    ):
        raise ValueError("bootstrap-image dictionary pointers are inconsistent")
    if not header.h0_value <= header.entry_cfa < header.dp_value:
        raise ValueError("bootstrap-image entry CFA is outside the dictionary")
    for name, address in (
        ("DP", header.dp_address),
        ("LATEST", header.latest_address),
    ):
        if not CPU_PAGE_BASE <= address <= CPU_PAGE_LIMIT - 2:
            raise ValueError(f"bootstrap-image {name} variable is outside the RAM window")
    return header, payload


def verify_bindings(header: ImageHeader, rom: bytes, sources: list[Path]) -> None:
    if len(rom) < RAM_PAGE_SIZE:
        raise ValueError("ROM is shorter than page 0")
    if sha256(rom[:RAM_PAGE_SIZE]) != header.kernel_sha256:
        raise ValueError("bootstrap image was captured from a different page-0 kernel")
    if source_digest(sources) != header.sources_sha256:
        raise ValueError("bootstrap source digest does not match the image")


def verify_runtime_dump(
    header: ImageHeader, payload: bytes, dump: bytes, layout: str
) -> None:
    """Verify that a running calculator installed IMAGE into RAM."""
    page = select_ram_page(dump, layout, header.ram_selector)
    start = header.load_address - CPU_PAGE_BASE
    end = start + header.payload_size
    if page[start:end] != payload:
        raise ValueError("runtime RAM dictionary does not match the image payload")
    if read_u16(page, header.dp_address - CPU_PAGE_BASE, "runtime DP") != header.dp_value:
        raise ValueError("runtime DP does not match the image header")
    if (
        read_u16(page, header.latest_address - CPU_PAGE_BASE, "runtime LATEST")
        != header.latest_value
    ):
        raise ValueError("runtime LATEST does not match the image header")


def metadata(header: ImageHeader) -> dict[str, int | str]:
    return {
        "format": MAGIC.decode("ascii"),
        "version": VERSION,
        "header_size": HEADER.size,
        "flags": header.flags,
        "model": MODEL.decode("ascii"),
        "ram_selector": f"0x{header.ram_selector:02x}",
        "load_address": f"0x{header.load_address:04x}",
        "payload_size": header.payload_size,
        "dp_address": f"0x{header.dp_address:04x}",
        "dp_value": f"0x{header.dp_value:04x}",
        "latest_address": f"0x{header.latest_address:04x}",
        "latest_value": f"0x{header.latest_value:04x}",
        "h0_value": f"0x{header.h0_value:04x}",
        "payload_crc32": f"{header.payload_crc32:08x}",
        "header_crc32": f"{header.header_crc32:08x}",
        "kernel_sha256": header.kernel_sha256.hex(),
        "sources_sha256": header.sources_sha256.hex(),
        "payload_sha256": header.payload_sha256.hex(),
        "entry_cfa": f"0x{header.entry_cfa:04x}",
    }


def default_sources(values: list[str] | None) -> list[Path]:
    return [Path(value) for value in (values or DEFAULT_SOURCES)]


def self_test() -> None:
    labels = {
        "var-state": 0x8402,
        "var-latest": 0x8404,
        "var-dp": 0x8406,
        "dp-start": 0x864B,
    }
    ram = bytearray(8 * RAM_PAGE_SIZE)
    page = memoryview(ram)[RAM_PAGE_SIZE : 2 * RAM_PAGE_SIZE]
    page[0x402:0x404] = (0).to_bytes(2, "little")
    menu_nfa = 0x8655
    menu_name = ENTRY_WORD
    menu_cfa = menu_nfa + 4 + len(menu_name)
    dp_value = menu_cfa + 4
    page[0x404:0x406] = menu_nfa.to_bytes(2, "little")
    page[0x406:0x408] = dp_value.to_bytes(2, "little")
    base = labels["dp-start"] - CPU_PAGE_BASE
    page[base : base + 2] = (0x1234).to_bytes(2, "little")
    page[base + 2] = 4
    page[base + 3 : base + 7] = b"BASE"
    menu = menu_nfa - CPU_PAGE_BASE
    page[menu : menu + 2] = labels["dp-start"].to_bytes(2, "little")
    page[menu + 2] = len(menu_name)
    page[menu + 3 : menu + 3 + len(menu_name)] = menu_name
    page[menu + 3 + len(menu_name)] = 0
    page[menu_cfa - CPU_PAGE_BASE : dp_value - CPU_PAGE_BASE] = b"\xcd\x00\x00\x00"
    rom = bytes(range(256)) * (RAM_PAGE_SIZE // 256)
    with tempfile.TemporaryDirectory() as directory:
        source = Path(directory) / "bootstrap.fs"
        source.write_bytes(b": TEST 1 ;\n")
        first = make_image(bytes(ram), "physical", labels, rom, [source])
        second = make_image(bytes(ram), "physical", labels, rom, [source])
        if first != second:
            raise AssertionError("bootstrap-image packing is not deterministic")
        header, payload = decode_image(first)
        verify_bindings(header, rom, [source])
        verify_runtime_dump(header, payload, bytes(ram), "physical")
        if payload != bytes(page[base : dp_value - CPU_PAGE_BASE]):
            raise AssertionError("bootstrap-image payload slice is incorrect")
        if header.entry_cfa != menu_cfa:
            raise AssertionError("bootstrap-image entry CFA is incorrect")
        bad_flags = replace(header, flags=2, header_crc32=0).with_checksum()
        try:
            decode_image(bad_flags.encode() + payload)
        except ValueError as error:
            if "flags" not in str(error):
                raise
        else:
            raise AssertionError("unknown image flags were not rejected")
        damaged = bytearray(first)
        damaged[-1] ^= 1
        try:
            decode_image(bytes(damaged))
        except ValueError as error:
            if "payload CRC32" not in str(error):
                raise
        else:
            raise AssertionError("payload corruption was not rejected")
    print("bootstrap image self-test passed")


def add_sources_argument(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--source",
        action="append",
        help="bootstrap source in digest order (repeat; defaults to all stages)",
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    pack = subparsers.add_parser("pack", help="package a post-bootstrap RAM dump")
    pack.add_argument("ram_dump", type=Path)
    pack.add_argument("output", type=Path)
    pack.add_argument(
        "--labelmap", type=Path, default=Path("zkeme80.ram-labelmap.json")
    )
    pack.add_argument("--rom", type=Path, default=Path("zkeme80.rom"))
    pack.add_argument(
        "--dump-layout", choices=("physical", "logical"), default="physical"
    )
    add_sources_argument(pack)

    verify = subparsers.add_parser("verify", help="verify checksums and build bindings")
    verify.add_argument("image", type=Path)
    verify.add_argument("--rom", type=Path, default=Path("zkeme80.rom"))
    add_sources_argument(verify)

    inspect = subparsers.add_parser("inspect", help="print verified image metadata")
    inspect.add_argument("image", type=Path)

    runtime = subparsers.add_parser(
        "verify-ram", help="verify an image installed in a runtime RAM dump"
    )
    runtime.add_argument("image", type=Path)
    runtime.add_argument("ram_dump", type=Path)
    runtime.add_argument(
        "--dump-layout", choices=("physical", "logical"), default="physical"
    )

    subparsers.add_parser(
        "self-test", help="exercise deterministic packing and corruption checks"
    )
    args = parser.parse_args(argv)

    try:
        if args.command == "pack":
            sources = default_sources(args.source)
            image = make_image(
                args.ram_dump.read_bytes(),
                args.dump_layout,
                load_labels(args.labelmap),
                args.rom.read_bytes(),
                sources,
            )
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_bytes(image)
            header, _ = decode_image(image)
            print(json.dumps(metadata(header), indent=2, sort_keys=True))
        elif args.command == "verify":
            header, _ = decode_image(args.image.read_bytes())
            verify_bindings(header, args.rom.read_bytes(), default_sources(args.source))
            print(f"verified {args.image}")
        elif args.command == "inspect":
            header, _ = decode_image(args.image.read_bytes())
            print(json.dumps(metadata(header), indent=2, sort_keys=True))
        elif args.command == "verify-ram":
            header, payload = decode_image(args.image.read_bytes())
            verify_runtime_dump(
                header, payload, args.ram_dump.read_bytes(), args.dump_layout
            )
            print(f"verified runtime image in {args.ram_dump}")
        else:
            self_test()
    except (OSError, KeyError, ValueError, json.JSONDecodeError) as error:
        parser.exit(1, f"bootstrap image: {error}\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
