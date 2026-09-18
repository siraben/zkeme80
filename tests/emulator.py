"""Shared helpers for disposable TilEm ROM tests; no production ROM writes."""

import re

PAGE = 16384
ROM_SIZE = 64 * PAGE


def kernel_constant(rom: bytes, name: str) -> int:
    """Decode the kernel's PUSH BC / LD BC,immediate constant/variable stub."""
    encoded = name.encode("ascii")
    pattern = bytes([len(encoded)]) + encoded + b"\0\xc5\x01"
    offset = rom[:PAGE].find(pattern)
    if offset < 0:
        raise AssertionError(f"kernel definition {name} not found")
    offset += len(pattern)
    return int.from_bytes(rom[offset:offset + 2], "little")


def patch_source(rom: bytearray, page: int, source: bytes) -> None:
    if len(rom) != ROM_SIZE or not 1 <= page < 56 or page in (2, 8):
        raise ValueError("source requires a 1MiB ROM and an unreserved source page")
    if len(source) >= PAGE or b"\0" in source:
        raise ValueError("injected source must fit one page and contain no NUL")
    rom[page * PAGE:(page + 1) * PAGE] = (source + b"\0").ljust(PAGE, b"\xff")


def inject(rom: bytes, source: str) -> bytes:
    """Run a fixture after every resident, preserving appended modules."""
    if len(rom) != ROM_SIZE:
        raise ValueError("expected a complete 1MiB TI-84+ ROM")
    image = bytearray(rom)
    workbench_page = kernel_constant(rom, "MODULE-WORKBENCH")
    scratch_page = next((page for page in range(1, 56)
                         if page not in (2, 8)
                         and rom[page * PAGE:(page + 1) * PAGE] == b"\xff" * PAGE), None)
    if scratch_page is None:
        raise ValueError("no erased scratch source page below reserved pages 56..63")
    page = workbench_page
    visited = set()
    while True:
        if page in visited or not 1 <= page < 56 or page in (2, 8):
            raise ValueError("invalid or cyclic resident startup chain")
        visited.add(page)
        payload = bytes(image[page * PAGE:(page + 1) * PAGE])
        if b"\0" not in payload:
            raise ValueError("resident source has no terminating EOF")
        resident = payload.split(b"\0", 1)[0]
        marker = b"\nMENU-DEMO\n"
        if resident.endswith(marker):
            patch_source(image, page,
                         resident[:-len(marker)]
                         + f"\n{scratch_page} LOAD-MODULE\n".encode("ascii"))
            break
        following = re.search(rb"\n([0-9]+) LOAD-MODULE\n$", resident)
        if following is None:
            raise ValueError("resident has no final startup action")
        page = int(following.group(1))
    patch_source(image, scratch_page, source.encode("ascii"))
    return bytes(image)


def variable(ram: bytes, rom: bytes, name: str) -> int:
    def read(address, count):
        if address >= 0x8000:
            return ram[address - 0x8000:address - 0x8000 + count]
        return rom[address:address + count]

    def word(address):
        return int.from_bytes(read(address, 2), "little")

    header = word(kernel_constant(rom, "LATEST"))
    visited = set()
    while header and header not in visited:
        visited.add(header)
        length = read(header + 2, 1)[0] & 31
        if read(header + 3, length) == name.encode("ascii"):
            cfa = header + 4 + length
            pointer = word(cfa + 5)
            if not 0x8400 <= pointer < kernel_constant(rom, "DP-LIMIT"):
                raise AssertionError(f"{name} has invalid VARIABLE address {pointer:#x}")
            return word(pointer)
        header = word(header)
    raise AssertionError(f"{name} missing from dictionary: boot or test did not finish")
