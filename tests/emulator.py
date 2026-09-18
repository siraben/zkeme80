"""Shared helpers for disposable TilEm ROM tests; no production ROM writes."""

import re

PAGE = 16384


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
    if len(source) >= PAGE or b"\0" in source:
        raise ValueError("injected source must fit one page and contain no NUL")
    rom[page * PAGE:(page + 1) * PAGE] = (source + b"\0").ljust(PAGE, b"\xff")


def inject(rom: bytes, source: str) -> bytes:
    image = bytearray(rom)
    workbench_page = kernel_constant(rom, "MODULE-WORKBENCH")
    scratch_page = next((page for page in range(1, 56)
                         if page != 8 and rom[page * PAGE:(page + 1) * PAGE] == b"\xff" * PAGE), None)
    if scratch_page is None:
        raise ValueError("no erased scratch source page below reserved pages 56..63")
    workbench = bytes(image[workbench_page * PAGE:(workbench_page + 1) * PAGE]).split(b"\0", 1)[0]
    marker = b"\nMENU-DEMO\n"
    if marker in workbench:
        before, after = workbench.rsplit(marker, 1)
    else:
        previous = re.search(rb"\n[0-9]+ LOAD-MODULE\n$", workbench)
        if previous is None:
            raise ValueError("resident workbench has no final startup action")
        before, after = workbench[:previous.start()], b""
    patch_source(image, workbench_page,
                 before + f"\n{scratch_page} LOAD-MODULE\n".encode("ascii") + after)
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
            if not 0x8400 <= pointer < 0xC000:
                raise AssertionError(f"{name} has invalid VARIABLE address {pointer:#x}")
            return word(pointer)
        header = word(header)
    raise AssertionError(f"{name} missing from dictionary: boot or test did not finish")
