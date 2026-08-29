#!/usr/bin/env python3
"""Fold a TilEm headless TLMT v2 instruction trace back onto zkeme80 symbols.

The trace is produced by the tilem-headless fork:

    tilem2 --headless --rom src/zkeme80.rom --model ti84p --normal-speed \
        --reset --macro scenario.macro --trace /tmp/zk80.trace --trace-range all

This analyzer replays the memory mapper (ports 4/5/6/7) from the OUT
instructions in the trace, so every sampled PC resolves to the right
coordinate space, then attributes execution counts to:

  * page-0 flash labels (the whole kernel + every Forth code word,
    image offset == CPU address there)
  * exact static RAM labels in the $8000-$BFFF window when selector
    0x81 maps physical RAM page 1 (runtime-compiled words are not in
    the static labelmap)

Usage:
    analyze_forth_trace.py TRACE LABELMAP [--top N] [--forth-only]
                           [--timeline] [--csv OUT] [--min-count N]
"""
import argparse
import csv
import json
import mmap
import struct
import sys
from bisect import bisect_right
from collections import Counter

HEADER_LEN = 20
REC_INSTR_LEN = 48
REC_MEMWRITE_LEN = 6
REC_KEY_LEN = 9

REG_ORDER = ("af", "bc", "de", "hl", "ix", "iy", "sp", "pc", "ir", "wz",
             "wz2", "af2", "bc2", "de2", "hl2")


class SymbolResolver:
    """Resolve an address to its nearest preceding symbol."""

    def __init__(self, entries, end=None):
        by_addr = {}
        for addr, name in entries:
            by_addr.setdefault(addr, name)
        self.names = by_addr
        self.addresses = sorted(by_addr)
        self.end = end

    def resolve(self, addr):
        if self.end is not None and addr >= self.end:
            return None
        index = bisect_right(self.addresses, addr) - 1
        if index < 0:
            return None
        return self.names[self.addresses[index]]


class ExactSymbolResolver:
    """Resolve only exact addresses, for non-code RAM symbols."""

    def __init__(self, entries):
        self.names = {}
        for addr, name in entries:
            self.names.setdefault(addr, name)

    def resolve(self, addr):
        return self.names.get(addr)


def _make_page_resolvers(entries, resolver=SymbolResolver):
    pages = {}
    for page, addr, name in entries:
        pages.setdefault(page, []).append((addr, name))
    return {page: resolver(symbols) for page, symbols in pages.items()}


def load_symbols(labelmap_path):
    """Build resolution tables from the labelmap JSON.

    Returns per-page flash and RAM resolvers plus a resolver for the
    contiguous page-0 Forth dictionary.
    """
    with open(labelmap_path) as f:
        data = json.load(f)
    flash_entries = []
    ram_entries = []
    for lab in data.get("labels", []):
        addr, name = lab["addr"], lab["name"]
        if lab.get("region") == "ram":
            # The ROM's $8000-$BFFF image initializes physical RAM
            # page 1.  Store offsets within a 16 KiB window.
            ram_entries.append((1, addr & 0x3FFF, name))
        else:
            flash_entries.append((addr >> 14, addr & 0x3FFF, name))

    forth_entries = []
    for w in data.get("forth_words", []):
        if w.get("region") == "flash" and w["addr"] < 0x4000:
            forth_entries.append((w["addr"], w["name"]))

    # Stop ownership at the first non-dictionary label following the
    # last word; otherwise the last word would absorb interrupt and
    # driver code placed later on page 0.
    forth_end = None
    if forth_entries:
        last_word = max(addr for addr, _ in forth_entries)
        following = [addr for page, addr, _ in flash_entries
                     if page == 0 and addr > last_word]
        if following:
            forth_end = min(following)

    return (_make_page_resolvers(flash_entries),
            _make_page_resolvers(ram_entries, ExactSymbolResolver),
            SymbolResolver(forth_entries, end=forth_end))


class Mapper:
    """Replay zkeme80's memory mapping from OUT instructions.

    Page 0 is hardwired at $0000-$3FFF.  In mode 0 (port 4 bit 0
    clear), ports 6, 7, and 5 select the other three windows.  In mode
    1, port 6 selects an even/odd pair for the middle two windows and
    port 7 selects the top window.  Ports 6/7 use bit 7 to select RAM;
    port 5 always selects RAM.
    """

    def __init__(self):
        self.port4 = 0x07
        self.port5 = 0x00
        self.port6 = 0x3F
        self.port7 = 0x3F

    def observe(self, opcode, regs):
        """Update mapper state from one executed instruction."""
        wz = regs["wz"]
        if (opcode & 0xFF) == 0xD3:  # OUT (n),A: WZ=(A<<8)|n
            self.out(wz & 0xFF, (wz >> 8) & 0xFF)
        elif (opcode & 0xFFFF) == 0xED79:  # OUT (C),A
            self.out(regs["bc"] & 0xFF, (regs["af"] >> 8) & 0xFF)

    def out(self, port, value):
        if port == 4:
            self.port4 = value
        elif port == 5:
            self.port5 = value
        elif port == 6:
            self.port6 = value
        elif port == 7:
            self.port7 = value

    def space(self, pc):
        if pc < 0x4000:
            return ("flash", 0)
        page_a = self._decode_ab(self.port6)
        page_b = self._decode_ab(self.port7)
        page_c = ("ram", self.port5 & 0x07)
        window = pc >> 14
        if self.port4 & 1:
            if page_a[0] == "ram":
                pair_base = ("ram", page_a[1] & ~1)
            else:
                pair_base = ("flash", page_a[1] & ~1)
            if window == 1:
                return pair_base
            if window == 2:
                return (pair_base[0], pair_base[1] | 1)
            return page_b
        if window == 1:
            return page_a
        if window == 2:
            return page_b
        return page_c

    @staticmethod
    def _decode_ab(sel):
        if sel & 0x80:
            return ("ram", sel & 0x07)
        return ("flash", sel & 0x3F)


def iter_records(buf, want_instr=True):
    """Yield ('instr', rec_dict) / ('memwrite', addr, val) / ('key', ...)."""
    if len(buf) < HEADER_LEN:
        raise ValueError("truncated TLMT header")
    off = HEADER_LEN + struct.unpack_from("<I", buf, 16)[0]
    n = len(buf)
    if off > n:
        raise ValueError("truncated TLMT initial-memory snapshot")
    while off < n:
        rtype = buf[off]
        if rtype == 0x01:
            if off + REC_INSTR_LEN > n:
                break
            pc, opcode, clock = struct.unpack_from("<III", buf, off + 1)
            regs = {}
            for i, name in enumerate(REG_ORDER):
                regs[name] = struct.unpack_from("<H", buf, off + 13 + 2 * i)[0]
            iff1, iff2, im, r7, halted = buf[off + 43:off + 48]
            if want_instr:
                yield ("instr", {
                    "pc": pc & 0xFFFF, "opcode": opcode, "clock": clock,
                    "regs": regs, "iff1": iff1, "iff2": iff2, "im": im,
                    "halted": halted,
                })
            off += REC_INSTR_LEN
        elif rtype == 0x02:
            if off + REC_MEMWRITE_LEN > n:
                break
            addr = struct.unpack_from("<I", buf, off + 1)[0]
            yield ("memwrite", addr, buf[off + 5])
            off += REC_MEMWRITE_LEN
        elif rtype == 0x03:
            if off + REC_KEY_LEN > n:
                break
            pressed, key = buf[off + 1], buf[off + 2]
            clock = struct.unpack_from("<I", buf, off + 3)[0]
            kpc = struct.unpack_from("<H", buf, off + 7)[0]
            yield ("key", pressed, key, clock, kpc)
            off += REC_KEY_LEN
        else:
            raise ValueError(
                "corrupt trace: unknown record type 0x%02x at offset %d"
                % (rtype, off))


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("trace", help="TLMT v2 trace file")
    ap.add_argument("labelmap", help="zkeme80.ram-labelmap.json")
    ap.add_argument("--top", type=int, default=40)
    ap.add_argument("--forth-only", action="store_true",
                    help="report only Forth dictionary words")
    ap.add_argument("--timeline", action="store_true",
                    help="print symbol transitions as they occur")
    ap.add_argument("--csv", metavar="OUT",
                    help="also write per-symbol counts to OUT")
    ap.add_argument("--min-count", type=int, default=1)
    args = ap.parse_args(argv)

    flash, ram, forth = load_symbols(args.labelmap)
    mapper = Mapper()

    counts = Counter()
    word_counts = Counter()
    unresolved = Counter()
    transitions = [] if args.timeline else None
    last_sym = None
    ninstr = 0

    with open(args.trace, "rb") as trace_file:
        if trace_file.seek(0, 2) < HEADER_LEN:
            sys.exit("truncated TLMT header")
        trace_file.seek(0)
        # Keep multi-gigabyte traces out of the Python heap while retaining
        # the indexed buffer interface used by iter_records.
        with mmap.mmap(trace_file.fileno(), 0, access=mmap.ACCESS_READ) as buf:
            magic = buf[:4]
            if magic != b"TLMT":
                sys.exit("not a TLMT trace (magic %r)" % magic)
            version = struct.unpack_from("<H", buf, 4)[0]
            if version != 2:
                sys.exit("unsupported TLMT version %d" % version)

            for rec in iter_records(buf):
                if rec[0] != "instr":
                    continue
                r = rec[1]
                ninstr += 1
                space, page = mapper.space(r["pc"])
                local_addr = r["pc"] & 0x3FFF
                sym = None
                if space == "flash":
                    resolver = flash.get(page)
                    if resolver is not None:
                        sym = resolver.resolve(local_addr)
                    w = forth.resolve(local_addr) if page == 0 else None
                    if w:
                        word_counts[w] += 1
                else:
                    resolver = ram.get(page)
                    if resolver is not None:
                        sym = resolver.resolve(local_addr)
                if sym is None:
                    unresolved["%s%02x:%04x" % (space, page, r["pc"])] += 1
                    sym = "%s%02x:%04x" % (space, page, r["pc"])
                counts[sym] += 1
                if transitions is not None and sym != last_sym:
                    transitions.append((r["clock"], sym))
                    last_sym = sym
                # The trace callback runs after each instruction.  Apply an
                # OUT only after attributing it under the old mapping.
                mapper.observe(r["opcode"], r["regs"])

    print("instructions: %d" % ninstr)
    print("unique symbols hit: %d" % len(counts))
    print()
    if args.forth_only:
        rows = [(w, c) for w, c in word_counts.most_common()
                if c >= args.min_count]
        print("top Forth words:")
    else:
        rows = [(s, c) for s, c in counts.most_common()
                if c >= args.min_count]
        print("top symbols:")
    for sym, c in rows[:args.top]:
        print("  %8d  %s" % (c, sym))
    print()
    print("top unresolved (banked flash / foreign RAM) PCs:")
    for s, c in unresolved.most_common(5):
        print("  %8d  %s" % (c, s))

    if args.csv:
        word_name_set = set(forth.names.values())
        csv_counts = word_counts if args.forth_only else counts
        with open(args.csv, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(("symbol", "count", "is_forth_word"))
            for sym, c in csv_counts.most_common():
                writer.writerow((sym, c,
                                 "y" if (args.forth_only or
                                         sym in word_name_set) else "n"))
        print("wrote %s" % args.csv)

    if transitions is not None:
        print()
        print("timeline (%d transitions):" % len(transitions))
        for clock, sym in transitions[:200]:
            print("  %10d  %s" % (clock, sym))


if __name__ == "__main__":
    main()
