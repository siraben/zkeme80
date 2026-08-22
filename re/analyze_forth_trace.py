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
  * RAM labels (the $8000-$BFFF window when RAM page 0x81 is banked,
    where words compiled from .fs sources live)

Usage:
    analyze_forth_trace.py TRACE LABELMAP [--top N] [--forth-only]
                           [--timeline] [--csv OUT] [--min-count N]
"""
import argparse
import json
import struct
import sys
from collections import Counter

HEADER_LEN = 20
REC_INSTR_LEN = 48
REC_MEMWRITE_LEN = 6
REC_KEY_LEN = 9

REG_ORDER = ("af", "bc", "de", "hl", "ix", "iy", "sp", "pc", "ir", "wz",
             "wz2", "af2", "bc2", "de2", "hl2")


def load_symbols(labelmap_path):
    """Build resolution tables from the labelmap JSON.

    Returns (flash0, ram, forth) where flash0 maps page-0 CPU addresses
    to names, ram maps RAM addresses to names, and forth maps
    page-0 addresses to Forth word names.
    """
    with open(labelmap_path) as f:
        data = json.load(f)
    flash0 = {}
    ram = {}
    for lab in data.get("labels", []):
        addr, name = lab["addr"], lab["name"]
        if lab.get("region") == "ram":
            ram[addr] = name
        elif addr < 0x4000:
            flash0[addr] = name
    forth = {}
    for w in data.get("forth_words", []):
        if w.get("region") == "flash" and w["addr"] < 0x4000:
            forth[w["addr"]] = w["name"]
    return flash0, ram, forth


class Mapper:
    """Replay zkeme80's memory mapping from OUT instructions.

    TI-84+ windows: port 6 -> $4000-$7FFF, port 7 -> $8000-$BFFF,
    port 5 -> $C000-$FFFF.  Values >= 0x80 select RAM page (v-0x80);
    page 0 is hardwired at $0000-$3FFF.
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
        if pc < 0x8000:
            return self._decode(self.port6)
        if pc < 0xC000:
            return self._decode(self.port7)
        return self._decode(self.port5)

    @staticmethod
    def _decode(sel):
        if sel >= 0x80:
            return ("ram", sel - 0x80)
        return ("flash", sel)


def iter_records(buf, want_instr=True):
    """Yield ('instr', rec_dict) / ('memwrite', addr, val) / ('key', ...)."""
    off = HEADER_LEN + struct.unpack_from("<I", buf, 16)[0]
    n = len(buf)
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

    flash0, ram, forth = load_symbols(args.labelmap)
    mapper = Mapper()

    counts = Counter()
    word_counts = Counter()
    unresolved = Counter()
    transitions = [] if args.timeline else None
    last_sym = None
    ninstr = 0

    with open(args.trace, "rb") as f:
        buf = f.read()
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
        mapper.observe(r["opcode"], r["regs"])
        space, page = mapper.space(r["pc"])
        sym = None
        if space == "flash" and page == 0:
            sym = flash0.get(r["pc"])
            w = forth.get(r["pc"])
            if w:
                word_counts[w] += 1
        elif space == "ram" and page == 1:
            sym = ram.get(r["pc"])
        if sym is None:
            unresolved["%s%02x:%04x" % (space, page, r["pc"])] += 1
            sym = "%s%02x:%04x" % (space, page, r["pc"])
        counts[sym] += 1
        if transitions is not None and sym != last_sym:
            transitions.append((r["clock"], sym))
            last_sym = sym

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
        word_name_set = set(forth.values())
        with open(args.csv, "w") as f:
            f.write("symbol,count,is_forth_word\n")
            for sym, c in counts.most_common():
                f.write("%s,%d,%s\n" % (
                    sym, c, "y" if sym in word_name_set else "n"))
        print("wrote %s" % args.csv)

    if transitions is not None:
        print()
        print("timeline (%d transitions):" % len(transitions))
        for clock, sym in transitions[:200]:
            print("  %10d  %s" % (clock, sym))


if __name__ == "__main__":
    main()
