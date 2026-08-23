#!/usr/bin/env python3
"""Regression tests for the zkeme80 TLMT trace analyzer."""

import struct
import unittest

from analyze_forth_trace import (
    ExactSymbolResolver,
    Mapper,
    SymbolResolver,
    iter_records,
)


class MapperTests(unittest.TestCase):
    def test_reset_mapping_uses_mode_one_pair(self):
        mapper = Mapper()
        self.assertEqual(mapper.space(0x0000), ("flash", 0))
        self.assertEqual(mapper.space(0x4000), ("flash", 0x3E))
        self.assertEqual(mapper.space(0x8000), ("flash", 0x3F))
        self.assertEqual(mapper.space(0xC000), ("flash", 0x3F))

    def test_mode_zero_maps_three_independent_windows(self):
        mapper = Mapper()
        mapper.out(4, 0x06)
        mapper.out(5, 0x02)
        mapper.out(6, 0x03)
        mapper.out(7, 0x81)
        self.assertEqual(mapper.space(0x4000), ("flash", 3))
        self.assertEqual(mapper.space(0x8000), ("ram", 1))
        self.assertEqual(mapper.space(0xC000), ("ram", 2))

    def test_mode_one_masks_ram_page_and_forms_pair(self):
        mapper = Mapper()
        mapper.out(4, 0x07)
        mapper.out(6, 0x85)
        mapper.out(7, 0x02)
        self.assertEqual(mapper.space(0x4000), ("ram", 4))
        self.assertEqual(mapper.space(0x8000), ("ram", 5))
        self.assertEqual(mapper.space(0xC000), ("flash", 2))

    def test_observe_decodes_both_out_forms(self):
        mapper = Mapper()
        regs = {"wz": 0x8206, "bc": 0, "af": 0}
        mapper.observe(0xD3, regs)
        self.assertEqual(mapper.space(0x4000), ("ram", 2))

        regs.update({"bc": 7, "af": 0x0300})
        mapper.observe(0xED79, regs)
        self.assertEqual(mapper.space(0xC000), ("flash", 3))


class SymbolResolverTests(unittest.TestCase):
    def test_resolves_ranges_and_honors_end(self):
        resolver = SymbolResolver(((0x10, "a"), (0x20, "b")), end=0x30)
        self.assertIsNone(resolver.resolve(0x0F))
        self.assertEqual(resolver.resolve(0x10), "a")
        self.assertEqual(resolver.resolve(0x1F), "a")
        self.assertEqual(resolver.resolve(0x20), "b")
        self.assertIsNone(resolver.resolve(0x30))

    def test_exact_resolver_does_not_claim_neighboring_ram(self):
        resolver = ExactSymbolResolver(((0x10, "variable"),))
        self.assertEqual(resolver.resolve(0x10), "variable")
        self.assertIsNone(resolver.resolve(0x11))


class TraceFormatTests(unittest.TestCase):
    def test_rejects_truncated_header_and_snapshot(self):
        with self.assertRaisesRegex(ValueError, "truncated TLMT header"):
            list(iter_records(b"TLMT"))

        header = bytearray(20)
        struct.pack_into("<I", header, 16, 1)
        with self.assertRaisesRegex(ValueError, "initial-memory snapshot"):
            list(iter_records(bytes(header)))


if __name__ == "__main__":
    unittest.main()
