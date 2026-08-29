import importlib.util
import unittest
from pathlib import Path


TOOL = Path(__file__).parents[1] / "tools" / "analyze_forth_compression.py"
SPEC = importlib.util.spec_from_file_location("forth_compression", TOOL)
compression = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(compression)


class CompressionTests(unittest.TestCase):
    def test_round_trip_literals_matches_and_overlap(self):
        samples = (b"", b"abc", b"abcabcabcabc", b"a" * 1000, bytes(range(256)) * 3)
        for window in (32, 512, 4096):
            for sample in samples:
                with self.subTest(window=window, length=len(sample)):
                    packed = compression.compress(sample, window)
                    self.assertEqual(sample, compression.decompress(packed))

    def test_rejects_bad_window(self):
        with self.assertRaises(ValueError):
            compression.compress(b"data", 4097)


if __name__ == "__main__":
    unittest.main()
