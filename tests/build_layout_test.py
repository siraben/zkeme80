import json
import unittest
from pathlib import Path


ROOT = Path(__file__).parents[1]


class BuildLayoutTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        labelmap = json.loads((ROOT / "zkeme80.ram-labelmap.json").read_text())
        cls.labels = {entry["name"]: entry["addr"] for entry in labelmap["labels"]}

    def test_flash_workers_match_their_copy_lengths(self):
        workers = (
            ("write-flash-buffer-ram", "write-flash-buffer-ram-end", 0x3C),
            ("erase-flash-sector-ram", "erase-flash-sector-ram-end", 0x3D),
        )
        for start, end, copied_length in workers:
            with self.subTest(worker=start):
                actual = self.labels[end] - self.labels[start]
                self.assertEqual(copied_length, actual)
                self.assertLessEqual(actual, 100)


if __name__ == "__main__":
    unittest.main()
