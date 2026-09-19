#!/usr/bin/env python3
"""Check disposable ROM patching without requiring an emulator or display."""

import unittest

from emulator import PAGE, ROM_SIZE, inject, patch_source


def image_with_workbench(source=b": WORKBENCH ;\nMENU-DEMO\n"):
    image = bytearray(b"\xff" * ROM_SIZE)
    # Match the native constant stub used by the ROM's dictionary.
    stub = b"\x10MODULE-WORKBENCH\0\xc5\x01\x05\x00"
    # MODULE-WORKBENCH has 16 characters, including its hyphen.
    image[:len(stub)] = stub
    patch_source(image, 5, source)
    return image


def page_source(image, page):
    return image[page * PAGE:(page + 1) * PAGE].split(b"\0", 1)[0]


class SourcePatchingTests(unittest.TestCase):
    def test_exact_page_fit_preserves_image_size(self):
        image = image_with_workbench()
        patch_source(image, 3, b"x" * (PAGE - 1))
        self.assertEqual(len(image), ROM_SIZE)
        self.assertEqual(image[4 * PAGE - 1], 0)

    def test_rejects_invalid_payloads_without_mutation(self):
        image = image_with_workbench()
        original = bytes(image)
        for source in (b"x" * PAGE, b"embedded\0EOF"):
            with self.subTest(source_length=len(source)), self.assertRaises(ValueError):
                patch_source(image, 3, source)
            self.assertEqual(image, original)

    def test_rejects_reserved_and_out_of_range_pages(self):
        image = image_with_workbench()
        original = bytes(image)
        for page in (-1, 0, 2, 8, 56, 63, 64, 1000):
            with self.subTest(page=page), self.assertRaises(ValueError):
                patch_source(image, page, b"TEST")
            self.assertEqual(image, original)

    def test_rejects_incomplete_rom(self):
        with self.assertRaises(ValueError):
            patch_source(bytearray(PAGE), 3, b"TEST")
        with self.assertRaises(ValueError):
            inject(bytes(PAGE), "TEST")

    def test_injection_changes_only_terminal_resident_and_scratch(self):
        image = image_with_workbench()
        original = bytes(image)
        result = inject(image, "FIXTURE")
        self.assertEqual(image, original)
        self.assertEqual(len(result), len(image))
        self.assertEqual(page_source(result, 5), b": WORKBENCH ;\n1 LOAD-MODULE\n")
        self.assertEqual(page_source(result, 1), b"FIXTURE")
        for page in range(64):
            if page not in (1, 5):
                self.assertEqual(result[page * PAGE:(page + 1) * PAGE],
                                 image[page * PAGE:(page + 1) * PAGE])

    def test_appended_residents_keep_their_startup_chain(self):
        image = image_with_workbench(b": WORKBENCH ;\n7 LOAD-MODULE\n")
        patch_source(image, 7, b": EDITOR ;\n9 LOAD-MODULE\n")
        patch_source(image, 9, b": LAST-RESIDENT ;\nMENU-DEMO\n")
        result = inject(image, "FIXTURE")
        self.assertEqual(page_source(result, 5), page_source(image, 5))
        self.assertEqual(page_source(result, 7), page_source(image, 7))
        self.assertEqual(page_source(result, 9), b": LAST-RESIDENT ;\n1 LOAD-MODULE\n")

    def test_interior_menu_token_is_not_a_startup_action(self):
        image = image_with_workbench(b": OPEN-DESKTOP\nMENU-DEMO\n;\n7 LOAD-MODULE\n")
        patch_source(image, 7, b": LAST-RESIDENT ;\nMENU-DEMO\n")
        result = inject(image, "FIXTURE")
        self.assertEqual(page_source(result, 5), page_source(image, 5))
        self.assertTrue(page_source(result, 7).endswith(b"\n1 LOAD-MODULE\n"))

    def test_rejects_invalid_startup_chains(self):
        for source in (b"5 LOAD-MODULE\n", b"\n5 LOAD-MODULE\n",
                       b"\n2 LOAD-MODULE\n", b"\n8 LOAD-MODULE\n",
                       b"\n64 LOAD-MODULE\n", b"\n7 LOAD-MODULE\n",
                       b"\nMENU-DEMO\nUNEXPECTED"):
            with self.subTest(source=source), self.assertRaises(ValueError):
                inject(image_with_workbench(source), "FIXTURE")

    def test_scratch_skips_erased_reserved_pages(self):
        image = image_with_workbench()
        patch_source(image, 1, b"CORE")
        result = inject(image, "FIXTURE")
        self.assertEqual(page_source(result, 3), b"FIXTURE")
        self.assertEqual(result[2 * PAGE:3 * PAGE], b"\xff" * PAGE)

    def test_rejects_rom_without_scratch_space(self):
        image = image_with_workbench()
        for page in range(1, 56):
            if page not in (2, 5, 8):
                patch_source(image, page, b"OCCUPIED")
        with self.assertRaisesRegex(ValueError, "no erased scratch"):
            inject(image, "FIXTURE")


if __name__ == "__main__":
    unittest.main()
