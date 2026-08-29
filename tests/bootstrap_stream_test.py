import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import tools.bootstrap_stream as stream


ROOT = Path(__file__).parents[1]
SOURCES = [ROOT / path for path in stream.DEFAULT_SOURCES]


class BootstrapLexerTests(unittest.TestCase):
    def test_lexes_forth_constructs_without_changing_any_byte(self):
        source = (
            b': name\tS" two words" \\ rest of line\r\n'
            b'( comment\ncontinues )\n." output text"  name\n'
        )
        tokens = stream.lex_forth(source)

        self.assertEqual(source, stream.reconstruct_tokens(tokens))
        self.assertEqual(
            [
                stream.Kind.WORD,
                stream.Kind.WHITESPACE,
                stream.Kind.WORD,
                stream.Kind.WHITESPACE,
                stream.Kind.QUOTED,
                stream.Kind.WHITESPACE,
                stream.Kind.LINE_COMMENT,
                stream.Kind.WHITESPACE,
                stream.Kind.PAREN_COMMENT,
                stream.Kind.WHITESPACE,
                stream.Kind.QUOTED,
                stream.Kind.WHITESPACE,
                stream.Kind.WORD,
                stream.Kind.WHITESPACE,
            ],
            [token.kind for token in tokens],
        )

    def test_unterminated_constructs_still_round_trip(self):
        for source in (b'\\ no newline', b'( never closes', b'S" never closes'):
            with self.subTest(source=source):
                self.assertEqual(source, stream.reconstruct_tokens(stream.lex_forth(source)))


class BootstrapTokenTests(unittest.TestCase):
    def test_dictionary_and_token_encoding_are_deterministic(self):
        token_lists = [stream.lex_forth(b"DUP DUP +\n"), stream.lex_forth(b"DUP + +\n")]
        first = stream.build_dictionary(token_lists)
        second = stream.build_dictionary(list(reversed(token_lists)))

        self.assertEqual(first, second)
        self.assertIn(b"DUP", first)
        encoded = stream.encode_token_stream(token_lists[0], first)
        self.assertIn(bytes((stream.Kind.WORD_REF,)), encoded)
        self.assertEqual(
            b"DUP DUP +\n",
            stream.reconstruct_tokens(stream.decode_token_stream(encoded, first)),
        )

    def test_token_decoder_rejects_bad_references_and_trailing_data(self):
        bad_reference = bytes((stream.Kind.WORD_REF, 0, stream.Kind.END))
        with self.assertRaisesRegex(ValueError, "out of range"):
            stream.decode_token_stream(bad_reference, ())
        with self.assertRaisesRegex(ValueError, "trailing bytes"):
            stream.decode_token_stream(bytes((stream.Kind.END, 0)), ())

    def test_uleb_encoding_is_canonical(self):
        with self.assertRaisesRegex(ValueError, "non-canonical"):
            stream._Reader(b"\x80\x00").uleb()


class BootstrapArchiveTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.stages = stream.load_sources([str(path) for path in SOURCES])

    def test_both_archive_codecs_round_trip_actual_bootstrap(self):
        for codec in (stream.CODEC_TOKENS, stream.CODEC_LZSS):
            with self.subTest(codec=codec):
                archive = stream.build_archive(self.stages, codec, 512)
                decoded = stream.parse_archive(archive)
                self.assertEqual(tuple(self.stages), decoded.stages)
                self.assertEqual(codec, decoded.codec)

    def test_archive_output_is_reproducible(self):
        first = stream.build_archive(self.stages, stream.CODEC_LZSS, 4096)
        second = stream.build_archive(self.stages, stream.CODEC_LZSS, 4096)
        self.assertEqual(first, second)

    def test_archive_rejects_bad_magic_truncation_and_corruption(self):
        archive = stream.build_archive([stream.Stage("one.fs", b"DUP DUP\n")])
        for damaged in (b"nope" + archive[4:], archive[:-1]):
            with self.subTest(length=len(damaged)):
                with self.assertRaises(ValueError):
                    stream.parse_archive(damaged)
        corrupted = bytearray(archive)
        corrupted[-1] ^= 1
        with self.assertRaises(ValueError):
            stream.parse_archive(bytes(corrupted))

    def test_cli_build_and_verify(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "bootstrap.zbs"
            command = [
                sys.executable,
                str(ROOT / "tools" / "bootstrap_stream.py"),
                "build",
                "--output",
                str(output),
                "--codec",
                "lzss",
                *map(str, SOURCES[:2]),
            ]
            subprocess.run(command, check=True, cwd=ROOT, capture_output=True, text=True)
            verified = subprocess.run(
                [
                    sys.executable,
                    str(ROOT / "tools" / "bootstrap_stream.py"),
                    "verify",
                    str(output),
                    *map(str, SOURCES[:2]),
                ],
                check=True,
                cwd=ROOT,
                capture_output=True,
                text=True,
            )
            self.assertIn("verified", verified.stdout)


if __name__ == "__main__":
    unittest.main()
