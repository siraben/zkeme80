# Forth source compression

zkeme80 currently stores each bootstrap stage as plain Forth text on a flash
page.  Run `python tools/analyze_forth_compression.py` to reproduce the size
of a classic LZSS format with a 4 KiB window, 3–18 byte matches, one flag bit
per token, and two-byte match records.  The tool decodes every result and
checks it byte-for-byte against the input.

On the current sources, separate per-stage streams reduce roughly 29.6 KiB of
text to 13.0 KiB.  This is small enough for one 16 KiB flash page plus a
decoder, instead of the current sources occupying pages 1, 2, 3, 4, and 5.
A 512-byte window retains most of the benefit while consuming much less of the
14.4 KiB initial dictionary space; measure it with `--window 512`.

The appropriate runtime design is a decompression-backed input device for
`GETC`/`UNGETC`, not whole-stage expansion into the dictionary area.  Its
state consists of the compressed pointer/page, flag byte and bit count, and a
sliding output ring.  `UNGETC` requires at least a one-byte decoded pushback;
error reporting that seeks backward to the current line needs either a small
line buffer or an explicit loss of source-line echo for compressed input.

Before changing the ROM layout:

1. Add the streaming decoder and round-trip fixtures without moving pages.
2. Trace boot and the full on-device suite with compressed and raw inputs and
   compare dictionary/RAM dumps.
3. Choose the smallest window whose compressed stream plus decoder is a net
   win, then pack stages and update the `.8xu` page manifest.
4. Keep an uncompressed developer build option so parser failures still show
   exact source lines.

Build-time comment stripping can save more, but it is a separate semantic
change: quoted strings, `\\` line comments, parenthesized comments, and line
boundaries used by error reporting must all be preserved deliberately.
