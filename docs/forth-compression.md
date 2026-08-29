# Forth bootstrap stream compression

The bootstrap sources are stored as plain text on Flash pages. Two offline
tools quantify ways to shrink them; neither changes the ROM format or the
runtime interpreter yet.

- `tools/analyze_forth_compression.py` applies LZSS directly to the source
  bytes. This is the smallest current baseline.
- `tools/bootstrap_stream.py` first makes a deterministic, Forth-aware token
  stream, then optionally applies the same LZSS codec. It reconstructs every
  source byte and is intended as a stable prototype for a future input device.

Run the report from the repository root:

```sh
python3 tools/bootstrap_stream.py report
python3 tools/bootstrap_stream.py report --window 512
```

It prints source, token-stream, and LZSS payload sizes for each stage, followed
by totals including archive framing and the dictionary. On the current source
set and a 4 KiB window, 29,584 source bytes become 13,504 bytes of separately
compressed token payloads or a 16,950-byte complete archive. The complete
archive includes a 403-word, 3,257-byte dictionary and stage metadata. Direct
raw-byte LZSS remains smaller at about 13.0 KiB; the token archive trades that
space for explicit lexical boundaries and dictionary references that a future
token input path can consume. These figures are measurements, not a proposed
flash layout.

Build and verify an archive with:

```sh
make bootstrap-stream
python3 tools/bootstrap_stream.py verify build/bootstrap.zbs

# Compare decoded stages byte-for-byte with explicit source files.
python3 tools/bootstrap_stream.py verify build/bootstrap.zbs \
  src/boot.fs src/bootstrap-flash{1,2,3,4,5}.fs
```

The make target is opt-in and writes an ignored archive; normal ROM builds are
unchanged. `build --codec tokens` omits LZSS, and `--window N` selects a
1–4096-byte LZSS history. Smaller windows reduce a future decoder's RAM need.

## Lexing and exactness

Lexing operates on bytes, not decoded text. It recognizes:

- runs of the current `WORD` delimiters (space, tab, and LF);
- ordinary whitespace-delimited words;
- `\` line comments through, but not including, the line ending;
- `(` comments through the next `)`;
- `S"` and `."` forms through the next `"`.

Recognition begins only when `\`, `(`, `S"`, or `."` is a complete Forth word.
That mirrors how the current input is written and prevents comment or string
contents from being dictionary-tokenized. Unterminated constructs consume the
remaining input, allowing malformed test cases to round-trip too. Names,
strings, comments, spaces, tabs, CR/LF bytes, and final-newline presence are all
preserved. A CR remains ordinary payload because the current `WORD` does not
treat it as a delimiter. Archive construction decodes its own output and
rejects differences.

The global word dictionary is deterministic. Repeated ordinary words are
considered in bytewise lexical order and retained only when literal-versus-
reference accounting shows a net token-stream saving. Input path spelling and
the current working directory therefore do not affect output; only unique file
basenames and contents are stored.

## Version 1 archive format

All integers marked `ULEB` use canonical unsigned LEB128. Fixed-width integers
are little-endian.

| Field | Encoding | Meaning |
| --- | --- | --- |
| magic | 4 bytes | `ZKBS` |
| version | byte | `1` |
| codec | byte | `0` token records, `1` LZSS records |
| window | u16 | zero for codec 0; 1–4096 for codec 1 |
| dictionary count | ULEB | number of shared word entries |
| dictionary entries | repeated ULEB + bytes | length and exact word bytes |
| stage count | ULEB | number of following stage records |

Each stage record contains, in order:

| Field | Encoding |
| --- | --- |
| basename | ULEB length + UTF-8 bytes |
| reconstructed source length | ULEB |
| lexical token count | ULEB |
| decoded token-stream length | ULEB |
| stored payload length | ULEB |
| reconstructed-source CRC-32 | u32 |
| token or LZSS payload | stored payload length bytes |

The decoded stage payload is a sequence of tagged records followed by `END`:

| Tag | Name | Body |
| --- | --- | --- |
| `00` | `END` | none |
| `01` | `WORD` | ULEB length + exact bytes |
| `02` | `WORD_REF` | ULEB dictionary index |
| `03` | `WHITESPACE` | ULEB length + exact bytes |
| `04` | `LINE_COMMENT` | ULEB length + exact bytes |
| `05` | `PAREN_COMMENT` | ULEB length + exact bytes |
| `06` | `QUOTED` | ULEB length + exact bytes |

Codec 1 uses the format in `analyze_forth_compression.py`: eight LSB-first
flags followed by literal bytes or two-byte matches. A match stores a 12-bit
distance minus one and a four-bit length minus three, giving distances of
1–4096 and lengths of 3–18 bytes. Payload bounds, decoded lengths, token counts,
dictionary indices, canonical integers, final CRCs, and trailing bytes are all
checked by the host decoder.

## Streaming decoder design

The archive is organized for sequential consumption. A future `GETC`-style
device can retain the current stage pointer, LZSS flag state, a sliding output
ring, the current token payload, and a one-byte pushback. It can replay exact
bytes into the existing parser; a later token-aware path may handle `WORD_REF`
directly while emitting other records unchanged. Error reporting that seeks
backward to the current line additionally needs a small line buffer or an
explicit loss of source-line echo.

The runtime integration requires these checks before changing the ROM layout:

1. Implement a bounded streaming decoder without moving pages.
2. Trace boot and the on-device suite with archived and raw inputs, comparing
   dictionary/RAM dumps and emitted text.
3. Measure decoder code plus archive size at several window sizes before
   choosing the page layout and `.8xu` page manifest.
4. Keep an uncompressed developer-build path for parser diagnostics.

Comment stripping or whitespace normalization could save more, but each would
change the source byte stream and remains outside this format version.

The [precompiled bootstrap image](precompiled-bootstrap.md) avoids parsing or
decompressing source on the calculator. It packages the post-bootstrap RAM
dictionary on Flash page `06`; compressed source remains an option for smaller
distribution images, while plain text remains the diagnostic fallback.
