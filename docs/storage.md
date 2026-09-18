# Shared object storage

`os-storage.fs` provides a persistent, case-sensitive flat namespace shared by
the shell and desktop. Names contain 1–16 bytes; slashes can organize names
such as `SRC/HELLO` or `DATA/NOTES` without implying directory objects.
Types distinguish text (1), Forth source (2), and binary data (3).

```forth
: SAVE-DEMO
  S" : HELLO 42 . ;"
  S" SRC/HELLO"
  2 FS-PUT . ;
SAVE-DEMO
S" SRC/HELLO" FS-LOAD .
HELLO
FS-LIST
```

The helper compiles its string literals into separate dictionary storage;
interpretive `S"` uses temporary space at `HERE`, so two consecutive
interpretive strings must not be kept as separate buffers.

The integer printed after each operation is its result code: zero means
success. Definitions loaded from source use the normal Forth dictionary.
Only explicit `FS-LOAD` evaluates source; browsing or fetching an object does
not execute it.

| Word | Stack effect | Meaning |
| --- | --- | --- |
| `FS-PUT` | `( data len name namelen type -- ior )` | Append or replace an object |
| `FS-GET` | `( name namelen -- data len type ior )` | Fetch the latest revision |
| `FS-DELETE` | `( name namelen -- ior )` | Append a deletion marker |
| `FS-LOAD` | `( name namelen -- ior )` | Evaluate a source object |
| `FS-NTH` | `( index -- name namelen size type flag )` | Enumerate live objects, zero-based |
| `FS-COUNT` | `( -- n )` | Live object count |
| `FS-FREE` | `( -- n )` | Remaining erased record slots |
| `FS-LIST` | `( -- )` | Print live objects |
| `FS-RESCAN` | `( -- )` | Invalidate the directory cache after raw flash changes |

Returned addresses refer to module buffers and should be treated as invalid
after another storage operation. `FS-GET` failures return three zero values
followed by the error. `FS-NTH` exhaustion returns five zero values. The
implementation uses shared scratch state and must not be entered recursively
or yield during a storage operation. A running source object also occupies
the shared data buffer: `FS-GET`, `FS-PUT`, `FS-DELETE`, and nested `FS-LOAD`
return 45 until its evaluation finishes. Directory listing remains available.

| Result | Meaning |
| --- | --- |
| 0 | Success |
| 40 | Empty or oversized name |
| 41 | Invalid type or length; loading a non-source object |
| 42 | Journal full |
| 43 | Missing or deleted object |
| 44 | Checksum mismatch |
| 45 | Source evaluation currently owns the storage data buffer |

Low-level `STORAGE-READ`/`STORAGE-WRITE` result codes can also propagate.
Payloads hold at most 992 bytes; source is limited to 991 to leave space for
the evaluator's terminating zero byte. Empty text and binary objects are
valid. Source loading is a convenience for ordinary definitions, not a
sandbox for untrusted programs.

## Flash format and recovery

Physical flash page 8 is reserved for sixteen 1024-byte records. Each record
has a 32-byte header and a 992-byte payload:

| Byte offset | Field |
| --- | --- |
| 0 | Commit marker `90` decimal; erased/incomplete records remain uncommitted |
| 1 | Format version `1` |
| 2 | Type, or `0` for deletion |
| 3 | Name length |
| 4–5 | Little-endian payload length |
| 6–7 | Little-endian 16-bit additive checksum |
| 8–23 | Name, padded with zero bytes |
| 24–31 | Reserved, zero |
| 32 onward | Data, padded with zero bytes |

The checksum sums bytes 2–5 and bytes 8 through the end of the actual data.
Directory lookup validates commit marker, version, type and bounds. Fetching
an object additionally validates its checksum. The last committed matching
record wins, including deletion markers. The body is programmed and verified
before the single commit byte is written; an interrupted body never replaces
the prior revision. Allocation verifies the entire target record is erased,
so interrupted writes are skipped rather than overwritten.

A 16-byte cache records live slots for responsive directory browsing. It is
rebuilt on first access and after object updates; `FS-RESCAN` requests a
rebuild after modifications made through raw storage primitives.

The journal never erases flash. Updating or deleting consumes another slot,
and exhaustion returns 42 without changing an existing object. Reclamation,
multi-page files, and redundant-sector compaction remain future work. Raw
flash primitives and `ERASE-SECTOR` are outside this API's guarantees. Emulator
persistence requires saving/reusing the modified flash image; rebuilding the
ROM creates an empty store again.

This takes the idea of named, typed shared objects from the TI-OS VAT and
archive, while keeping source accessible as text in a Forth environment.
The local comparison was `~/ti84p-re/docs/variables-vat.md` and
`~/ti84p-re/docs/flash-memory.md`. Records are 1 KiB allocation units, but this
is **not** an ANS `BLOCK` implementation: the payload includes metadata
overhead and does not expose 16 lines of 64 characters as a standard screen.

`tests/os-storage.fs` exercises validation, create/replace, alias-safe copying,
lookup, enumeration, deletion, empty objects, source evaluation, and an
interrupted record against a disposable, initially erased storage page.

After building, run the emulator regression with a TilEm binary supporting
headless macros:

```sh
nix develop --command python3 tests/test-storage.py --emulator /path/to/tilem2 --full --output /tmp/storage-test
```

The runner only modifies temporary ROM copies. It checks the Forth assertion
counters, decodes and verifies the resulting flash journal independently,
and starts a second emulator with that flash image to verify persistence after
a cold boot. `--full` also verifies journal exhaustion preserves older files.
