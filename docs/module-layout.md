# ROM modules and compilation

`src/modules.scm` is the source manifest. It uses SRFI-9 records to distinguish
logical modules (`<rom-module>`) from physical page allocations
(`<rom-allocation>`). A module has a symbolic name, a kind (`resident` or
`tool`), and an ordered list of source filenames relative to `src/`.
Consumers refer to generated `MODULE-<NAME>` Forth constants rather than
embedding physical page numbers.

The current allocation is:

| Module | Kind | Sources | Flash page |
| --- | --- | --- | --- |
| `core` | resident | `bootstrap-flash1.fs` | 1 |
| `storage` | resident | `os-services.fs`, `os-storage.fs` | 3 |
| `desktop` | resident | `os-tasks.fs`, `os-catalog.fs`, `os-desktop.fs` | 4 |
| `workbench` | resident | `bootstrap-flash5.fs` | 5 |
| `tests` | tool | `bootstrap-flash4.fs` | 6 |

Page 0 contains the native kernel. Page 2 supplies the initial fixed-RAM image.
Page 8 belongs to the persistent object journal, pages 56–59 to the legacy
swap sector, and pages 60–63 are reserved for unlock/boot support. Module
allocation skips every reserved page. Adding or reordering a module can change
its physical page; use names in application code.

## Adding a module

Create `src/editor.fs` containing ordinary Forth definitions. Add a record to
`rom-modules` after its dependencies, for example:

```scheme
(make-rom-module 'editor 'resident '("editor-buffer.fs" "editor.fs"))
```

A module may join several files. The builder inserts newlines between files
and adds one terminating zero byte after the complete module, so an earlier
file's EOF cannot silently prevent later files from compiling. File reads use
Guile's `call-with-input-file` and `get-string-all`; UTF-8 encoding uses Guile's
bytevector library. The byte budget includes file separators, generated loader
code, and the final EOF.

Residents compile in manifest order. The builder appends a `LOAD-MODULE`
transition to the next resident, skipping tools. After the final resident it
appends `MENU-DEMO` to activate the desktop. Do not put a blocking application
loop at the end of a resident source: it would prevent later residents from
loading. A resident should define words, initialize its state, and return.
Appending a resident after the workbench is supported; desktop activation
moves to the new final resident automatically.

A `tool` is stored in ROM but does not run at boot:

```scheme
(make-rom-module 'diagnostics 'tool '("diagnostics.fs"))
```

Its generated constant is `MODULE-DIAGNOSTICS`. Foreground launch code can
call `MODULE-DIAGNOSTICS LOAD-MODULE` and return to the outer interpreter so
it can read that source. A tool must arrange its own completion behavior; see
the test suite's explicit dictionary cleanup and return to `MENU-DEMO`.
The existing launcher choices are ordinary Forth definitions, not an
automatically populated application registry.

## Checks and boundaries

The builder rejects duplicate names, invalid kinds or filenames, names that
cannot safely form generated constants, and manifests whose first entry is
not resident `core`. Core is deliberately anchored at page 1 because it
provides the resident loader itself. Allocation cannot reserve page 1 or
release the built-in reserved pages, and it fails when available pages run out.
Names use lowercase ASCII letters, digits, and hyphens, beginning with a letter;
they are limited to 24 characters so the `MODULE-` prefix fits Forth's 31-byte
dictionary-name limit.

Every module's UTF-8 source, trailer, and EOF must fit within 16 KiB. Oversized
modules fail with their name and encoded size; split their source-file group
into multiple named modules. The builder does not silently split a colon
definition, comment, or string across pages. Embedded zero bytes in source
are rejected as premature EOF markers.

This is automatic source-module placement and boot chaining, not a banked
native-code linker. Source is interpreted at boot into the shared RAM
dictionary. Source-page capacity and compiled RAM usage are independent:
`UNUSED` reports the latter at runtime. A future cross-compiler or overlay
manager would need relocation metadata, resident export stubs, and explicit
ownership of code/data that outlives an unloaded module. The current system's
service IDs offer a starting contract for that evolution.

Run the manifest regression tests from the repository root:

```sh
nix develop -c guile --no-auto-compile tests/modules.scm
```

These tests cover reserved-page exclusion, manifest errors, page exhaustion,
exact-fit and overflowing sources, one-EOF multi-file composition, the real
module source sizes, and activation after appending a resident beyond a tool.
Build the complete image with `make build`; interactive emulator checks remain
necessary for Forth compilation and runtime behavior.

`make upgrade` derives its code-page list from this same manifest through
`rom-upgrade-pages`. It includes the kernel, RAM template, all resident/tool
source pages, and unlock support, and excludes the writable object journal.
The upgrade tool `mktiupgrade` is required separately; `make -n upgrade` shows
the generated page list without creating or installing an upgrade.
