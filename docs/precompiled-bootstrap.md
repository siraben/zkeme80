# Precompiled bootstrap image

The normal zkeme80 build still boots by interpreting `src/boot.fs` and the
five `src/bootstrap-flash*.fs` files.  An opt-in build captures the result,
packages the live `H0` through `DP` RAM dictionary deterministically, places
it on flash page 6, and boots directly from it.  The text path remains the
fallback when page 6 is erased or validation fails.

## Capture and package

The `tilem-headless` fork already provides the required observation hook.
Its `memdump ... ram` command writes all eight physical 16 KiB RAM pages.
`re/macros/capture-bootstrap.macro` boots normally, waits for the main menu,
and writes `build/bootstrap.ram`.  Run the complete pipeline with:

```sh
make bootstrap-image \
  TILEM_HEADLESS="$HOME/Git/tilem-headless/result/bin/tilem2"

# Build a ROM with the verified image on page 6.
make precompiled-rom \
  TILEM_HEADLESS="$HOME/Git/tilem-headless/result/bin/tilem2"

# Boot it for one second and compare live RAM with the packaged payload.
make precompiled-smoke \
  TILEM_HEADLESS="$HOME/Git/tilem-headless/result/bin/tilem2"
```

This builds the ROM, captures RAM, creates `build/bootstrap.zkbi`, and verifies
the result against the current page-0 kernel and bootstrap sources.  To package
an existing physical dump without starting TilEm, run `make bootstrap-pack`.
`make bootstrap-verify` rechecks a packaged image.  The packer can also consume
the first page of the older logical dump format with `--dump-layout logical`.

The physical dump is important: `ram-logical` iterates for the calculator's
full 128 KiB RAM size even though CPU addresses are only 16 bits.  Its first
32 KiB represent the currently mapped `$8000-$FFFF` windows; later bytes do
not represent a stable logical address space.  Physical selector `0x81` is
unambiguous and owns the dictionary window `$8000-$BFFF`.

`precompiled-rom` writes `zkeme80-precompiled.rom`; `precompiled-upgrade`
writes an upgrade containing page 6 as well as the ordinary OS pages.

## Image format, version 2

All integers are little-endian.  The 160-byte fixed header is followed by one
raw payload.  The payload begins at `H0` and ends exactly at the captured
`DP`, so transient input buffers, the find cache, and the live return-stack
reservation below `H0` are not stored.  The captured `DP` and `LATEST` values
and their variable addresses remain in the header for loader initialization.

| Header field | Purpose |
|---|---|
| `ZKEME80B`, version `2`, header size | format identification and evolution |
| flags, model `TI84`, selector `0x81` | target and payload interpretation |
| load address and payload size | future loader copy range |
| addresses and captured values of `DP` and `LATEST`; `H0` | structural validation |
| payload CRC32 and SHA-256 | fast and strong corruption detection |
| page-0 kernel SHA-256 | rejects incompatible native words/layout |
| ordered bootstrap-source SHA-256 | records exactly which text produced the image |
| header CRC32 | detects corrupted metadata before loading |
| entry CFA | address of the captured `MENU-DEMO` colon definition |

No timestamp, absolute path, emulator state file, or host metadata enters the
format.  Repacking the same RAM dump and inputs produces identical bytes.
`python3 tools/bootstrap_image.py inspect build/bootstrap.zkbi` prints the
verified metadata as stable JSON.

## Runtime validation and fallback

The page-0 loader runs after normal RAM and core-variable initialization.  It
maps flash page 6, checks the magic, version, selector, header size, RAM
addresses, dictionary bounds, and entry bounds, then computes CRC-32 over the
payload before copying it.  It restores `DP` and `LATEST`, confirms that the
entry begins with a colon-word `CALL`, and executes `MENU-DEMO`.  Menu click
handlers can then redirect `INPUT-PTR` to the test-suite or shell pages; the
loader resumes `INTERPRET` exactly as the text bootstrap does.

The SHA-256 bindings and header CRC are checked by the host build/verification
step.  The calculator performs the bounded structural checks and payload CRC,
which protect every byte it will execute.  A failed check restores the normal
flash mapping and continues into `QUIT`, so an ordinary build with erased page
6 behaves exactly like the source-only ROM.

On the current image, the stored dictionary is 4,789 bytes plus the 160-byte
header.  A traced image boot reaches `MENU-DEMO` in 563,249 Z80 instructions;
the optimized text path reaches it in 7,274,904 instructions, so the optional
path removes roughly 92% of bootstrap
instructions.  Both paths pass the 265/265 on-device suite and return to the
menu.
