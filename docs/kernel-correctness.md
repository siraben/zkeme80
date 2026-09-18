# Dictionary and page-mapping correctness

These fixes build on master's ANS94 kernel. The existing 286-case suite remains
unchanged; the additional target suite covers allocation and mapping failures.

## Allocation contracts

`DP-LIMIT` is the exclusive dictionary end, `0xC000`. This retains the existing
RAM layout: flash programming routines use scratch memory starting at that
address. `UNUSED` derives its result from the same limit.

`ROOM ( u -- )` checks an unsigned byte count against the space from `HERE` to
`DP-LIMIT`, including validating that `HERE` is within the dictionary. It throws
standard exception `-8` before an ordinary dictionary write would exceed that
space. `ALLOT` still accepts signed counts; a negative allocation may reclaim
space but cannot move below `H0`.

`,`, `C,`, and `DOES>` validate their complete writes. `CREATE`, `:`, `VARIABLE`,
`CONSTANT`, and `VALUE` validate names and reserve their complete fixed header
and body before changing `DP` or `LATEST`. Missing names throw `-16`; names
longer than the header's 31-byte capacity throw `-19`. `CREATE` and `VARIABLE`
retain their standard DOVAR and `>BODY` behavior.

`S"` scans the bounded source first, using `SOURCE` and `>IN`, then checks the
complete copy size before writing. Compiled strings include their private
length field and terminator; interpreted strings retain master's transient
`HERE` buffer and address/length result. EOF before a closing quote continues
to throw `-18`. `[CHAR]` parses its name before emitting a literal.

These are allocation guards, not general memory protection: direct stores and
writes to `DP` remain available to Forth programs. Standard `EVALUATE` semantics
and compiler-state behavior are unchanged.

## Flash mapping

`MAP-FLASH ( page -- flag )` selects flash page 0–63 in bank A and returns
canonical true (`-1`). Invalid 16-bit values return false without changing the
bank. Bootstrap and application code use `MAP-FLASH` directly.

The old implementation examined only the low byte, so page selector `256`
selected page zero. It also used flash aliases `64+n`; the new operation writes
the flash selector directly. Callers borrowing a page remain responsible for
restoring it before reading their original banked source again.

## Reproduce

The development shell provides Guile and Python. The target tests additionally
require an extended TilEm with `--headless`, `--macro`, and `memdump`, such as
the local `~/Git/tilem-headless` build, plus an available X display:

```sh
TILEM="$HOME/Git/tilem-headless/result/bin/tilem2" DISPLAY=:99 \
  nix develop --command make test-emulator
```

For an existing ROM and a persistent evidence directory:

```sh
nix develop --command python3 tests/master-kernel.py \
  --emulator "$HOME/Git/tilem-headless/result/bin/tilem2" \
  --display :99 --output /tmp/zkeme80-master-checks
```

The runner uses disposable ROM/state copies. It verifies all 286 original
assertions and that unloading the suite restores `DP`/`LATEST`. A separate
fixture replaces only the final menu action in a disposable bootstrap page and
loads checks from erased page 6. Its 151 assertions cover exact-fit and rejected
writes, signed release, malformed names, quoted EOF, manually adjusted `>IN`,
DOVAR bodies, and page selector bounds. RAM counters, a completion marker, and
recorded failure values determine success; screenshots are retained for review.

`make test-build` separately verifies both Guile build entry points from paths
containing spaces, output-directory behavior, and upgrade packager inputs.
Those portable checks also run inside the Nix package build. The packager test
uses a stub and does not exercise physical-device signing or installation.
