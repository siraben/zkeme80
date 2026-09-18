# Building and testing the workbench

Build and run the portable layout and emulator-helper checks in the pinned
development environment:

```sh
nix develop --command make build test
nix develop --command tilem2 --rom zkeme80.rom
```

The shell provides Guile, Python, ImageMagick, and TilEm; on Linux it also
provides Xvfb and xdotool.
The packaged TilEm is sufficient for interactive use. Automated target tests
require the extended TilEm build that supplies `--headless`, `--macro`,
`memdump`, and `scanstring` (the checkout used here is
`~/Git/tilem-headless`). Point `TILEM` at that build explicitly:

```sh
export TILEM=/path/to/extended/tilem2
export DISPLAY=:99
nix develop --command Xvfb :99 -screen 0 1024x768x24 -nolisten tcp &
nix develop --command make test-emulator
```

All Python runners use disposable ROM/state copies. They locate named module
constants in the built kernel and select an erased scratch source page,
leaving the production ROM and its journal unchanged. Each runner accepts
`--output /tmp/example` to retain screenshots, RAM dumps, and replay macros.

| Check | Evidence |
| --- | --- |
| ROM layout / harness | 30 SRFI-64 assertions and 10 Python regressions, including budgets, upgrade selection, appended resident startup, and invalid page writes |
| Forth language | 288/288 ANS and shell assertions; explicit unloading still works |
| RAM / parser | 154 target assertions: allocation bounds, atomic defining words, exact-fit strings, quoted EOF, missing/unknown names, fixed RAM banks, and checked flash mapping |
| Core services | 133 assertions: scoped banks, service IDs, nested evaluation, token bounds, file-load stack balance, unfinished definitions, and caller compilation recovery |
| Workspace | Multiline and bracketed definitions, error/exit rollback, deliberately hidden completed words, and definitions retained across desktop visits |
| Scheduler | 89 target assertions, including bank changes on normal/error returns; counters advance during desktop and shell idle waits |
| Storage | 74 target assertions, 12 cold-boot checks, and 12 damaged-payload checks; independent flash record verification |
| Desktop | Empty/multiple objects, source load/error feedback, selection and paging bounds, task lifecycle, page restoration, service paging, and 29 exact LCD comparisons |
| Shell editing | Thirteen cursor/full-buffer states, 512-entry and 4096-byte history boundaries, eviction, compiler rollback, and persistent definitions |

For the original language suite, choose **Test suite** in the workbench or run
`tests/full-suite.macro` using the extended emulator. The resident editor check uses `tests/shell-editing.macro` followed by
`nix develop --command python3 tests/verify-shell-editing.py`. It verifies
cursor state, the full line buffer, history capacity/eviction, compiler
rollback, and retained definitions after `BYE`.

Raw `key` commands in the extended emulator require explicit `wait` commands
after release. Its `key_delay` setting only spaces characters in typed strings;
using it alone can merge repeated navigation keys. Desktop and recording
runners now supply release intervals.

The target runs also found and fixed inherited interpreter problems: the
return stack now begins at its reserved address, `INTERPRET` returns to nested
callers, token EOF remains visible to the next parser call, and overlong tokens
raise a recoverable error instead of overwriting the token-pointer cell.
The adversarial cases also cover strings crossing dictionary bounds, failed
defining words, nested evaluators that close their caller’s definition, file
loads that leave unwanted stack results, and callbacks that change banks.
Unfinished definitions have a separate header flag, so ordinary private words
are not mistaken for failed compilations.
These checks establish emulator behavior; physical flash timing and unexpected
power loss during programming still need hardware qualification.

A stock TilEm GUI was also exercised under Xvfb using mouse presses on its
calculator skin: open Tasks, create a counter, and pause it. The screenshot
shows the redesigned task list with a paused job and its retained run count.

See the [recorded walkthroughs](ui-demos.md) for reproducible LCD GIFs of
source loading, workspace execution, and the system inspectors:

![Interactive emulator task inspector](workbench-emulator.png)

## Integration and memory layout

The branch is rebased onto the current shell/tooling stack after master merged
#12. The kernel keeps counted strings, standard `FIND` and `EVALUATE`, canonical
flags, signed arithmetic, checked dictionary writes, and `MAP-FLASH`.
`EVALUATE0` adds transactional workspace cleanup around standard bounded
evaluation; it restores compiler checkpoints and active loop contexts.

The renderer and tables are copied from the page-2 initializer into fixed RAM
at boot. The dictionary ends at `0xF000`, leaving 4 KiB for the data stack.
The 512-entry/4096-byte shell history lives in banked RAM page 2, with scoped
mapping that restores the foreground page. Flash source page 6 remains
reserved for the tooling branch's optional precompiled image format.

The workbench uses text bootstrap. Its resident dictionary spans both fixed
RAM banks, so the current single-RAM-page precompiled snapshot format does not
support this configuration. Do not use `make precompiled-rom` for the workbench
until its image format captures both banks and resident service state.
