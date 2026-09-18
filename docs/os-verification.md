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
| Forth language | 266/266 original suite assertions; explicit unloading still works |
| RAM / parser | 113 target assertions: allocation bounds, atomic defining words, exact-fit strings, quoted EOF, missing/unknown names, fixed RAM banks, and checked flash mapping |
| Core services | 128 assertions: scoped banks, service IDs, nested evaluation, token bounds, file-load stack balance, unfinished definitions, and caller compilation recovery |
| Workspace | Multiline and bracketed definitions, error/exit rollback, deliberately hidden completed words, and definitions retained across desktop visits |
| Scheduler | 75 target assertions, including bank changes on normal/error returns; counters advance during desktop and shell idle waits |
| Storage | 74 target assertions, 12 cold-boot checks, and 12 damaged-payload checks; independent flash record verification |
| Desktop | Empty/multiple objects, source load/error feedback, selection and paging bounds, task lifecycle, page restoration, service paging, and 29 exact LCD comparisons |
| Shell rendering | Five exact 96x64 pixel comparisons, including error recovery |

For the original language suite, choose **Test suite** in the workbench or run
`tests/full-suite.macro` using the extended emulator. The screen-model check
uses `tests/shell-screen.macro` followed by
`nix develop --command python3 tests/verify-shell-screen.py`.

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

## Integration with current master

This feature branch extends the older shell kernel at `9d77cd5`. Its 266-case
language suite predates the ANS94 CORE work merged to master as `77105f5`.
The branch is a draft integration: reconcile counted strings, parser/source
contracts, `FIND`, true flags, signed arithmetic, and `KEY`/`RAW-KEY`, then
preserve and pass master's 286-case conformance suite before merging. The
emulator results above validate this branch, not that pending ABI port.
