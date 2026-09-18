# Resident services

`OS-ABI` identifies the append-only service table. `SERVICE@` returns an
execution token or zero for an invalid ID; `OS-CALL` invokes it, throwing 20
for an invalid ID. Service tokens and the dictionary reside outside the
switchable source-page window.

`MAP-FLASH ( page -- flag )` maps a flash page 0–63 into bank A, rejecting
invalid 16-bit selectors without changing the bank. `SET-RAM-MEMA` remains a
compatibility alias for the misleading original name. Most clients should
borrow a bank through `WITH-PAGE` instead of leaving it changed.

`WITH-PAGE ( xt page -- ior )` validates flash pages 0 through 63, runs the
callback, and restores the prior raw bank selector after either return or
`THROW`. Nested calls are supported. The callback must reside outside the
banked window; raw `(BANK!)` remains an unrestricted low-level primitive.

`EVALUATE0 ( zaddr -- ior )` evaluates a zero-terminated source while preserving
the caller's input pointer, numeric base, and compilation state. Its frame
uses the return stack, so evaluation can nest. Normal program results remain
on the data stack beneath the returned status. A thrown exception restores
the data stack through `CATCH` and returns its exception code.
The source buffer must remain valid throughout evaluation. Interpretive `S"`
uses temporary storage at `HERE`, which compilation can overwrite; compile
the string in a helper word or copy it into a separate buffer before evaluating
source that adds dictionary entries. `FS-LOAD` already uses a separate buffer.

Source must finish in the compilation state in which evaluation began. A
source entered while interpreting must finish its definitions; otherwise it
returns 22. Opening `[` does not make an unfinished definition complete:
new unfinished entries are also checked, including entries below later definitions.
The unfinished flag is distinct from visibility, so completed private words
can remain hidden.
An error that leaves such a source unfinished rolls its dictionary
back to the entry `HERE`/`LATEST` snapshots. This discards partial definitions
and any other definitions added in the same failed evaluation. Successful,
balanced definitions remain available. Evaluation from compilation state is
permitted if it stays inside the caller's definition. Closing that definition,
even when followed by `]`, returns 22. A failed evaluation entered while
compiling restores the entry dictionary pointer, latest word, and header flags,
so partial generated code cannot corrupt the caller's unfinished definition.
Bracket interpretation within a definition is part of that same compiler
transaction even though `STATE` is zero; recovery restores that zero state.

Evaluation is not a general transaction: completed definitions before an error
that leaves no unfinished definition, arbitrary memory writes, output, and
flash updates are retained. Numeric input currently accepts decimal digits;
`BASE` controls numeric output. Tokens may contain at most 31 bytes; longer
tokens return 19 without overflowing the parser buffer.
An unterminated quoted string returns 18. Missing parsed names return 16;
an unresolved name required by a defining or lookup word returns 1.

`tests/test-core.py` exercises service lookup, bank restoration, invalid bounds,
nested evaluation, parser limits, failed-definition cleanup, and interactive
multiline compilation and workspace persistence. Run it with a headless TilEm
binary via `--emulator` or `$TILEM`; use `nix develop --command` if needed for
the emulator's dependencies.

The dictionary grows through the two fixed RAM windows, from `H0` to the
exclusive `DP-LIMIT` (`0xF000`). The top 4 KiB is reserved for the data stack;
the return stack has its own reserved area below `H0`. `UNUSED` measures this
actual dictionary budget. `ROOM`, `ALLOT`, `,`, `C,`, `CREATE`, and `DOES>` check
capacity before their ordinary writes and throw 8 when it is exhausted.
`VARIABLE`, `CONSTANT`, and `VALUE` reserve their complete definitions before
changing the dictionary. Both compiled and interpreted `S"` check space for
the string and its terminator before copying any bytes; unterminated quotes
stop at EOF. Interpreted string results remain transient.
Negative `ALLOT` can reclaim space within the dictionary bounds. A missing
`CREATE`/colon name throws 16. Raw memory stores and direct writes to `DP`
remain sharp tools; these guards are not memory protection or stack isolation.

`tests/test-memory.py` verifies these limits and distinct fixed RAM above
`0xC000` with 154 target assertions.
