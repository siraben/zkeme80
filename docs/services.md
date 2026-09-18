# Resident services

`OS-ABI` identifies the append-only service table. `SERVICE@` returns an
execution token or zero for an invalid ID; `OS-CALL` invokes it, throwing 20
for an invalid ID. Service tokens and the dictionary reside outside the
switchable source-page window.

`WITH-PAGE ( xt page -- ior )` validates flash pages 0 through 63, runs the
callback, and restores the prior raw bank selector after either return or
`THROW`. Nested calls are supported. The callback must reside outside the
banked window; raw `(BANK!)` remains an unrestricted low-level primitive.

`EVALUATE0 ( zaddr -- ior )` evaluates a zero-terminated source while preserving
the caller's input pointer, numeric base, and compilation state. Its frame
uses the return stack, so evaluation can nest. Normal program results remain
on the data stack beneath the returned status. A thrown exception restores
the data stack through `CATCH` and returns its exception code.

Source must finish in the compilation state in which evaluation began. A
source entered while interpreting must finish its definitions; otherwise it
returns 22. An error that leaves such a source compiling rolls its dictionary
back to the entry `HERE`/`LATEST` snapshots. This discards partial definitions
and any other definitions added in the same failed evaluation. Successful,
balanced definitions remain available. Evaluation from compilation state is
permitted if the source ends in that same state.

Evaluation is not a general transaction: completed definitions before an error
that leaves the compiler interpreting, arbitrary memory writes, output, and
flash updates are retained. Numeric input currently accepts decimal digits;
`BASE` controls numeric output. Tokens may contain at most 31 bytes; longer
tokens return 19 without overflowing the parser buffer.

`tests/test-core.py` exercises service lookup, bank restoration, invalid bounds,
nested evaluation, parser limits, failed-definition cleanup, and interactive
multiline compilation and workspace persistence. Run it with a headless TilEm
binary via `--emulator` or `$TILEM`; use `nix develop --command` if needed for
the emulator's dependencies.
