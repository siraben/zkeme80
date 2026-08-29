# zkeme80 reverse-engineering tools

This directory adapts the workflow developed in the TI-84 Plus OS
reverse-engineering project (`~/ti84p-re`) to zkeme80 itself.  Because
the OS source lives here, we get something TI's ROM cannot offer:
every label and every Forth word is emitted by the assembler with its
exact address.  These scripts load that information into Ghidra and
into dynamic traces so that "what is the CPU doing" always resolves to
a named Forth word or kernel routine.

## Data flow

```text
make build
  └─► src/zkeme80.rom               1 MiB flash image
  └─► src/zkeme80.ram-labelmap.json labels + forth_words + RAM layout

Ghidra (flat import of the ROM)
  ├─ Zkeme80ApplyLabelmap.java      symbols + functions from the JSON
  ├─ Zkeme80AnnotateForth.java      decompile threaded colon definitions
  └─ Zkeme80ForthCallgraph.java     word-to-word call graph (CSV)

TilEm headless (dynamic)
  ├─ macros/*.macro                 boot / menu navigation scenarios
  └─ analyze_forth_trace.py         TLMT v2 trace → Forth word hits
```

## Address spaces

Two coordinate systems matter, exactly as with TI-OS:

* **Image offset** — byte position in `zkeme80.rom` (and in the JSON).
  Page N occupies `[N*0x4000, (N+1)*0x4000)`.  All labelmap addresses
  are image offsets.
* **CPU address** — what the Z80 sees.  Page 0 is permanently mapped at
  `0000h–3FFFh` and other pages bank into `4000h–7FFFh`.  The image
  slice `[8000h,C000h)` is an assembly-time address template for RAM
  `8000h–BFFFh`; boot clears the actual RAM, so only its labels—not its
  emitted initial bytes—describe runtime state.

All Forth code words (`defcode`/`defword` in `src/forth.scm`) live in
page 0, where image offset equals CPU address, so threaded-code
pointers compare directly against labelmap addresses.  Words compiled
from `.fs` sources at boot live in RAM and only exist at runtime.  The
static JSON can name their backing RAM variables but cannot name those
runtime-created dictionary entries; use `tilem_trace.py --forth-rom`
when you need reconstructed RAM-word transitions.

## Ghidra usage

The scripts are plain GhidraScript Java sources (Ghidra 12 removed
Jython; Java keeps them working everywhere).  Headless batch — adjust
the `analyzeHeadless` path to your install:

```sh
GHIDRA=/path/to/ghidra/support/analyzeHeadless
mkdir -p /tmp/zk80-proj
$GHIDRA /tmp/zk80-proj zkeme80 \
  -import src/zkeme80.rom \
  -processor z80:LE:16:default \
  -scriptPath re/ghidra \
  -postScript Zkeme80ApplyLabelmap.java src/zkeme80.ram-labelmap.json \
  -postScript Zkeme80AnnotateForth.java src/zkeme80.ram-labelmap.json \
  -postScript Zkeme80ForthCallgraph.java src/zkeme80.ram-labelmap.json /tmp/callgraph.csv
```

Run all three in one pass: `ApplyLabelmap` also force-disassembles at
every code label so the later stages see flows.

`Zkeme80ApplyLabelmap.java` names every assembler label (prefixing RAM
symbols with `ram_`) and creates functions for kernel routines and
Forth code words.  `Zkeme80AnnotateForth.java` walks the `call docol`
threads of every colon definition and writes the full word sequence
(resolving code-field addresses back to word names) as a plate
comment plus per-cell end-of-line comments and DATA references, so
threaded Forth reads like source inside Ghidra.
`Zkeme80ForthCallgraph.java` attributes page-0 code (stopping before
the embedded bootstrap source and flash padding) to its nearest
preceding named routine and emits caller,callee edges from CALL flows
and threaded-cell references as properly escaped CSV.  Note: `rst 38h`
traps show up as calls to `swap-sector` (that symbol is equated to
`0x38`, the interrupt vector) — filter that edge when summarizing.

## Dynamic tracing

Requires the [tilem-headless](https://github.com/siraben/tilem-headless)
fork (`nix build .#tilem` there).  Capture a full instruction trace
while driving the OS with a macro:

```sh
TILEM=~/Git/tilem-headless/result/bin/tilem2
$TILEM --headless --rom src/zkeme80.rom --model ti84p --normal-speed \
  --reset --macro re/macros/run-test-suite.macro \
  --trace /tmp/zk80.trace --trace-range all
python3 re/analyze_forth_trace.py /tmp/zk80.trace \
  src/zkeme80.ram-labelmap.json --forth-only --top 30
```

The analyzer replays the mapper from the trace's OUT instructions
(ports 4/5/6/7), including TI-84+ mode-1 even/odd pairing, so banked
window PCs resolve correctly.  It reports execution counts per static
kernel label and per page-0 **Forth dictionary word**; static RAM
labels in `$8000–$BFFF` resolve when selector `$81` maps physical RAM
page 1.  Use a full trace here: a ring/backtrace can discard the early
mapper writes and does not preserve their resulting state in its
header.  The analyzer memory-maps the trace rather than reading the whole
multi-gigabyte file into the Python heap.  The same fork's
`tools/tilem_trace.py` offers runtime Forth
dictionary reconstruction, key timelines, and DROP-underflow
detection for deeper forensics.

Run the mapper/resolver regression tests with:

```sh
python3 re/test_analyze_forth_trace.py -v
```

`re/macros/boot-only.macro` just boots and screenshots;
`re/macros/run-test-suite.macro` navigates the main menu to the test
suite button, captures the final tally (265/265 as of this writing)
and the menu it returns to — handy as a smoke test after any kernel
change.

The macro waits for the final tally before sending the unload key.
`KEY` first flushes held keys, so sending input while the suite is
still running—or holding a key before it reaches `PAUSE`—will be
discarded by design.  With the current 35-second normal-speed wait, a
single ENTER completes the full lifecycle: tests → unload via
`FORGET TEST-SUITE-START` → menu.
