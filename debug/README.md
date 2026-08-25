# zkeme80 reverse-engineering tools

This directory adapts the workflow developed in the TI-84 Plus OS
reverse-engineering project (`~/src/ti84p-re`) to zkeme80 itself.  Because
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
  `0000h–3FFFh`, other pages bank into `4000h–7FFFh`, and the image
  slice `[8000h,C000h)` is the initial contents of RAM `8000h–BFFFh`
  (copied by the boot code), so its labels double as RAM addresses.

All Forth code words (`defcode`/`defword` in `src/forth.scm`) live in
page 0, where image offset equals CPU address, so threaded-code
pointers compare directly against labelmap addresses.  Words compiled
from `.fs` sources at boot live in RAM and only exist at runtime;
catch them with the dynamic tracer.

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
  -scriptPath debug/ghidra \
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
`Zkeme80ForthCallgraph.java` attributes every page-0 instruction to
its nearest preceding named routine and emits caller,callee edges
from CALL flows and threaded-cell references as CSV.  Note: `rst 38h`
traps show up as calls to `swap-sector` (that symbol is equated to
`0x38`, the interrupt vector) — filter that edge when summarizing.

## Dynamic tracing

Requires the [tilem-headless](https://github.com/siraben/tilem-headless)
fork (`nix build .#tilem` there).  Capture a full instruction trace
while driving the OS with a macro:

```sh
TILEM=~/Git/tilem-headless/result/bin/tilem2
$TILEM --headless --normal-speed --rom src/zkeme80.rom --model ti84p \
  --reset --macro debug/macros/run-test-suite.macro \
  --trace /tmp/zk80.trace --trace-range all
python3 debug/analyze_forth_trace.py /tmp/zk80.trace \
  src/zkeme80.ram-labelmap.json --forth-only --top 30
```

The analyzer replays the mapper from the trace's OUT instructions
(ports 4/5/6/7), so banked-window PCs resolve correctly, then reports
execution counts per kernel label and per **Forth dictionary word** —
page-0 labels are image offsets, RAM `$8000–$BFFF` resolves when RAM
page `81` is banked.  The same fork's `tools/tilem_trace.py` offers
RAM reconstruction, key timelines, and DROP-underflow detection if you
need deeper forensics.

`debug/macros/boot-only.macro` just boots and screenshots;
`debug/macros/run-test-suite.macro` navigates the main menu to the test
suite button, captures and acknowledges its introduction, and captures the terminal
`286/286` tally plus a logical RAM dump while the suite waits for a key.
It then acknowledges the result, verifies the transient suite unloads, and
captures the restored main menu.

Verify the dump rather than relying on pixels alone:

```sh
python3 debug/verify_test_ram.py \
  debug/macros/SUITE_RAM zkeme80.ram-labelmap.json \
  --before debug/macros/BEFORE_SUITE_RAM \
  --after debug/macros/AFTER_SUITE_RAM
```

`KEY` returns cooked character input.  The calculator-specific `RAW-KEY`
and `KEYC` extensions expose blocking and edge-polled keypad scan codes.
Blocking input now waits for a physical release through the level scanner;
menu polling retains edge semantics.
