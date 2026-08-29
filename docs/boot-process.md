# The TI-84+ boot process and how zkeme80 boots

This document cross-references zkeme80's boot path with the
hardware-level boot behavior documented in the separate TI-84+ OS
reverse-engineering project (OS 2.55MP retail boot page).  Claims about
TI's retail boot are tagged like there: `[confirmed]` (read from the
retail boot page / traced), `[standard]` (publicly documented), or
`[hypothesis]`.

## 1. What happens between reset and an OS

On reset the Z80 starts executing the **boot sector**, flash page `3F`,
mapped at logical `8000h–BFFFh`.  Emulators disagree on the exact reset
PC (TilEm starts at `8000`; Wabbitemu at `0000`), so an OS must not
depend on which window the boot page appears in `[confirmed]`.

TI's retail boot stub then:

1. Programs ports `04`/`06`(/`07`) explicitly instead of trusting reset
   mapper defaults.
2. Runs a ~0.29 s delay loop, sets `IM 1`, `SP := 0xFFC5`.
3. Reprograms every mapper window; final runtime map is window A
   (`4000h–7FFFh`, port 6) = flash page `3F`, windows B/C = RAM pages
   `[standard]`.
4. Initializes link assist, bus-timing wait states, execution
   protection bounds, GPIO/USB control `[confirmed]`.
5. Scans the keypad once: DEL held → serial recovery, STAT held → USB
   recovery, otherwise → the *installed-OS check* `[confirmed]`.

### The installed-OS handshake

The fast reset path checks exactly two things on page 0 before handing
off `[confirmed]`:

```z80
ld a,(0x0038); cp 0xFF      ; IM1 vector must contain real code
ld hl,(0x0056); ld bc,0xA55A; sbc hl,bc
jp z,0x0053                 ; -> jump to the installed OS entry
```

i.e. byte `0x38` must not be blank flash, the little-endian word at
`0x0056` must equal `A55Ah`, and the `JP` at `0x0053` receives control.
(Cryptographic validation via `_CheckOSValidated` only runs on the
recovery/diagnostic paths, not on this fast path.)

**zkeme80 satisfies this handshake**: `header.scm` places
`jp sys-interrupt` at `0x38`, `jp boot` at `0x53`, and the bytes
`5A A5 FF` at `0x56`.  With these bytes, a calculator whose retail
boot page is intact soft-resets straight into zkeme80.  (Historically
zkeme80 shipped `FF A5 FF` here — the word read `A5FFh`, the check
failed, and real hardware fell into the *"Waiting… Please install
operating system now"* recovery screen.  TilEm never noticed because a
blank boot page reaches page 0 through a different path.)

## 2. The zkeme80 image layout

| Image offset | Contents | Runtime home |
|---|---|---|
| `0x0000–0x3FFF` | kernel: header, `boot`, `sys-interrupt`, Forth VM + code words | flash page 0, hardwired at `0000h–3FFFh` |
| `0x4000–0x7FFF` | `bootstrap-flash1.fs` source text | interpreted from banked flash |
| `0x8000–0xBFFF` | assembly-time RAM layout: Forth variables, input buffers, stacks region marker | address template only; boot clears runtime RAM and `forth-main` initializes live state |
| `0xC000–0xFFFF` … | more `.fs` source (flash2–5) | interpreted from banked flash |
| `0x18000–0x1BFFF` (page `06`) | optional verified precompiled dictionary; erased in ordinary builds | copied to RAM or rejected before the text bootstrap |
| `0xF0000` (page `3C`) | `wtf-prog`: the unlock/lock-flash trampolines | banked by `unlock-flash`/`lock-flash` |

The `mktiupgrade`-produced installer writes pages `00` through `06` plus
`3C`; writing page `06` also clears any stale precompiled image during an
ordinary upgrade.  It leaves the retail boot page `3F` intact, which is why the
handshake above matters.

## 3. zkeme80's boot sequence (`src/boot.scm`)

```text
boot/shutdown:  DI ; OUT(4)=6        slowest timer tick, independent mapping
                OUT(7)=0x81          window B -> RAM page 81
                LD SP,0 ; CALL sleep IM1 gets set inside sleep
restart/reboot: OUT(0E)=3, OUT(0F)=0 extended flash bits
                unlock-flash ... execution-protection ports 22/23/25/26 ... lock-flash
                OUT(20)=1            CPU clock 15 MHz
                OUT(3)=0b0001011     interrupt mask: ON + timer1 (+bit3 keep-power-in-HALT)
                zero RAM $8000-$FFFF
                LCD init writes to port 10 with fixed delays
                JP into the Forth main loop
```

Cross-checked against TI's own initialization `[confirmed unless noted]`:

* **Interrupt mode**: `sleep` executes `IM 1` before `EI`, and the
  handler lives at the fixed `0038h` vector — same model as TI-OS,
  which also runs IM 1 with its ISR at `ram:0038`.
* **Interrupt mask `0x0B`** equals TI's normal-work mask (ON key +
  standard timer 1 enabled, bit 3 keeping power during `HALT`).  Timer
  1 ticks at ~107.8 Hz with the `OUT(4)=6` rate bits, matching TI's
  choice.
* **ISR acknowledgement** uses the clear-on-zero dance (`IN(3)`,
  `RES n`, `OUT(3)`, `SET n`, `OUT(3)`), equivalent to TI's
  latch-clearing sequence.  USB events are drained separately through
  port `55h`/`56h`/`57h`; note TI's ISR treats port `55h` as active-low
  in bits 0–4 and port `56h` as an event bitmap that port-3 acks do
  *not* clear — an OS that ignores USB will spin in its ISR, so
  zkeme80's drain is required, not optional.
* **Execution-protection ports** (`22/23/25/26`) are written while
  flash is unlocked.  On real hardware TI additionally gates each such
  write behind the fetched-byte pattern `00 00 ED 56 F3 D3`
  (`nop nop im 1 di out`) executed from specific physical pages
  `[confirmed for TI-OS]`; it is `[hypothesis]` whether the ASIC
  enforces this for all writers.  Even if the writes silently fail,
  the stock bounds still permit zkeme80's use: pages `00–07` are
  executable and default mode 0 makes odd RAM pages fully executable,
  which covers both the page-0 kernel and RAM-resident Forth code.
* **LCD init** writes the command sequence `05 01 03 17 0B EF` to port
  `10h` guarded by fixed delays rather than polling the ready bit on
  port `02h`.  TI polls readiness before every command and starts with
  `40h`; zkeme80's sequence works on emulators and has worked on
  hardware `[standard/hypothesis]` — tightening this would be cheap.
* **Stack**: early boot uses `SP=0` (wrapping into `$FFxx` RAM, safe
  because RAM is cleared after interrupts are masked); the Forth VM
  then installs `SP=0xFFF4`-ish and return stack at `IX=$C000`
  (`forth-shared-header`).  TI-OS uses `SP=0xFFF7` steady-state.

## 4. Practical consequences

* **Soft resets land in zkeme80** thanks to the `A55Ah` handshake — no
  key needed, matching TI-OS UX.
* **DEL/STAT at reset still reach TI's recovery paths** (serial/USB
  OS receive).  That is desirable: it preserves a recovery channel.
* **Do not ship code on flash pages `08h–29h`**: with the stock
  protection bounds a fetch there resets the machine.  zkeme80 keeps
  everything in pages `00–06` plus the `3C` trampoline page — well
  inside the legal set.
* **The `wtf-prog` page (`3C`)** provides the two routines reached at
  logical `4001h`/`4017h` when page `3C` is banked: they perform the
  protected `OUT (14h)` gate write (unlock/lock flash) using the same
  instruction-shape the ASIC expects.
* When tracing under TilEm headless, seed the mapper replay with
  `port4=0x07, port5=0, port6=0x3F, port7=0x3F` (TilEm reset state);
  `re/analyze_forth_trace.py` does this automatically.
