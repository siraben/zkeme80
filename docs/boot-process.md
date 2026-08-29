# TI-84+ and zkeme80 boot process

This document relates zkeme80's startup code to the TI-84+ OS 2.55MP retail
boot page. Evidence tags distinguish byte- or trace-backed claims
(`[confirmed]`), public platform behavior (`[standard]`), and interpretations
that still require a device test (`[hypothesis]`).

## Reset and OS handoff

Hardware reset begins on fixed Flash page `3F` at `3F:4000`. The retail boot
code converges at `3F:412C`, configures the mapper and peripherals, and tests
for an installed OS at `3F:4238`. [confirmed]

The installed-OS test is equivalent to:

```text
if byte[ram:0038] != 0xFF and word_le[ram:0056] == 0xA55A:
    jump ram:0053
```

The first condition requires code at the IM 1 vector. The second requires the
little-endian signature bytes `5A A5` at `ram:0056`. A successful check jumps
through `ram:0053`; cryptographic OS validation belongs to recovery and
diagnostic paths, not this direct handoff. [confirmed]

zkeme80 supplies all three values in `src/header.scm`:

```scheme
(jp sys-interrupt)          ; ram:0038
...
(jp boot)                   ; ram:0053
(db (#x5a #xa5 #xff))       ; ram:0056
```

Before this check, the retail code scans the keypad. Holding **DEL** selects
serial recovery, and holding **STAT** selects USB recovery. [confirmed]

## Image layout

The assembler produces a 1 MiB Flash image. Source text is interpreted from
banked Flash during bootstrap.

| Image offsets | Flash page | Contents |
|---|---:|---|
| `0x0000`–`0x3FFF` | `00` | Header, kernel, native Forth words, and `boot.fs` |
| `0x4000`–`0x7FFF` | `01` | `bootstrap-flash1.fs` |
| `0x8000`–`0xBFFF` | `02` | `bootstrap-flash2.fs` and the assembly-time RAM layout |
| `0xC000`–`0xFFFF` | `03` | `bootstrap-flash3.fs` |
| `0x10000`–`0x13FFF` | `04` | `bootstrap-flash4.fs` |
| `0x14000`–`0x17FFF` | `05` | `bootstrap-flash5.fs` |
| `0x18000`–`0x1BFFF` | `06` | Optional verified precompiled dictionary |
| `0xF0000`–`0xF3FFF` | `3C` | Flash unlock and lock trampolines |

The page-`02` RAM layout assigns addresses to system variables, buffers, and
stacks during assembly. `src/boot.scm` clears live RAM at `0x8000`–`0xFFFF`
and does not copy this image slice into RAM. `forth-main` then initializes the
live dictionary pointers and other kernel variables. The label map marks the
slice as RAM so its symbols resolve at the intended CPU addresses. [confirmed]

Normal builds fill page `06` with `0xFF`; precompiled builds place a validated
dictionary image there. Both upgrade targets package pages `00`–`06` and `3C`,
so an ordinary upgrade also removes a stale precompiled image. The retail boot
page `3F` remains intact. [confirmed]

## zkeme80 startup

`src/boot.scm` performs the following sequence:

```text
disable interrupts
select timer rate 6 and map selector 81 into window B
enter sleep with IM 1 enabled

on restart:
    disable interrupts and restore the stack and mapper state
    unlock Flash
    program execution-protection ports
    lock Flash
    select the 15 MHz CPU clock
    enable ON-key and timer-1 interrupts
    clear 0x8000–0xFFFF
    initialize the LCD
    enter the Forth VM
```

The sequence has these hardware-facing properties:

- **Interrupt mode.** `sleep` selects IM 1 before enabling interrupts. The
  handler is at `ram:0038`, matching the fixed vector used by TI-OS.
- **Interrupt mask.** Port `0x03` receives `0x0B`, enabling the ON key and
  timer 1 while preserving power during `HALT`. Port `0x04` receives `0x06`.
- **Interrupt acknowledgement.** The handler clears port-`0x03` latches and
  drains USB events through ports `0x55`–`0x57`.
- **Execution protection.** The code writes ports `0x22`, `0x23`, `0x25`, and
  `0x26` while Flash is unlocked. The retail OS also places a fetched-byte
  sequence before protected writes. Whether every ASIC revision requires that
  sequence for zkeme80's writes remains unverified. [hypothesis]
- **LCD initialization.** zkeme80 sends `05 01 03 17 0B EF` to port `0x10`
  after fixed delays. TI-OS polls the port-`0x02` ready bit before commands.
  The zkeme80 sequence works in supported emulators; its timing margin needs a
  physical-device trace. [hypothesis]
- **Stacks.** Early startup uses `SP = 0x0000`. The Forth VM later places the
  data stack near `0xFFF4` and starts its return stack at `0xC000`.

Except where tagged separately, these properties are confirmed by the
assembled source and retail-boot disassembly. [confirmed]

## Operational constraints

- Soft reset enters zkeme80 through the `0xA55A` installed-OS signature.
- **DEL** and **STAT** retain access to the retail serial and USB recovery
  paths.
- The stock execution bounds permit zkeme80's pages `00`–`06` and its
  RAM-resident Forth code. Code placed on Flash pages `08`–`29` can trigger an
  execution-protection reset. [confirmed]
- The page-`3C` trampolines execute at `3C:4001` and `3C:4017` when page `3C`
  is selected. They write the Flash-control gate on port `0x14`.
- TilEm trace replay begins with mapper values `0x04 = 0x07`, `0x05 = 0x00`,
  `0x06 = 0x3F`, and `0x07 = 0x3F`. `re/analyze_forth_trace.py` uses these
  values as explicit analyzer inputs. [confirmed]
