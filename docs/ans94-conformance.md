# ANS Forth 94 CORE audit

zkeme80's normal persistent dictionary contains all 133 required names
from the ANS Forth 94 CORE word set.  The inventory covers the native
kernel in `src/forth.scm` and the normal page-1 bootstrap in
`src/bootstrap-flash1.fs`; it excludes the
on-demand test vocabulary on page 4 and the transient page-5 demo.

Name coverage is only the first check.  The audit also corrected the
previously incompatible behavior of signed comparisons and arithmetic,
division, flags, counted strings and dictionary lookup, defining words,
loops and `LEAVE`, parsing and input sources, exception exits, memory
movement, numeric output, and character input.  The on-device regression
suite's final verified tally is 286/286, recorded by the headless TilEm
macro together with a logical RAM dump while the result screen waits for a
key.  The macro then acknowledges the result, checks that the transient
dictionary is reclaimed, and returns to the main menu.
That suite includes targeted cases for these changes, but it remains a
project regression suite rather than an independent standards
certification.

## Environmental dependencies

- A cell, address, and execution token are 16 bits.  Signed cells use
  two's-complement representation; unsigned cells range from 0 through
  65535.
- A character and address unit are 8 bits.  Character addresses advance
  by one byte, and `CHAR+` and `CHARS` therefore have no scaling effect.
- Cell access is little-endian.  The Z80 permits unaligned cell access,
  so `ALIGN` is a no-op and `ALIGNED` returns its input unchanged.
- `/MOD`, `/`, `MOD`, `*/MOD`, and `*/` use symmetric division, rounding
  the quotient toward zero.  `SM/REM` exposes the same model and
  `FM/MOD` provides floored division explicitly.
- Dictionary names are limited to 31 characters.  `WORD` returns a
  counted string with capacity for 255 characters; a longer field is an
  ambiguous condition and is safely truncated while the remainder is
  consumed.
- The pictured numeric-output buffer holds 128 characters; `HOLD` throws
  the standard overflow exception before crossing its lower bound.  Input
  sources and strings use byte counts; no trailing NUL is part of the
  standard `addr u` contract.
- Input sources are text and may not contain an embedded zero byte.  The
  private parser uses zero as its end-of-source sentinel, so a bounded
  `EVALUATE` string containing NUL is truncated at that byte.
- `ENVIRONMENT?` conservatively reports every query as unknown.  Programs
  that require a particular environmental attribute must use the values
  documented here rather than discovering them through that word.
- Numeric text conversion honors `BASE` from 2 through 36 and accepts an
  optional leading minus sign.  Values outside the 16-bit cell range wrap;
  portable programs must avoid that overflow.

## Input and calculator extensions

`SOURCE`, `>IN`, `WORD`, `FIND`, `EVALUATE`, `ACCEPT`, and `KEY` use their
standard data representations and stack contracts.  `KEY` returns a
cooked input character.  The calculator-specific `RAW-KEY` and `KEYC`
extensions return keypad scan codes for applications that need the
physical matrix.  `SOURCE!` and `CSTRING-SOURCE` are system extensions
used by the built-in flash-page loaders.  `CSTRING-SOURCE` requires a
NUL terminator within the mapped flash page; it is not a general bounded
string interface.

`EXPECT` is retained as the obsolescent line-editor interface and records
the received count in `SPAN`; `ACCEPT` is the portable interface.  The
editor's visual behavior depends on the TI-84+ display and keypad.  Its
cursor and boundary regressions require a dedicated emulator scenario and
are not counted by the stack-only suite yet.

## Scope of the claim

The system contains many non-CORE names.  Some, such as `CATCH`, `THROW`,
`CASE`, `VALUE`, `REFILL`, `NIP`, and `TUCK`, belong to optional standard
word sets.  Others implement graphics, flash, interrupts, raw keypad
access, or zkeme80's threaded compiler.  Their presence does not imply
that the corresponding optional word set is complete.

In particular, this audit makes a CORE claim, not a claim of a complete
Block, Double-Number, Exception, Facility, File-Access, Floating-Point,
Locals, Memory-Allocation, Programming-Tools, Search-Order, or String word
set.  Portable programs should restrict themselves to CORE plus explicitly
documented extensions.
