# A Z80 assembler in Scheme

**TLDR:** `assembler3.scm` is the current best working version, no
file dependencies, but your Scheme implementation needs to be recent
enough to have the required modules.

## Motivation
Existing assemblers for the Z80 (and other instruction sets) are
extremely lacking in their macro abilities.  This limits their
abstraction abilities, especially when the programmer wishes to create
data structures in assembly through macros (e.g. a linked list of
dictionary entries).  What better macro system exists than a Lisp like
Scheme?

## Files included
There are currently three candidate assemblers:

- `assembler.scm` parses assembly code directly into binary via
  monadic parsing, using an opcode data oriented approach to
  assembling (i.e. separating code from data).  `parse-asm-file`
  accepts a filename leading to Z80 assembly code.
- `assembler2.scm` parses assembler code into a Lisp-style via monadic
  parsing, `parse-asm-file2` accepts a filename leading to assembly
  code.
- `assembler3.scm` assembles assembly-style Scheme s-exps into
  binary.  **This is the most feature complete**.  Simply run `(load
  "assembler3.scm")` into your Scheme REPL and run`(assemble-prog
  sample-prog)` to see the binary data.  Run `(assemble-to-file2
  sample-prog "out.bin")` to write a binary file.

It is planned that `assembler2.scm` and `assembler3.scm` will be used
in conjunction, as `assembler2.scm` performs the transformation from
text to s-exps, and `assembler3.scm` transforms the s-exps into binary
data.

## Design of `assembler3.scm`
`assembler3.scm` heavily relies on pattern matching.  The program
counter is implemented as a mutable Scheme object `*pc*`.  Labels are
kept in a global alist `*labels*`.  To allow for the use of jumps that
refer to labels declared after it, we must use multiple passes:

### Pass 1
Run each expression through `assemble-expr` (automatically checking
for malformed expressions).  `assemble-expr` returns a record type
that has the following fields (for a normal instruction):

| Record entry | Type      | Description                                       |
| :-:          | :-:       | :-:                                               |
| `cycles`     | `integer` | The number of cycles this instruction takes.      |
| `length`     | `integer` | The length of the instruction, in bytes.          |
| `gen-instr`  | `lambda`  | Thunk that computes the actual instruction bytes. |

The use of converting expressions into record types like this allows
us to easily compute the length of the program (and resolve look ahead
labels), and allows macros to be easily be used later.  For instance,
we could add a new "instruction" called `fill`, and use it like this
`(fill c b)`.  We modify the assembler to match this new instruction
and generate `c` bytes of the byte `b`.  Using thunks also allows for
directives that depend on external state (e.g. some sort of linked
list, perhaps).

### Pass 2
Once the program makes it through Pass 1, we resolve the absolute
addresses of the labels.  All instructions/macros that generate bytes
are required to have their length known at assembly time, so we simply
adjust the value of `*pc*` based on the `org` directive and keep a
running total of the number of bytes up to a label, then add it to the
global variable `*labels*`, which is an alist consisting of
expressions of the form `((label-name . addr) ...)` where `label-name`
is a symbol and `addr` is an unsigned 16-bit number.  Now we just
force each instruction's thunk, and the result is flattened into a
list of unsigned numbers representing the binary data.


## Debugging
The debugging process is pretty simple.  One just has to write a valid
Z80 assembly program in my s-exp format and run it through a
disassembler then compare the output.  If you're feeling particularly
brave you may skip this step and try your program out on a Z80 chip.

## Limitations
(Maybe) I should have used Haskell.  Type errors are very scary, and
there was a lot of trial and error getting the necessary safety checks
in place so that the assembler could signal an error before the type
system complained.  This very well could be a hybrid Haskell/Lisp
program later, as the Parsec library would greatly improve parsing.

