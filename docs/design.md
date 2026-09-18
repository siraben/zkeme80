# The design and philosophy of zkeme80

zkeme80 is designed to be a *very* sharp-edged tool.  Extremely
powerful in the hands of an experienced user, but easily misused in
the hands of a novice.  Nevertheless, we outline some important design
decisions.

## RAM is volatile
It is likely that the user will crash the operating system at any
point.  This is Forth, after all, and a single hanging `ELSE` in a
word definition or an infinite loop can leave the system stuck.  For
this reason, RAM must be treated as something volatile.  More
permanent data should be stored in flash.

## Expose enough words
One knee-jerk reaction when writing a Forth-based operating system is
to "Forthify" as much as possible, wrapping all the various
subroutines into Forth words.  However you'll notice that only a
fraction of the subroutines are directly accessible from the Forth
interpreter.  This almost gives a userspace/kernelspace separation,
where system calls are words.  This also protects the user from
accessing words that have specific calling conventions, for instance
`unlock-ram`, which must be followed by an assembly call to
`lock-ram`.

## Defining words in assembly or Forth
Many Forth implementations go the way of defining most of it in
itself, compromising speed for portability.  This doesn't make much
sense on the Z80 which is much slower than modern chips, so generally
if one is debating whether to make a word a CODE word or a WORD word,
write it in whatever it faster.  However, we also don't want to spend
more development time on words that are easily writable in Forth, and
where speed isn't completely essential so a balance must be striken.
So when doubt, write it in the language that more succinctly expresses
the behavior of the word.

## Named objects on block storage

The original plan exposed only numbered 1024-byte blocks. The current system
adds a flat, case-sensitive namespace and text, source, and binary types over
1024-byte flash records. Programs share objects by name rather than depending
on their physical address. `FS-PUT` explicitly saves a new revision and
`FS-LOAD` explicitly evaluates source through the normal interpreter.

The append-only journal commits each record after writing its contents. It
has finite capacity and no automatic compaction yet; replacements and deletion
markers also consume records. See [storage](storage.md) for the format, result
codes, and buffer-lifetime rules. Conventional 1024-byte Forth source screens
and a source editor remain possible interfaces above the storage mechanism.

## A shared resident workspace

Leaving the shell keeps the dictionary, objects, and task records available to
the desktop and other applications. RAM remains volatile across resets; saved
source is the reproducible form of a definition. Cooperative jobs execute one
callback at a time when input polling or an application calls `YIELD`. They
must return promptly and preserve the shared interpreter's conventions.
Exceptions stop a failed job, but arbitrary memory writes or infinite loops
are outside that protection. See [tasks](tasks.md).

## Modules and physical pages

The ROM manifest in `src/modules.scm` names logical modules and lists their
source files in dependency order. It assigns flash pages while skipping
reserved regions, joins each module's source with one EOF, generates module
page constants, and generates transitions between resident modules. A module
must fit one 16 KiB source page; split a larger source group into named modules.
This removes page numbers from ordinary module consumers without pretending
that a source-page budget is also a compiled dictionary-space budget. See the
[module guide](module-layout.md) and [architecture roadmap](os-design.md).

## No security
Security is hard.  So let's not have any.  It is unlikely that the
calculator will be used for cryptographic applications (it is slow,
after all), or run external code via network/link connections.  But of
course if the user desires, extra protection may be implemented,
perhaps a password being asked on boot, and so on.

## Standardize words when possible
There is considerable debate among Forth programmers whether or not to
make words ANS standard-conforming.  Here's my take on it: standards
exist for a reason.  I should be able to copy and paste code written
with only CORE words with the expectation that it will work
flawlessly.  The programmer should not need to care whether the Forth
system is little endian or big endian (but *should* worry about the
max unsigned integer size, etc.), and should not write code with
environmental dependencies if portability is in mind.
