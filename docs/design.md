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

## No files
This Forth implementation will implement a block-based system, where
each block is 1024 bytes.  This keeps things simple.  The user can
specify exactly when to save their work and which block to load.
Furthermore, this allows the ability for the user to extend the
system.  Searching within blocks can be implemented, and it would be
trivial to enumerate the list of blocks.  Block names can be aliased
via constants as well, so the initial block might serve as the block
that allows the user to choose which block to load next, and so on.

This also allows for relatively easy facilities later on to backup and
restore state, single the only mutable state will be in the blocks.

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
