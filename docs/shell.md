# Designing a good shell

## Current implementation

Flash page 5 now starts an interactive Forth REPL.  Alphabetic input is
the default; press `2ND` before a digit or arithmetic symbol.  `DEL`
backspaces, `HELP` prints the on-device summary, and `BYE` unloads the
transient shell words and returns to the menu.

The editor accepts up to 128 bytes and wraps across display rows.  Left and
right move through the complete buffer, including across visual row
boundaries; typed characters overwrite at the cursor, and `DEL` removes the
character before it.  The filled block cursor follows the edited byte.
Command output is preserved between prompts, and the framebuffer scrolls by
one text row whenever the wrapped field reaches the bottom.

Up and down navigate a circular command history.  It has 512 entry slots and
a 4096-byte shared text ring: all 512 one-character commands fit, while long
commands evict the oldest entries once their combined text fills the ring.
Moving down past the newest entry restores the draft that was present before
history navigation.

On the calculator, alphabetic legends are direct and `2ND` selects numbers
and punctuation.  The project TilEm launcher maps every printable desktop
ASCII key into those physical key sequences; desktop letters normalize to
uppercase because the Forth shell is uppercase-oriented.

## Roadmap

A good shell is the heart of an operating system.  Let's make
something that's easy to use and is inspired by the TI operating
system (TI-OS).  What does TI-OS do well?  I think it comes down to
the following things.
    
- Cursor movement
  - Fast cursor movement.
    - Go to the beginning of the line with `2ND <-`.
    - Go to the end of the line with `2ND ->`.
  - `ALPHA` locking (by pressing `2ND ALPHA`).
- Modal editing
  - Pressing `CLEAR` clears the current input line.
  - User can switch between `ALPHA`, `2ND` and normal key input
    modes.
  - Overwrite mode by default.
  - Can go into insert mode through `2ND DEL`, in which the cursor
    changes to an underscore and point (an Emacs terminology) is
    placed just before it.
    - On input, the field shifts to the right by 1.
  - Insert mode changes `DEL` to delete the character just after point;
    overwrite mode keeps its current backspace behavior.
- Command history
  - Pressing `ENTER` allows you to paste into the current input the
    currently highlighted expression, either entry or result.

But what can we do better?  Let's imagine that instead of switching
input modes by pressing `2ND or ALPHA`, these keys are modifier keys
instead.  For instance, we can detect the pressing of `2ND-(` and
translate it to the character `K` directly.  Or, we may make
alphabetic input the default input mode and simultaneously holding
`2ND` would allow us to access the numbers.

What to do with the keys that aren't mapped to a printable character?
The five keys just below the display, and keys like `X,T,θ,n`, `STAT`
, `MODE` or `CLEAR`?  We should assign them special bindings, or maybe
even act as an additional modifier.

One of the tricky challenges ever single I started working on zkeme80
was the lack of a screen scroll.  To scroll the screen, we must detect
that `CUR-COL` and `CUR-ROW` have both exceeded the limit, and this
involves checking its value after every `EMIT` or `PLOT-STRING`.  Of
course, we don't always want to scroll automatically, for instance if
we are drawing an editor using ASCII characters.  An alternative
approach would be to use vectored execution.  So when a call to EMIT
is made, scrolling is automatically handled for us based on what word
is being used.

Actually, could we take it one step further and allow any word to be
used?  This would truly make `EMIT` a generic output device.  In this
way, we could perform automated logging, or storing things in RAM to
be pulled out later by screenshots, and so on.
