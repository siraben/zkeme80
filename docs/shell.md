# Designing a good shell

A good shell is the heart of an operating system.  Let's make
something that's easy to use and is inspired by the TI operating
system (TI-OS).  What does TI-OS do well?  I think it comes down to
the following things.
    
- Cursor movement
  - Ability to see cursor and scroll back and forth using left/right
    arrow keys.
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
  - Pressing `DEL` shifts the field to the left by 1 and deletes the
    character just after point.
- Command history
  - Pressing the up/down arrow keys allows you to scroll back to
    previous history entries results.
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
