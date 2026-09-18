# Cooperative services

`src/os-tasks.fs` provides four cooperative jobs. Each callback receives its
context cell, performs one bounded step, and returns with that cell consumed.
`YIELD` visits each slot once. The desktop calls it when polling input; the
shell's keyboard wait calls the optional `IDLE-XT` hook, installed as `YIELD`
when the task module loads. Jobs advance while waiting for a key or key release.
Applications should call `YIELD` during long computations. A callback can
keep its continuation/state in its context object, as a small state machine.

```forth
VARIABLE COUNT
0 COUNT !
: COUNT-STEP ( addr -- ) 1 SWAP +! ;
' COUNT-STEP COUNT TASK-NEW  ( -- id; 0 if the table is full )
YIELD
COUNT @ .
TASKS
```

| Word | Stack | Meaning |
| --- | --- | --- |
| `TASK-NEW` | `xt context -- id/0` | Allocate a runnable job. |
| `TASK-RUN` | `id -- flag` | Resume/restart; clear error and delay. |
| `TASK-PAUSE` | `id -- flag` | Suspend a job. |
| `TASK-STOP` | `id -- flag` | Stop, retaining its record for inspection. |
| `TASK-FREE` | `id -- flag` | Release a slot; cannot free the executing job. |
| `TASK-SLEEP` | `passes id -- flag` | Skip that many future scheduling passes. |
| `TASK-INFO` | `id -- state steps error` | Inspect status; invalid IDs return zeros. |
| `TASK-CONTEXT` | `id -- context/0` | Read the application's context cell. |
| `TASK-ID` | `-- id/0` | Identify the currently executing callback. |
| `YIELD` | `--` | Run one scheduling pass; nested calls do nothing. |
| `TASKS` | `--` | List occupied slots. |
| `TASK-DEMO` | `-- id/0` | Start a counter job using `TASK-COUNT @`. |

States are 0 free, 1 runnable, 2 paused, 3 stopped, 4 failed, and 5 waiting.
Step counts include failed attempts and wrap at 65536. Sleep counts scheduling
passes, not wall time. A sleep of zero makes a job runnable immediately. An ID
can be reused after freeing; callers must discard stale IDs. A newly created
job in a later slot may execute in the current pass.

`CATCH` contains a callback's `THROW`: the error is retained and that job stops,
while other jobs continue. A changed data-stack depth throws -4 and stops the
job. This check detects ordinary callback mistakes; it does not protect against
arbitrary memory writes, return-stack corruption, or consuming/overwriting
values beneath the callback's context. The scheduler restores `BASE`, `STATE`,
and `INPUT-PTR` after each step, including failed steps. Callbacks must preserve
page mappings, input handlers, and other shared interpreter state. Callbacks
must not reset the task table, roll back the dictionary, or block for input.

These are cooperative jobs, not isolated processes or stackful threads. A
callback that never returns prevents all other work. The old
`words/coroutines.fs` swaps return addresses between two words but supplies no
private stacks, context, lifecycle, or scheduler. A future stackful layer must
save the data/return stacks, instruction pointer, exception handler, user
variables, and any bank/input state before switching; the resident table and
service API provide a place to attach that later.

Execution tokens and context addresses must remain allocated for a job's
lifetime. Before reclaiming shell definitions or unloading a module, stop and
free its jobs. `TASK-INIT` clears all jobs and is intended for boot or a quiescent
test environment. Definitions made in a persistent shell remain available
across desktop visits, which makes that shell a useful service workbench.

`tests/os-tasks.fs` exercises lifecycle, full-table handling, bad IDs, fairness,
exception and stack-leak containment, nested yielding, and delayed callbacks
using the existing `T{ ... -> ... }T` harness.

`python3 tests/test-tasks.py --display :99` runs the standalone suite in the
headless TilEm emulator using a private copy of the built ROM, then verifies
counter progress during both desktop and shell idle waits, interactive `STAR`,
and `BYE`. Use `--emulator` to specify your emulator binary and `--output` to
retain its screenshots and RAM snapshots in a chosen directory. The binary
defaults to `$TILEM` or `tilem2`; launch through `nix develop --command` if its
dependencies require the development environment. Target
verification passes all 64 assertions; the keyboard regression confirms that
background callbacks keep advancing while the shell awaits input.
