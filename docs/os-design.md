# From launcher to Forth system

The workbench is the system's home context. It connects the interactive
dictionary, named flash objects, cooperative jobs, the memory map, and a
discoverable service interface. Applications can use those same facilities
from Forth instead of being independent demonstrations behind menu buttons.

## Audit and architectural choices

The original implementation already has a capable resident interpreter,
compiler, dictionary introspection, exceptions, display operations, and raw
flash access. The missing layer is ownership and lifetime: the old shell
forgets its definitions on exit, bank switching is global, storage lacks a
catalog, and the menu blocks all useful work while waiting for a key. The
coroutine example in `words/coroutines.fs` demonstrates return-stack control
but does not provide task identities, lifecycle management, or error handling.

Source organization also mixes layers. The large first bootstrap file combines
language extensions, graphics demonstrations, grid-menu implementation, and
application launch. Numbered bootstrap stages obscure dependencies and make
the physical flash layout dictate module boundaries. Resident modules should
have descriptive names, explicit load order, and a checked ROM layout. A source
module is a compilation unit; a hardware page is an allocation unit. They need
not remain one-to-one as the system grows.

The new layers are separate Forth modules:

| Layer | Responsibility |
| --- | --- |
| Services | Versioned export IDs, protected bank borrowing, evaluation contexts |
| Storage | Named typed objects, lookup, replacement, deletion, source loading |
| Tasks | Cooperative callbacks with identity, state, context, and error reporting |
| Desktop | Foreground navigation and views of the actual shared system state |
| Shell | Interactive development in the resident dictionary |

The trust model remains a single shared address space. The exported API helps
programs cooperate; it does not isolate hostile Forth code or stop a callback
that never returns.

## Using the desktop

UP/DOWN selects an entry and ENTER opens it. CLEAR or MODE returns from an
inspector. Keyboard polling calls `YIELD`, so scheduled callbacks make progress
while a foreground view waits for input. A held key generates one event until
it is released. Views refresh after a navigation event.

* **Forth workspace:** enter the interpreter; `BYE` returns to the workbench.
* **Files:** UP/DOWN selects a live named object and displays its type and byte
  length. ENTER previews its contents; LEFT/RIGHT moves by 128 bytes. The viewer
  replaces control bytes with dots, so reading an object does not execute it.
* **Tasks:** UP/DOWN chooses a slot, RIGHT creates a counter demo, and ENTER
  toggles a ready task to paused or starts an occupied task. The inspector shows
  attempt count and the last error. Use `TASK-FREE` in the shell to
  reclaim a slot.
* **Pages / memory:** shows remaining dictionary space, the current raw bank
  selector, and a 32-byte preview of a flash page. LEFT/RIGHT browses pages.
  The original bank mapping is restored before the view resumes polling.
* **System services:** lists the numeric service IDs and the actual dictionary
  names resolved by `SERVICE@`; LEFT/RIGHT changes the listing page.
* **Test suite:** runs the interpreter regression suite.

The file viewer resolves names again before every preview. This is necessary
because storage pointers refer to a shared scratch buffer and must not be kept
across a scheduling point. Applications should similarly copy data they need
to retain or reacquire it by name after yielding.

## Historical influences

Traditional Forth systems put the compiler, interpreter, editor, and storage
into one live development environment. Source often lived in 1024-byte blocks,
displayed as 16 lines of 64 characters, and `LOAD` sent that source through the
same text interpreter as keyboard input. The useful principle here is that
stored source remains editable and reloadable, while compiled definitions are
working state. Naming objects adds organization without losing that model.
See [Starting Forth, chapter 3](https://www.forth.com/starting-forth/3-forth-editor-blocks-buffer/)
and [Rather and Moore, The Forth Approach to Operating Systems](https://figforth.org.uk/library/p233-rather_The.Forth.Approach.to.Operating.Systems.pdf).

The local `~/ti84p-re` study suggests three complementary TIOS patterns:

* Its [bcall dispatcher](../../ti84p-re/docs/bcall-mechanism.md) exports stable
  service IDs and restores a caller's bank mapping. Forth execution tokens and
  a versioned service table can provide the same kind of decoupling without
  reproducing TI's calling convention.
* Its [VAT](../../ti84p-re/docs/variables-vat.md) separates an object's identity
  and type from the location of its bytes. The named object layer adopts that
  distinction, which can survive future movement or compaction of records.
* Its [context system](../../ti84p-re/docs/boot-contexts-errors.md) connects
  key routing, foreground state, and error unwinding. The workbench is a first
  foreground context; a future editor should return to its caller without
  discarding unrelated definitions or jobs.

TIOS is documented there as a single-tasking monitor, so cooperative jobs are
an addition to this design, not a claim of reproducing TIOS processes. The
[RAM-page study](../../ti84p-re/docs/ram-pages.md) also distinguishes selector
values from physical memory: some reported hardware revisions alias multiple
RAM selectors. A future page allocator must probe or identify actual backing
and reserve kernel/stack pages before allocating RAM by selector number.

## Limits and next layers

This is a useful minimum system, with deliberately visible limits. Storage is
an append-only flash journal with a finite number of record slots. Replacing
and deleting objects consumes slots. There is no automatic garbage collection,
directory tree, or general file-descriptor interface. Task records hold
callbacks and explicit context; they are cooperative jobs, not independent
Forth stacks or protected processes. Page browsing is inspection, not an
allocator. The workbench has no full-screen source editor yet.

The next changes should build on those contracts:

1. **Editor and source workspaces.** Select an object, edit a working buffer,
   save a new version, and explicitly load it. Keep dirty state, source names,
   and error positions in an evaluation-context record. Offer a block view
   alongside named source files rather than tying editing to raw flash pages.
2. **Managed storage.** Add per-sector erase ownership, two-sector recovery
   and compaction, streaming reads, and directory/path conventions. Preserve
   object identity while its physical record moves. Do not expose a destructive
   erase operation as an ordinary file-browser action.
3. **Page and module manager.** Track page kind, owner, residency, and pins.
   Use explicit handles for banked objects. A build manifest should pack source
   or compiled modules, enforce page budgets, and emit an inspectable module
   directory. A stable export table lets implementations move between pages.
4. **Richer cooperative execution.** Add mailboxes and event waits first, then
   private data/return stacks and task-local interpreter/input state if suspended
   Forth continuations are needed. The existing callback API remains useful for
   timers and background services. Add ownership rules for display and storage
   buffers before allowing arbitrary jobs to use them during UI operations.
5. **Foreground applications.** Put calculator, editor, dictionary browser,
   and file browser behind one enter/event/leave context protocol. A small
   application registry can replace the hard-coded launcher choices once
   applications have stable entry and cleanup contracts.

Testing should exercise whole interactions: define a word, leave and reenter
the shell, save source, inspect it, reload it, run a background counter while
navigating, pause it, and verify that a failed callback does not stop the
workbench. Bank restoration should be checked on both normal returns and
exceptions; object replacement and interrupted writes need checks at the
flash-record level as well as through the desktop.
