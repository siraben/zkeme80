# Shell debugging

The shell launcher installs temporary TilEm host-key bindings. It maps desktop
letters to uppercase calculator input and leaves the user's TilEm configuration
unchanged:

```sh
nix develop --command make all
```

`verify_tilem_keybindings.py` checks every configured binding against the
assembled key tables:

```sh
nix develop --command python3 debug/verify_tilem_keybindings.py
```

For an extended TilEm build, run `tests/shell-screen.macro` and
`tests/shell-editing.macro` on disposable copies of the ROM. They write PNG and
logical RAM evidence under `/tmp`; verify it with:

```sh
nix develop --command python3 tests/verify-shell-screen.py
nix develop --command python3 tests/verify-shell-editing.py
```

The [reverse-engineering tools](../re/README.md) provide Ghidra annotations,
mapper-aware trace analysis, bootstrap-image checks, and headless run recipes.
The [kernel regression guide](../docs/kernel-correctness.md) documents the
complete ANS and dictionary-boundary emulator runner.
