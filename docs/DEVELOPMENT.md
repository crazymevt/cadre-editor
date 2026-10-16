# Developing Cadre

```sh
make test     # headless tests of the editor model (src/core)
make smoke    # drive a real window through every feature; screenshots in build/smoke/
make smoke ONLY=run-app   # one section, and the ones it needs (several: ONLY="git folding")
make smoke-list           # the sections, and what each needs
make gallery  # the screenshots in docs/gallery/, light and dark (one: make gallery ONLY=profiler)
make perf     # measure against the performance budgets (and JavaScript, with its grammar); report in build/perf/latest.txt
```

The smoke test is a chain of sections (`(section "name" :needs (...))` in
`scripts/smoke.lisp`). Each one passes run alone: it opens the files it
uses, and starts a Lisp or loads Quicklisp if it needs one; one that builds
on another's work names it in `:needs`. Run a section while fixing
something, and the whole test when the work is done.

`make gallery` stages the same kind of window on a small demo project
(`scripts/gallery.lisp`, sharing the smoke test's driving code in
`scripts/drive.lisp`) and saves each scene at twice its size, once with
Cadre's light theme and once with its dark one. Run it after a change to
how Cadre looks, and commit the pictures with the change.

`make perf` opens a window on generated files and times what you'd feel:
- **Keys and scrolling:** a key press to its repaint at three places in a
  10,000-line file (budget 16 ms), and scrolling it a page at a time (16 ms).
- **Opening:** a 10,000-line file and a 1 MB file, until each is painted
  (300 ms).
- **A 5,000-file project:** the explorer, a 1,000-file folder, and Quick
  Open's file list (300 ms).
- **Stalls:** throughout, it watches for the main loop being busy, which is
  where deferred work like highlighting shows up. No stall may pass 100 ms.

It exits 1 if a budget is missed. `CADRE_PERF_PROFILE=1 make perf` also prints
SBCL's statistical profile.

| Path | Contents |
| --- | --- |
| `src/core/` | The editor model, with no GTK: text protocol, buffers, commands, keymaps, modes, hooks, options, fuzzy matching |
| `src/core/lisp/` | The Lisp lexer, the per-line syntax cache, s-expression navigation, indentation, faces |
| `src/core/claude/` | JSON, the Claude Code CLI driver (stream-json), the MCP server, line diffs |
| `src/core/terminal.lisp`, `src/ui/terminal.lisp` | The Terminal page: what runs, keys, file references (core); VTE through CFFI (UI) |
| `src/core/tree-sitter/` | tree-sitter: a C shim (by-value structs as integers), the FFI, the language list, building grammars, highlight queries (with their predicates), folds and the outline |
| `src/core/swank/` | The Swank client: safe s-expression reader/writer, connection, starting a Lisp, request helpers, inspector/xref/debugger replies, classifying symbols in the image |
| `vendor/slime/` | The bundled Swank (see its `CADRE-NOTES.md` for what is included and the licenses) |
| `src/ui/` | The GTK interface: window, explorer, tabs, editor view, panel, layouts, commands, keybindings, REPL, debugger, inspector, references, systems |
| `tests/` | Parachute tests for `src/core/` |
| `scripts/` | `run.lisp`, `test.lisp`, `smoke.lisp`, `gallery.lisp` (with `drive.lisp`, which both use), `perf.lisp`; the app: `build-app.lisp`, `make-app.sh`, the `cadre` launcher |
| `packaging/` | The app icon (`Cadre.svg`), the macOS launcher, entitlements and disk image, the Linux package, notices, pinned versions (see [docs/RELEASING.md](RELEASING.md)) |
| `icons/` | Cadre's own symbolic icons |
