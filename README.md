# Cadre

A Common Lisp editor, written in Common Lisp: Emacs's depth for Lisp, with an
interface like VS Code's. Built on the [gtk4](../gtk4) bindings and libadwaita.

**Status:** milestone M2 (talking to a running Lisp). See [docs/DESIGN.md](docs/DESIGN.md)
for the design and the milestones.

- **M0:** a window with a file explorer, tabs, an editor with line numbers, a
  panel (Output; REPL and Problems arrive in M2), a status bar, the horizontal
  and vertical layouts, opening and saving files, and two keybinding profiles
  (Standard and Emacs) on top of Cadre's command and keymap model.
- **M1:** syntax highlighting from Cadre's own Lisp lexer (re-lexing only the
  lines that change), rainbow parentheses, matching-paren and current-line
  highlighting, Lisp indentation, moving over s-expressions, a command
  palette, quick open, switching buffers, go to line, and a find bar.
- **M2:** a Swank client and a bundled Swank (SLIME 2.32): start a Lisp or
  connect to one, a REPL, evaluating and compiling forms and files, compiler
  notes (Problems page and underlines), argument hints in the status bar,
  completion, go to definition and back, describe, and a debugger page with
  restarts and the backtrace.

## Running

Requirements: SBCL with Quicklisp, GTK 4.14+, libadwaita 1.5+, and the gtk4
bindings checked out next to this directory (`../gtk4`).

```sh
make run                      # open the last folder (or none)
make run DIR=~/projects/foo   # open a folder
```

The first run asks which keyboard shortcuts to use.

| Action | Standard (⌘ on macOS) | Emacs |
| --- | --- | --- |
| Open file | `Ctrl+O` | `C-x C-f` |
| Open folder | `Ctrl+K Ctrl+O` | `C-x d` |
| Save / Save as | `Ctrl+S` / `Ctrl+Shift+S` | `C-x C-s` / `C-x C-w` |
| New file | `Ctrl+N` | — |
| Close tab | `Ctrl+W` | `C-x k` |
| Next / previous tab | `Ctrl+Tab` / `Ctrl+Shift+Tab` | `C-x →` / `C-x ←` |
| Toggle sidebar / panel | `Ctrl+B` / `Ctrl+J` | `C-x t s` / `C-x t p` |
| Toggle layout | `Ctrl+K Ctrl+L` | `C-x t l` |
| Command palette | `Ctrl+Shift+P`, `F1` | `M-x` |
| Quick open (file by name) | `Ctrl+P` | `C-x p f` |
| Switch buffer | (palette) | `C-x b` |
| Find / next / previous | `Ctrl+F` / `F3` / `Shift+F3` | `C-s` / `C-s` / `C-r` |
| Go to line | `Ctrl+G` | `M-g g` |
| Quit | `Ctrl+Q` | `C-x C-c` |

In Lisp files, in both profiles:

| Action | Keys |
| --- | --- |
| Indent line or selection / new line, indented | `Tab` / `Return` |
| Forward / backward s-expression | `C-M-f` / `C-M-b` |
| Up / down a list | `C-M-u` / `C-M-d` |
| Start / end of top-level form | `C-M-a` / `C-M-e` |
| Select s-expression | `C-M-Space` |
| Indent top-level form / selection | `C-M-q` / `C-M-\` |

(`C-M-` is Ctrl+Alt; on macOS, Ctrl+Option.)

Talking to the Lisp, in Lisp files (Cadre starts a Lisp with `*lisp-command*`,
`sbcl` by default, the first time one is needed):

| Action | Standard (⌘ on macOS) | Emacs |
| --- | --- | --- |
| Evaluate or compile top-level form (definitions compile, other forms show their value) | `Ctrl+Return` | — |
| Compile top-level form | (palette) | `C-c C-c` |
| Evaluate top-level form | (palette) | `C-M-x` |
| Evaluate expression before cursor / selection | `Ctrl+Shift+Return` | `C-x C-e` / `C-c C-r` |
| Compile and load file | `F5` | `C-c C-k` |
| Go to definition / back | `F12` / `Ctrl+Alt+-` | `M-.` / `M-,` |
| Describe symbol | `Ctrl+K Ctrl+I` | `C-c C-d d` |
| Complete symbol | `Ctrl+Space` | `C-M-i` |
| Next / previous compiler note | `F8` / `Shift+F8` | `M-n` / `M-p` |
| Show the REPL | `` Ctrl+` `` | `C-c C-z` |
| Load the folder's ASDF system into the Lisp | `F6` | `C-c L` |

Values from evaluating appear inline after the form (`⇒ 42`) until you edit,
and in the status bar.

In the REPL: `Return` sends a complete form, `M-p`/`M-n` (or `Ctrl+↑`/`Ctrl+↓`)
walk the history, `Tab` completes. `M-x connect` connects to a Swank server
you started yourself.

## Configuration

`~/.config/cadre/init.lisp` is loaded at startup in the `cadre-user` package:

```lisp
(setf *editor-font* "JetBrains Mono 13pt")
(bind-key *standard-global-keymap* "C-k C-s" 'save-all)
(define-indentation my-with-macro 1)   ; indent like WHEN
(define-command insert-date ()
  "Insert today's date."
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignore s m h))
    (buffer-insert (current-buffer) (format nil "~d-~2,'0d-~2,'0d" year month day))))
```

Cadre writes its own choices (keybindings, layout, last folder) to
`~/.config/cadre/settings.sexp`.

## Development

```sh
make test     # headless tests of the editor model (src/core)
make smoke    # drive a real window through the M0 features; screenshots in build/smoke/
```

| Path | Contents |
| --- | --- |
| `src/core/` | The editor model, with no GTK: text protocol, buffers, commands, keymaps, modes, hooks, options, fuzzy matching |
| `src/core/lisp/` | The Lisp lexer, the per-line syntax cache, s-expression navigation, indentation, faces |
| `src/core/swank/` | The Swank client: safe s-expression reader/writer, connection, starting a Lisp, request helpers |
| `vendor/slime/` | The bundled Swank (see its `CADRE-NOTES.md` for what is included and the licenses) |
| `src/ui/` | The GTK interface: window, explorer, tabs, editor view, panel, layouts, commands, keybindings |
| `tests/` | Parachute tests for `src/core/` |
| `scripts/` | `run.lisp`, `test.lisp`, `smoke.lisp` |
| `icons/` | Cadre's own symbolic icons |

## License

MIT.
