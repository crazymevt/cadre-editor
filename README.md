# Cadre

A Common Lisp editor, written in Common Lisp: Emacs's depth for Lisp, with an
interface like VS Code's. Built on the [gtk4](../gtk4) bindings and libadwaita.

**Status:** milestone M5 (Emacs depth). See [docs/DESIGN.md](docs/DESIGN.md)
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
- **M3:** a full debugger (frame locals, evaluate in a frame, frame source,
  restart or return from a frame, more frames, inspect the condition), an
  inspector, cross-references (who calls, references, binds, sets, expands,
  specializes), a macroexpander, an ASDF Systems view in the sidebar, and
  highlighting from the running image (user macros, special variables,
  constants, calls to undefined functions).
- **M4:** Claude, through the Claude Code CLI: a Claude panel with streaming
  replies, context from the editor, tool calls shown as they happen, and
  approval prompts; an MCP server through which Claude reads your buffers and
  asks your running Lisp (describe, arglists, definitions, cross-references,
  macroexpansion, compiler notes, the backtrace); and edits proposed as inline
  diffs in the editor, which you accept or reject.
- **M5:** Emacs depth: structural editing and paredit mode, a kill ring
  shared with the system clipboard, the mark and mark ring, incremental
  search and query-replace (and replace in the find bar), keyboard macros,
  numeric arguments, help about keys and commands, split editors, session
  restore, and a REPL in Cadre's own image.

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
| Inspect a value | `Ctrl+K I` | `C-c I` |
| Find references (all kinds) | `Shift+F12` | `M-?` |
| Who calls / references / binds / sets | (palette) | `C-c C-w c` / `r` / `b` / `s` |
| Who expands a macro / specializes a class | (palette) | `C-c C-w m` / `a` |
| Callers / callees | (palette) | `C-c <` / `C-c >` |
| Macroexpand once / completely | `Ctrl+K Ctrl+M` / `Ctrl+K Ctrl+A` | `C-c C-m` / `C-c M-m` |
| Show the explorer | `Ctrl+Shift+E` | — |

Values from evaluating appear inline after the form (`⇒ 42`) until you edit,
and in the status bar.

In the debugger, digits choose a restart, `a` aborts and `c` continues. Open a
frame to see its locals (click one to inspect it) and to evaluate in the frame.
Expanding a macro inside the *Macroexpansion* tab expands it there, in place.
The sidebar's second page (the box icon) lists the folder's ASDF systems: load,
reload with compiler notes, test, and open their files.

### Claude

Cadre runs the Claude Code CLI (`claude`), so you need it installed and signed
in: `claude auth login` (with a Claude subscription or a Console account).
Cadre finds `claude` on your PATH, in `~/.local/bin`, or inside the Claude
desktop app; set `*claude-program*` to use another.

Open the Claude tab (`Ctrl+Alt+I`, Emacs `C-c C-a a`, or "Chat with Claude" in
the menu) and ask. `Return` sends, `Shift+Return` starts a new line. The
**File** chip tells Claude where you are and what is selected; **Problems** and
**Debugger** attach the compiler's notes or the current error. The Problems
tab's *Ask Claude to Fix* and the debugger's *Ask Claude* buttons fill these in
for you.

Claude can't write files directly: every change is a `propose_edit`, shown in
its tab as an inline diff (removed lines struck out, added lines green) with
*Accept* and *Reject*. Accepting applies it as one undo step and saves the
file if it had no unsaved changes. Evaluating or compiling code in your Lisp,
and Claude Code's own commands (Bash, web fetches), ask first in the chat:
*Allow Once*, *Allow for This Conversation* or *Deny*. Claude never runs code
in Cadre's own Lisp.

`*claude-model*` (default `sonnet`, also in the panel's menu), `*claude-effort*`
and `*claude-isolated*` (ignore your own Claude Code settings and MCP servers)
are options for your init file.

### Image-aware highlighting

Symbols are colored by what the connected Lisp knows: your macros like Common
Lisp's, special variables and constants, and calls to functions that don't
exist underlined. Set `*highlight-from-image*` to `nil` to turn this off.

### Emacs depth

| Action | Standard (⌘ on macOS) | Emacs |
| --- | --- | --- |
| Split right / below | `Ctrl+\` / `Ctrl+K Ctrl+\` | `C-x 3` / `C-x 2` |
| Next group / group 1, 2, 3 | — / `Ctrl+1`, `Ctrl+2`, `Ctrl+3` | `C-x o` |
| Close this group / all other groups | `Ctrl+K W` / `Ctrl+K Ctrl+W` | `C-x 0` / `C-x 1` |
| Move tab to the next group | `Ctrl+Alt+→` | (palette) |
| Find and replace | `Ctrl+H` | `M-%` (query-replace) |
| Incremental search forward / backward | (find bar) | `C-s` / `C-r` |
| Toggle comment | `Ctrl+/` | `M-;` |
| Delete line | `Ctrl+Shift+K` | `C-S-Backspace` |
| Editor REPL (Cadre's own image) | `Ctrl+Shift+R` | `C-c R` |
| Evaluate in Cadre's image | (palette) | `M-:` |
| Describe key | `Ctrl+K Ctrl+K` | `C-h k` |
| Describe command / where is / all bindings | (palette) | `C-h f` / `C-h w` / `C-h b` |

The Emacs profile also has the kill ring (`C-k`, `C-w`, `M-w`, `C-y`, `M-y`,
`M-d`, `M-DEL`, `C-M-k`), the mark (`C-SPC`, `C-u C-SPC` to go back,
`C-x C-x`, `C-x h`), `C-u` and `M-0`…`M-9` numeric arguments, keyboard macros
(`C-x (`, `C-x )`, `C-x e` then `e` to repeat, or `F3`/`F4`), and `C-o`,
`C-t`, `M-u`/`M-l`/`M-c`, `M-\`, `M-SPC`, `M-^`, `M-m`, `C-l` and `M-/`. The
kill ring and the system clipboard stay in step, so `C-y` pastes what you
copied elsewhere and `M-y` (not right after a yank) picks from the ring.

Structural editing works in Lisp buffers and REPLs in both profiles:

| Action | Standard | Emacs |
| --- | --- | --- |
| Slurp / barf forward | `Ctrl+Alt+Shift+→` / `←` | `C-)` / `C-}` (or `C-→` / `C-←`) |
| Slurp / barf backward | (palette) | `C-(` / `C-{` |
| Raise / splice | `Ctrl+Alt+Shift+↑` / `↓` | `M-r` / `M-s` |
| Splice, killing backward / forward | (palette) | `M-↑` / `M-↓` |
| Wrap in parentheses | `Ctrl+Alt+Shift+9` | `M-(` |
| Split / join | (palette) | `M-S` / `M-J` |
| Kill expression | `Ctrl+Alt+Shift+K` | `C-M-k` |

`M-x paredit-mode` (saved as a setting) also keeps parentheses balanced as you
type: `(` and `"` insert pairs, `)` moves past the end of the list, `DEL` and
`C-d` step over parentheses instead of deleting half a pair, and `C-k` kills
whole expressions.

Cadre remembers each folder's open files, splits, cursor positions, panel and
sidebar, and whether a Lisp was running, and restores them the next time you
open that folder (without naming files). Turn this off with
`*restore-session*`, or just the Lisp with `*restore-lisp*`.

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
| `src/core/claude/` | JSON, the Claude Code CLI driver (stream-json), the MCP server, line diffs |
| `src/core/swank/` | The Swank client: safe s-expression reader/writer, connection, starting a Lisp, request helpers, inspector/xref/debugger replies, classifying symbols in the image |
| `vendor/slime/` | The bundled Swank (see its `CADRE-NOTES.md` for what is included and the licenses) |
| `src/ui/` | The GTK interface: window, explorer, tabs, editor view, panel, layouts, commands, keybindings, REPL, debugger, inspector, references, systems |
| `tests/` | Parachute tests for `src/core/` |
| `scripts/` | `run.lisp`, `test.lisp`, `smoke.lisp` |
| `icons/` | Cadre's own symbolic icons |

## License

MIT.
