# Cadre — Design Document

**Status:** Draft 1, 2026-10-03
**Author:** Jessie Hughart
**Name:** Cadre (system and package name `cadre`; see 13.1)

## 1. Summary

Cadre is a code editor for Common Lisp, written in Common Lisp. It pairs
the parts of Emacs that make it good for Lisp (a live connection to a running
image, structural editing, commands and keymaps you can redefine while the
editor runs) with an interface like VS Code's: a file tree, tabs, a command
palette, panels and a status bar. It also builds Claude in as an assistant
that can see the live Lisp image.

It runs on SBCL and uses the [`gtk4`](../../gtk4) bindings (GTK 4.14+, with
libadwaita 1.5+), so it gets native windows on Linux, macOS and Windows.

## 2. Goals and non-goals

### Goals

1. **Interactive Lisp development.** Evaluate, compile, inspect, debug,
   jump to definitions, view cross-references and expand macros against a
   running Lisp image, the way SLY and SLIME do.
2. **An interface that is easy to learn.** Someone who uses VS Code should be
   productive within an hour. Mouse, menus and the palette all work; Emacs
   chords are there for those who want them.
3. **Extensible in Lisp, while it runs.** Every command, keymap, mode and
   panel is a Lisp object you can redefine from the editor's own REPL. Your
   init file is Lisp.
4. **Claude as a collaborator.** Chat, inline edits and agent-style tasks,
   through the Claude Code CLI, with Claude able to see what the running
   image knows: arglists, docs,
   compiler notes, backtraces.
5. **Built on the gtk4 bindings,** and testing them hard along the way.

### Non-goals (for 1.0)

- Supporting many languages. Lisp comes first. The mode system should allow
  other languages later, but we won't build LSP support before 1.0. Two
  small modes are planned after 1.0 because Lisp projects use them every
  day: JSON and Markdown (7.5).
- Matching Emacs feature for feature, or running Emacs Lisp.
- A terminal (TTY) interface.
- Remote or collaborative editing.

## 3. Feature list

Priority: **P0** = must have for the first usable release, **P1** = 1.0,
**P2** = later.

| Area | Feature | Priority |
| --- | --- | --- |
| Workspace | Navigation tree (project explorer) with lazy loading, file watching, and create/rename/delete | P0 |
| Workspace | Tabs: reorder, close, pin, unsaved indicator, middle-click to close | P0 |
| Workspace | Split editors (horizontal and vertical) | P1 |
| Workspace | Horizontal layout (panel below) and vertical layout (panel beside), switchable at runtime, with an automatic option | P0 |
| Workspace | Quick open (fuzzy find files), command palette (`M-x` / `Ctrl+Shift+P`) | P0 |
| Workspace | Outline view: the definitions in the current buffer | P1 |
| Workspace | ASDF system view: systems, components, load and test | P1 |
| Editing | Syntax highlighting for Common Lisp | P0 |
| Editing | Matching and rainbow parentheses, auto-indentation in the Lisp style | P0 |
| Editing | Structural editing (paredit-style: slurp, barf, raise, splice, wrap) | P1 |
| Editing | Kill ring, mark and region, incremental search, query-replace | P0 |
| Editing | Undo/redo (linear first, undo tree later) | P0 / P2 |
| Editing | Multiple cursors | P2 |
| Editing | Keyboard macros | P1 |
| Lisp | Connect to a Swank/Slynk server; start a local Lisp and connect | P0 |
| Lisp | REPL panel with history, presentations (clickable results) and an input mode that understands Lisp | P0 |
| Lisp | Evaluate or compile the form, region, defun or file; inline result overlays | P0 |
| Lisp | Compiler notes: underlines, the problems panel, next/previous note | P0 |
| Lisp | Arglist hints in the status bar, completion (fuzzy, as in SLY), documentation lookup | P0 |
| Lisp | Go to definition (`M-.`) and back (`M-,`); who-calls, who-references, and other cross-references | P0 |
| Lisp | Debugger (SLDB): backtrace, restarts, frame locals, eval in frame | P0 |
| Lisp | Inspector | P1 |
| Lisp | Macroexpander (step by step, in place) | P1 |
| Lisp | Tracing and stepping | P2 |
| Lisp | Highlighting from the image (macros, special variables, undefined functions) | P1 |
| Claude | Chat panel with editor context (buffer, selection, notes, backtrace) | P0 |
| Claude | Inline edit: select code, describe a change, review the diff, accept or reject | P1 |
| Claude | Agent mode: Claude uses tools (read files, inspect the image, evaluate with your approval) | P1 |
| Claude | Claude Code CLI driver: sign-in check, conversations, streaming, resume | P0 |
| Claude | MCP server exposing editor and Swank tools to Claude Code | P0 |
| Claude | Interactive Claude Code in a terminal panel | Later |
| Extensibility | Commands, keymaps, major/minor modes, hooks, user init file | P0 |
| Extensibility | Editor REPL: a REPL in the editor's own image | P0 |
| Extensibility | Themes: GTK CSS plus syntax colours; dark and light | P1 |
| Extensibility | Packages: load extensions through Quicklisp/ASDF | P2 |
| Other files | JSON mode: highlighting, validation, formatting, folding (7.5) | P2 |
| Other files | Markdown mode with a live, rendered preview beside the source (7.5) | P2 |

## 4. Architecture

### 4.1 Processes

The editor and the Lisp you're working on run as **two separate processes**,
the same split SLIME uses:

```
┌──────────────────────────────┐   Swank wire protocol    ┌─────────────────────────┐
│  Cadre (SBCL + GTK)          │  (TCP, length-prefixed   │  Target image           │
│                              │   s-expressions)         │  (SBCL, CCL, ECL, …)    │
│  UI · buffers · commands     │ ◄──────────────────────► │  swank or slynk server  │
│  Swank client                │                          │  your code              │
│  Claude driver + MCP server  │                          └─────────────────────────┘
│  Editor REPL (in-process)    │
└──────────────┬───────────────┘
               │ stdin/stdout (stream-json) + MCP over local HTTP
               ▼
        claude (Claude Code CLI) ──► Anthropic
```

Why two processes:

- **Isolation.** If your code crashes, runs out of heap, or loops forever, the
  editor keeps running. You can restart the target image without closing
  anything.
- **Any implementation.** Swank already runs on SBCL, CCL, ECL, ABCL, Allegro,
  LispWorks and others. Using its existing protocol gets us all of them.
- **Remote images.** You can connect to a server, a container or an
  embedded device.
- **No GTK threading problems in user code.** The target never touches GTK.
  (Code that *uses* GTK, such as gtk4 apps, follows the existing recipe in
  the gtk4 manual's threads chapter.)

The editor still gets Emacs-style extensibility from a second, in-process
**Editor REPL**. It evaluates in the editor's own image, so you can redefine
commands or widgets live. Wrong code there can break the editor, just as in
Emacs; `M-x restart-editor` and a safe mode (`--no-init`) cover recovery.

### 4.2 Threads

GTK is single-threaded and, on macOS, has to run on the first thread. The
editor follows the rules from the gtk4 manual:

- **Main thread:** the GTK main loop, all UI and buffer changes.
- **Swank connection thread** (one per connection): reads messages from the
  socket, decodes them, and sends them to the main thread with
  `glib:in-main-thread`. Writes go through a lock-protected queue.
- **Claude:** the CLI's output is read with asynchronous GIO reads on the
  main loop, so it needs no thread. The MCP server runs on its own thread;
  tool calls reach buffers through `glib:in-main-thread`.
- **Background jobs:** file indexing, file watching (or `gio:file-monitor`),
  and fuzzy matching for large projects.

Rule: **only the main thread touches widgets or buffers.** Everything else
sends messages to it. That keeps locks out of the editor core.

### 4.3 Module layout

```
cadre/
  cadre.asd
  src/
    core/          ; GTK-free model: buffers, marks, commands, keymaps, modes, hooks, kill ring
    lisp-mode/     ; reader-aware lexer, indentation, paredit, form navigation
    swank/         ; wire protocol, connection, RPC, events (GTK-free)
    claude/        ; CLI driver, stream-json codec, MCP server, tools, context builders
    ui/            ; GTK: window, explorer, tabs, editor view, panels, palette, status bar
    ui/lisp/       ; REPL, SLDB, inspector, xref, notes views
    ui/claude/     ; chat panel, inline-edit diff view
    app.lisp       ; startup, init file, command-line arguments
  vendor/slime/    ; pinned, bundled Swank (8.2)
  themes/
  tests/           ; parachute; core/, swank/ and claude/ tested headless
```

Everything under `core/`, `swank/` and `claude/` must load and pass its tests
**without GTK**. That keeps most of the logic fast to test and leaves room
for other front ends later.

## 5. Core model (the Emacs layer)

### 5.1 Buffers, windows and tabs

The separation follows Emacs:

- A **buffer** holds text plus state: file, major mode, minor modes,
  modified flag, undo history, local variables. Buffers exist whether or not
  they are on screen (`*repl*`, `*claude*`, `*scratch*`, scratch output).
- A **view** (Emacs calls it a window) shows a buffer: point (cursor),
  scroll position, selection. One buffer can appear in several views.
- A **tab** belongs to an editor group (a split pane) and holds a view.
  Closing a tab doesn't kill the buffer unless it is the last view and
  `close-kills-buffer` is set (the default, to behave like VS Code).

Buffer text lives in a `gtk:text-buffer`. A thin `core` protocol (`insert`,
`delete-region`, `char-at`, `point`, marks, text properties) wraps it, so
commands never call GTK directly. Headless tests use a plain Lisp
implementation of the same protocol.

### 5.2 Commands

```lisp
(define-command eval-defun (&key (buffer (current-buffer)))
  "Evaluate the top-level form around point in the connected Lisp."
  (:modes lisp-mode)
  (swank-eval-and-show (top-level-form-at-point buffer)))
```

- Commands are named functions with metadata: docstring, modes where they
  apply, how arguments are read (from the minibuffer/palette, the region, a
  numeric prefix).
- The command palette lists every command that applies, with its key binding.
- `describe-command`, `describe-key` and `where-is` behave as in Emacs.
- Commands are called **by symbol**, so redefining one takes effect at once.
  This uses the same pattern the gtk4 bindings use for signal handlers.

### 5.3 Keymaps

- Keymaps are layered: global → major mode → minor modes → buffer-local →
  transient (for example, inside incremental search).
- Multi-key chords (`C-x C-f`, `C-c C-k`) are supported. The status bar
  shows the keys typed so far and, after a pause, the bindings that can
  follow (like which-key).
- Shipped **keybinding profiles**:
  - `:standard`: VS Code / CUA keys (`Ctrl+S`, `Ctrl+P`,
    `Ctrl+Shift+P`, `F12`), with SLY's `C-c` bindings for Lisp.
  - `:emacs`: full Emacs keys (`C-x` prefix, `M-x`, `C-k`/`C-y` kill ring).
- **On first run** the editor asks which profile to use, with a short
  preview of the main keys in each. The choice is saved as the option
  `*keybinding-profile*` and can be changed later in settings or with
  `M-x set-keybinding-profile`.
- On macOS, `Cmd` maps to the `:standard` profile's `Ctrl` actions. `Meta`
  can be set to Option or Esc.

Key events come from a `gtk:event-controller-key` on the editor view, set to
run in the capture phase so the keymap sees keys before GtkTextView does.
Keys we don't bind fall through to GtkTextView, which keeps input methods
(IME) and dead keys working.

### 5.4 Modes and hooks

- A **major mode** per buffer (`lisp-mode`, `repl-mode`, `text-mode`,
  `claude-chat-mode`) supplies the keymap, highlighter, indenter and
  commands.
- **Minor modes** turn on extra behaviour (`paredit-mode`,
  `rainbow-parens-mode`, `auto-save-mode`, `claude-inline-mode`).
- **Hooks**: `after-change`, `before-save`, `after-save`, `buffer-opened`,
  `mode-enabled`, `connection-established`, and others.

### 5.5 Kill ring, mark, search

- A kill ring that syncs with the system clipboard
  (`gdk:clipboard`). `C-y` and `M-y` cycle through it.
- Mark and region alongside normal shift-click selection. Both use the same
  selection.
- Incremental search (`C-s`/`C-r` and `Ctrl+F`) in the minibuffer, with
  highlighted matches. Search across the project uses a ripgrep-style
  backend and opens results in a panel.

### 5.6 Configuration

- `~/.config/cadre/init.lisp` (with an XDG fallback; `~/.cadre.lisp`
  also works) is loaded at startup in the `cadre-user` package.
- Settings are `defvar`-style variables declared with `define-option`. Each
  has a type and docstring, which lets us build a searchable settings page,
  as VS Code has.
- Each project can have its own settings in `.cadre.lisp` at the project
  root. Loading it requires the user to trust the project once, because it
  runs code.

## 6. The interface (the VS Code layer)

### 6.1 Layouts

The window has two layouts. Both use the same widgets and differ only in
**where the panel goes** (the panel holds REPL, Problems, Debugger,
Inspector, Claude and Output).

- **Horizontal layout** (the default): the panel is a full-width strip
  **below** the editor. This is VS Code's default and suits most screens.
- **Vertical layout:** the panel is a full-height column **beside** the
  editor, on the right. Code and REPL sit side by side, the way many people
  run Emacs with SLY. It suits wide monitors, long backtraces and the Claude
  chat.

#### Horizontal layout

```
┌──────────────────────────────────────────────────────────────────────────┐
│ Header bar:  ◧ sidebar   project ▾    [ command palette / search ]   ⚙  │
├───┬────────────────┬─────────────────────────────────────────────────────┤
│ A │ EXPLORER       │ ┌ foo.lisp ● ┐┌ bar.lisp ┐┌ *repl* ┐                  │
│ c │ ▾ src          │ │                                                   │
│ t │   ▸ core       │ │ (defun frob (x)                                   │
│ i │   · app.lisp   │ │   (let ((y (* x 2)))      ⇒ 42                   │
│ v │ ▾ tests        │ │     (+ y 1)))                                     │
│ i │ ▸ docs         │ │                                                   │
│ t │                │ │                                                   │
│ y │ OUTLINE        │ ├───────────────────────────────────────────────────┤
│   │  frob          │ │ REPL │ PROBLEMS │ DEBUGGER │ INSPECTOR │ CLAUDE  │
│ b │  *frob-table*  │ │ CL-USER> (frob 20)                                │
│ a │                │ │ 41                                                │
│ r │                │ │ CL-USER> ▌                                        │
├───┴────────────────┴─────────────────────────────────────────────────────┤
│ ● sbcl-2.4 @ localhost:4005   CL-USER   (frob x)   Ln 3, Col 9   Lisp  │
└──────────────────────────────────────────────────────────────────────────┘
```

#### Vertical layout

```
┌──────────────────────────────────────────────────────────────────────────┐
│ Header bar:  ◧ sidebar   project ▾    [ command palette / search ]   ⚙  │
├───┬──────────────┬───────────────────────────────┬───────────────────────┤
│ A │ EXPLORER     │ ┌ foo.lisp ● ┐┌ bar.lisp ┐      │ REPL│PROBLEMS│CLAUDE…│
│ c │ ▾ src        │ │                              │                       │
│ t │   ▸ core     │ │ (defun frob (x)              │ CL-USER> (frob 20)    │
│ i │   · app.lisp │ │   (let ((y (* x 2)))  ⇒ 42  │ 41                    │
│ v │ ▾ tests      │ │     (+ y 1)))                │ CL-USER> ▌            │
│ i │ ▸ docs       │ │                              │                       │
│ t │              │ │                              │                       │
│ y │ OUTLINE      │ │                              │                       │
│   │  frob        │ │                              │                       │
│ b │  *frob-table*│ │                              │                       │
│ a │              │ │                              │                       │
│ r │              │ │                              │                       │
├───┴──────────────┴───────────────────────────────┴───────────────────────┤
│ ● sbcl-2.4 @ localhost:4005   CL-USER   (frob x)   Ln 3, Col 9   Lisp  │
└──────────────────────────────────────────────────────────────────────────┘
```

#### How switching works

- The editor area and the panel are the two children of a single
  `gtk:paned`. Switching layout just changes that paned's orientation with
  `gtk:orientable-set-orientation`. (GTK's names are the reverse of ours:
  the horizontal layout stacks the children, so the paned is
  `:vertical`; the vertical layout puts them side by side, so the paned is
  `:horizontal`.) No widget is rebuilt, so the REPL, debugger state, scroll
  positions and the Claude conversation all stay as they are.
- Each layout remembers its own panel size, so switching back restores it.
- The panel's tab strip stays at the top of the panel in both layouts.
- `M-x toggle-layout` (and a header-bar button) switches between them.
  `M-x set-layout` picks one. The option `*layout*` takes `:horizontal`,
  `:vertical`, or `:auto`. With `:auto`, the editor uses the vertical
  layout when the window is wider than `*auto-vertical-min-width*`
  (default 1600 px) and the horizontal one otherwise, re-checking when the
  window is resized. A layout you choose by hand overrides `:auto` until
  you set it back.
- The panel can still be hidden (`Ctrl+J` / `C-c C-z` shows it again) and
  maximised in either layout.
- Split editors (section 3) work inside the editor area in both layouts;
  they are independent of where the panel goes.

### 6.2 Widgets

| Element | GTK widgets |
| --- | --- |
| Window | `adw:application-window` |
| Activity bar | Vertical `gtk:box` of toggle buttons: Explorer, Search, Lisp (systems and connections), Claude |
| Sidebar | `gtk:stack` inside a `gtk:paned`, so it can be resized and hidden |
| Navigation tree | `gtk:list-view` over a `gtk:tree-list-model`, with `gtk:tree-expander` rows; children load lazily from `gio:file-enumerate-children-async` |
| Tabs | `adw:tab-view` + `adw:tab-bar` (reorder, pin, unsaved indicator and drag between groups come with them) |
| Editor | `gtk:text-view` inside `gtk:scrolled-window`, plus a gutter (line numbers, note markers, fold arrows) drawn with `gtk:text-view-set-gutter` |
| Panel | `gtk:stack` with a tab strip; REPL, Problems, Debugger, Inspector, Claude, Output. Shares a `gtk:paned` with the editor area; the paned's orientation sets the layout (6.1) |
| Command palette / minibuffer | Popover over the editor: `gtk:entry` + `gtk:list-view` with fuzzy-ranked results. Doubles as the minibuffer for commands that need arguments |
| Status bar | `gtk:box` of labels: connection, package, arglist, position, mode, Claude status |
| Notifications | `adw:toast-overlay` |

### 6.3 Session restore

Open tabs, splits, the layout and each layout's panel size, the expanded tree nodes
and the last connection are saved to `~/.local/state/cadre/` on exit and
restored at startup.

## 7. Syntax highlighting and Lisp editing

### 7.1 Decision: our own Lisp-aware highlighter on `GtkTextView`

**Decided (2026-10-03):** we write our own Lisp-aware highlighter and apply
it with `gtk:text-tag`s on a plain `gtk:text-buffer`. We won't use
GtkSourceView.

Why:

- **Correctness.** GtkSourceView's Lisp grammar is regex-based and gets
  nested `#| |#`, reader conditionals (`#+`/`#-`), and package-prefixed
  symbols wrong. Lisp's syntax is small enough that a correct lexer is a few
  hundred lines.
- **One lexer for everything.** The same lexer drives paredit, indentation,
  form navigation, and finding the form to evaluate, so we need it anyway.
- **Colours from the live image.** Our own highlighter can show macros,
  special variables and undefined functions as the running image sees them
  (7.3).
- **Fewer dependencies.** No GtkSourceView bindings to add to the
  generator, and no extra library to ship on macOS and Windows.

What we build ourselves because of this choice (GtkSourceView would have
given us these):

| Feature | Where | Milestone |
| --- | --- | --- |
| Line-number and marker gutter | `gtk:text-view-set-gutter` with a custom widget | M0 |
| Matching-paren and current-line highlight | Text tags updated when the cursor moves | M1 |
| Search-match highlighting | Text tags, visible range only | M1 |
| Completion popup | `gtk:popover` + `gtk:list-view` at the cursor | M2 |
| Code folding | Invisible-text tags over a form, with fold arrows in the gutter | Later |
| Minimap | Not planned | — |

### 7.2 Lexer and parse state

- An incremental, **line-cached lexer**. For each line it stores the state
  at the start of the line (inside a string, inside a block comment and how
  deeply, inside `|…|`, paren depth). After an edit it re-lexes from the
  changed line until the stored state matches again. Typing stays cheap even
  in files of 10,000+ lines.
- Tokens: parens, strings, characters (`#\x`), numbers, symbols (with
  package prefix split out), keywords, line and block comments (nested),
  `#'`, quote/backquote/comma, reader conditionals, `#.`, vectors,
  and invalid input.
- The same token stream drives **form navigation** (`forward-sexp`,
  `up-list`, `beginning-of-defun`), **paredit**, and **indentation**.

### 7.3 Highlighting, in layers

1. **Lexical** (immediate): comments, strings, numbers, keywords,
   characters, `defun`/`defmacro`/… names, and CL symbols from a built-in
   table.
2. **From the image** (when connected, debounced): ask Swank which symbols
   in the visible text are macros, special variables or undefined functions,
   and colour them differently. Results are cached per package.
   "Undefined function" is claimed only where a symbol is surely called:
   the head of a list in an evaluated place (a top-level form, a
   function's argument, the body of `let`, `when`, `defun`…), and not a
   local function from `flet`/`labels`. A `let` binding, a `case` key or a
   slot specifier is data, and is left alone. If the buffer's package
   doesn't exist in the image (the project isn't loaded), nothing is
   marked.
3. **Diagnostics:** compiler notes as wavy underlines (error, warning,
   style-warning), with tooltips.
4. **Decorations:** rainbow parens, a highlighted matching paren, the
   current form subtly shaded, inline eval results drawn after the form.

Only the visible range plus a margin is tagged. The rest is tagged when it
is scrolled into view. Applying tags doesn't change the text, so it never
enters undo history.

### 7.4 Indentation

Indentation follows SLIME/SLY's `lisp-indent-function` rules: `&body`
argument positions found from the image's arglists, the standard CL table,
and `define-indentation` for user overrides. It runs as you type (on
newline and closing paren) and on `indent-region`.

### 7.5 Other file types: JSON and Markdown (after 1.0)

Requested 2026-10-03. Both are small major modes built the same way as
Lisp mode: a line lexer whose end-of-line state goes into the per-line
syntax cache (7.2), faces mapped to text tags, and commands in the mode's
keymap. Generalising the cache so each major mode supplies its own lexer is
the first step; Lisp mode is then one client of it.

**JSON mode** (`.json`, `.jsonl`, `.asd`-adjacent config, `package.json`, …)

- Highlighting: keys, strings, numbers, `true`/`false`/`null`, punctuation,
  and invalid tokens; rainbow brackets and bracket matching reuse Lisp
  mode's paren code.
- Validation as you type: unbalanced brackets, trailing commas, bad escapes
  and unquoted keys shown as notes (the same underline + Problems page as
  compiler notes, 8.3).
- Format document / format selection (pretty-print with a configurable
  indent), and compact.
- Indentation on Return; folding of objects and arrays when folding lands
  (7.1).
- Navigation: the outline view shows the key structure; "copy path" puts
  the path of the value at the cursor (`$.dependencies.foo`) on the
  clipboard.

**Markdown mode with preview** (`.md`, `.markdown`)

- Source highlighting: headings, emphasis, inline code, links, lists,
  block quotes, tables, and fenced code blocks, with Lisp (and JSON) code
  in fences highlighted by those modes' lexers.
- **Live preview**, like Claude's rendered Markdown: a second view beside
  the source (`Ctrl+K V` / `C-c C-c p`), updated as you type and scrolled
  in step with the source. It renders headings, emphasis, lists, task
  lists, block quotes, tables, links (clickable), images, horizontal rules
  and syntax-highlighted code blocks.
- **How it renders: natively, not with a web view.** The preview is a
  read-only GtkTextView whose buffer Cadre fills from a Markdown parse
  tree, using text tags for styles and child anchors for images, tables
  and rules. Reasons:
  - The gtk4 bindings don't cover WebKitGTK, and WebKitGTK is hard to ship
    on macOS and Windows.
  - It stays light, follows the editor's theme and fonts, and reuses the
    same highlighter for code blocks.
  - It can be updated incrementally from the changed blocks.
  The parser is a CommonMark subset plus GitHub tables and task lists,
  written in Lisp and GTK-free (tested headlessly like the Lisp lexer).
  If full HTML fidelity is ever needed, a web-view preview can be added
  behind the same command.
- Commands: toggle preview, bold/italic/code/link on the selection, insert
  a table, and "copy as HTML".

## 8. Lisp integration (the SLY/Swank layer)

### 8.1 Decision: be a Swank client

We won't write our own server. We will implement the **Swank wire
protocol** that SLIME, SLY (via Slynk) and others use.

- Message framing: a 6-hex-digit length followed by a UTF-8 s-expression.
  We read it with a **safe reader**: `*read-eval*` nil, symbols interned
  into a sandbox package or kept as strings, so a hostile server can't run
  code in the editor.
- RPC: `(:emacs-rex form package thread id)` → `(:return (:ok value) id)` /
  `(:abort …)`. Asynchronous events: `:write-string`, `:debug`,
  `:debug-return`, `:new-features`, `:indentation-update`, `:presentation-*`,
  `:read-string`, `:ping`, `:channel-send`.
- Contribs we use: `swank-repl`, `swank-fuzzy`, `swank-arglists`,
  `swank-fancy-inspector`, `swank-presentations`, `swank-c-p-c`,
  `swank-indentation`, `swank-trace-dialog`, `swank-macrostep`.
- **Slynk** (SLY's fork) is a stretch goal. Its core protocol is close to
  Swank's, but its REPL uses channels. We'll hide both behind a
  `connection` protocol and start with Swank.

The client API is built from futures that run their callbacks on the main
thread:

```lisp
(swank-rex conn `(swank:operator-arglist ,name ,package)
           (lambda (arglist) (show-arglist arglist))
           :error #'report-swank-error)
```

### 8.2 Connecting

- `M-x connect` (`slime-connect`): host and port.
- `M-x lisp` (`sly`): start a configured implementation (default
  `sbcl`) with a bootstrap that loads the **Swank copy bundled with the
  editor** (see below), starts a server on a free port, and prints the port. The editor
  reads the port and connects. Output from the process goes to an
  `*inferior-lisp*` buffer.
- Several connections can be open at once. One is the default; a buffer
  can be tied to a particular one.
- The status bar shows the implementation, version, host and port, and
  current package. Clicking it switches connection or package.

#### Bundled Swank

The editor ships a pinned copy of Swank (from SLIME), with the contribs
listed in 8.1, in `vendor/slime/`. This means:

- Starting a Lisp works on first run, without Quicklisp.
- The client and server always speak the same protocol version; we update
  the bundled copy deliberately and test it.
- The bootstrap loads the bundled Swank into the target with
  `(load ".../swank-loader.lisp")` and `swank-loader:init`, using a
  per-user fasl cache, so the target needs nothing installed.

When connecting to a server someone else started (`M-x connect`), its Swank
version may differ. The editor compares versions in `connection-info` and
warns about a mismatch, but still connects. The option
`*swank-source*` (`:bundled` or `:quicklisp`) lets users load Swank from
Quicklisp instead.

SLIME's files are public domain unless a file says otherwise; the release
checklist verifies that for each file we bundle.

### 8.3 Features and the Swank calls behind them

| Feature | Swank call(s) |
| --- | --- |
| REPL eval | `swank-repl:listener-eval` |
| Eval form / region | `swank:interactive-eval`, `swank:eval-and-grab-output` |
| Compile defun | `swank:compile-string-for-emacs` (with buffer position for notes) |
| Compile/load file | `swank:compile-file-for-emacs`, `swank:load-file` |
| Notes | Results of the compile calls → Problems panel + underlines |
| Arglists | `swank:autodoc` (arglist with the current argument highlighted) |
| Completion | `swank:fuzzy-completions` / `swank:simple-completions` |
| Docs | `swank:describe-symbol`, `swank:documentation-symbol` |
| Go to definition | `swank:find-definitions-for-emacs` |
| Cross-references | `swank:xref` (`:calls`, `:callers`, `:references`, `:binds`, `:sets`, `:macroexpands`, `:specializes`) |
| Macroexpand | `swank:swank-macroexpand-1`, `swank:swank-macroexpand-all`, macrostep contrib |
| Debugger | `:debug` events; `swank:invoke-nth-restart-for-emacs`, `swank:frame-locals-and-catch-tags`, `swank:eval-string-in-frame`, `swank:sldb-abort` |
| Inspector | `swank:init-inspector`, `swank:inspect-nth-part`, `swank:inspector-pop`, … |
| Interrupt | `:emacs-interrupt` |
| ASDF | `swank:list-systems`, `swank:operate-on-system-for-emacs` (via `swank-asdf`) |

### 8.4 REPL

- A buffer in `repl-mode`. Text before the prompt is read-only; input is
  edited with full Lisp editing (paredit, completion, arglists).
- `Enter` sends input when the form is complete; otherwise it inserts a
  newline. `C-Enter` always sends.
- History with prefix search (`M-p`/`M-n`, Up/Down at the end of input).
- **Presentations:** results are objects you can click — inspect, copy, or
  insert back into input as the actual object (`#.(swank:lookup-presented-object …)`).
- Output from other threads appears above the prompt, not mixed into it.

### 8.5 Debugger

When an error happens, the Debugger panel opens (or a floating window, if
configured) and shows the condition, restarts (numbered, clickable, `0`–`9`
to choose), and the backtrace. Expanding a frame shows its locals; you can
evaluate in a frame, jump to the frame's source, restart a frame, or return
from it. Nested errors stack up as levels, as in SLDB.

## 9. Claude integration

### 9.1 What Claude can do here that it can't elsewhere

Claude's advantage in this editor is the **live image**. Before it answers,
it can look up the real arglist, docstring, class slots, generic function
methods, compiler notes, or the current backtrace. It doesn't have to guess
from source text.

### 9.2 Decision: drive the Claude Code CLI

**Decided (2026-10-03):** the editor runs the **Claude Code CLI**
(`claude`) as a subprocess. It does not call the Anthropic API itself.

Why:

- **Subscriptions work.** Users sign in once with `claude auth login`,
  using a Claude subscription or a Console account billed per token. The
  editor never sees or stores credentials.
- **We get Claude Code's agent loop for free:** tool use, file reading and
  searching, context compaction, prompt caching, subagents, `CLAUDE.md`
  project memory, and the user's own skills, hooks and MCP servers.
- **Less code.** No HTTP client, SSE parser, tool-use loop or caching logic
  in Lisp.
- **One integration point.** The editor offers its tools over MCP (9.5).
  The same server later lets an interactive Claude Code session, or any
  other MCP agent, drive the editor.

The costs: the editor depends on an external program and its stream-json
format (see Risks, section 12), and starting a CLI process takes longer than
an HTTP request (handled by keeping processes warm, 9.6).

### 9.3 Modes

1. **Chat panel** (P0). A conversation in the sidebar or panel. Each message
   can attach context with one click or automatically: the current buffer
   or selection, the problems list, the debugger's condition and backtrace,
   the last REPL interaction. Code blocks in replies have *Insert*, *Replace
   selection*, *Eval* (with confirmation) and *Copy* buttons. Tool calls
   appear in the conversation as collapsible rows.
2. **Inline edit** (P1). Select code, press `Ctrl+I` / `C-c C-a e`, and
   describe the change. Claude's edit appears as an inline diff (deletions
   struck out, additions highlighted). Accept, reject, or ask for changes.
   Accepting applies the diff as a single undo step.
3. **Agent mode** (P1). The same chat, but Claude works through a larger
   task with more turns and more tools allowed (9.4). Anything that changes
   files, runs commands or evaluates code waits for your approval.
4. **Explain this error** (P1). A button in the debugger that sends the
   condition, backtrace and frame source to Claude.
5. **Claude Code in a terminal** (Later). An interactive `claude` session in
   a terminal panel, connected to the editor's MCP server. This needs a
   terminal widget (VTE for GTK 4), which the gtk4 bindings don't cover yet.

### 9.4 Tools

Claude has two kinds of tools: Claude Code's **built-in tools**, and the
**editor's tools**, served over MCP (9.5).

**Built-in tools.** Claude Code's `Edit` and `Write` tools write straight to
disk, bypassing the editor's buffers: they would ignore unsaved changes and
skip diff review. So by default the editor starts the CLI with those tools
disabled, and edits go through `propose_edit` instead.

| Built-in tool | Default |
| --- | --- |
| `Read`, `Grep`, `Glob` | Allowed |
| `Bash`, `WebFetch`, `WebSearch` | Ask each time (through the editor's approval prompt) |
| `Edit`, `Write`, `NotebookEdit` | Disabled; replaced by `propose_edit` |

The option `*claude-direct-edits*` turns `Edit` and `Write` back on for
people who prefer Claude Code's own edit flow. The editor then watches the
files: an unmodified buffer reloads silently; a buffer with unsaved changes
shows a conflict bar (keep mine / take Claude's / show diff).

**Editor tools** (MCP server `cadre`, so Claude sees them as
`mcp__cadre__…`):

| Tool | Does | Needs approval |
| --- | --- | --- |
| `read_buffer`, `list_buffers` | Read open buffers, including unsaved changes | No |
| `current_context` | Current file, cursor position, selection, package, connection | No |
| `describe_symbol`, `arglist`, `find_definitions`, `who_calls`, `who_references`, `macroexpand` | Ask the image (Swank) | No |
| `apropos`, `list_systems` | Search the image's symbols and ASDF systems | No |
| `get_problems`, `get_backtrace` | Current notes and debugger state | No |
| `propose_edit` | A diff to a buffer or new file, shown inline for review | Always (it is the review) |
| `eval` | Evaluate a form in the target image | Yes, unless allowed for the session |
| `compile_defun`, `compile_file`, `load_system`, `run_tests` | Compile, load or run tests | Configurable |
| `approve` | Internal: answers Claude Code's permission prompts (9.5) | — |

Tools that read never change anything. Tools that change things show the
exact form or diff before running. **The editor's own image is never
exposed to Claude**: no tool evaluates in the editor process.

### 9.5 How the pieces connect

```
┌───────────────────────────── Cadre ─────────────────────────────┐
│  Chat panel ◄── claude driver ──────── MCP server (HTTP,        │
│                   │      ▲             127.0.0.1, random port,  │
│                   │      │             bearer token)            │
└───────────────────┼──────┼────────────────────▲─────────────────┘
       stdin: user  │      │ stdout: events     │ tool calls
       messages     ▼      │ (NDJSON)           │ (JSON-RPC)
                 ┌─────────┴────────────────────┴──┐
                 │  claude -p (Claude Code CLI)     │ ──► Anthropic
                 └──────────────────────────────────┘
```

**Starting a conversation.** The driver starts the CLI in the project root
with `gio:subprocess`:

```
claude -p --input-format stream-json --output-format stream-json \
       --verbose --include-partial-messages \
       --session-id <uuid> \
       --model <model> --effort <level> \
       --mcp-config <runtime-dir>/mcp.json \
       --permission-prompt-tool mcp__cadre__approve \
       --disallowedTools Edit Write NotebookEdit \
       --allowedTools Read Grep Glob "mcp__cadre__read_buffer" … \
       --append-system-prompt-file <cadre-prompt.md>
```

- **One process per conversation,** kept running between messages. User
  messages are written to stdin as stream-json lines. The process exits
  when the conversation is closed or after it has been idle for a while.
  Reopening it uses `--resume <session-id>`, so conversations survive
  restarts.
- **Reading events.** stdout is read line by line with asynchronous GIO
  reads on the main loop, so no extra thread is needed. Each line is one
  JSON event:
  - `system`/`init`: model, tools and MCP servers. We check that
    `cadre` connected.
  - `stream_event`: partial text and tool-input deltas. These are
    appended to the chat as they arrive, batched about every 16 ms.
  - `assistant` / `user`: complete messages, tool calls and tool results.
  - `result`: the turn has ended, with cost, usage, duration and any error.
- **stderr** goes to an `*claude-log*` buffer.
- **Stopping.** The *Stop* button interrupts the turn; if that fails, the
  editor terminates the process and resumes the session next time.

**The MCP server.** The editor runs a small **MCP server over streamable
HTTP**, bound to `127.0.0.1` on a random port, implementing only what we
need (`initialize`, `tools/list`, `tools/call`). Each editor run writes an
`mcp.json` with the URL and a random bearer token to a private runtime
directory (mode `0700`), and passes it with `--mcp-config`. Requests without
the token are refused. Tool calls arrive on the server's thread; they read
or change buffers by hopping to the main thread with
`glib:in-main-thread :wait t`.

**Approval.** Claude Code asks the `approve` tool before running a tool
that isn't pre-allowed. The tool call blocks while the editor shows an
approval bar in the chat (the tool, its input, and *Allow once*, *Allow for
this session*, *Deny*), then returns the user's answer to Claude Code.
Editor tools that need approval (`eval`, `propose_edit`) show their own,
richer review UI (the form, or an inline diff) the same way.

**Inline edits** don't need the full agent. They run a short-lived process
with no tools (`claude -p --tools "" --no-session-persistence --model …`),
with the selection, the surrounding top-level forms, and the request in the
prompt, and `--json-schema` asking for `{replacement, explanation}`.

### 9.6 Details

- **Finding the CLI.** The option `*claude-program*` (default: `claude` on
  `PATH`, then `~/.local/bin/claude`). At startup the editor runs
  `claude --version` and `claude auth status`. If the CLI is missing, too
  old, or signed out, the Claude panel says so and offers to run
  `claude auth login` in an external terminal. The rest of the editor works
  without Claude.
- **Models and effort.** Passed as `--model` (aliases such as `sonnet`,
  `opus`, `haiku`, or full model names) and `--effort`. Defaults: `sonnet`
  for chat and inline edits, `opus` for agent mode. A picker in the Claude
  panel changes them for the next message.
- **System prompt.** We **append** to Claude Code's prompt rather than
  replacing it, keeping its tool and safety guidance. Our addition explains
  the editor, the live image, and to prefer `propose_edit` and the Swank
  tools over guessing.
- **Keeping it fast.** After the first launch, the editor keeps one idle
  `claude` process ready for the next chat or inline edit, so users don't
  wait for startup.
- **Cost and usage.** Each `result` event carries cost and token usage; the
  Claude panel shows totals for the conversation. `--max-budget-usd` can cap
  agent runs.
- **User configuration.** Claude Code loads the user's and project's
  settings, `CLAUDE.md`, skills and MCP servers as usual. The option
  `*claude-isolated*` adds `--strict-mcp-config` and
  `--setting-sources project` for a clean, predictable setup.
- **Privacy.** Nothing is sent until the user acts (sending a message or
  starting an inline edit). Files matching `*claude-exclude*` patterns
  (`.env`, `secrets/**`) are never attached as context, and become deny
  rules for the `Read` tool.

## 10. Milestones

| Milestone | Contents | Exit criteria |
| --- | --- | --- |
| **M0 — Skeleton** | ASDF system, window with sidebar/editor/panel in both layouts, `core` buffer + command + keymap model, open/save, tabs | Open a project, edit and save several files in tabs |
| **M1 — Lisp editing** | Lexer, highlighting, paren matching, indentation, form navigation, navigation tree with lazy loading, palette, quick open | Editing the editor's own source is comfortable |
| **M2 — Swank** | Wire protocol, connect/start Lisp, REPL, eval/compile, notes, arglists, completion, `M-.` | Daily Lisp work without Emacs, except debugging |
| **M3 — Debugger and tools** | SLDB, inspector, xref, macroexpander, ASDF view, image-aware highlighting | Debug a real error end to end |
| **M4 — Claude** | CLI driver (sign-in check, streaming, resume), MCP server with the tools that read, approval prompts, chat panel with context, `propose_edit` with inline diff | Fix a compiler error by asking Claude, accept the diff |
| **M5 — Emacs depth** | Paredit, kill ring, incremental search/query-replace, keyboard macros, `:emacs` keybindings, splits, session restore, editor REPL | An Emacs user can switch |
| **M6 — 1.0** | Agent mode, themes, settings page, packaging (macOS `.app`, Flatpak, Windows installer) using the gtk4 deployment tools | Shipped executables on three platforms |
| **Later** | Claude Code in a terminal panel, Slynk, multiple cursors, undo tree, stepper, JSON mode, Markdown mode with live preview (7.5), LSP for other languages | — |

M0–M2 make it **usable**. Once M2 is done, Cadre should be used to develop
itself.

## 11. Testing

- **Headless unit tests** (parachute) for `core`, the lexer, indentation,
  paredit, the Swank protocol codec and the Claude client. These are most of
  the tests and need no display.
- **Swank integration tests:** start a real SBCL with Swank and test the
  calls each feature uses.
- **Claude tests** replay recorded stream-json transcripts through the
  driver, and call the MCP server with JSON-RPC requests; no CLI or network
  needed in CI. A separate, opt-in suite runs the real CLI.
- **UI smoke tests** under a virtual display (`xvfb` on Linux, the
  `QUIT_AFTER` pattern from the gtk4 examples): open files, run commands,
  check widget state.
- **Performance budgets:** a key press to its repaint under 16 ms in a
  10,000-line file; opening a 1 MB file under 300 ms; the tree showing 5,000
  files without stalling.

## 12. Risks

| Risk | Mitigation |
| --- | --- |
| GtkTextView is slow with many tags on very large files | Tag only the visible range; skip image-based highlighting above a size limit; set the performance budgets (section 11) in M1 and profile against them |
| Key handling (IME, dead keys, macOS Option) conflicts with Emacs chords | Capture-phase controller that only takes keys a keymap binds; test with IME early (M0) |
| Gaps in the gtk4 bindings | We maintain them. File and fix gaps as they come up |
| Swank protocol details that aren't documented | Read SLIME's `slime.el` and `swank.lisp` as the reference; integration tests against real servers |
| Scope: Emacs + VS Code + SLY + Claude is a lot | Strict milestones; everything after M2 is optional until the editor is used daily |
| The Claude Code CLI changes its flags or stream-json format | Require a minimum CLI version; keep all parsing in one codec module; test against recorded transcripts and run the real CLI in an opt-in suite |
| Claude Code edits files behind the editor's back | `Edit`/`Write` disabled by default in favour of `propose_edit`; file watching and a conflict bar when direct edits are turned on |
| The MCP server is reachable by other local programs | Bind to `127.0.0.1` only, random port, per-run bearer token in a `0700` directory |
| Claude evaluating harmful code in the image | Evaluation always needs approval by default; the editor's own image is never exposed |

## 13. Decisions and open questions

1. **Name:** Decided: **Cadre**. From `cadr`, a basic Lisp function and the
   name of MIT's Lisp Machine; a cadre is also a small, close team, which
   fits working with your running image and with Claude.

   Name check (2026-10-03):
   - **Lisp world: clear.** No Quicklisp project or known Lisp project is
     called `cadre`, so `cadre` works as the ASDF system and package name.
   - **Other software named Cadre:** a 1990s company, Cadre Technologies,
     that made software engineering tools; a holding company, Cadre Software,
     that makes vertical business software; a wellness app at cadre.io; and
     several small, recent AI developer tools: a Tauri IDE that coordinates
     Claude Code agents (`alvin-reyes/cadre-ide`), a VS Code extension for
     multi-agent workflows, an agent automation project (`jafreck/CADRE`),
     and a macOS screen recorder with a Claude plugin. The AI tools are tiny
     (a handful of stars or installs) and we found no trademark filings by
     them, but they are the closest to what we're building and could cause
     confusion in search.
   - **Not checked:** a full trademark search. Before any commercial use or
     trademark filing, search USPTO (classes 9 and 42) and get legal advice.
     For a free, MIT-licensed project the risk looks low.
   - **Domains:** `cadre.dev` is taken and `cadre.app` is parked for sale.
     `cadre-editor.dev` and `cadreeditor.org` had no DNS records, so they
     may be available.
   - **To reduce confusion,** use a qualified name where names are global:
     the repository `cadre-editor`, the application ID
     `io.github.crazymevt.Cadre`, and the tagline "Cadre, a Lisp editor".
     Inside Lisp the plain `cadre` is fine.
2. **libadwaita:** Decided: required (1.5 or newer). The UI uses
   `adw:application-window`, `adw:tab-view`/`adw:tab-bar`, toasts and
   preference pages directly, with no fallbacks.
3. **Default keybindings:** Decided: ask on first run (5.3).
4. **Swank:** Decided: bundle a pinned copy (8.2).
5. **Claude access:** Decided: the Claude Code CLI (9.2).
6. **License:** Decided: MIT, the same as gtk4. Nothing we depend on
   requires otherwise; see section 14.

## 14. Licensing

Cadre is MIT-licensed. Its dependencies are compatible:

| Dependency | License | How we use it | Obligation |
| --- | --- | --- | --- |
| gtk4 bindings | MIT | Linked into the executable | Keep the copyright notice |
| GTK 4, GLib, Pango, libadwaita, etc. | LGPL 2.1+ | Loaded as shared libraries at runtime; bundled in the macOS `.app` and Windows installer | Ship the LGPL text and a notice; keep them as separate, replaceable shared libraries (never linked statically); say where to get the source |
| SBCL runtime | Public domain / BSD-style | Part of the saved executable | Keep the notices |
| Swank (SLIME) | Public domain unless a file says otherwise | Bundled; loaded into the target image | Check each bundled file |
| Quicklisp libraries (JSON, HTTP server, etc.) | Choose MIT/BSD-style ones | Linked into the executable | Keep their notices |
| Claude Code CLI | Anthropic's commercial terms | **Not bundled**; the user installs it and signs in | None for us; users accept Anthropic's terms themselves |

An `about` page and a `THIRD-PARTY-NOTICES` file list every bundled
component with its license. When choosing Lisp libraries, avoid GPL ones
(they would require a GPL editor); LLGPL is acceptable but MIT/BSD is
preferred.
