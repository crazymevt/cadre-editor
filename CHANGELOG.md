# Changelog

## Unreleased

- **Runs page.** Builds, tests in a new Lisp and other project processes now
  have separate session history with status, exit code, duration and retained
  output. Filter output, open file:line diagnostics, rerun in the original
  project or stop the active process. The Output log still receives all output.

- **Reliable database smoke checks.** Trigger Test Connection directly so
  dialog animation timing cannot drop the simulated click.

- **Smoke test failure recap.** Repeat failed check names and details at the
  end of the smoke run, beside the totals, so failures stay easy to find.

- **Assistant approval prompts.** Shared evaluation, compilation, loading,
  test and save prompts now say “Assistant” so Codex requests are no longer
  attributed to Claude.

- **Reliable editor tabs.** Avoid adding an absent Markdown control to
  ordinary file tabs, which could interrupt tab creation and break tab
  selection, pin controls and Markdown preview switching.

- **Assistant layout in narrow panels.** Model, provider, mode and thinking
  controls wrap as the panel narrows. Status has its own row so usage and
  activity stay readable in the vertical layout.

- **Assistant thinking level.** Choose thinking effort beside the model in
  the Assistant panel for Claude and Codex. Codex offers the levels supported
  by the selected model; Default keeps the provider’s default behavior.

- **Markdown source and preview in one tab.** Each Markdown tab now has a
  Preview / Source button to switch views in place. Switching back restores
  the full editor, including line numbers and cursor updates. Closing a
  preview asks about unsaved changes in its source file.

## 1.2.0

- **Scratch button.** Open the project's persistent scratch buffer directly
  from the header bar.

- **CLOS browser.** Browse classes and generic functions in the connected
  Lisp by package and name, with inherited slots, method qualifiers and
  specializers, superclass and subclass links, source navigation and live
  inspection. The browser refreshes after evaluation and compilation;
  classes defined directly in the REPL are included.
- **Codex CLI support.** Choose Claude Code or Codex in the Assistant
  panel, with a separate conversation for each. Codex uses your existing
  `codex login` sign-in, offers model selection and Chat / Agent modes,
  streams replies with Markdown formatting, and receives the enabled File,
  Problems and Debugger context. It can use Cadre's editor and Lisp tools:
  edits appear as inline diffs to accept or reject, and evaluation,
  compilation, loading and tests use Cadre's approval prompts. Stop cancels
  pending tool approvals, and conversations follow the open project folder.

## 1.1.1

Fixes, and a manual. The packages are named as in 1.1.0, with 1.1.1 for
the version.

- The panel stays inside the window. Dragging its divider, or a narrow
  window, no longer pushes the panel's content off the edge: the dividers
  stop at each side's minimum size again, the panel is only as wide as the
  page it shows needs (not as its widest page), and the Profiler's toolbar
  scrolls sideways in a narrow panel, as the tabs do.
- Racket: documentation lookups made while the docs index was loading
  gave a short arglist (`(first x)`) and no documentation link. Lookups
  now take turns, and the index starts loading when the REPL starts.
- **A manual and a gallery.** The README's long feature list is now a manual
  in `docs/manual/`, a page for each part of Cadre, and `docs/gallery/`
  shows each one, taken from a real window by `make gallery`.
- The MCP server Claude Code talks to checks its token before reading a
  request's body, refuses bodies over 16 MB or with a bad length, closes
  connections that stall for 30 seconds, and serves at most 32 at once.
- Table edits on PostgreSQL and MySQL: a `?` inside a `/* */` comment, a
  MySQL `#` comment, a PostgreSQL `$$` string or an `E'…'` string is no
  longer taken for a parameter.
- A multi-line message (a condition's report) no longer makes the status
  bar taller; it shows on one line, in full in its tooltip.
- The project-wide Outline reads large files faster (a file's lines are
  found once, not again for each line).
- Malformed JSON numbers and negative bencode lengths are reported as JSON
  and bencode errors.

## 1.1.0

New tools for Lisp work, databases and HTTP APIs. The packages are named as
in 1.0.0, with 1.1.0 for the version.

### Lisp tools

- **Profiler**: Profile Form runs the form at the cursor under sb-sprof;
  Profile All Threads samples the whole Lisp, a running app included, until
  Stop. Functions are listed by self and total time, with callers and
  callees, or drawn as a flame graph that zooms on click.
- **Tests page** (Project › Test Explorer): a project's Parachute and FiveAM
  tests, Clojure's `deftest`s and Racket's `test-case`s, each passed or
  failed, with Run All, Run Failed and ▶; a failure shows its checks, and
  clicking a test goes to it.
- **Threads page** (Lisp › Threads): the Lisp's threads, to debug (interrupt
  into the debugger) or kill.
- **Libraries**: a system's `:depends-on`, with Remove, and a search of
  Quicklisp's systems; Add edits the `.asd` as text, keeping its layout and
  comments, and loads the library.
- **Expand in Place** (`Ctrl+K Ctrl+E`, `C-c M-e`) replaces the macro form at
  the cursor with its expansion in the file; Collapse (`Ctrl+K Ctrl+U`) or
  Undo folds it back, and saving collapses first.
- **Scratch buffer** (`Ctrl+K Shift+S`, `C-x M-s`) for each project, kept
  between sessions outside the project.

### Projects, databases and APIs

- **Outline: the whole project.** The Outline page's Project view lists every
  Lisp, Racket and Clojure definition in the project, grouped by package or
  namespace (or by file), with a filter and Sort by name; Go to Symbol in
  Project (`Ctrl+T`) picks one by name.
- **Databases**: a page of database connections (SQLite, PostgreSQL, MySQL and MariaDB), saved with
  the project or for you alone. Tables open in tabs with a filter above each
  column, sorting and paging, and can be edited (changes are saved in one
  transaction); a SQL console runs statements into a Results page. SQL
  files are highlighted (tree-sitter) and complete table and column names.
- **API requests**: a front end to curl. Requests are kept in the project's
  `.http` files (REST Client / JetBrains format), with environments in
  `http-client.env.json` and secrets kept outside the project; responses
  show status, time, headers and a body highlighted (and laid out) as JSON,
  XML or HTML. Saving offers to move secrets out of the file; curl commands
  can be copied and pasted. Bodies can be text, a file, a form or multipart
  (with file uploads). Cookies are kept in a jar for each environment. An
  Auth tab sets up Bearer, Basic and API-key
  authentication with the secret kept outside the project.
- New Lisp Project is now **New Project** (`M-x new-project`), since it
  makes Racket packages and Clojure projects too.

### Fixes

- macOS: with the window maximized and a dialog open, the rest of the
  window no longer goes see-through.

## 1.0.0

The first release of Cadre: a Lisp editor written in Common Lisp, with
Emacs's depth for Lisp and an interface like VS Code's, built on GTK 4 and
libadwaita.

### Packages

- **macOS** (Apple silicon, macOS 15 or newer): `Cadre-1.0.0-macos-arm64.dmg`,
  with its own GTK. The app is signed ad hoc, not by a registered developer,
  so macOS blocks it the first time: open System Settings → Privacy &
  Security and choose Open Anyway. `Cadre-1.0.0-macos-sources.tar` is the
  source of the GPL and LGPL libraries the app carries.
- **Linux** (x86-64, Ubuntu 24.04 or newer, or another distribution with
  GTK 4.14+ and libadwaita 1.5+): `cadre-1.0.0-linux-x86_64.tar.gz` and a
  `.deb`.
- Cadre itself is compiled into the package. For the REPL you need a Lisp
  (SBCL); Git, the Claude Code CLI, Racket, the Clojure CLI and the
  tree-sitter library are used when they're installed.

### Common Lisp

- Editing: Cadre's own incremental lexer for highlighting, rainbow
  parentheses, Lisp indentation (learning your macros' from the running
  Lisp), moving over and editing s-expressions, paredit mode, folding, an
  outline, and Format Document.
- A connected Lisp through a bundled Swank (SLIME 2.32): a REPL with
  presentations; evaluating and compiling forms and files, with compiler
  notes on a Problems page and in the code; argument hints; completion; go to
  definition; describe; cross-references; a macroexpander; highlighting from
  the running image (your macros, special variables, undefined functions).
- A debugger page (restarts, frames and their locals, evaluating in a frame,
  restarting or returning from one), an inspector, a trace page and a stepper.
- Projects: a Project page listing the folder's ASDF systems (load, reload
  with notes, test), New Lisp Project (libraries, programs and GTK apps,
  with Parachute or FiveAM tests), Build Project, Run Tests, and ▶ to run the
  project's program, GTK apps included.
- The HyperSpec, and libraries' docsets, in a tab beside the code.

### Racket and Clojure

- Both edit like Common Lisp: their own reader rules, brackets in paredit,
  their indentation styles, folding, outlines.
- **Clojure** through nREPL with cider-nrepl, as with CIDER: Jack In or
  connect; evaluate, compile and load, with errors in Problems; argument
  hints, completion, go to definition, describe, who calls, macroexpand;
  errors with their stack on the Debugger page; running a namespace's or the
  project's tests; the inspector; program input; clj-kondo on save;
  indentation from `.cljfmt.edn` and `:style/indent`.
- **Racket** through Cadre's own back end: code runs in its file's module,
  Run File as in DrRacket, argument hints from the documentation, go to
  definition, references, renaming and problems from check-syntax (without
  running the code), raco make and raco test, Scribble files.
- Their REPLs share the panel's REPL page, which shows the selected tab's
  language's.
- The Project page shows a Racket package's or Clojure project's
  dependencies (installing new ones; a `deps.edn` REPL loads them without a
  restart), modules or namespaces, and tests; ▶ runs its program.
- Docs: signatures from your installed Racket, docstrings of Clojure's
  standard library (with ClojureDocs' examples if you choose), and a
  project's own, all made on your machine for Look Up in Docs.
- New Project makes Racket packages and Clojure projects too.

### Claude

- A Claude panel using the Claude Code CLI, in chat or agent mode, with the
  editor as context; an MCP server through which Claude reads your buffers,
  asks the running Lisp (Common Lisp, Clojure or Racket) and proposes edits,
  shown as inline diffs you accept or reject.

### And

- Two key profiles (Standard and Emacs, with the kill ring, the mark,
  incremental search, query-replace, keyboard macros and numeric
  arguments), split editors, tabs, session restore, a terminal, Git (status,
  staging, blame, history, merges), project-wide search and replace,
  JavaScript/TypeScript/JSON/HTML/CSS/XML through tree-sitter, Markdown
  preview, pictures, color themes, and a settings page for every option.
