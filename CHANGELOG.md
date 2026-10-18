# Changelog

## Unreleased

- **More responsive opening and conversation history.** Project discovery
  stops at the open folder even when its path uses a different symlink spelling.
  Hidden HTTP pages no longer scan a newly opened project; they catch up when
  shown again. Automatic Lisp fold discovery runs on a private background
  snapshot, one worker at a time, discards stale results and doesn't retry a
  failure until the next edit.
  Saved assistant transcripts initially show the latest 50 entries, with earlier
  entries available on demand, and repeated scroll requests share one callback.

- **Sturdier rename previews.** If a file's context can't be checked, the
  search still finishes and that file's rows say why, with their check
  boxes disabled until a refreshed preview can check them. Adding or
  removing an unrelated form no longer skips a match that stays where the
  preview found it, while adding a reader conditional (`#+`, `#-`) in front
  of a match's form does. Checking context before renaming is faster in
  large files.

- **Rename rejects changed context.** Matches whose enclosing form or package
  context changed after preview are skipped, while edits to unrelated
  definitions do not block unchanged matches. Compact fingerprints keep
  previews from retaining whole files. The preview stays on the old name so the
  skipped places can be checked and renamed again. Files that disappear
  or become unreadable also count as skipped, keeping the old name in
  the preview when their replacements could not be applied.

- **Reference search package context.** Finding references without a Lisp
  connection uses the requested symbol's package rather than a previous
  rename's package, and a qualified name such as `other:foo` uses its
  prefix's package. GTK smoke checks also rate-limit the initial title probe
  and its retries together.

- **Finding recent features.** The README and manual index now point to
  Runs history, local history and recovery, and saved assistant conversations.
  The README identifies the current 1.2.0 release and the unreleased changes.

- **Rename knows packages and local bindings.** Renaming a local variable
  or function (bound by `let`, a lambda list, `dolist`, `flet` and the like)
  changes that binding and the uses that refer to it, in its form: not an
  inner binding of the same name, nor a call to a function of that name.
  Renaming any other symbol finds it across the project by package: its
  prefix, `defpackage` clauses, or the `in-package` before it, resolved by
  the connected Lisp when there is one. The Search page shows each place as
  the symbol, a `defpackage` clause, another symbol of that name, a local
  binding, or one it can't tell; only the symbol's own start checked.
  Places that changed since they were found are left alone.

- **Earlier assistant conversations.** Cadre keeps the conversations you
  have in the Assistant panel, Claude's and Codex's, as you go: messages,
  replies, tool calls, plans, approvals with your answers, and proposed
  edits with their changes and outcomes. The new history button (or
  `M-x assistant-conversations`) lists them; choosing one shows its
  transcript, and the next message continues it on the same Claude Code
  session or Codex thread. Nothing runs again, and approvals given for the
  conversation aren't restored. `*assistant-conversations-kept*` (default
  100) limits how many are kept, removing those with the oldest activity
  first. A conversation is saved as soon as Claude Code or Codex starts its
  session, so it can be resumed even if no reply came; damaged records are
  skipped.

- **Runs history across restarts, by project.** Finished runs are kept for
  the next session, each with the last 64 KB of its output. Build and
  process runs from earlier sessions can be rerun. A project menu on the
  Runs page shows one project's runs; with All projects, each run names its
  project, and runs from earlier days show their date. The Clear button
  (or Project › Clear Finished Runs…) forgets the finished runs shown, for
  one project or all, after asking; runs still going are kept. A damaged
  record in the saved history is skipped without losing the others.

- **Format on save no longer blocks the editor.** External formatters such
  as Prettier now run in the background, for saving and for Format Document
  alike. A formatter that takes longer than 20 seconds is stopped. If it
  fails, takes too long, or the text changes meanwhile, the file is saved
  unformatted. Saves of a file still happen in the order they were asked
  for.

- **API client redirects.** Redirects are followed again for requests whose
  headers are only standard ones (Content-Type, Accept, User-Agent and
  similar). A header that could carry a secret still stops a redirect, since
  curl would send it to the new host, and the status line now says so.

- **Trim trailing whitespace keeps meaningful spaces.** Two or more spaces
  ending a Markdown line (a line break) are kept, and so are the spaces of
  Lisp lines that end inside a string.

- **Runs page output.** Streaming output is added as it arrives instead of
  being redrawn ten times a second, so you can select or scroll back while a
  run continues. The view follows the end only when it is already there. A
  GTK app's run no longer collects what you evaluate in the REPL while it
  runs.

- **Codex Stop.** A stopped Codex turn now says "Stopped." instead of
  reporting a failure.

- **Unsaved-buffer recovery after a crash.** Each Cadre process now holds a
  lock on its recovery folder. Checkpoints are no longer hidden when the
  crashed process's ID has been reused by another program.

- **MySQL single-float parameters** are written as valid numbers.

- **Markdown preview.** A tab switched to its rendered preview shows GTK's
  Copy and Select All menu and no symbol tooltips. Previously it showed the
  code editor's menu and tooltips.

- **MySQL string values are safe in every sql_mode.** Values in queries and
  table edits are now sent as hex string literals. Previously, with the
  server's default sql_mode, a backslash in a value was read as an escape:
  it corrupted text such as `C:\new` and let a value like `\' OR 1=1 -- `
  run as SQL.

- **Codex acts only through Cadre's tools.** Codex's own shell commands,
  plugins (computer use, the browser) and apps are now off, and so are the
  MCP servers configured in `~/.codex/config.toml`, for the threads Cadre
  starts. Previously, commands that fit the read-only sandbox could run
  without asking. Threads use the `untrusted` approval policy, and Cadre
  declines what Codex asks to run.

- **Session approvals are per assistant.** "Allow for this session" now
  covers only the assistant that asked. Switching between Claude and Codex no
  longer carries an approval over. A new conversation forgets only that
  assistant's approvals.

- **More activities in Runs.** Record Test Explorer selections, REPL tests,
  and console, GTK, Racket and Clojure apps with retained output and status.
  Rerun in the original project or stop the selected activity; test selections
  include per-test failures, and stopped or disconnected runs retain their state.

- **macOS troubleshooting.** Document the window-dragging session issue and
  the confirmed sign-out/sign-in recovery in the README.

- **Test preference isolation.** UI and crash-recovery tests now isolate both
  configuration and state so temporary buffers cannot overwrite the user's
  preferences.

- **Local history and unsaved-buffer recovery.** Keep bounded saved revisions
  outside the project, compare them with the current buffer and restore as an
  undoable unsaved edit. Checkpoint unsaved files and untitled buffers, offer
  recovery from exited processes as separate copies, and preserve newer disk
  contents. Scratch autosaves now replace their file atomically.

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
