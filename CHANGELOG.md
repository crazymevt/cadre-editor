# Changelog

## Unreleased

- **Databases**: a page of database connections (SQLite for now), saved with
  the project or for you alone. Tables open in tabs with a filter above each
  column, sorting and paging, and can be edited (changes are saved in one
  transaction); a SQL console runs statements into a Results page.
- New Lisp Project is now **New Project** (`M-x new-project`), since it
  makes Racket packages and Clojure projects too.

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
