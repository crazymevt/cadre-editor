# Racket and Clojure in Cadre — plan

**Status:** Draft 2, 2026-10-14; open questions decided (section 6). R0 is built.

Cadre is built for Common Lisp. This plan adds Racket and Clojure as
first-class languages: the same structural editing, and as much of the live
connection as each language's tools allow. It is written against the code as
it stands (`src/core/lisp/`, `src/core/swank/`, the UI's Swank calls).

## 1. What each language gets

| Feature | Common Lisp today | Racket | Clojure |
| --- | --- | --- | --- |
| Highlighting, rainbow brackets, matching | Own lexer | Own lexer, dialect-aware | Own lexer, dialect-aware |
| Paredit, form navigation, folding, outline | Yes | Yes, with `[ ]` and `{ }` | Yes, with `[ ]`, `{ }`, `#{ }`, `#( )` |
| Indentation | SLIME rules + image | Racket table (racket-mode / DrRacket style) | cljfmt-style rules |
| REPL | Swank | Cadre's Racket back end | nREPL |
| Eval form / region / file | Yes | Yes (module-aware) | Yes (`eval`, `load-file`) |
| Completion, arglists, docs | Yes | Namespace + docs index | nREPL `completions`, `lookup` / cider-nrepl `info` |
| Go to definition | Yes | `drracket/check-syntax` | cider-nrepl `info` |
| Cross-references | Yes | check-syntax (in-file arrows), later project-wide | cider-nrepl `fn-refs` |
| Errors as Problems | Compiler notes | Exception srclocs, check-syntax | Stack-trace analysis, clj-kondo |
| Debugger | SLDB with restarts | Error + context, no restarts | Stack trace, no restarts |
| Inspector, macroexpand | Yes | Macroexpand (`expand-once`), no inspector at first | Macroexpand (cider-nrepl), inspector later |
| Stepper, Trace page | Yes | Later, if ever | Later, if ever |
| Docs page | HyperSpec | Racket's installed docs | `clojure.repl/doc` + ClojureDocs docset |
| Projects page | ASDF systems | `info.rkt`, `raco make`, `raco test` | `deps.edn` / `project.clj`, test runner |

ClojureScript (`.cljs`) gets the static editing at first. A live
ClojureScript REPL (shadow-cljs over nREPL) comes later. `.cljc` files are
read as Clojure, with reader conditionals highlighted.

## 2. Decisions

### 2.1 Keep our own lexer and generalise it; don't use tree-sitter

The Lisp lexer (`lexer.lisp`) drives highlighting, paredit, indentation, form
navigation and folding all at once. A tree-sitter grammar would only give
highlighting. Racket and Clojure differ from Common Lisp mostly in how they
read characters, so a **dialect** is a set of reader rules, not a new lexer:

| | Common Lisp | Racket | Clojure |
| --- | --- | --- | --- |
| Brackets | `( )` | `( ) [ ] { }` | `( ) [ ] { }`, `#{ }`, `#( )`, `#?( )` |
| Line comment | `;` | `;` | `;` |
| Block comment | `#\| \|#` nested | `#\| \|#` nested | none |
| Datum comment | none | `#;` | `#_` |
| Keywords | `:kw` | `#:kw` | `:kw`, `::kw`, `:ns/kw` |
| Characters | `#\a` | `#\a` | `\a`, `\newline` |
| Quoting | `'` `` ` `` `,` `,@` | same, plus `#'` `` #` `` `#,` (syntax) | `'` `` ` `` `~` `~@`, `@` (deref), `#'` (var) |
| Other | `#+`/`#-`, `\|sym\|` | `#lang` line, `#rx""`, `#px""`, here strings `#<<`, `#hash(…)` | `^meta`, `#"regex"`, `#inst`, tagged literals, `#:ns{…}` maps |
| Symbols, case | Upcased | Case-sensitive | Case-sensitive, `ns/name` |

As built in R0 (2026-10-14):

- A `dialect` struct (`src/core/lisp/dialect.lisp`) holds the reader rules:
  which brackets open lists, what ends a symbol, whether a comma is whitespace,
  `#| |#` and `|…|`, colon keywords, the namespace separator (`:` or `/`), and
  the definer heads. The `#` dispatch differs most between the three, so the
  lexer has one dispatch function per dialect. The indentation table and the
  highlighting symbol table join the struct in R1.
- `lex-line` and `make-lisp-syntax` take the dialect, which defaults to Common
  Lisp, so every existing caller is unchanged.
- The depth in the line state stays a count. Instead of recording which
  brackets are open in the state, each `:open` and `:close` token carries its
  bracket character in `token-subtype`, and `paren-match-at` reports a `]`
  closing a `(` as not matched. This leaves the state shape alone, which the
  indenter and the Markdown and REPL callers rely on.
- `#;` and `#_` are `:datum-comment` tokens. They belong to the element after
  them, as `#+` does, so form navigation steps over the pair. Showing that
  element in the comment color is the highlighter's job in R1, because it
  can span lines.
- Racket here strings (`#<<END`) add a `(:here depth "END")` state. Clojure's
  multi-line strings use the existing `(:string depth)` state.
- New token types: `:constant` (`#t`, `#false`) and `:datum-comment`, both
  mapped to faces in `faces.lisp`.

### 2.2 One "Lisp family" of modes

```lisp
(define-major-mode racket-mode  (:title "Racket"  :extensions ("rkt" "rktl" "rktd" "scrbl")))
(define-major-mode clojure-mode (:title "Clojure" :extensions ("clj" "cljc" "cljs" "edn" "bb")))
```

The dozen places that test `(eq … 'lisp-mode)` (`hints.lisp`'s `lisp-buffer-p`,
`highlight.lisp`, `bindings.lisp`, `format.lisp`, `refactor.lisp`, …) change to
`lisp-family-p`, which is true for any mode with a dialect. Each one is
then checked: some features (Swank-driven image colours, `in-package`
lookup) stay Common Lisp only and test the dialect. Keymaps are shared through
a parent `lisp-family` keymap, so paredit and evaluation keys are the same
everywhere. `major-mode-for-text` learns `#lang` and `(ns `.

`.scrbl` (Scribble) is Racket-reader text with `@`-expressions. It gets the
Racket dialect with `@` read as a prefix. Full Scribble highlighting comes later.

### 2.3 A connection protocol, with Swank as one back end

The UI calls Swank directly in about 80 places. The busiest files are
`debugger.lisp`, `claude-tools.lisp`, `systems.lisp`, `inspector.lisp`,
`lisp-eval.lisp`, `stepper.lisp` and `presentations.lisp`. DESIGN.md 8.1
already planned a `connection` protocol for Slynk. This plan builds it now:

- `src/core/backend.lisp`: a `language-connection` class and generic
  functions with callbacks on the main thread, as `swank-rex` has now:
  `backend-eval`, `backend-load-file`, `backend-completions`,
  `backend-arglist`, `backend-describe`, `backend-find-definitions`,
  `backend-xref`, `backend-macroexpand`, `backend-interrupt`,
  `backend-repl-send`, `backend-set-namespace`.
- `backend-supports-p connection feature` (`:inspector`, `:restarts`,
  `:stepper`, `:trace`, `:systems`, `:presentations`, …). Pages, menus and
  palette commands a connection can't do are hidden, not greyed out.
- `swank-connection` implements all of them with the calls it makes today.
  This is a refactor with no change in behaviour. `make test` and the full
  `make smoke` must pass unchanged before any new back end is added.
- Connections are per dialect. A buffer uses the default connection for its
  dialect, and the status bar shows that one (`Racket 9.3 · my-app/main.rkt`,
  `Clojure 1.12 · user`). Several can be open at once, as now.
- REPL, Output and Debugger pages take their connection from the current
  buffer, or from the page's own picker.

### 2.4 Clojure: nREPL, with cider-nrepl when we start the process

nREPL is what every Clojure editor speaks (CIDER, Calva, Cursive,
Conjure). It uses bencode over TCP, with sessions and ops.

- `src/core/nrepl/`: a bencode codec, a connection (reader thread, `id` →
  callback, `session`), and the back end methods above. It is GTK-free and
  tested headless against a real nREPL in CI.
- **Connect:** `M-x connect-clojure` reads `.nrepl-port` from the project,
  or asks for host and port.
- **Jack in** (`M-x clojure`): it starts the project's REPL with nREPL and
  cider-nrepl added, as CIDER does:
  - `deps.edn`: `clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version …} cider/cider-nrepl {:mvn/version …}}}' -M -m nrepl.cmdline --middleware '[cider.nrepl/cider-middleware]' --port 0`, plus the user's chosen aliases.
  - `project.clj`: `lein update-in :plugins conj '[cider/cider-nrepl "…"]' -- repl :headless`.
  - `bb.edn` / no project: Babashka (`bb nrepl-server`) if it is installed, or a plain `clojure` REPL.
  - Versions are pinned in an option, like Swank is in `vendor/`, and the
    packages come from Maven at jack-in. Cadre doesn't ship them.
    (cider-nrepl is EPL; using it at runtime is fine.)
- **Without cider-nrepl** (connected to someone else's plain nREPL): eval,
  load-file, completions and lookup (built-in ops since nREPL 0.8), interrupt,
  stdin. Go to definition, xref, the inspector and test running are hidden.
- **With cider-nrepl:** `info` (docs, arglists, file and line), `complete`,
  `fn-refs` and `fn-deps`, `macroexpand`, `analyze-last-stacktrace` (shown
  on the Debugger page as a stack trace with source links, without restarts),
  `test-var-query` (a Tests page), `inspect-*` (the Inspector), and `format-code`.
- **Static analysis:** clj-kondo, if installed, runs on save, and its findings
  go to Problems with underlines, as compiler notes do. clojure-lsp is not
  used: DESIGN.md keeps LSP out for now, and clj-kondo covers what matters
  most.

### 2.5 Racket: our own small back end, in Racket

Racket has no Swank or nREPL. racket-mode (for Emacs) has a back end, but
it is GPL-3 and shaped around Emacs. Cadre should ship its own, MIT-licensed,
in `vendor/cadre-racket/` (about 600–900 lines of Racket):

- **Wire:** the same 6-hex-digit-length framing and s-expressions as Swank
  (`(:emacs-rex …)` → `(:return …)`, plus `:write-string` and `:debug`-like
  events). `connection.lisp`'s framing and safe reader are reused as they are.
  Only the operations differ.
- **Starting:** `racket vendor/cadre-racket/main.rkt --port-file …`, the
  same as the Swank bootstrap. The port file, startup timeout and
  `*inferior-lisp*` output work the same way.
- **REPL namespace:** "run" a file means `(dynamic-require path #f)` then
  `module->namespace`, so the REPL is inside the module (racket-mode's
  model, which Racket programmers expect). Errors use `errortrace` when the
  user turns it on (it's slower).
- **What powers each feature:**
  - Eval and REPL: `eval` in the current namespace, with output captured per
    request.
  - Completions: `namespace-mapped-symbols` plus the module's imports.
  - Arglists and docs: the documentation index (`setup/xref`,
    `xref-binding->definition-tag`) and procedure arity. Links open the
    locally installed HTML docs.
  - Definitions, references and unbound identifiers: `drracket/check-syntax`
    (`show-content`) on the buffer's text, which needs no running module.
    This also gives "unbound identifier" Problems and in-file
    rename-symbol.
  - Macroexpand: `expand-once` / `expand` on the form, with syntax shown as
    data.
  - Projects page: `info.rkt` collections, plus `raco make` and `raco test`
    with results to Problems.
- All of these libraries are in the standard Racket distribution (checked on
  this Mac: Racket 9.3 from Homebrew has `drracket/check-syntax`,
  `setup/xref`, `scribble/xref` and `syntax-color`).

### 2.6 Indentation

Each dialect has its own table and rules, in the same `indent.lisp` engine:

- **Racket:** DrRacket and racket-mode conventions: `define`-like and
  `lambda`-like heads, `let` and `for` families (`for/list`, `for/fold`, …),
  `match`, `syntax-parse`, `struct`. A form in `[ ]` (a `let` binding,
  `cond` clause) lines up like a list. `#lang` heads aren't forms.
- **Clojure:** the cljfmt / "semantic" community style. Body forms
  (`defn`, `let`, `fn`, `when`, `ns`, `deftype` methods, …) indent 2. Other
  calls line up under the first argument. `[ ]`, `{ }` and `#{ }` line up their
  elements. A `.cljfmt.edn` in the project adds or overrides rules, so
  Cadre matches the formatter the project uses.
- When connected, indentation hints come from the image where the language
  has them (Clojure `:style/indent` metadata through cider-nrepl), as
  `learn-indentation` does now for Swank.

### 2.7 Docs pages

The HyperSpec page reads a docset. Each language gets the same page:

- **Racket:** the installed HTML docs are the source of truth. Going to a
  symbol's docs opens the tag the docs index gives, either in the browser or
  converted to a docset with the docset library, if that proves practical.
  Start with the browser.
- **Clojure:** `clojure.core` doc strings plus ClojureDocs examples, built
  once into a docset from ClojureDocs' export (its license is to be checked
  before it is committed). When connected, cider-nrepl `info` covers
  libraries.

### 2.8 Claude

The Claude tools in `claude-tools.lisp` call Swank directly. They move to
the `backend-*` functions, so Claude can evaluate, describe and find
definitions in a Racket or Clojure process the same way. Tool descriptions say
which language a connection runs, and the system prompt names the buffer's
dialect.

## 3. Milestones

Each milestone ends with `make test` and the relevant smoke sections passing
(`make smoke ONLY=…`), and a full `make smoke` at the end.

| # | Milestone | Main files | Size |
| --- | --- | --- | --- |
| R0 | **Dialects in the lexer:** `dialect` struct, bracket kinds, datum comments, Racket and Clojure readers; CL unchanged | `lisp/lexer.lisp`, new `lisp/dialect.lisp`, tests | M |
| R1 | **Modes and static editing:** `racket-mode`, `clojure-mode`, `lisp-family-p` where `lisp-mode` is tested, highlighting tables, paredit with brackets, indentation tables, outline and folding, comment toggling, project detection (`info.rkt`, `deps.edn`, `project.clj`, `bb.edn`), New Project templates | `modes.lisp`, `paredit.lisp`, `indent.lisp`, `definitions.lisp`, `ui/highlight.lisp`, `ui/outline.lisp`, `new-project` | L |
| R2 | **Connection protocol:** `language-connection` + `backend-*` generics; Swank ported with no change in behaviour; pages hide what a connection can't do | new `core/backend.lisp`, the UI files that call Swank | L |
| R3 | **Clojure live:** bencode, nREPL connection, connect and jack-in, REPL, eval/load, completion, arglists, docs; with cider-nrepl: go to definition, xref, stack traces, macroexpand, test page; clj-kondo Problems | new `core/nrepl/`, `ui/repl.lisp`, `ui/debugger.lisp` | L |
| R4 | **Racket live:** `vendor/cadre-racket/` back end; start/connect, module-aware REPL, eval/run, completion, docs links, check-syntax definitions/references/unbound identifiers, macroexpand, `raco make`/`raco test` | new `vendor/cadre-racket/*.rkt`, `core/racket.lisp` | L |
| R5 | **Polish:** docs pages, Claude tools through the back end, `.cljfmt.edn`, `:style/indent`, ClojureScript via shadow-cljs, Scribble, CI jobs that run nREPL and Racket headless | various | M |

R0 and R1 are useful alone: Racket and Clojure files edit well with no
process running. R3 and R4 are independent of each other once R2 is done,
so they can be built in parallel (in separate sessions or branches).

## 4. Testing

- Lexer and indentation: table-driven tests per dialect, including
  real-world files (a few from Racket's own collections and `clojure.core`)
  that must lex without `:invalid` tokens and keep their indentation under
  indent-region.
- Back ends: headless tests start `racket` / `clojure` in CI (both are a
  package install on Ubuntu runners). They are skipped with a note when the
  program is missing, as tree-sitter tests are now.
- Smoke: new sections `racket-edit`, `clojure-edit`, `racket-repl`,
  `clojure-repl`, each opening a sample project in `tests/fixtures/`.

## 5. Packaging

Nothing new is bundled. Racket, Java, the Clojure CLI, Leiningen, Babashka
and clj-kondo are the user's own, found on `PATH` or set in Settings (a
"Languages" category, as for tree-sitter). The Racket back end ships as
source in `vendor/cadre-racket/`, like Swank in `vendor/slime/`. nREPL and
cider-nrepl come from Maven when Clojure starts. THIRD-PARTY-NOTICES only
gains a line if any ClojureDocs data is committed.

## 6. Decisions on the open questions (2026-10-14)

1. **Racket back end:** Cadre writes its own (2.5), MIT-licensed, in
   `vendor/cadre-racket/`. It doesn't use racket-mode's.
2. **Clojure jack-in:** cider-nrepl is added by default. An option turns it
   off, and a plain nREPL that someone else started still works with the
   smaller feature set (2.4).
3. **clj-kondo:** runs on save when it is installed, and an option turns it off.
4. **Other Schemes** (Guile, Chicken, Chez): later, after Racket. Once the
   dialect system exists, each is just a dialect and an indentation table.
5. **Order:** R0 → R1 → R2 → R3 (Clojure) → R4 (Racket) → R5.
