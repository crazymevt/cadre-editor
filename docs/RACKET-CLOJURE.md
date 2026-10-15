# Racket and Clojure in Cadre — plan

**Status:** Draft 2, 2026-10-14; open questions decided (section 6). R0 to R5 are built, except ClojureScript through shadow-cljs (3.1).

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

As built in R1 (2026-10-14):

```lisp
(define-major-mode racket-mode  (:title "Racket"  :extensions ("rkt" "rktl" "rktd" "scrbl")))
(define-major-mode clojure-mode (:title "Clojure" :extensions ("clj" "cljc" "cljs" "cljd" "edn" "bb")))
```

- `*mode-dialects*` (`modes.lisp`) maps each Lisp mode to its dialect, and
  `lisp-family-mode-p` is true for all three. `major-mode-for-text` reads
  `#lang` as Racket and `(ns ` as Clojure.
- Every Lisp-family buffer gets a syntax read by its dialect. Two accessors
  keep Swank away from Racket and Clojure without touching the places that
  call Swank: `buffer-sexp-syntax` returns any dialect's syntax, for editing
  (highlighting, motion, paredit, indentation, folding, Toggle Comment,
  Format Document). `buffer-syntax` returns only Common Lisp's, for
  everything Swank and Common Lisp's own rules drive: image colours, hints,
  evaluation, macroexpansion, the inspector, cross-references.
- Racket and Clojure keymaps inherit Lisp mode's editing keymap, and the
  structural-editing keys are bound in their profile keymaps. The keys that
  talk to the Lisp stay in Lisp mode's profile keymaps, so `C-RET` and `C-c C-c` do
  nothing in a `.rkt` or `.clj` file until R3/R4 give those languages a
  connection. In paredit mode, `[`/`]` and `{`/`}` pair up where the dialect
  makes them brackets, and insert themselves in Common Lisp.
- Highlighting: each dialect has its own table of built-in forms and
  constants (`faces.lisp`). Whatever `#;`/`#_` comments out is shown in the
  comment colour, across lines (`datum-comment-columns`). Clojure metadata
  (`^:private`) no longer hides the name in `(defn ^:private f …)`.
- The Outline page lists Racket and Clojure definitions read from the
  tokens (`dialect-definitions`). Go to Symbol uses the same list.
- `.scrbl` files open as Racket. Their `@`-expressions aren't read specially
  yet.
- Moved out of R1: project detection (`info.rkt`, `deps.edn`,
  `project.clj`, `bb.edn`) and New Project templates go to R4 and R3, where
  starting a REPL first needs them.

### 2.3 A connection protocol, with Swank as one back end

As built in R2 (2026-10-14):

- `src/core/backend.lisp`: a `lisp-connection` class (host, port, state,
  package, prompt, process), which `swank-connection` now extends, and
  generic functions for each request: `backend-eval`, `backend-eval-region`,
  `backend-compile-form`, `backend-compile-file`, `backend-load-file`,
  `backend-describe`, `backend-arglist`, `backend-autodoc`,
  `backend-completions`, `backend-find-definitions`, `backend-xref`,
  `backend-macroexpand`, `backend-repl-eval`, `backend-set-package`,
  `backend-interrupt` and `backend-disconnect`, plus `connection-language`,
  `connection-implementation` and `backend-supports-p`. A request a back end
  doesn't implement signals an editor-error naming the Lisp ("Clojure 1.12
  can't expand macros").
- Replies keep Swank's shapes (listed at the top of `backend.lisp`), so the
  UI's parsing (`parse-location`, `parse-compilation-result`, the completion
  and xref readers) is unchanged, and the nREPL and Racket back ends
  translate their answers into those shapes.
- `src/core/swank/backend.lisp` implements every request with the Swank call
  the UI made before, so Common Lisp behaves as it did. `make test` (with
  new tests through the protocol, against a real SBCL and a fake back end)
  and the full `make smoke` pass.
- The UI calls requests through `request` (`session.lisp`), which reports
  errors in callbacks as `rex` does. Evaluating, compiling, loading, go to
  definition, describe, argument hints, completion, cross-references,
  macroexpansion, the REPL's input and package, interrupting, and Claude's
  describe/arglist/definition/xref/macroexpand tools now go through it.
- Still Swank's own calls, since only Swank has them: the debugger and its
  restarts, the inspector, the stepper, the Trace page, ASDF systems,
  presentations, image colours, GTK apps, running the project's program,
  and Claude's evaluate (with output), compile, load-system, test-system
  and apropos tools.
- Connections per language: `*connection*` stays the Common Lisp one, and
  `*connections*` holds one per other language, started by the function a
  back end puts in `*connection-starters*`. `with-connection` takes
  `:language`. Until R3/R4 register starters, it says "Cadre can't run
  Clojure code yet".
- Moved to R3: hiding pages and commands a connection can't do. With Swank
  as the only back end there is nothing to hide yet, and nothing to test it
  with. The status bar, REPL page and Debugger page choosing their
  connection from the current buffer also come with R3.

### 2.4 Clojure: nREPL, with cider-nrepl when we start the process

As built in R3 (2026-10-14):

- `src/core/nrepl/` (GTK-free): `bencode.lisp`; `connection.lisp`, an
  `nrepl-connection` (a `lisp-connection`) with a reader thread, requests
  matched to answers by id, and two cloned sessions: one for evaluation and
  the REPL, one for tooling (completion, info), so those answer while code
  runs; `backend.lisp`, the protocol's requests; `start.lisp`, finding the
  project and starting its REPL.
- **Starting** (`clojure-jack-in`, or any Clojure command when nothing is
  connected): the project is the nearest folder with `deps.edn`,
  `project.clj` or `bb.edn`. If it has a `.nrepl-port` that answers, Cadre
  connects to that REPL. Otherwise it starts one in that folder:
  - `deps.edn`, or no project: `clojure -Sdeps '{:deps {nrepl/nrepl …
    cider/cider-nrepl …}}' -M<aliases> -m nrepl.cmdline --middleware
    '[cider.nrepl/cider-middleware]' --port 0`.
  - `project.clj`: `lein update-in :plugins conj '[cider/cider-nrepl "…"]'
    -- repl :headless :port 0`.
  - `bb.edn`: `bb nrepl-server 0`.
  The port comes from what the server prints, and its output goes to the
  Output page. Options (Settings → Clojure): `*cider-nrepl*` (on),
  `*nrepl-version*` 1.7.0 and `*cider-nrepl-version*` 0.62.2 (both pinned at
  releases a few months old), `*clojure-aliases*`, and the three commands.
  `clojure-connect` connects to any nREPL by host:port.
- **What works in a Clojure buffer**, through the same commands and keys as
  Common Lisp (Emacs keys follow CIDER's: `C-c C-c`, `C-M-x`, `C-x C-e`,
  `C-c C-k`, `C-c C-z`, `M-.`, `C-c C-d d`, `C-c C-m`, `C-c M-j`):
  evaluate a form, the last expression or the region (value inline and in the
  status bar); compile a form (a Clojure error becomes a note in Problems,
  at the place Clojure names); load the file; describe; go to definition;
  argument hints in the status bar; completion; macroexpand (once, or all);
  who calls / calls who (cider-nrepl's `fn-refs` and `fn-deps`); interrupt.
  Code is evaluated in the file's namespace, from its `(ns …)`. If that
  namespace doesn't exist yet, the file's `ns` form is evaluated first.
- **The REPL** is a tab, `*clojure-repl*` (`C-c C-z` from Clojure code, or
  `clojure-repl-tab`), like the editor REPL: Clojure highlighting and
  paredit, history, the namespace in the prompt, output and errors as they
  come. It starts in `user`. `clojure-set-namespace` (`C-c M-n n`) switches
  it to the file's namespace.
- **The status bar** shows the Clojure connection (`● Clojure 1.12.6  user`)
  while a Clojure tab is selected, and the Common Lisp one otherwise.
  Clicking it shows that language's REPL.
- **Hidden in Clojure buffers:** the right-click menu offers Go to
  Definition, Describe, Evaluate, Find References and a Clojure submenu
  (Macroexpand, Switch the REPL to This Namespace, Load File), not the
  HyperSpec, tracing or stepping. Swank-only commands keep their Lisp-mode
  `:modes`, so the palette doesn't offer them there.
- **Without cider-nrepl** (someone else's plain nREPL): evaluation, loading,
  the REPL, completion (nREPL's `completions`) and interrupt work.
  Describing falls back to `clojure.repl/doc` and macroexpansion to
  `macroexpand-1`. Go to definition uses nREPL's `lookup` if the server has
  it. Cross-references say they can't be found.
- **Tested:** `tests/nrepl.lisp` (bencode, the helpers, and every request
  against a real nREPL with cider-nrepl, skipped without the Clojure CLI)
  and the `clojure` smoke section (starting from a `deps.edn` project,
  compiling, inline values, argument hints, `M-.`, describe, a compile
  error, the REPL tab, disconnecting).
- **Not yet, moved to R5:** stack traces on the Debugger page
  (`analyze-last-stacktrace`), a Tests page, the inspector, reading input
  the program asks for (`need-input`), clj-kondo on save (not installed
  here to test against), and a Clojure New Project template (since built in R5).

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

As built in R4 (2026-10-14):

- `vendor/cadre-racket/main.rkt` (MIT, about 600 lines, standard
  distribution only, its heavier libraries loaded lazily) and
  `src/core/racket/backend.lisp`. A `racket-connection` is a
  `framed-connection`, the Swank framing and reader split out of
  `swank-connection`, so it reuses `swank-connect`, `swank-rex` and the safe
  reader. Requests are `(:emacs-rex (:op arg …) package t id)`. The back end
  writes its answers so Common Lisp's reader reads them (`#t` is `t`, `#f`
  and `'()` are `nil`, strings escape only `"` and `\`), as plain data that
  the methods put in Swank's shapes.
- **Starting:** `start-inferior-lisp` gained `:arguments`, `:directory`,
  `:server` and `:timeout`, so `start-racket` runs `racket main.rkt
  --port-file …` in the file's package folder (the nearest with `info.rkt`)
  the same way Swank starts. Any Racket command starts it, as for Clojure.
  It exits when its connection or its standard input closes. Options
  (Settings → Racket): `*racket-command*`, `*raco-command*`,
  `*racket-startup-timeout*`.
- **Modules:** a package is a module file's path. Code from a file is
  evaluated in that module's namespace (`module->namespace`), and the module
  is run from source first if it hasn't been (with
  `compile-enforce-module-constants` off, so redefining a function changes
  its callers). Run File (`racket-run`, `C-c C-k`, F5) runs the module
  afresh, with its `main` submodule, and puts the REPL in it, as DrRacket's
  Run does. The REPL starts in a plain `racket` namespace.
- **Lookups:** arglists come from the documentation's signatures
  (`scribble/blueboxes`), else the `define` in the binding's source file,
  else the procedure's arity. Describe shows the signature, its contracts
  and the docs' URL; Racket: Documentation (`C-c C-d`, F1) opens that page.
  Completion lists the namespace's symbols, by kind.
- **check-syntax**, on the buffer's text (saved or not), with nothing run:
  go to definition (an arrow in the file, or an import's `define` in its
  library); Find References (the binding's uses in the file); and Problems:
  the first error (an unbound identifier, bad syntax) and unused requires,
  checked on every save while Racket runs, or with Racket: Check Syntax.
- **raco:** `raco-make` (the file) and `raco-test` (the package folder, or the
  file) run like Build Project, on the Output page; the places they report
  (`file.rkt:3:4:` and rackunit's `location:`) go to Problems.
- **New Project** has a Racket package kind: `info.rkt`, `main.rkt` (with
  `test` and `main` submodules), README, `.gitignore`, LICENSE.
- **The REPL tab** (`*racket-repl*`) shares its code with Clojure's
  (`ui/language-repl.lisp`). Output arrives a line at a time; entering a
  module replaces the prompt with a note and the module's prompt.
- **Not yet, moved to R5:** errortrace stack traces, the Debugger page,
  rename by check-syntax, program input, and docs in a Docs page rather than
  the browser.
- **Tested:** `tests/racket.lisp` (the helpers, the New Project template with
  `raco test` on it, and every request against a real Racket, skipped
  without one) and the `racket` smoke section (starting, compiling, inline
  values, argument hints, `M-.`, describe, Run File, the REPL in the module,
  a syntax error, Check Syntax, stopping).

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
| R1 | **Modes and static editing** (built): `racket-mode`, `clojure-mode`, editing in all three Lisps, highlighting tables, paredit with brackets, indentation tables, outline and folding, comment toggling. Project detection and New Project templates moved to R3/R4 | `modes.lisp`, `paredit.lisp`, `indent.lisp`, `definitions.lisp`, `ui/highlight.lisp`, `ui/outline.lisp`, `new-project` | L |
| R2 | **Connection protocol** (built): `language-connection` + `backend-*` generics; Swank ported with no change in behaviour; pages hide what a connection can't do | new `core/backend.lisp`, the UI files that call Swank | L |
| R3 | **Clojure live** (built; see 2.4 for what moved to R5): pages and commands a connection can't do hidden; status bar, REPL and Debugger pages following the buffer's language; `gtk-thread-source` only for Common Lisp; project detection (`deps.edn`, `project.clj`, `bb.edn`), a New Project template, bencode, nREPL connection, connect and jack-in, REPL, eval/load, completion, arglists, docs; with cider-nrepl: go to definition, xref, stack traces, macroexpand, test page; clj-kondo Problems | new `core/nrepl/`, `ui/repl.lisp`, `ui/debugger.lisp` | L |
| R4 | **Racket live** (built; see 2.5 for what moved to R5): project detection (`info.rkt`), a New Project template, `vendor/cadre-racket/` back end; start/connect, module-aware REPL, eval/run, completion, docs links, check-syntax definitions/references/unbound identifiers, macroexpand, `raco make`/`raco test` | new `vendor/cadre-racket/main.rkt`, `core/racket/backend.lisp`, `ui/racket.lisp`, `ui/language-repl.lisp` | L |
| R5 | **Polish** (built, except docs pages and shadow-cljs; see 3.1): CI runs the Racket and nREPL tests; program input; clj-kondo on save; `.cljfmt.edn` and `:style/indent`; Racket rename; Clojure and Racket errors on the Debugger page; Clojure tests and inspector; Claude tools by language; a Clojure New Project template; Scribble's @-expressions | various | M |

R0 and R1 are useful alone: Racket and Clojure files edit well with no
process running. R3 and R4 are independent of each other once R2 is done,
so they can be built in parallel (in separate sessions or branches).

### 3.1 R5 as built (2026-10-14)

- **CI:** the test workflow installs Racket (the full distribution) and the
  Clojure CLI. `CADRE_REQUIRE_LANGUAGES` makes their live tests fail, not
  skip, when either is missing.
- **Program input:** `backend-send-input` answers a program reading its
  standard input: Swank's `:emacs-return-string` (which the Racket back end
  takes too) or nREPL's `stdin`. nREPL's `need-input` and the Racket back
  end's asking input port become `:read-string` events, and the language's
  REPL tab takes the next line.
- **clj-kondo:** saving a Clojure file lints its text when
  `*clj-kondo-command*` is installed; findings go to Problems.
- **Clojure indentation:** a project's `.cljfmt.edn` (`:indents`,
  `:extra-indents`, `:block`/`:inner` rules) is read when a file opens.
  cider-nrepl's state messages bring each macro's `:style/indent`. Both make
  those heads indent as body forms (cljfmt's `[:block n]` argument count
  isn't followed yet).
- **Racket rename:** Rename Symbol in a Racket file renames the binding at
  the cursor and its uses in the file (check-syntax), in one undo step.
- **Debugger page:** Clojure and Racket errors arrive already unwound, as
  post-mortem levels: message, type, stack (frames with a source open it),
  Dismiss and Ask Claude. Clojure's stack is cider-nrepl's
  `analyze-last-stacktrace`; Racket's is the exception's context, which
  without errortrace shows little for tail calls (errortrace isn't used:
  it slows everything).
- **Clojure tests and inspector:** Run Tests in Namespace (cider-nrepl's
  `test-var-query`) puts failures in Problems at their lines, with a summary
  in the status bar (Problems, not a separate Tests page). Inspect uses
  cider-nrepl's inspector on the Inspector page.
- **Scribble:** a Scribble dialect lexes @-expressions, their bodies as
  prose. Multi-line strings and block comments inside an @-expression's
  code drop back to plain Racket lexing at the line's end.
- **Docs (decided 2026-10-14):** Look Up in Docs (`C-c C-d h`, `C-c C-h`
  in Racket, `C-k h`) shows Racket's and Clojure's docs in the HyperSpec
  tab, from docsets made on the user's machine in `~/.local/share/docsets/`,
  nothing shipped with Cadre:
  - **Racket: Build Docs:** `vendor/cadre-racket/docs.rkt` lists every
    documented export of the installed Racket with its signature and
    contracts (the docs' blue boxes, about 22,800 in 11 seconds). Each entry
    links to its full page.
  - **Clojure: Build Docs:** the arglists and docstrings of every public
    var in the namespaces the connected Clojure has loaded.
  - **Clojure: Build Docs with ClojureDocs Examples:** the same, plus
    ClojureDocs' examples, see-alsos and notes, from its public export
    (`clojuredocs-export.json`), downloaded by the user's machine. The
    export states no license, so it is never bundled; the docset credits
    ClojureDocs and its contributors.
- **Not done:**
  - **ClojureScript through shadow-cljs.** It needs Node, which this
    machine doesn't have to test against, and a second connection per
    project (CIDER's clj and cljs REPLs side by side).

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
