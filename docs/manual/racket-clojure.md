# Racket and Clojure

<img alt="Clojure and Racket files side by side" src="../gallery/dark/racket-clojure.png">

Racket files (`.rkt` `.rktl` `.rktd` `.scrbl`) and Clojure files (`.clj`
`.cljc` `.cljs` `.cljd` `.edn` `.bb`) are read by the same lexer as Common
Lisp, using each language's own reader rules. They get:
- syntax colors, including `[ ]`, `{ }`, `#{ }` and the rest of each
  language's syntax
- what `#;` or `#_` comments out shown as a comment
- moving over forms, paredit's structural editing, with `[` and `{` typed in
  pairs in paredit mode
- indentation: Racket's conventions, and cljfmt's default style for Clojure
- folding, an Outline of the file's definitions, Toggle Comment and Format
  Document

**Clojure runs too**, through nREPL, as with CIDER. The first command that
needs it (`C-c C-c`, `C-RET`, or Clojure: Jack In from the palette) starts
the project's REPL: the nearest folder with `deps.edn`, `project.clj` or
`bb.edn`, with nREPL and cider-nrepl added. If the project already has a
REPL running (a `.nrepl-port` file), Cadre connects to it instead.
Clojure: Connect connects to any nREPL by host and port. Then the Common
Lisp commands work on Clojure code, in the file's namespace:
- evaluate a form, the last expression or the selection, with the value
  shown inline
- compile a form (errors go to Problems), and load the file
- describe a symbol, go to its definition, argument hints in the status bar
- completion, macroexpand, who calls, and interrupt
- the REPL (`C-c C-z`), on the panel's REPL page, which shows the REPL of
  the selected tab's language: Common Lisp's, Clojure's or Racket's
- errors with their stack on the Debugger page, Run Tests in Namespace
  (failures in Problems), and the Inspector
- clj-kondo's findings on save when it's installed, and indentation from the
  project's `.cljfmt.edn` and macros' `:style/indent`
- Look Up in Docs (`C-c C-d h`) after the Clojure docs are built (Settings →
  Clojure → Documentation, or Clojure: Build Docs): Clojure's standard
  namespaces' docstrings, from a Clojure started for it (not your project),
  and, if you choose, ClojureDocs' examples, downloaded to your machine

The status bar shows the Clojure connection while a Clojure tab is selected.
This needs the Clojure CLI (or Leiningen, or Babashka). The first start
downloads nREPL and cider-nrepl. New Project makes a Clojure project too
(`deps.edn`, with tests run by `clojure -M:test`).

**Racket runs too**, through Cadre's own back end (`vendor/cadre-racket/`,
needing only an installed Racket). The first command that needs it starts
Racket in the file's package (the nearest folder with `info.rkt`). Code is
evaluated in the file's module, and Run File (`C-c C-k`, or F5) runs the
module and puts the REPL in it, as DrRacket's Run does. Then:
- evaluate and compile forms, with values inline and errors in Problems;
  redefining a function changes its callers
- argument hints from the documentation's signatures, describe, completion,
  macroexpand, and Racket: Documentation (`C-c C-d`, or F1) to open a name's
  page in the installed docs
- go to definition and Find References from check-syntax, which reads the
  buffer's text without running it. Saving checks the file the same way:
  unbound identifiers, bad syntax and unused requires go to Problems.
- `raco make` and `raco test`, with the places they report in Problems
- the REPL (`C-c C-z`), on the REPL page while a Racket tab is selected
- Rename Symbol for a binding and its uses in the file, and run-time errors
  with their stack on the Debugger page
- Scribble files (`.scrbl`) with their @-expressions' text read as prose
- Look Up in Docs (`C-c C-h`) after the Racket docs are built (Settings →
  Racket → Documentation, or Racket: Build Docs): every documented name's
  signature from your installed Racket. Rebuild them after installing
  packages.
- a Racket package in New Project

Programs reading their input get the lines typed in the language's REPL
tab, and Claude's tools work in Racket and Clojure files too.
[docs/RACKET-CLOJURE.md](../RACKET-CLOJURE.md) has the plan and what is
still to come (ClojureScript).
