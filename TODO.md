# Cadre todo

Priorities from the October 18, 2026 IDE review. Checked subtasks describe
implemented work; a feature stays open until its remaining scope is covered.

## 1. Runs page

- [ ] Bring builds, tests and program runs together in a Runs panel.
  - [x] Keep recent process runs with status, duration and separate output.
  - [x] Filter output, open reported file locations, rerun and stop.
  - [x] Include Test Explorer, REPL tests and console/GTK/Racket/Clojure apps.
  - [ ] Retain history across restarts and group/filter by project.

## 2. Local history and crash recovery

- [x] Recover unsaved file and scratch buffers after an unexpected exit.
- [x] Keep bounded saved revisions outside Git, with compare and restore.
- [x] Handle external changes and newer disk contents without overwriting them.
- [x] Provide retention settings and a way to clear stored history.

## 3. Persistent assistant conversations

- [ ] Browse and resume Claude and Codex conversations inside Cadre.
- [ ] Restore transcripts, proposed edits and approval history without
      restoring permission grants or replaying actions.

## 4. More precise refactoring

- [ ] Make rename aware of package identity and lexical binding scope.
- [ ] Preview affected references, including ambiguous cases, before applying.

## 5. Coverage and continuous testing

- [ ] Show coverage in the editor and retain previous test results.
- [ ] Offer opt-in test reruns after saving, with cancellation and debouncing.

## 6. Saved run configurations

- [ ] Name configurations with arguments, environment, working directory and
      Lisp startup options; keep secrets out of shared project settings.

## 7. Live expression watches

- [ ] Pin expressions and their package/connection context beside the editor.
- [ ] Refresh explicitly or opt in to refresh after evaluation; show errors
      without disrupting other watches.
