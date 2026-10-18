# Stabilization review — October 18, 2026

TODO items 5–7 (coverage/continuous testing, saved run configurations and
expression watches) are deferred while the implemented features are validated.

## Fixes and discoverability

- Rename rejects matches whose enclosing form or package context changed
  after preview, allowing unrelated definitions to change. Compact form
  fingerprints replace retained whole-file text. Checking only the old name's
  columns could rename quoted data or a different binding. A regression exercises
  both project and local rename application. When places are skipped, the
  preview now stays on the old name instead of switching to the new one,
  so the skipped places remain visible to rename again. Missing or unreadable
  files also count as skipped; a regression verifies that every selected
  occurrence is counted and a missing file is not recreated.
- GTK restart smoke checks retry title probes while the new window starts.
  The first full run observed `run-app-restart:NIL`: the probe ran before
  window creation, and the old check never tried again.
- The README identifies version 1.2.0 and distinguishes unreleased work.
  It and the manual index link to Runs, recovery and conversation history.
  The editing manual explains when to refresh a rename preview.

## Validation

Host: Apple silicon, macOS 27.2 build 26B5112a, SBCL 2.6.9. Test settings
and history were isolated from the user's configuration.

| Check | Result |
| --- | --- |
| `make test` | 1,609 checks passed, none failed or skipped |
| `make test-ui` | 602 checks passed after review follow-ups, none failed or skipped |
| `make test-recovery` | Actual SIGKILL recovery passed; newer disk text preserved |
| Full `make smoke` | 991 checks, one GTK startup-probe race; corrected afterward |
| `make smoke ONLY='run-app rename'` after fixes | 46 checks passed, none failed |
| `make smoke ONLY=rename` after reader/diagnostic follow-ups | 7 checks passed, including visible failure diagnostics and disabled checkboxes |
| `make app` | Built; opened this repository and exited normally |
| `scripts/make-app.sh --bundle` | Built with 70 bundled libraries; opened this repository and exited normally |
| `packaging/macos/package.sh` | Local disk image and SHA-256 checksum created; ad hoc signed, not notarized |
| GUI performance suite with transcript probe, repeated without another test running | Three existing budgets missed; timings below |

The full smoke run included Racket, Clojure, PostgreSQL and MariaDB checks.
Its 990 other checks passed. The focused rerun covers the changed rename
path and GTK restart probe; the entire suite was not rerun after those fixes.

The local app requires macOS 27.0 because it uses this host's Homebrew
libraries. It is a validation build, not a replacement for the macOS 15
release workflow. No version bump, tag or publication was performed.
The initial stabilization changes were committed as `8e7cbb8`; subsequent
review fixes are separate changes.

### Scale probes

Single-run timings, useful as baselines rather than performance guarantees:

| Operation | Fixture | Time |
| --- | --- | --- |
| Text search | 5,001 lines, 5,000 matches | 2.6 ms |
| Rename context analysis | Same file | 659.2 ms |
| Read and sort saved conversations | 100 conversations, 1,000 messages each | 41.3 ms |

Search and rename analysis run on worker threads. The conversation probe
measures storage loading, not rendering 100,000 messages into GTK widgets.

### GUI responsiveness

The performance suite now includes an exploratory 500-message transcript
probe, using the actual saved-entry rendering functions with recording
disabled. The second run was performed after the smoke tests finished:

| Measurement | Time | Existing budget |
| --- | --- | --- |
| Open a 10,000-line Lisp file | 320.8 ms | 300 ms — missed |
| Longest stall after opening a 1 MB Lisp file | 164.4 ms | 100 ms — missed |
| Longest stall opening a 5,000-file project | 112.5 ms | 100 ms — missed |
| Open a 10,000-line JavaScript file | 257.1 ms | 300 ms — passed |
| 500-message transcript through first paint | 206.4 ms | Exploratory |
| Longest stall restoring 500 messages | 146.4 ms | Exploratory |

Typing and scrolling stayed within their 16 ms budgets. Quick Open's
5,000-file listing took 99.1 ms, within 300 ms. The full table is written
to `build/perf/latest.txt` by `make perf`. These results identify follow-up
work; this pass does not claim all performance budgets are met.

## Responsiveness follow-up — October 18, 2026

A fresh pre-change run reproduced all three opening budget misses. Targeted
function timings identified project discovery walking above the open folder
when `/var` and `/private/var` named the same macOS directory, a hidden HTTP
page scanning the new project's files, and gutter fold discovery lexing a whole
Lisp file on the GTK thread.

Project discovery now recognizes the canonical boundary. The HTTP page defers
its scan until opened. Automatic Lisp fold discovery uses a private source
snapshot on a worker and rejects results after edits, mode changes or buffer
closure; explicit fold commands still calculate fresh ranges when requested.

Restored conversations render the latest 50 entries with earlier pages available
on demand, preserving the complete saved transcript. Scroll requests are
coalesced. The performance probe now uses the same paged rendering path as
restoration, with a 100 ms stall budget and a 300 ms first-paint budget.

The final full `make perf` run, without another test running, met every budget:

| Measurement | Fresh baseline | After fixes | Budget |
| --- | --- | --- | --- |
| Open a 10,000-line Lisp file | 368.3 ms | 230.2 ms | 300 ms |
| Longest stall opening a 1 MB Lisp file | 122.4 ms | 28.4 ms | 100 ms |
| Longest stall opening a 5,000-file project | 160.7 ms | 69.8 ms | 100 ms |
| 500-message transcript through first paint | 204.0 ms | 38.1 ms | 300 ms |
| Longest stall restoring 500 messages | 145.6 ms | 19.9 ms | 100 ms |

The transcript comparison measures initial restoration; the new path creates
only the latest page's widgets. Earlier entries remain accessible and load in
chronological order. These are individual measurements on this macOS host,
not cross-platform performance guarantees.

Follow-up validation: `make test-ui` passed 665 checks. Focused
`make smoke ONLY='conversations conversation-pages folding'` passed 109 checks,
including Claude/Codex resume, complete chronological pagination and folding.
Regressions cover symlinked project boundaries, coalesced scrolling, deferred
gutter discovery and rejecting stale fold snapshots.

## Remaining release checks

- Run Linux validation on Ubuntu 24.04 or newer; this host has no Linux
  runtime. The release workflow already builds the tarball and `.deb`,
  starts the packaged app under Xvfb, and checks `.deb` installation.
- Before publishing, choose the next version, move its entries out of
  Unreleased, and build on the supported release runners. Keep the changelog's
  Unreleased section above the latest release.
- Repeat responsiveness validation on Linux; the macOS follow-up above addresses
  the measured opening and transcript stalls on this host.
