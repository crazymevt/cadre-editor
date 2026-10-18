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
| `make test-ui` | 570 checks passed after review follow-ups, none failed or skipped |
| `make test-recovery` | Actual SIGKILL recovery passed; newer disk text preserved |
| Full `make smoke` | 991 checks, one GTK startup-probe race; corrected afterward |
| `make smoke ONLY='run-app rename'` after fixes | 46 checks passed, none failed |
| `make app` | Built; opened this repository and exited normally |
| `scripts/make-app.sh --bundle` | Built with 70 bundled libraries; opened this repository and exited normally |
| `packaging/macos/package.sh` | Local disk image and SHA-256 checksum created; ad hoc signed, not notarized |
| GUI performance suite with transcript probe, repeated without another test running | Three existing budgets missed; timings below |

The full smoke run included Racket, Clojure, PostgreSQL and MariaDB checks.
Its 990 other checks passed. The focused rerun covers the changed rename
path and GTK restart probe; the entire suite was not rerun after those fixes.

The local app requires macOS 27.0 because it uses this host's Homebrew
libraries. It is a validation build, not a replacement for the macOS 15
release workflow. No version bump, tag, commit or publication was performed.

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

## Remaining release checks

- Run Linux validation on Ubuntu 24.04 or newer; this host has no Linux
  runtime. The release workflow already builds the tarball and `.deb`,
  starts the packaged app under Xvfb, and checks `.deb` installation.
- Before publishing, choose the next version, move its entries out of
  Unreleased, and build on the supported release runners. Keep the changelog's
  Unreleased section above the latest release.
- Investigate the confirmed file/project opening stalls and the transcript
  rendering stall before treating responsiveness validation as complete.
