# Swank, bundled with Cadre

This is the Lisp side of [SLIME](https://github.com/slime/slime) **2.32**,
copied from Quicklisp's `slime-v2.32`. Cadre loads it into the Lisp you
work with (see `src/swank/inferior.lisp`); the Emacs Lisp files are left out.

## What was left out, and why

| File | Reason |
| --- | --- |
| `*.el`, `doc/`, `lib/`, tests | Emacs-only |
| `swank/clisp.lisp` | GPL; bundling it would make Cadre GPL. CLISP users can load Swank from Quicklisp (`*swank-source*` :quicklisp). |
| `contrib/swank-media.lisp` | GPLv2 or later; not needed |

## Licenses of what is here

SLIME's README: "All files, unless explicitly stated otherwise, are public
domain." The files that say otherwise:

| File | License |
| --- | --- |
| `swank/ccl.lisp` | LLGPL |
| `swank/corman.lisp` | zlib-style (permissive) |
| `swank/match.lisp` | Permissive, keep the copyright notice |
| `xref.lisp` | Permissive (Mark Kantrowitz, 1990), keep the notice |

To update: copy the same files from a newer SLIME, repeat the license check
(`grep -il "GPL\|copyright"`), and run `make test`.
