# Releasing Cadre

## Packages

| Command | Writes | GTK |
| --- | --- | --- |
| `make app` | `build/Cadre.app`, for your own Mac | Homebrew's |
| `make release-macos` | `dist/Cadre-VERSION-macos-arm64.dmg`, and `dist/Cadre-VERSION-macos-sources.tar` | Bundled in `Contents/Frameworks/` |
| `make release-linux` (on Linux) | `dist/cadre-VERSION-linux-ARCH.tar.gz`, and a `.deb` where `dpkg-deb` exists | The distribution's (GTK 4.14+, libadwaita 1.5+) |

All three run `build/app/cadre.core` (saved by `scripts/build-app.lisp`)
with a copy of the SBCL runtime that saved it. The core is kept apart from
the runtime because macOS can't code-sign an executable with an image
appended to it, and notarization needs a valid signature. On macOS a small
launcher (`packaging/macos/launcher.c`) starts the runtime with a 4 GB heap;
on Linux `bin/cadre` is a shell script that does the same.

The Lisp libraries are compiled into the core, so nothing Lisp is needed at
run time, except for the REPL: that is the user's own `sbcl` (or another
Lisp), with Cadre's bundled Swank loaded into it. Git, tree-sitter,
Prettier and the Claude Code CLI also stay the user's.

## Making a release

1. Set `:version` in `cadre.asd` (semantic versioning), and add a
   `## VERSION` section to `CHANGELOG.md`: it becomes the release's notes.
2. If Cadre needs newer gtk4 or docset commits or a newer Quicklisp dist,
   update `packaging/versions.env` (push those repositories first).
3. Commit, tag `vVERSION`, push the tag. `.github/workflows/release.yml`
   builds both packages, starts each once, and makes a **draft** release
   with the files and `SHA256SUMS`. Check it, then publish it.

Started from the Actions tab, the workflow only builds (artifacts, no
release).

## Signing and notarizing (macOS)

Without a Developer ID the app is signed ad hoc: it runs, but a downloaded
copy is blocked by Gatekeeper (System Settings → Privacy & Security → Open
Anyway, or `xattr -dr com.apple.quarantine /Applications/Cadre.app`).

To sign and notarize, join the Apple Developer Program, make a "Developer
ID Application" certificate and an App Store Connect API key (Developer
role), and add the repository secrets listed at the top of
`release.yml`. Locally: `CODESIGN_IDENTITY="Developer ID Application: …"`
and `NOTARY_KEYCHAIN_PROFILE` (from `xcrun notarytool store-credentials`)
before `make release-macos`.

Everything is signed with the hardened runtime and
`packaging/macos/Cadre.entitlements`: SBCL needs executable memory
(`allow-jit`, `allow-unsigned-executable-memory`), and Cadre loads
libraries it didn't ship (tree-sitter grammars it compiles, Homebrew's
tree-sitter) (`disable-library-validation`).

## Bundling and licenses

- **macOS:** GTK, libadwaita, VTE and their ~70 libraries (77 MB; the disk
  image is ~53 MB) come from Homebrew, unmodified apart from their load
  paths. Almost all are LGPL, MIT or BSD-style. LGPL is fine as separate,
  replaceable shared libraries: `THIRD-PARTY-NOTICES.md` says how to
  replace one, and `Contents/Resources/licenses/` holds the texts. One is
  **GPL-2.0+: lzo**, which GTK pulls in through cairo-script-interpreter
  (and gdk-pixbuf's icns loader says GPL). That's allowed with MIT-licensed
  Cadre, as long as the source is offered from the same place as the
  binary (GPLv2 §3, GPLv3 §6, LGPLv2.1 §6). So each release also carries
  `Cadre-VERSION-macos-sources.tar` (`packaging/macos/sources.sh`, ~160 MB):
  for every bundled package under the GPL or LGPL, the source release
  Homebrew built from, the formula it was built with, and Homebrew's
  patches. It refuses to run when an installed package is older than its
  formula (the source would not match); on a fresh build machine they
  match. `SOURCES_ALLOW_OUTDATED=1` makes that a warning, for a test build
  on your own Mac only.
- Homebrew builds for the macOS it runs on, so the app needs at least the
  build machine's macOS (`LSMinimumSystemVersion` is set from the
  libraries). The workflow builds on `macos-15`. Only Apple silicon is built.
- **Linux:** GTK comes from the distribution (bundling it, as AppImage
  does, is fragile; Flatpak would need Cadre to run `sbcl`, `git` and the
  rest through `flatpak-spawn --host`). The packages need glibc from
  Ubuntu 24.04 or newer.
- `scripts/make-app.sh --bundle` fails if any library still points outside
  the app. When it starts, Cadre points gdk-pixbuf and GIO at the app's own
  modules (so Homebrew's don't load beside the app's GLib), then removes
  those variables so the programs it starts don't inherit them.

## Not done yet

- Update checks: e.g. look at GitHub's latest release once a day (opt-in)
  and show a toast. A Homebrew cask in a tap would give `brew upgrade`.
- Intel Macs, Linux arm64, Flatpak, Windows.
