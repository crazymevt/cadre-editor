# Installing and running

## Installing a release

The [Releases](https://github.com/crazymevt/cadre-editor/releases) page has
packages with Cadre compiled in:

- **macOS** (Apple silicon, macOS 15 or newer): open the `.dmg` and drag
  Cadre to Applications. It carries its own GTK. It isn't signed by a
  registered developer yet, so macOS blocks it the first time: open System
  Settings → Privacy & Security and choose **Open Anyway** (or run `xattr -dr
  com.apple.quarantine /Applications/Cadre.app`).
- **Linux** (x86-64, with GTK 4.14+ and libadwaita 1.5+, as on Ubuntu 24.04
  or newer): install the `.deb` (`sudo apt install ./cadre_*.deb`), or unpack
  the `.tar.gz` anywhere and run `bin/cadre`.

For the REPL, install a Lisp (`brew install sbcl`, or `sudo apt install
sbcl`). The optional tools below (Git, Claude Code, Racket, the Clojure
CLI, tree-sitter, Prettier) are used when they're installed.

On the first run, Cadre checks for SBCL, Quicklisp, Git and Claude Code and,
if one is missing, shows the command that installs it, with a button to copy
it. It can install Quicklisp itself: **Install** downloads its installer,
runs it with SBCL and adds it to `~/.sbclrc`. Later, the same check shows
only when SBCL can't be found (`*check-setup-at-startup*` turns it off), and
**Check Setup…** in the main menu shows it any time.

## Setup

To run Cadre from source, or build its packages. Cadre is developed on macOS
(Apple silicon, with Homebrew) and also runs on Linux.

### Required

| What | Version | Why |
| --- | --- | --- |
| [SBCL](https://www.sbcl.org) | 2.2.9 or newer (tested with 2.6.9) | Cadre runs in it, and it's the default Lisp for the REPL |
| [Quicklisp](https://www.quicklisp.org) | — | Fetches Cadre's Lisp libraries (cl-ppcre, cffi, alexandria; parachute for the tests) on the first run |
| GTK | 4.14 or newer | The interface |
| libadwaita | 1.5 or newer | Window styling, light and dark themes |
| The [gtk4 bindings](https://github.com/crazymevt/gtk4) | — | Checked out next to Cadre (`../gtk4`), or in `~/quicklisp/local-projects/` |
| The [docset library](https://github.com/crazymevt/docset) | — | Reads the documentation the HyperSpec tab shows. Checked out next to Cadre (`../docset`), or in `~/quicklisp/local-projects/` |

The gtk4 bindings and the docset library aren't in Quicklisp's dist, so you
check them out yourself (step 3).

On macOS:

1. Install the system libraries:

   ```sh
   brew install sbcl gtk4 libadwaita
   ```

2. Install Quicklisp, if you don't have it (from
   [its instructions](https://www.quicklisp.org/beta/#installation)):

   ```sh
   curl -O https://beta.quicklisp.org/quicklisp.lisp
   sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
   ```

3. Check out Cadre, the gtk4 bindings and the docset library side by side,
   and link the two libraries into Quicklisp's local projects:

   ```sh
   mkdir -p ~/projects/lisp && cd ~/projects/lisp
   git clone https://github.com/crazymevt/cadre-editor.git
   git clone https://github.com/crazymevt/gtk4.git
   git clone https://github.com/crazymevt/docset.git
   ln -s ~/projects/lisp/gtk4 ~/quicklisp/local-projects/gtk4
   ln -s ~/projects/lisp/docset ~/quicklisp/local-projects/docset
   cd cadre-editor
   ```

   Cadre's scripts find the libraries next to it on their own. The links let
   anything else load them: the REPL, your own GTK programs, and the
   `docsets` project below. If Quicklisp doesn't see a new link, run
   `(ql:register-local-projects)` once.

   Any folder works. Cadre itself needs only one of the two: the libraries
   side by side with it, or linked into Quicklisp's local projects.

4. Start Cadre with `make run`. The first run takes a minute or two, while
   Quicklisp downloads the libraries and SBCL compiles Cadre and the bindings.
   Later runs start quickly.

On Linux (untested), install SBCL, GTK 4 and libadwaita from your
distribution. On Debian or Ubuntu, that's
`sudo apt install sbcl libgtk-4-1 libadwaita-1-0`. Then follow steps 2–4.

To update, pull all three, since Cadre may need a newer gtk4 or docset:

```sh
cd ~/projects/lisp
for repo in cadre-editor gtk4 docset; do git -C $repo pull; done
```

### Optional

Cadre works without any of these. Each one turns on a feature, and Cadre tells
you what's missing when you use a feature that needs it.

| Feature | Needs | Install (macOS) |
| --- | --- | --- |
| Git: the gutter, the Source Control page, history, merges | `git` | Comes with the Xcode Command Line Tools: `xcode-select --install` |
| [JavaScript, TypeScript, JSON, HTML, CSS and XML modes](other-languages.md) | The tree-sitter library, a C compiler, and `git` | `brew install tree-sitter`, and `xcode-select --install` for `cc`. Then run **Install Language Grammar…** in Cadre once for each language. |
| [Format Document](other-languages.md#format-document) for JavaScript, TypeScript and HTML | [Prettier](https://prettier.io), which needs Node.js | `brew install node`, then `npm install -g prettier`. Or point `*formatter-commands*` at another formatter. JSON, CSS, XML and Lisp need nothing. |
| The [Terminal](terminal.md) page | VTE, GNOME's terminal widget, for GTK 4 | `brew install vte3`. On Debian or Ubuntu: `sudo apt install libvte-2.91-gtk4-0` |
| [Claude](using-claude.md) | The [Claude Code](https://claude.com/claude-code) CLI, signed in | Install it from its site, then `claude auth login` |
| [Codex](using-claude.md#codex) | Codex CLI, signed in | Install Codex CLI, then `codex login` |
| Run GTK App, and GTK projects from New Project | The gtk4 bindings, which you already have from setup | — |

Some things need nothing extra:
- **Swank** is bundled (SLIME 2.32 in `vendor/slime/`).
- **Running a project's tests** uses whatever test library the project
  names. Quicklisp fetches it the first time.
- **A different Lisp for the REPL:** set `*lisp-command*` to start it
  (default `("sbcl" "--noinform")`). You can also use `M-x connect` to reach a
  Swank server you started yourself, local or remote.

Where Cadre keeps things:

| Path | Contents |
| --- | --- |
| `~/.config/cadre/init.lisp` | Your own Lisp configuration (see [Configuration](settings.md#configuration)) |
| `~/.config/cadre/settings.sexp` | Settings Cadre saves for you |
| `~/.local/share/cadre/tree-sitter/` | Installed grammars (`$XDG_DATA_HOME/cadre/tree-sitter/` if that's set) |

## Running

```sh
make run                      # open the last folder (or none)
make run DIR=~/projects/foo   # open a folder
```

### The macOS app

```sh
make app           # build/Cadre.app
make install-app   # build it and put it in /Applications
```

Then start Cadre from the Dock, Launchpad or Spotlight like any app. The app
holds Cadre itself, its icons and the bundled Swank. GTK, libadwaita and the
optional tools still come from Homebrew, so it's for Macs set up as in
[Setup](installing.md#setup). Started from the Finder or the Dock, it takes `PATH` and
the rest of the environment from your login shell, as a terminal would, so
it finds `sbcl`, `git` and `claude` the same way. Rebuild it (`make
install-app`, a few seconds once Cadre is compiled) to pick up changes.

Release packages that carry their own GTK (macOS) or use the system's
(Linux): `make release-macos`, `make release-linux`; see
[docs/RELEASING.md](../RELEASING.md).

To open folders and files from a terminal, link the launcher onto your
`PATH`:

```sh
ln -s "$PWD/scripts/cadre" ~/.local/bin/cadre
cadre .            # a Cadre window on this folder
cadre notes.md     # with a file open
```
