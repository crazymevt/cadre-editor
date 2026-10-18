SBCL ?= sbcl
LISP = $(SBCL) --dynamic-space-size 4096 --non-interactive
DIR ?= .

.PHONY: run test test-ui test-recovery smoke smoke-list gallery perf core app install-app release-macos release-linux

# Start Cadre on a folder: make run DIR=~/projects/foo
run:
	$(SBCL) --dynamic-space-size 4096 --load scripts/run.lisp --end-toplevel-options $(DIR)

# Headless tests of the editor model (no GTK dependency)
test:
	$(LISP) --load scripts/test.lisp

# Save and close regression tests (loads GTK bindings without opening a window)
test-ui:
	$(LISP) --load scripts/test-ui.lisp

# Verify checkpoints survive an actual terminated process, with isolated settings.
test-recovery:
	python3 scripts/test-recovery-crash.py

# Open a window, drive it through every feature, and quit. While fixing
# something, run one section and what it needs: make smoke ONLY=run-app
# (several: ONLY="git folding"); make smoke-list names the sections.
smoke:
	ONLY="$(ONLY)" $(LISP) --load scripts/smoke.lisp

smoke-list:
	@SMOKE_LIST=1 $(SBCL) --noinform --non-interactive --load scripts/smoke.lisp 2>/dev/null | grep -v '^;' | grep -v '^$$'

# The screenshots in docs/gallery/: a demo project staged scene by scene,
# in the dark theme, and the editing scene in the light one too (for
# Themes). One scene: make gallery ONLY=profiler
gallery:
	GALLERY_SCHEME=dark ONLY="$(ONLY)" $(LISP) --load scripts/gallery.lisp
	case " $(ONLY) " in "  "|*" editing "*) GALLERY_SCHEME=light ONLY=editing $(LISP) --load scripts/gallery.lisp;; esac

# Measure against the performance budgets (docs/DESIGN.md, section 11)
perf:
	$(LISP) --load scripts/perf.lisp

# The image the packages run: build/app/cadre.core
core:
	$(LISP) --load scripts/build-app.lisp

# The macOS app: build/Cadre.app (GTK and the tools come from Homebrew)
app: core
	SBCL=$(SBCL) scripts/make-app.sh

# Build the app and put it in /Applications
install-app: app
	rm -rf /Applications/Cadre.app
	ditto build/Cadre.app /Applications/Cadre.app
	@echo "Installed /Applications/Cadre.app"

# Release packages in dist/ (docs/RELEASING.md). The macOS app carries its
# own GTK, with the source of its GPL and LGPL libraries beside it; it's signed and notarized when CODESIGN_IDENTITY and the
# NOTARY_* variables are set.
release-macos: core
	SBCL=$(SBCL) scripts/make-app.sh --bundle
	packaging/macos/package.sh
	packaging/macos/sources.sh

release-linux: core
	SBCL=$(SBCL) packaging/linux/package.sh
