SBCL ?= sbcl
LISP = $(SBCL) --dynamic-space-size 4096 --non-interactive
DIR ?= .

.PHONY: run test smoke smoke-list perf app install-app

# Start Cadre on a folder: make run DIR=~/projects/foo
run:
	$(SBCL) --dynamic-space-size 4096 --load scripts/run.lisp --end-toplevel-options $(DIR)

# Headless tests of the editor model
test:
	$(LISP) --load scripts/test.lisp

# Open a window, drive it through every feature, and quit. While fixing
# something, run one section and what it needs: make smoke ONLY=run-app
# (several: ONLY="git folding"); make smoke-list names the sections.
smoke:
	ONLY="$(ONLY)" $(LISP) --load scripts/smoke.lisp

smoke-list:
	@SMOKE_LIST=1 $(SBCL) --noinform --non-interactive --load scripts/smoke.lisp 2>/dev/null | grep -v '^;' | grep -v '^$$'

# Measure against the performance budgets (docs/DESIGN.md, section 11)
perf:
	$(LISP) --load scripts/perf.lisp

# The macOS app: build/Cadre.app (GTK and the tools come from Homebrew)
app:
	$(LISP) --load scripts/build-app.lisp
	scripts/make-app.sh

# Build the app and put it in /Applications
install-app: app
	rm -rf /Applications/Cadre.app
	ditto build/Cadre.app /Applications/Cadre.app
	@echo "Installed /Applications/Cadre.app"

