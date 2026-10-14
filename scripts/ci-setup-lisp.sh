#!/bin/bash
# Set up the Lisp side of a build machine as packaging/versions.env pins
# it: Quicklisp with that dist, and the gtk4 bindings and the docset
# library checked out next to Cadre (where scripts/build-app.lisp looks).
#
#   scripts/ci-setup-lisp.sh       (the release workflow runs it)
#
# It replaces your Quicklisp dist and moves ../gtk4 and ../docset to the
# pinned commits, so on your own Mac it asks for --force.
set -euo pipefail
cd "$(dirname "$0")/.."
. packaging/versions.env
if [ -z "${CI:-}" ] && [ "${1:-}" != "--force" ]; then
  echo "This sets up a build machine (it changes ~/quicklisp, ../gtk4 and ../docset); run it with --force to do that here" >&2
  exit 2
fi

if [ ! -f ~/quicklisp/setup.lisp ]; then
  curl -fsSL -o build-quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
  sbcl --noinform --non-interactive --load build-quicklisp.lisp \
       --eval '(quicklisp-quickstart:install)' \
       --eval '(ql-util:without-prompting (ql:add-to-init-file))'
  rm build-quicklisp.lisp
fi
sbcl --noinform --non-interactive \
     --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/$QUICKLISP_DIST/distinfo.txt\" :prompt nil :replace t)" \
     --eval '(format t "~&Quicklisp dist ~a~%" (ql-dist:version (ql-dist:dist "quicklisp")))'

checkout() {
  local repo=$1 ref=$2 dir=$3
  if [ ! -d "$dir/.git" ]; then git clone --quiet "$repo" "$dir"; fi
  git -C "$dir" fetch --quiet origin "$ref"
  git -C "$dir" checkout --quiet --detach "$ref"
  echo "$dir at $(git -C "$dir" rev-parse --short HEAD)"
}
checkout "$GTK4_REPO" "$GTK4_REF" ../gtk4
checkout "$DOCSET_REPO" "$DOCSET_REF" ../docset
