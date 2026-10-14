#!/bin/bash
# Collect the source of the GPL and LGPL libraries bundled in build/Cadre.app,
# to publish beside the disk image (GPLv2 section 3, LGPLv2.1 section 6,
# GPLv3 section 6: the source offered from the same place as the binary):
#
#   packaging/macos/sources.sh      (make release-macos runs it)
#
# Reads build/app/homebrew-formulae.txt (scripts/make-app.sh --bundle
# writes it) and writes dist/Cadre-VERSION-macos-sources.tar. For each
# Homebrew package with a GPL or LGPL license, it holds:
#
#   PACKAGE-VERSION/SOURCE-TARBALL   the release Homebrew built from
#   PACKAGE-VERSION/PACKAGE.rb       the formula it was built with (how)
#   PACKAGE-VERSION/patches/         the patches Homebrew applied, if any
#
# The installed packages must be the versions Homebrew's formulae have
# now (as on a fresh build machine), since the source comes from them
# (SOURCES_ALLOW_OUTDATED=1 makes that a warning, for a test build).
set -euo pipefail
cd "$(dirname "$0")/../.."

list=build/app/homebrew-formulae.txt
[ -f "$list" ] || { echo "No $list: build the app with scripts/make-app.sh --bundle first" >&2; exit 1; }
version=$(sed -n 's/.*:version "\([^"]*\)".*/\1/p' cadre.asd | head -1)
prefix=${HOMEBREW_PREFIX:-$(brew --prefix)}
out=build/sources/Cadre-$version-macos-sources
work=build/sources-work
rm -rf build/sources build/sources-work
mkdir -p "$out" "$work" dist

formulae=$(awk '{print $1}' "$list" | sort -u)
tap_head() { awk -v n="$1" '$1 == n {print $5}' "$work/info.txt"; }

# name, installed version, current version, license, homebrew-core commit.
# shellcheck disable=SC2086
brew info --json=v2 $formulae | python3 -c '
import json, sys
for f in json.load(sys.stdin)["formulae"]:
    stable = f["versions"]["stable"] + ("_%d" % f["revision"] if f.get("revision") else "")
    # The linked version is the one in lib/, which the app bundles.
    installed = f.get("linked_keg") or (f["installed"][-1]["version"] if f["installed"] else "")
    print(f["name"], installed, stable, (f.get("license") or "").replace(" ", "_"), f.get("tap_git_head") or "-")
' > "$work/info.txt"

while read -r name installed current license _; do
  license=${license//_/ }
  case "$license" in *GPL*) ;; *) continue ;; esac
  if [ "$installed" != "$current" ]; then
    message="$name $installed is installed, but its formula is now $current: brew upgrade $name, and build again"
    # For a test build only: the source and patches come from the newer formula.
    if [ -n "${SOURCES_ALLOW_OUTDATED:-}" ]; then echo "warning: $message" >&2
    else echo "error: $message" >&2; exit 1; fi
  fi
  dir=$out/$name-$installed
  mkdir -p "$dir"
  brew fetch --quiet --build-from-source "$name" >/dev/null
  cp "$(brew --cache --build-from-source "$name")" "$dir/$(brew --cache --build-from-source "$name" | sed 's#.*/##; s/^[0-9a-f]*--//')"
  cp "$prefix/Cellar/$name/$installed/.brew/$name.rb" "$dir/"
  # Homebrew's patches, as the formula names them: downloaded from their
  # URLs, from homebrew-core (for "file"), or the formula's own __END__ data.
  python3 packaging/macos/formula-patches.py "$dir/$name.rb" "$(tap_head "$name")" "$dir"
  echo "$name $installed ($license)"
done < "$work/info.txt"

cat > "$out/README.md" <<README
# Source of the GPL and LGPL libraries in Cadre $version for macOS

Cadre.app carries these libraries in Contents/Frameworks/, built by
Homebrew from this source. Each folder has the source release, the Homebrew
formula that built it (its build steps and options), and in patches/ the
changes Homebrew made to the source, if it made any. The app
changes only the paths the libraries load each other from
(install_name_tool). Their licenses are in Cadre.app/Contents/Resources/licenses/.
README

tar -cf "dist/Cadre-$version-macos-sources.tar" -C build/sources "$(basename "$out")"
(cd dist && shasum -a 256 "Cadre-$version-macos-sources.tar" > "Cadre-$version-macos-sources.tar.sha256")
echo "Wrote dist/Cadre-$version-macos-sources.tar ($(du -h "dist/Cadre-$version-macos-sources.tar" | cut -f1))"
