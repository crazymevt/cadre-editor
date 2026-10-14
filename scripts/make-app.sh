#!/bin/bash
# Put the core scripts/build-app.lisp saved into a macOS app bundle:
#
#   scripts/make-app.sh            (make app runs it)
#   scripts/make-app.sh --bundle   (make release-macos runs it)
#
# Writes build/Cadre.app:
#
#   Contents/MacOS/Cadre        the launcher (packaging/macos/launcher.c)
#   Contents/MacOS/sbcl         SBCL's runtime, the one that saved the core
#   Contents/Resources/cadre.core
#   Contents/Resources/cadre/   Cadre's own files: icons, the bundled Swank,
#                               the tree-sitter shim's source, the docsets
#
# Without --bundle, GTK, libadwaita and VTE come from Homebrew on the Mac
# running the app. With it, the app carries its own copies: those libraries
# and every non-system library they use go into Contents/Frameworks (with
# their references rewritten to point there), with gdk-pixbuf's image
# loaders, GTK's GSettings schemas, the license of each, and
# THIRD-PARTY-NOTICES.md. The optional tools (sbcl for the REPL, git,
# tree-sitter, claude) still come from the Mac running it.
#
# Everything is signed with CODESIGN_IDENTITY (a "Developer ID
# Application: …" certificate for a release), or ad hoc ("-") if it isn't
# set, with the hardened runtime and packaging/macos/Cadre.entitlements.
set -euo pipefail
cd "$(dirname "$0")/.."

bundle=
[ "${1:-}" = "--bundle" ] && bundle=1
sbcl=${SBCL:-sbcl}
identity=${CODESIGN_IDENTITY:--}
prefix=${HOMEBREW_PREFIX:-$(brew --prefix 2>/dev/null || echo /opt/homebrew)}
core=build/app/cadre.core
app=build/Cadre.app
version=$(sed -n 's/.*:version "\([^"]*\)".*/\1/p' cadre.asd | head -1)
[ -f "$core" ] || { echo "No $core: run scripts/build-app.lisp first (make app does)" >&2; exit 1; }

rm -rf "$app"
contents=$app/Contents
mkdir -p "$contents/MacOS" "$contents/Resources/cadre/src/core/tree-sitter" "$contents/Resources/cadre/vendor" \
         "$contents/Resources/cadre/data"
cp "$core" "$contents/Resources/cadre.core"
cp -R icons "$contents/Resources/cadre/"
cp -R vendor/slime vendor/cadre-racket "$contents/Resources/cadre/vendor/"
cp src/core/tree-sitter/shim.c "$contents/Resources/cadre/src/core/tree-sitter/"
cp -R data/docsets "$contents/Resources/cadre/data/"
cp LICENSE "$contents/Resources/LICENSE"

# The launcher, and the runtime that saved the core (they must match).
cc -O2 -Wall -mmacosx-version-min=12.0 -o "$contents/MacOS/Cadre" packaging/macos/launcher.c
runtime=$("$sbcl" --noinform --non-interactive --no-userinit --no-sysinit --eval '(princ sb-ext:*runtime-pathname*)')
cp "$runtime" "$contents/MacOS/sbcl"
chmod u+w "$contents/MacOS/sbcl"

# The icon, from packaging/Cadre.svg.
iconset=build/app/Cadre.iconset
rm -rf "$iconset"; mkdir -p "$iconset"
for size in 16 32 128 256 512; do
  rsvg-convert -w $size -h $size packaging/Cadre.svg -o "$iconset/icon_${size}x${size}.png"
  rsvg-convert -w $((size * 2)) -h $((size * 2)) packaging/Cadre.svg -o "$iconset/icon_${size}x${size}@2x.png"
done
iconutil -c icns "$iconset" -o "$contents/Resources/Cadre.icns"

# The non-system libraries FILE refers to, as paths on this Mac.
dependencies() {
  local file=$1 dir
  dir=$(dirname "$(realpath "$file")")
  otool -L "$file" | tail -n +2 | awk '{print $1}' | while read -r dep; do
    case "$dep" in
      /usr/lib/*|/System/*) ;;
      @loader_path/*|@rpath/*)
        base=$(basename "$dep")
        if [ -e "$dir/$base" ]; then echo "$dir/$base"
        elif [ -e "$prefix/lib/$base" ]; then echo "$prefix/lib/$base"
        else echo "warning: can't find $dep (from $file)" >&2; fi ;;
      @*) ;;
      *) echo "$dep" ;;
    esac
  done
}

if [ -n "$bundle" ]; then
  frameworks=$contents/Frameworks
  mkdir -p "$frameworks"
  # The libraries the gtk4 bindings load by name (gtk4's
  # src/runtime/libraries.lisp), VTE for the Terminal page, and gdk-pixbuf's
  # image loaders (libpixbufloader*.so, from gdk-pixbuf and librsvg).
  queue=()
  for lib in libglib-2.0.0 libgobject-2.0.0 libgmodule-2.0.0 libgio-2.0.0 libcairo.2 \
             libcairo-gobject.2 libharfbuzz.0 libharfbuzz-gobject.0 libpango-1.0.0 \
             libpangocairo-1.0.0 libgraphene-1.0.0 libgdk_pixbuf-2.0.0 libgtk-4.1 libadwaita-1.0 \
             libvte-2.91-gtk4.0; do
    if [ -e "$prefix/lib/$lib.dylib" ]; then queue+=("$prefix/lib/$lib.dylib")
    elif [ "$lib" = libvte-2.91-gtk4.0 ]; then echo "warning: VTE isn't installed (brew install vte3): the app's Terminal page will need it" >&2
    else echo "error: $prefix/lib/$lib.dylib is missing (brew install gtk4 libadwaita)" >&2; exit 1; fi
  done
  loaders_cache=$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
  while read -r loader; do queue+=("$loader"); done < <(sed -n 's/^"\(\/.*\.so\)"$/\1/p' "$loaders_cache")
  while read -r dep; do queue+=("$dep"); done < <(dependencies "$contents/MacOS/sbcl")

  # Copy each, and every non-system library it uses. (bash 3.2: no
  # associative arrays, so the copies themselves say what's done.)
  while [ ${#queue[@]} -gt 0 ]; do
    src=${queue[0]}; queue=("${queue[@]:1}")
    base=$(basename "$src")
    [ -e "$frameworks/$base" ] && continue
    cp "$(realpath "$src")" "$frameworks/$base"
    chmod u+w "$frameworks/$base"
    # Which Homebrew formula it came from, for the notices.
    formula=$(realpath "$src" | sed -n "s#^$(realpath "$prefix")/Cellar/\([^/]*\)/\([^/]*\)/.*#\1 \2#p")
    [ -n "$formula" ] && echo "$formula $base" >> "$frameworks/.formulae"
    while read -r dep; do queue+=("$dep"); done < <(dependencies "$src")
  done

  # Point every reference at the copies beside it.
  for lib in "$frameworks"/*.dylib "$frameworks"/*.so; do
    [ -e "$lib" ] || continue
    install_name_tool -id "@loader_path/$(basename "$lib")" "$lib" 2>/dev/null
    otool -L "$lib" | tail -n +2 | awk '{print $1}' | while read -r dep; do
      case "$dep" in
        /usr/lib/*|/System/*) ;;
        *) [ "$(basename "$dep")" != "$(basename "$lib")" ] &&
             install_name_tool -change "$dep" "@loader_path/$(basename "$dep")" "$lib" 2>/dev/null || true ;;
      esac
    done
    # Homebrew's search paths (LC_RPATH) point outside the app.
    otool -l "$lib" | awk '/cmd LC_RPATH/{r=1} r&&/path /{print $2; r=0}' | while read -r rpath; do
      install_name_tool -delete_rpath "$rpath" "$lib" 2>/dev/null || true
    done
  done
  for dep in $(otool -L "$contents/MacOS/sbcl" | tail -n +2 | awk '{print $1}'); do
    case "$dep" in
      /usr/lib/*|/System/*|@*) ;;
      *) install_name_tool -change "$dep" "@executable_path/../Frameworks/$(basename "$dep")" "$contents/MacOS/sbcl" 2>/dev/null ;;
    esac
  done
  # Nothing may still point outside the app.
  outside=$(for file in "$contents/MacOS/sbcl" "$frameworks"/*.dylib "$frameworks"/*.so; do
              otool -L "$file" | tail -n +2 | awk '{print $1}' | grep -v '^\(@loader_path\|@executable_path\|/usr/lib\|/System\)' |
                sed "s#^#$(basename "$file") -> #"
            done || true)
  [ -z "$outside" ] || { echo "error: libraries outside the app:" >&2; echo "$outside" >&2; exit 1; }
  # GIO's modules: none (Cadre points GIO_MODULE_DIR here so Homebrew's
  # aren't loaded beside the app's GLib).
  mkdir -p "$frameworks/gio-modules"

  # gdk-pixbuf's list of loaders, with @FRAMEWORKS@ for the folder: Cadre
  # writes it out with the app's path when it starts (use-bundled-gtk).
  sed 's#^"/[^"]*/\(libpixbufloader[^"/]*\.so\)"$#"@FRAMEWORKS@/\1"#' "$loaders_cache" | grep -v '^#' \
    > "$contents/Resources/gdk-pixbuf-loaders.cache"

  # Data GTK reads at run time; the gtk4 bindings point XDG_DATA_DIRS here.
  share=$contents/Resources/share
  mkdir -p "$share/glib-2.0/schemas"
  cp "$prefix/share/glib-2.0/schemas/"*.xml "$share/glib-2.0/schemas/"
  glib-compile-schemas "$share/glib-2.0/schemas" 2>&1 | grep -v "are deprecated" >&2 || true

  # Licenses: each formula's license files, then THIRD-PARTY-NOTICES.md.
  licenses=$contents/Resources/licenses
  mkdir -p "$licenses"
  sort -u "$frameworks/.formulae" | awk '{print $1, $2}' | sort -u | while read -r name ver; do
    mkdir -p "$licenses/$name"
    find "$prefix/Cellar/$name/$ver" -maxdepth 1 -type f \( -iname 'COPYING*' -o -iname 'LICENSE*' -o -iname 'COPYRIGHT*' -o -iname 'NOTICE*' \) \
      -exec cp {} "$licenses/$name/" \;
  done
  sbcl_cellar=$(realpath "$runtime" | sed -n "s#^\($(realpath "$prefix")/Cellar/sbcl/[^/]*\)/.*#\1#p")
  mkdir -p "$licenses/sbcl"
  if [ -n "$sbcl_cellar" ]; then cp "$sbcl_cellar/COPYING" "$licenses/sbcl/"
  else cp "$(dirname "$runtime")/../share/doc/sbcl/COPYING" "$licenses/sbcl/" 2>/dev/null || true; fi
  python3 packaging/notices.py --lisp build/app/lisp-systems.tsv --licenses "$licenses" \
    --homebrew "$frameworks/.formulae" --version "$version" --out "$contents/Resources/THIRD-PARTY-NOTICES.md"
  mv "$frameworks/.formulae" build/app/homebrew-formulae.txt
  count=0
  for lib in "$frameworks"/*.dylib "$frameworks"/*.so; do [ -e "$lib" ] && count=$((count + 1)); done
  echo "Bundled $count libraries ($(du -sh "$frameworks" | cut -f1))"
else
  python3 packaging/notices.py --lisp build/app/lisp-systems.tsv --licenses "$contents/Resources/licenses" \
    --out "$contents/Resources/THIRD-PARTY-NOTICES.md"
fi

# The oldest macOS every binary in the app runs on: Homebrew builds its
# libraries for the macOS it runs on, so a bundled app needs at least that.
minimum=12.0
for file in "$contents/MacOS/"* "$contents/Frameworks/"*.dylib "$contents/Frameworks/"*.so; do
  [ -f "$file" ] || continue
  v=$(otool -l "$file" | awk '/LC_BUILD_VERSION/{b=1} b&&/minos/{print $2; exit}')
  [ -n "$v" ] && minimum=$(printf '%s\n%s\n' "$minimum" "$v" | sort -t. -k1,1n -k2,2n | tail -1)
done

cat > "$contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleExecutable</key><string>Cadre</string>
  <key>CFBundleIdentifier</key><string>io.github.crazymevt.Cadre</string>
  <key>CFBundleName</key><string>Cadre</string>
  <key>CFBundleDisplayName</key><string>Cadre</string>
  <key>CFBundleIconFile</key><string>Cadre</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>$version</string>
  <key>CFBundleVersion</key><string>$version</string>
  <key>LSApplicationCategoryType</key><string>public.app-category.developer-tools</string>
  <key>LSMinimumSystemVersion</key><string>$minimum</string>
  <key>NSHighResolutionCapable</key><true/>
  <key>NSHumanReadableCopyright</key><string>Copyright © 2026 Jessie Hughart. MIT License.</string>
</dict>
</plist>
PLIST

# Sign the inside first, then the app. The entitlements matter for sbcl,
# the process the launcher becomes.
sign=(codesign --force --sign "$identity" --options runtime)
[ "$identity" != "-" ] && sign+=(--timestamp)
for file in "$contents/Frameworks/"*.dylib "$contents/Frameworks/"*.so; do
  [ -f "$file" ] && "${sign[@]}" "$file" 2>&1 | grep -v 'replacing existing signature' || true
done
"${sign[@]}" --entitlements packaging/macos/Cadre.entitlements "$contents/MacOS/sbcl"
"${sign[@]}" --entitlements packaging/macos/Cadre.entitlements "$app"
codesign --verify --deep --strict "$app"

echo "Wrote $app ($(du -sh "$app" | cut -f1), macOS $minimum or newer, signed by ${identity/#-/ad hoc})"
