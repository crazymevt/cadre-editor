#!/bin/bash
# Put the executable scripts/build-app.lisp saved into a macOS app bundle:
#
#   scripts/make-app.sh        (make app runs it)
#
# Writes build/Cadre.app: the executable, Cadre's own files (icons, the
# bundled Swank, the tree-sitter shim's source) in Contents/Resources/cadre,
# and the icon. GTK, libadwaita and the optional tools (sbcl for the REPL,
# git, tree-sitter, VTE, claude) come from Homebrew on the Mac running it.
set -euo pipefail
cd "$(dirname "$0")/.."

exe=build/app/Cadre
app=build/Cadre.app
version=$(sed -n 's/.*:version "\([^"]*\)".*/\1/p' cadre.asd | head -1)
[ -x "$exe" ] || { echo "No $exe: run scripts/build-app.lisp first (make app does)" >&2; exit 1; }

rm -rf "$app"
mkdir -p "$app/Contents/MacOS" "$app/Contents/Resources/cadre/src/core/tree-sitter"
cp "$exe" "$app/Contents/MacOS/Cadre"
cp -R icons "$app/Contents/Resources/cadre/"
mkdir -p "$app/Contents/Resources/cadre/vendor"
cp -R vendor/slime "$app/Contents/Resources/cadre/vendor/"
cp src/core/tree-sitter/shim.c "$app/Contents/Resources/cadre/src/core/tree-sitter/"

# The icon, from packaging/macos/Cadre.svg.
iconset=build/app/Cadre.iconset
rm -rf "$iconset"; mkdir -p "$iconset"
for size in 16 32 128 256 512; do
  rsvg-convert -w $size -h $size packaging/macos/Cadre.svg -o "$iconset/icon_${size}x${size}.png"
  rsvg-convert -w $((size * 2)) -h $((size * 2)) packaging/macos/Cadre.svg -o "$iconset/icon_${size}x${size}@2x.png"
done
iconutil -c icns "$iconset" -o "$app/Contents/Resources/Cadre.icns"

cat > "$app/Contents/Info.plist" <<PLIST
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
  <key>LSMinimumSystemVersion</key><string>12.0</string>
  <key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
PLIST

echo "Wrote $app ($(du -sh "$app" | cut -f1))"
