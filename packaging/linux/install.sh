#!/bin/sh
# Install Cadre from this folder: ./install.sh (into ~/.local), or
# PREFIX=/usr/local sudo ./install.sh. Uninstall with ./install.sh --uninstall.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
prefix=${PREFIX:-$HOME/.local}
id=io.github.crazymevt.Cadre

if [ "${1:-}" = "--uninstall" ]; then
  rm -rf "$prefix/lib/cadre" "$prefix/share/cadre" "$prefix/share/doc/cadre"
  rm -f "$prefix/bin/cadre" "$prefix/share/applications/$id.desktop" \
        "$prefix/share/icons/hicolor/scalable/apps/$id.svg"
  echo "Removed Cadre from $prefix"
  exit 0
fi

rm -rf "$prefix/lib/cadre" "$prefix/share/cadre" "$prefix/share/doc/cadre"
mkdir -p "$prefix"
cp -R "$here/bin" "$here/lib" "$here/share" "$prefix/"
# The desktop entry runs the launcher by its full path, in case PREFIX/bin
# isn't on the desktop's PATH.
desktop=$prefix/share/applications/$id.desktop
sed "s|^Exec=cadre |Exec=$prefix/bin/cadre |" "$here/share/applications/$id.desktop" > "$desktop"
command -v update-desktop-database >/dev/null && update-desktop-database "$prefix/share/applications" 2>/dev/null || true
command -v gtk-update-icon-cache >/dev/null && gtk-update-icon-cache -q "$prefix/share/icons/hicolor" 2>/dev/null || true
echo "Installed Cadre in $prefix (run it as $prefix/bin/cadre, or from your applications menu)"
