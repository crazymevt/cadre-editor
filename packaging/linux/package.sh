#!/bin/bash
# Package the core scripts/build-app.lisp saved for Linux:
#
#   packaging/linux/package.sh      (make release-linux runs it)
#
# Writes dist/cadre-VERSION-linux-ARCH.tar.gz, a folder to unpack anywhere:
#
#   bin/cadre                  the launcher
#   lib/cadre/sbcl             SBCL's runtime, the one that saved the core
#   lib/cadre/cadre.core
#   share/cadre/               Cadre's own files (icons, Swank, docsets)
#   share/applications/, share/icons/   the desktop entry and the icon
#   share/doc/cadre/           LICENSE, THIRD-PARTY-NOTICES.md, licenses/
#   install.sh                 copies it to ~/.local (or PREFIX)
#
# and, where dpkg-deb is installed, dist/cadre_VERSION_ARCH.deb with the
# same files under /usr. GTK 4.14+, libadwaita 1.5+ and (for the Terminal)
# VTE come from the distribution, so it runs where those are packaged:
# Ubuntu 24.04, Debian 13, Fedora 40 or newer.
set -euo pipefail
cd "$(dirname "$0")/../.."

sbcl=${SBCL:-sbcl}
core=build/app/cadre.core
version=$(sed -n 's/.*:version "\([^"]*\)".*/\1/p' cadre.asd | head -1)
[ -f "$core" ] || { echo "No $core: run scripts/build-app.lisp first (make release-linux does)" >&2; exit 1; }
case "$(uname -m)" in
  x86_64) arch=x86_64 deb_arch=amd64 ;;
  aarch64|arm64) arch=arm64 deb_arch=arm64 ;;
  *) arch=$(uname -m) deb_arch=$arch ;;
esac
id=io.github.crazymevt.Cadre
name=cadre-$version-linux-$arch
root=build/linux/$name

# Lay out PREFIX/{bin,lib,share} in $1.
lay_out() {
  local dir=$1
  mkdir -p "$dir/bin" "$dir/lib/cadre" "$dir/share/cadre/src/core/tree-sitter" "$dir/share/cadre/vendor" \
           "$dir/share/cadre/data" "$dir/share/applications" "$dir/share/icons/hicolor/scalable/apps" \
           "$dir/share/doc/cadre"
  cp "$core" "$dir/lib/cadre/cadre.core"
  cp "$runtime" "$dir/lib/cadre/sbcl"
  chmod 755 "$dir/lib/cadre/sbcl"
  cp -R icons "$dir/share/cadre/"
  cp -R vendor/slime vendor/cadre-racket "$dir/share/cadre/vendor/"
  cp src/core/tree-sitter/shim.c "$dir/share/cadre/src/core/tree-sitter/"
  cp -R data/docsets "$dir/share/cadre/data/"
  cp packaging/Cadre.svg "$dir/share/icons/hicolor/scalable/apps/$id.svg"
  cp packaging/linux/$id.desktop "$dir/share/applications/"
  cp LICENSE "$dir/share/doc/cadre/"
  cp -R build/linux/licenses "$dir/share/doc/cadre/licenses"
  cp build/linux/THIRD-PARTY-NOTICES.md "$dir/share/doc/cadre/"
  cat > "$dir/bin/cadre" <<'LAUNCHER'
#!/bin/sh
# Cadre: cadre [FOLDER] [FILE...]
lib=$(dirname "$(readlink -f "$0")")/../lib/cadre
exec "$lib/sbcl" --core "$lib/cadre.core" --dynamic-space-size 4096 --noinform --end-runtime-options "$@"
LAUNCHER
  chmod 755 "$dir/bin/cadre"
}

runtime=$("$sbcl" --noinform --non-interactive --no-userinit --no-sysinit --eval '(princ sb-ext:*runtime-pathname*)')
rm -rf build/linux
mkdir -p build/linux/licenses/sbcl dist
for copying in /usr/share/doc/sbcl/copyright "$(dirname "$runtime")/../share/doc/sbcl/COPYING" \
               "$(dirname "$runtime")/../../share/doc/sbcl/COPYING"; do
  [ -f "$copying" ] && { cp "$copying" build/linux/licenses/sbcl/COPYING; break; }
done
python3 packaging/notices.py --lisp build/app/lisp-systems.tsv --licenses build/linux/licenses \
  --system-libraries --out build/linux/THIRD-PARTY-NOTICES.md

lay_out "$root"
cp packaging/linux/install.sh "$root/"
tar -C build/linux -czf "dist/$name.tar.gz" "$name"
(cd dist && sha256sum "$name.tar.gz" > "$name.tar.gz.sha256")
echo "Wrote dist/$name.tar.gz ($(du -h "dist/$name.tar.gz" | cut -f1))"

if command -v dpkg-deb >/dev/null; then
  deb=build/linux/deb
  lay_out "$deb/usr"
  mkdir -p "$deb/DEBIAN"
  cat > "$deb/DEBIAN/control" <<CONTROL
Package: cadre
Version: $version
Architecture: $deb_arch
Maintainer: Jessie Hughart <https://github.com/crazymevt/cadre-editor>
Depends: libgtk-4-1 (>= 4.14), libadwaita-1-0 (>= 1.5), libharfbuzz-gobject0, libgraphene-1.0-0, libzstd1, libsqlite3-0, libc6
Recommends: sbcl, git, libvte-2.91-gtk4-0
Suggests: libtree-sitter0, build-essential
Section: devel
Priority: optional
Homepage: https://github.com/crazymevt/cadre-editor
Description: Common Lisp editor
 Cadre is a Common Lisp editor, written in Common Lisp: Emacs's depth for
 Lisp (Swank, the debugger, the inspector, paredit) with an interface like
 VS Code's, built on GTK 4 and libadwaita.
CONTROL
  dpkg-deb --root-owner-group --build "$deb" "dist/cadre_${version}_$deb_arch.deb" >/dev/null
  (cd dist && sha256sum "cadre_${version}_$deb_arch.deb" > "cadre_${version}_$deb_arch.deb.sha256")
  echo "Wrote dist/cadre_${version}_$deb_arch.deb"
fi
