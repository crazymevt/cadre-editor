#!/bin/bash
# Put build/Cadre.app (from scripts/make-app.sh --bundle) in a disk image:
#
#   packaging/macos/package.sh      (make release-macos runs it)
#
# Writes dist/Cadre-VERSION-macos-ARCH.dmg and its .sha256. When the app
# was signed with a Developer ID (CODESIGN_IDENTITY), the image is signed
# too, and with notary credentials both are notarized and stapled, so
# Gatekeeper opens them without a warning. The credentials are either
#
#   NOTARY_KEYCHAIN_PROFILE   a profile saved with
#                             xcrun notarytool store-credentials
#
# or an App Store Connect API key (what the release workflow uses):
#
#   NOTARY_KEY                the .p8 file
#   NOTARY_KEY_ID, NOTARY_ISSUER
set -euo pipefail
cd "$(dirname "$0")/../.."

app=build/Cadre.app
identity=${CODESIGN_IDENTITY:--}
version=$(sed -n 's/.*:version "\([^"]*\)".*/\1/p' cadre.asd | head -1)
arch=$(lipo -archs "$app/Contents/MacOS/sbcl" | tr ' ' '-')
[ -d "$app/Contents/Frameworks" ] || { echo "$app doesn't carry GTK: build it with scripts/make-app.sh --bundle" >&2; exit 1; }
mkdir -p dist
dmg=dist/Cadre-$version-macos-$arch.dmg

notary=()
if [ -n "${NOTARY_KEYCHAIN_PROFILE:-}" ]; then
  notary=(--keychain-profile "$NOTARY_KEYCHAIN_PROFILE")
elif [ -n "${NOTARY_KEY:-}" ]; then
  notary=(--key "$NOTARY_KEY" --key-id "${NOTARY_KEY_ID:?}" --issuer "${NOTARY_ISSUER:?}")
fi
if [ ${#notary[@]} -gt 0 ] && [ "$identity" = "-" ]; then
  echo "Notarizing needs the app signed with a Developer ID: set CODESIGN_IDENTITY" >&2
  exit 1
fi

notarize() {
  xcrun notarytool submit "$1" "${notary[@]}" --wait --timeout 30m --output-format json > build/notary.json || true
  status=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("status", ""))' build/notary.json)
  if [ "$status" != "Accepted" ]; then
    cat build/notary.json >&2
    id=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("id", ""))' build/notary.json)
    [ -n "$id" ] && xcrun notarytool log "$id" "${notary[@]}" >&2
    echo "Notarization failed ($status)" >&2
    exit 1
  fi
}

# The app first, so a copy dragged out of the image has its own ticket.
if [ ${#notary[@]} -gt 0 ]; then
  ditto -c -k --keepParent "$app" build/Cadre-notarize.zip
  notarize build/Cadre-notarize.zip
  xcrun stapler staple "$app"
  rm build/Cadre-notarize.zip
fi

staging=build/dmg
rm -rf "$staging" "$dmg"
mkdir -p "$staging"
ditto "$app" "$staging/Cadre.app"
ln -s /Applications "$staging/Applications"
hdiutil create -volname "Cadre $version" -srcfolder "$staging" -fs HFS+ -format ULFO -ov "$dmg" 2>&1 | grep -v "is deprecated" >&2 || true
rm -rf "$staging"
[ -f "$dmg" ] || { echo "hdiutil didn't write $dmg" >&2; exit 1; }

if [ "$identity" != "-" ]; then
  codesign --force --sign "$identity" --timestamp "$dmg"
fi
if [ ${#notary[@]} -gt 0 ]; then
  notarize "$dmg"
  xcrun stapler staple "$dmg"
  spctl --assess --type open --context context:primary-signature -v "$dmg"
fi

(cd dist && shasum -a 256 "$(basename "$dmg")" > "$(basename "$dmg").sha256")
echo "Wrote $dmg ($(du -h "$dmg" | cut -f1))$([ ${#notary[@]} -gt 0 ] && echo ", notarized" || echo ", not notarized")"
