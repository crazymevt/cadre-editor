#!/usr/bin/env python3
"""Write THIRD-PARTY-NOTICES.md for a Cadre package, and copy the Lisp
libraries' license files beside it.

  packaging/notices.py --lisp build/app/lisp-systems.tsv --licenses DIR \
      [--homebrew FORMULAE] [--system-libraries] --out NOTICES.md

--lisp is the list scripts/build-app.lisp writes (name, version, license,
source folder for each library in the image). --homebrew is a file of
"formula version library" lines for the libraries bundled from Homebrew
(scripts/make-app.sh --bundle writes it); their license files must already
be in DIR/<formula>/. --system-libraries says GTK comes from the system
(the Linux package) instead.
"""
import argparse
import glob
import json
import os
import shutil
import subprocess

LICENSE_GLOBS = ("LICENSE*", "LICENCE*", "COPYING*", "COPYRIGHT*", "NOTICE*")


def lisp_libraries(path, licenses):
    rows = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 4:
                continue
            name, version, license, source = parts[:4]
            if name.startswith("cadre") or name.startswith("sb-"):
                continue
            files = [p for g in LICENSE_GLOBS for p in glob.glob(os.path.join(source, g))]
            if files:
                os.makedirs(os.path.join(licenses, "lisp", name), exist_ok=True)
                for p in files:
                    if os.path.isfile(p):
                        shutil.copy(p, os.path.join(licenses, "lisp", name))
            rows.append((name, version, license or "see its source", bool(files)))
    return rows


def homebrew_formulae(path):
    found = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            parts = line.split()
            if len(parts) >= 3:
                found.setdefault((parts[0], parts[1]), []).append(parts[2])
    names = sorted({name for name, _ in found})
    info = {}
    if names:
        out = subprocess.run(["brew", "info", "--json=v2", *names], capture_output=True, text=True, check=True)
        for formula in json.loads(out.stdout)["formulae"]:
            info[formula["name"]] = formula
    rows = []
    for (name, version), libs in sorted(found.items()):
        formula = info.get(name, {})
        rows.append((name, version, formula.get("license") or "see its source",
                     formula.get("homepage", ""), formula.get("urls", {}).get("stable", {}).get("url", ""),
                     sorted(set(libs))))
    return rows


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lisp", required=True)
    parser.add_argument("--licenses", required=True)
    parser.add_argument("--homebrew")
    parser.add_argument("--system-libraries", action="store_true")
    parser.add_argument("--version", default="VERSION")
    parser.add_argument("--out", required=True)
    args = parser.parse_args()
    os.makedirs(args.licenses, exist_ok=True)

    lisp = lisp_libraries(args.lisp, args.licenses)
    out = [
        "# Third-party notices",
        "",
        "Cadre is MIT-licensed (see LICENSE). This package also contains the",
        "software below, under its own licenses. Their license texts are in the",
        "`licenses/` folder beside this file.",
        "",
        "## SBCL",
        "",
        "Cadre runs on SBCL's runtime, which is in this package. SBCL is mostly in",
        "the public domain, with some parts under BSD-style licenses",
        "(`licenses/sbcl/COPYING`). Source: https://www.sbcl.org",
        "",
        "## Swank (SLIME)",
        "",
        "Cadre loads Swank, from SLIME 2.32, into the Lisp you work with. Its files",
        "are public domain unless a file says otherwise; see",
        "`cadre/vendor/slime/CADRE-NOTES.md`. Source: https://github.com/slime/slime",
        "",
        "## Lisp libraries compiled into Cadre",
        "",
        "| Library | Version | License |",
        "| --- | --- | --- |",
    ]
    for name, version, license, _ in lisp:
        out.append(f"| {name} | {version} | {license} |")
    out.append("")
    if args.homebrew:
        rows = homebrew_formulae(args.homebrew)
        out += [
            "## Shared libraries",
            "",
            "GTK, libadwaita, VTE and the libraries they use are included as separate",
            "shared libraries (in `Contents/Frameworks/`), unmodified apart from the",
            "paths they load each other from, as built by Homebrew. Libraries under the",
            "LGPL may be replaced with your own builds of the same versions: put them in",
            "`Contents/Frameworks/` under the same names and sign the app again",
            "(`codesign --force --deep --sign - Cadre.app`).",
            "",
            "The source of the GPL and LGPL ones, exactly as built (the release, the",
            "Homebrew formula, and Homebrew's patches), is published with each release",
            f"of Cadre, beside the disk image: `Cadre-{args.version}-macos-sources.tar` at",
            "https://github.com/crazymevt/cadre-editor/releases. The project page of",
            "each is linked below.",
            "",
            "| Package | Version | License | Libraries | Released source |",
            "| --- | --- | --- | --- | --- |",
        ]
        for name, version, license, homepage, url, libs in rows:
            out.append(f"| [{name}]({homepage}) | {version} | {license} | {', '.join(libs)} | {url} |")
        out.append("")
    if args.system_libraries:
        out += [
            "## Shared libraries",
            "",
            "GTK, libadwaita and VTE are not included: Cadre uses your system's.",
            "",
        ]
    with open(args.out, "w", encoding="utf-8") as f:
        f.write("\n".join(out))


if __name__ == "__main__":
    main()
