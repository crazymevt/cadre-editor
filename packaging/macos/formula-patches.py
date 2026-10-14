#!/usr/bin/env python3
"""Save the patches a Homebrew formula applies to its source.

  formula-patches.py FORMULA.rb HOMEBREW-CORE-COMMIT OUT-DIR

Writes OUT-DIR/patches/: each `patch do url "…"` download, each
`patch do file "…"` from homebrew-core at the commit, and a formula's
inline patch (`patch :DATA`, the text after __END__). Nothing if it has
none. Patches inside `resource` blocks belong to those resources, which
aren't part of what is bundled, and are skipped.
"""
import os
import re
import sys
import urllib.request

formula, commit, out = sys.argv[1:4]
text = open(formula, encoding="utf-8").read()
source, _, data = text.partition("\n__END__\n")


def top_level_lines(source):
    """The formula's lines outside `resource ... do ... end` blocks."""
    depth_in_resource = 0
    depth = 0
    for line in source.splitlines():
        stripped = line.strip()
        if depth_in_resource:
            if re.search(r"\bdo\b(\s*\|.*\|)?\s*$", stripped):
                depth += 1
            elif stripped == "end":
                depth -= 1
                if depth == 0:
                    depth_in_resource = 0
            continue
        if re.match(r"resource\s+\"", stripped) and stripped.endswith(" do"):
            depth_in_resource, depth = 1, 1
            continue
        yield stripped


patches = []
in_patch = False
for line in top_level_lines(source):
    if re.match(r"patch\b.*\bdo$", line):
        in_patch = True
    elif in_patch and line == "end":
        in_patch = False
    elif in_patch:
        m = re.match(r'url\s+"([^"]+)"', line) or re.match(r'file\s+"([^"]+)"', line)
        if m:
            kind = line.split()[0]
            patches.append((kind, m.group(1)))
    elif re.match(r"patch\s+:DATA\b", line):
        patches.append(("data", None))

if patches:
    os.makedirs(os.path.join(out, "patches"), exist_ok=True)
for i, (kind, where) in enumerate(patches, 1):
    if kind == "data":
        name, body = f"{i:02d}-inline.patch", data.encode()
    else:
        url = where if kind == "url" else f"https://raw.githubusercontent.com/Homebrew/homebrew-core/{commit}/{where}"
        name = f"{i:02d}-" + re.sub(r"[^A-Za-z0-9._-]", "_", url.rsplit("/", 1)[-1])[:80]
        if not name.endswith((".diff", ".patch")):
            name += ".patch"
        with urllib.request.urlopen(url, timeout=60) as response:
            body = response.read()
    with open(os.path.join(out, "patches", name), "wb") as f:
        f.write(body)
    print(f"  patch {name}")
