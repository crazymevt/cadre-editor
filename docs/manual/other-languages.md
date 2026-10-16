# JavaScript, HTML, CSS, JSON and XML

## JavaScript, TypeScript, JSON, HTML, CSS and XML (tree-sitter)

Files ending in `.js` `.mjs` `.cjs` `.jsx`, `.ts` `.mts` `.cts`, `.tsx`,
`.json` `.jsonc`, `.html` `.htm`, `.css` and `.xml` open in their own modes.
So do other XML formats: `.xsd` `.xsl` `.xslt` `.svg` `.plist` `.rss`
`.atom` `.xaml` `.csproj` `.props` `.targets` `.wsdl` `.xliff` `.xlf` `.gpx`
`.kml`. These
modes get several things from the language's tree-sitter grammar:
- syntax colors
- folding by the syntax tree
- an Outline: functions, classes and methods; interfaces, types and enums in
  TypeScript; top-level keys in JSON; headings in HTML; rules and keyframes
  in CSS; the root element's children in XML
- in HTML, `<script>` and `<style>` contents colored as JavaScript and CSS
- Toggle Comment with `//`, or by wrapping lines in `<!-- -->` (HTML and
  XML) or `/* */` (CSS)
- Return keeps the indentation and indents after an opening bracket or a
  tag. Between brackets or tags, it puts the closing one on its own line.

**Setting it up:**
1. Install the library: `brew install tree-sitter`.
2. In Cadre, run **Install Language Grammar…** (in the main menu, or
   `M-x install-language-grammar`) once for each language. It clones the
   grammar at a pinned version and compiles it with `cc`, into
   `~/.local/share/cadre/tree-sitter/`.

A file in one of these languages tells you if its grammar isn't installed.
Parsing happens on a background thread shortly after you stop typing, so
even very large files don't make typing stall. Only the lines on screen are
colored.

## Format Document

**Format Document** (`Shift+Alt+F`, Emacs `C-c f`, or the right-click menu)
lays out the whole file again. It's one undo step, and the cursor stays on
its line.
- **JSON, CSS and XML** use Cadre's own formatters. They work on tokens, so
  strings, numbers, comments and key order stay exactly as written, and a
  file on one line comes out readable. JSON puts each value on its own line;
  CSS puts each declaration on its own line, as `property: value;`. XML puts
  each element on its own line, indented, and keeps an element holding only
  text on one line (`<a>text</a>`). Tags, comments and CDATA stay as
  written. Text is trimmed, so the spaces around text mixed with elements
  can change.
- **Lisp** indents every line.
- **JavaScript, TypeScript and HTML** use an external formatter, Prettier by
  default (`npm install -g prettier`). The `*formatter-commands*` setting
  names the program for any mode. A mode listed there uses it even where
  Cadre has its own formatter.
