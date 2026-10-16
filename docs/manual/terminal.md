# Terminal

The **Terminal** page in the panel runs your shell in the project's folder.
It's a real terminal (VTE, the widget GNOME Terminal uses), so full-screen
programs like `vim`, `htop` and Claude Code work, with colors and the mouse.
- **Opening one:** open the Terminal tab, or use **Show Terminal**
  (`` Ctrl+Shift+` ``, Emacs `C-x p s`). It starts a shell if there's none.
  `+` opens another. Each terminal gets a tab. Middle-click a tab, or use
  the trash button (Kill Terminal), to end one.
- **Ending:** a shell that exits normally closes its terminal. One that
  fails stays open, ending with "[Process exited with code N]", so you can
  read what went wrong.
- **Keys:** keys go to the terminal, so `Ctrl+C`, `Ctrl+R` and `Esc` work as
  in any terminal. These stay Cadre's:
  - on macOS, ⌘ keys (⌘P, ⌘⇧P, ⌘B, ⌘J…). In the terminal, ⌘C and ⌘V copy
    and paste, ⌘A selects all, ⌘K clears, and ⌘← ⌘→ ⌘⌫ go to the start or
    end of the line or delete it.
  - `` Ctrl+` ``, `` Ctrl+Shift+` ``, `Ctrl+Shift+P` and `F1`
  - in the Emacs profile, `C-x` and `M-x` (and so `C-x o`, `C-x b`, …)

  On Linux, `Ctrl+Shift+C` and `Ctrl+Shift+V` copy and paste. On macOS,
  Option works as Meta (`M-b`, `M-f`, `M-DEL` in the shell); turn off
  `*terminal-option-as-meta*` to type Option's characters instead.
  `*terminal-editor-keys*` changes which keys stay Cadre's.
- **Links:** ⌘-click (Ctrl+click on Linux) a URL to open it in the browser.
  ⌘-click a file reference such as `src/app.lisp:42:7`, as compilers and
  test runners print them, to open the file at that line.
- **Send to Terminal** runs the selection, or the current line, in the
  current terminal.
- **Claude Code in a Terminal** (the ✦ button, `C-c C-a t`, or the main
  menu) runs Claude Code's own interface in a terminal. It's connected to
  Cadre's MCP server, so Claude can read your buffers, ask your running
  Lisp, and propose edits as diffs in the editor, as in the Claude tab.

Right-click a terminal for Copy, Paste, Select All, Clear, New Terminal and
Kill Terminal. Terminals use the editor's font (and zoom) and the theme's
colors. Settings, under Terminal: the shell (`*terminal-shell*`, default
`$SHELL`), scrollback lines, Option as Meta, and the keys that stay
Cadre's. Without VTE, the Terminal tab tells you how to install it.
