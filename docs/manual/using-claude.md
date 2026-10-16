# Claude

![Cadre editing Lisp, with Claude explaining the code in the side panel](../screenshot.png)

Cadre runs the Claude Code CLI (`claude`), so you need it installed and signed
in: `claude auth login` (with a Claude subscription or a Console account).
Cadre finds `claude` on your PATH, in `~/.local/bin`, or inside the Claude
desktop app; set `*claude-program*` to use another. Claude works in the open folder,
so that's where Claude Code keeps its notes and memory for the project.
Opening another folder moves it: the conversation in the old folder ends (it
stays in Claude Code's history there) and the next starts in the new one.

To change some code, select it (or just put the cursor in a top-level form),
press `Ctrl+I` (Emacs `C-c C-a e`, or "Ask Claude to Change…" in the
right-click menu) and say what you want; Claude's edit comes back as a diff
in the editor to accept or reject.

Open the Claude tab (`Ctrl+Alt+I`, Emacs `C-c C-a a`, or "Chat with Claude" in
the menu) and ask. `Return` sends, `Shift+Return` starts a new line. The
**File** chip tells Claude where you are and what is selected; **Problems** and
**Debugger** attach the compiler's notes or the current error. The Problems
tab's *Ask Claude to Fix* and the debugger's *Ask Claude* buttons fill these in
for you.

Claude can't write files directly: every change is a `propose_edit`, shown in
its tab as an inline diff (removed lines struck out, added lines green) with
*Accept* and *Reject*. Accepting applies it as one undo step and saves the
file if it had no unsaved changes. Evaluating or compiling code in your Lisp,
and Claude Code's own commands (Bash, web fetches), ask first in the chat:
*Allow Once*, *Allow for This Conversation* or *Deny*. Claude never runs code
in Cadre's own Lisp.

The panel's **Chat / Agent** switch picks the mode. In **Agent** mode Claude
works through a larger task: it keeps a plan (shown as a checklist in the
chat), edits through `propose_edit`, saves, compiles forms or files, loads
systems and runs the tests, and shows you code with `open_file`. Each edit,
evaluation, compile, load and test run still asks you first (unless you
allow it for the conversation). Agent mode uses `*claude-agent-model*`
(default `opus`). `*claude-direct-edits*` lets Claude Code write files
itself, each write approved in the chat; open files reload afterwards.

`*claude-model*` (default `sonnet`, also in the panel's menu), `*claude-effort*`
and `*claude-isolated*` (ignore your own Claude Code settings and MCP servers)
are options for your init file.
