# Assistant

The Assistant panel supports Claude Code and Codex CLI. Choose the provider
from the dropdown at the top of the panel. Each provider has its own
conversation. Both can use Cadre's editor context and tools.

## Codex

Install Codex CLI and sign in with `codex login`. Cadre finds `codex` on your
`PATH`, or in the Codex CLI bundled with the ChatGPT desktop app on macOS. Set
`*codex-program*` in your `init.lisp` to use another executable. Cadre uses
the existing local CLI sign-in; it does not ask for or store a separate API
key.

Choose **Codex** in the provider dropdown. Cadre checks the CLI sign-in and
loads the model choices advertised by its app-server. If the model list is
unavailable, Codex's default model remains available. The **Chat / Agent**
switch changes the guidance sent with each turn, including when you change
modes in an existing conversation.

Codex runs in the open project folder. Cadre sends enabled File, Problems, and
Debugger context with your prompt. Codex's native shell and file-change
requests are denied; code changes go through `propose_edit` and appear as an
inline diff to accept or reject. Lisp evaluation, compilation, loading, and
tests continue to use Cadre's approval prompts.

## Claude Code

Cadre runs the Claude Code CLI (`claude`), so you need it installed and signed
in: `claude auth login` (with a Claude subscription or a Console account).
Cadre finds `claude` on your PATH, in `~/.local/bin`, or inside the Claude
desktop app; set `*claude-program*` to use another. Claude works in the open
folder, so that's where Claude Code keeps its notes and memory for the
project. Opening another folder moves it: the conversation in the old folder
ends (it stays in Claude Code's history there) and the next starts in the new
one.

## Using the panel

Open the Assistant tab (`Ctrl+Alt+I`, Emacs `C-c C-a a`, or choose a provider
from the Assistant menu). `Return` sends, `Shift+Return` starts a new line.
The **File** chip tells the selected assistant which file you are in and what
is selected; **Problems** and **Debugger** attach compiler notes or the
current error. The Problems tab and debugger buttons ask the selected provider
to help.

To change code, select it (or put the cursor in a top-level form), press
`Ctrl+I` (Emacs `C-c C-a e`, or **Ask Assistant to Change…** in the right-click
menu), and describe the change. The assistant's edit comes back as a diff in
the editor to accept or reject. Accepting applies it as one undo step and
saves the file if it had no unsaved changes.

Claude can't write files directly: every change is a `propose_edit`, shown in
its tab as an inline diff (removed lines struck out, added lines green) with
**Accept** and **Reject**. Evaluating or compiling code in your Lisp, and
Claude Code's own commands (Bash, web fetches), ask first in the chat: **Allow
Once**, **Allow for This Conversation**, or **Deny**. Claude never runs code in
Cadre's own Lisp.

In **Agent** mode Claude works through a larger task: it keeps a plan (shown as
a checklist in the chat), edits through `propose_edit`, saves, compiles forms
or files, loads systems and runs the tests, and shows you code with
`open_file`. Each edit, evaluation, compile, load and test run still asks you
first (unless you allow it for the conversation). Agent mode uses
`*claude-agent-model*` (default `opus`). `*claude-direct-edits*` lets Claude
Code write files itself, each write approved in the chat; open files reload
afterwards.

`*claude-model*` (default `sonnet`, also in the panel's model menu),
`*claude-effort*` and `*claude-isolated*` (ignore your own Claude Code settings
and MCP servers) are options for your init file.
