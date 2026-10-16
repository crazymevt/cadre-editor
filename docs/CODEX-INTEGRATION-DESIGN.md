# Codex Integration Design

## Status and Scope

**Status:** Proposed design, not yet implemented.

This document describes adding Codex as a second coding assistant in Cadre. The target is Codex CLI authenticated with the user's existing local Codex login, embedded in Cadre's assistant panel and connected to Cadre's existing editor tools. Claude remains available.

The design uses Codex app-server as the process interface. It is designed for product integrations that need a local Codex process, open conversations, streamed events, interruption, tools, and approval requests. Its JSON-RPC protocol is distinct from MCP. Cadre will expose its existing tool definitions as app-server client-side dynamic tools, then dispatch calls to the existing handlers. This avoids running a second copy of the tool implementations. [Codex app-server overview](https://developers.openai.com/blog/codex-as-a-platform)

## Goals

- Let users chat with Codex from the existing assistant panel, with streaming replies and stop behavior.
- Reuse the local Codex CLI installation and login. Cadre must not create or store a separate OpenAI credential.
- Give Codex the same useful editor context Claude receives: current file and selection, compiler problems, debugger state, open buffers, and the connected Lisp image.
- Make Cadre tools available to Codex, retaining Cadre's edit review and approval behavior.
- Support starting and resuming Codex conversations, choosing a model, and using a project folder as the working directory.
- Keep Claude behavior working while provider-specific process and protocol code is added.

## Non-Goals

- Replacing Claude or migrating Claude conversations.
- Implementing direct Responses API calls or a separate API-key configuration path.
- Building a new OAuth sign-in UI or managing ChatGPT tokens in Cadre.
- Letting Codex silently edit project files in the first release. Changes should go through Cadre's reviewed `propose_edit` flow.
- Reproducing every Codex CLI or desktop capability. The first release focuses on chat and Cadre's existing editor tools.

## Current Architecture

Claude support is implemented as a mostly provider-specific slice:

- `src/core/claude/cli.lisp` finds the Claude CLI, checks its login, constructs CLI arguments, parses Claude stream-json events, and manages a subprocess.
- `src/ui/claude.lisp` owns the panel, chat state, streaming presentation, context assembly, process lifecycle, approval cards, and Claude-specific commands.
- `src/ui/claude-tools.lisp` defines Cadre's editor and Lisp-image tools. These are registered with the MCP server and currently described as tools for Claude.
- `src/core/claude/mcp.lisp` serves the tools over local streamable HTTP with a bearer token; it also owns the registered tool schemas and handlers.
- The panel is constructed and checked during startup in `src/ui/app.lisp`; names and entry points also appear in menus, key bindings, help, tests, and the manual.

The current process driver expects Claude's `claude -p` stream-json protocol. It is not suitable for Codex by changing the executable name: Codex app-server has request/response JSON-RPC, notifications, thread IDs, and server-initiated requests that need replies. The chat widgets and parts of the context and diff-review experience are reusable, but the process and event protocol need a separate adapter.

## Proposed Architecture

### Components

1. **Codex driver:** Add a provider-specific core module, for example `src/core/codex/app-server.lisp`. It launches the discovered `codex` executable as `codex app-server --listen stdio://`, reads and writes newline-delimited JSON-RPC, tracks request IDs, and dispatches notifications and server requests. Keep this module independent of GTK.
2. **Event normalization:** Convert app-server notifications into a small Cadre assistant event vocabulary, such as `:text-delta`, `:tool-start`, `:tool-result`, `:approval-request`, `:turn-complete`, `:turn-failed`, and `:turn-interrupted`. The UI should consume these normalized events instead of parsing Codex protocol objects itself.
3. **Cadre tool bridge:** Pass the registered Cadre tool names, descriptions, and JSON schemas in app-server's `dynamicTools` when starting a thread. Handle `item/tool/call` requests in the Codex driver by calling Cadre's existing `call-mcp-tool` handler and returning its result. This keeps the MCP endpoint available to Claude while Codex uses app-server's client-side tool bridge.
4. **Shared panel behavior:** Reuse the existing message list, markdown rendering, editor context chips, and inline diff review. Add a provider selection control and route send, stop, new conversation, and readiness actions to the selected provider's adapter.
5. **Provider-specific status:** Keep installation and login checks separate (`check-claude` and `check-codex`) behind a shared presentation model for “checking”, “not installed”, “not signed in”, “ready”, and “error”.

### Process and conversation model

- Start one app-server subprocess for the active Codex conversation and keep it available while that conversation is active.
- Initialize the JSON-RPC session, start a thread with Cadre's dynamic tools, and save the returned thread ID. The first implementation uses the Codex CLI's default model; model discovery and selection remain follow-up work.
- Start each user turn against that thread. Render streamed assistant text as it arrives; use completion, failure, and interruption notifications to settle the UI state.
- Resume a saved thread after restarting the process. A new conversation starts a new thread. The working directory is the project folder captured when the conversation begins, matching Claude's folder behavior.
- Interrupt through the app-server protocol and stop the child process during Cadre shutdown. Treat an unexpected process exit as a failed active turn and clear pending approval UI.
- Keep protocol request IDs and turn IDs distinct from the persistent thread ID. Ignore late notifications from a process that no longer owns the active chat.

The app-server overview documents creating threads, starting turns, receiving events, and handling approval requests. Model discovery should use its `model/list` capability rather than hard-coding a list where practical. [Codex app-server overview](https://developers.openai.com/blog/codex-as-a-platform), [app-server with ChatGPT sign-in](https://developers.openai.com/siwc/token-sharing-open-source/codex-app-server)

## Authentication and Readiness

- Discover `codex` on `PATH`, with an option to configure an explicit executable path if Cadre's existing option conventions call for it.
- Check installation/version and login status asynchronously, so opening the panel does not block the UI. Prefer a documented, non-interactive Codex CLI status command; handle older CLI versions and unrecognized status output with a useful error.
- If the CLI is missing, show installation guidance. If it is installed but logged out, explain that the user should sign in with Codex from a terminal, then offer **Check Again**. Do not run a browser or credential flow inside Cadre for v1.
- Launch app-server in the user's normal Codex configuration environment so it can use the CLI's stored login. Do not copy credentials into settings, project files, logs, or MCP configuration.
- If the executable is found but app-server cannot authenticate, surface the child process's concise diagnostic and point to the CLI login command. Avoid reporting “ready” based only on executable presence.

OpenAI documents app-server authentication using a Codex CLI login state (or `CODEX_ACCESS_TOKEN`); this design chooses the saved local login. The app-server-specific `Sign in with ChatGPT` route, including custom OAuth and token refresh, is explicitly out of scope. [Codex access tokens](https://developers.openai.com/zh-Hans/docs/enterprise/access-tokens), [app-server authentication example](https://developers.openai.com/siwc/token-sharing-open-source/codex-app-server)

## Conversation and Event Lifecycle

1. The user selects **Codex** and sends a message.
2. Cadre captures enabled context chips and starts or reuses the provider session.
3. On first use, Cadre launches app-server with the project as its working directory, performs protocol initialization, and starts a thread with the selected model, Codex instructions, and Cadre's dynamic tool definitions.
4. Cadre sends the turn input, including the user's prompt and optional editor context. Newlines and file contents must be encoded as protocol data, never interpolated into shell arguments.
5. The driver dispatches each JSON-RPC response/notification. The UI displays text deltas and tool activity. A tool result is associated with its originating call ID; Cadre's existing tools show their own approval UI when needed.
6. Turn completion updates busy state and usage/status details. Failure or interruption leaves the conversation resumable when the server provides a thread ID.
7. On **New Conversation**, close the active thread/process as appropriate, clear visible messages and tool rows, and start a fresh thread on the next send.

An app-server request may be initiated by the server and require Cadre to answer it. The driver must support bidirectional traffic rather than treating stdout as a one-way event stream. It must also preserve JSON-RPC correlation and serialize writes to stdin.

## Cadre Tools and Approvals

- Reuse the handlers and schemas in `src/ui/claude-tools.lisp` and the registered tools in `src/core/claude/mcp.lisp`. Rename or relocate Claude-specific comments and APIs only when doing so helps make the tools provider-neutral; avoid duplicating tool implementations.
- Preserve `propose_edit` as the editing path. Show the proposed patch in Cadre's existing inline review UI; accepting or rejecting remains a user action.
- Continue to require Cadre approval for Lisp evaluation, compilation, loading, and test execution according to the existing tool handlers. Decline app-server requests for native command execution and file changes so Codex cannot bypass Cadre's review flow.
- Configure the Codex process so its native file-writing and shell capabilities cannot bypass the Cadre review flow in the first release. Start with the narrowest available sandbox and approval policy, and verify behavior empirically before enabling any broader capability.
- Fail clearly if Cadre's registered tool set is unavailable. The conversation may still answer text-only questions, but the panel should show that editor tools are disconnected.
- Validate tool names, arguments, call IDs, and results at the protocol boundary. A malformed or unknown tool request must fail that call without crashing the UI or leaving the turn permanently busy.

Codex app-server's own protocol is not MCP. The integration uses app-server's dynamic tools and the `item/tool/call` client request to dispatch to Cadre handlers. It must not try to replace app-server with the removed/deprecated `codex mcp-server` interface. [Codex app-server overview](https://developers.openai.com/blog/codex-as-a-platform)

## User Interface and Commands

- Rename the panel and menu surface from Claude-only wording to a general **Assistant** or **AI** surface, while retaining provider-specific statuses and settings. Decide final user-facing naming during implementation.
- Add a provider dropdown with Claude and Codex. Preserve Claude as the current default to avoid silently changing existing user behavior.
- Keep model selection provider-aware. Populate Codex models from app-server when possible; keep Claude's existing model choices unchanged.
- Retain Chat / Agent mode if it maps cleanly to app-server turn configuration. Send mode-specific instructions with the turn. If the protocol cannot safely change a thread's configuration mid-session, restart/resume or begin a new thread when mode changes and explain that behavior in the UI.
- Preserve File, Problems, and Debugger context toggles and update their tooltips to say “assistant” or the selected provider where appropriate.
- Keep **Stop**, **New Conversation**, prompt submission, and return-key behavior consistent across providers.
- Generalize “Ask Claude” actions from the debugger, problems panel, and edit command to work with the selected provider. During migration, retain existing command names and key bindings as aliases so user init files and muscle memory do not break.
- Update package exports, menu labels, manual pages, screenshots if needed, and smoke/UI coverage. Add an “Ask Codex” terminal entry only if it remains a distinct useful action; it is not required for the in-panel integration.

## Configuration and Persistence

- Add a `*codex-program*` option only if users need an executable override; otherwise follow `find-claude-program`'s search style without exposing an unnecessary setting.
- Keep provider-specific model and reasoning/effort options separate. Do not pass Claude's effort values through to Codex without confirming the app-server field and accepted values.
- Store the selected provider as an ordinary Cadre setting if provider choice should persist across launches; do not persist credentials or raw auth responses.
- Keep the thread ID, selected provider, working directory, and mode in the in-memory chat state. Codex owns its own thread history. Decide separately whether Cadre should persist thread IDs across app restarts; initial scope can match the existing Claude session behavior and keep only the active session in memory.
- Keep Codex dynamic tool calls inside the app-server JSON-RPC process boundary and route them only to registered Cadre handlers. Claude's local MCP endpoint remains loopback-only and bearer-token protected.

## Error Handling and Recovery

- **CLI missing:** show install guidance and a retry action.
- **Not logged in:** explain how to sign in with `codex` and retry readiness detection.
- **Unsupported app-server:** show detected version and a clear minimum-version message once the required protocol baseline is known.
- **Initialization/configuration error:** show a concise user-facing explanation and retain full sanitized diagnostics in the Output panel.
- **Cadre tools unavailable:** continue text-only if possible, show tool status, and allow retry/restart.
- **Turn failure:** clear busy state, preserve prior messages and thread ID, display the failure, and permit retry.
- **Interrupted turn:** distinguish user cancellation from unexpected process termination.
- **Process exit:** deny or cancel pending approval requests, close streams, stop reader threads, and prevent stale callbacks from mutating a newer conversation.
- **Malformed protocol line:** log a bounded, sanitized diagnostic and continue if safe; terminate the driver if framing or request state can no longer be trusted.
- **Model unavailable:** show the selected model and app-server error; refresh the model catalog when practical.

## Security Considerations

- Run Codex in the selected project directory, not an unrelated default directory. Respect the same trust boundary as other project-aware features.
- Keep Codex's native sandbox and approval policy restrictive. Editor-backed actions that change buffers or run code continue through Cadre's explicit tools and approvals.
- Do not expose the local MCP bearer token in prompts, UI, logs, or exception messages. Do not write it into a project-local Codex configuration file.
- Do not execute shell commands constructed from user content. Start the configured executable with a fixed argument vector and pass JSON-RPC messages over stdin.
- Treat tool arguments, model output, and protocol diagnostics as untrusted input. Bound message sizes and avoid logging file contents or auth material.
- Route dynamic tool calls only to registered Cadre handlers. Keep Claude's MCP server lifetime tied to Cadre's lifecycle and stop it on shutdown. Codex's native shell and file-change approval requests are declined in the first release; code edits must go through `propose_edit`, and runtime actions must go through Cadre's existing approval-aware tools.
- Document that editor context and selected buffer contents are sent to the configured assistant service when the user submits a prompt, consistent with the existing Claude integration disclosure.

## Implementation Plan

### Phase 1: Protocol spike

- Confirm the supported Codex CLI/app-server command, protocol initialization, thread lifecycle, model listing, dynamic tool calls, event forms, interruption, and approval requests against the minimum supported CLI version.
- Verify app-server can use the user's saved CLI login in the supported launch environment.
- Build a small headless driver prototype and fake JSON-RPC peer for deterministic protocol coverage.
- Resolve the initial trust/sandbox policy so native writes cannot bypass inline review.

### Phase 2: Core driver

- Add CLI discovery and readiness status.
- Implement app-server process management, JSON-RPC request correlation, server-initiated requests, asynchronous event delivery, thread start/resume, turn start, interruption, and shutdown.
- Normalize protocol events for the UI and add core tests for framing, IDs, notifications, errors, and process exit.

### Phase 3: Cadre panel integration

- Add Codex provider selection and status page.
- Connect normalized events to streaming text, tool activity, approval cards, stop, and new-conversation behavior.
- Pass existing Cadre tools to app-server as dynamic tools and route tool calls to the registered handlers.
- Reuse editor context assembly and inline edit review; ensure approval actions complete the correct app-server request.

### Phase 4: Product polish

- Add model selection and Chat/Agent prompt behavior.
- Generalize menu, debugger, problems, and inline-edit actions while preserving command aliases.
- Update manual/settings and startup checks; add UI smoke coverage and test failure/recovery paths.
- Run a manual integration pass with a signed-in Codex CLI in a disposable project, including one read-only tool, one proposed edit, one approved runtime action, cancellation, and session resume.

## Validation Plan

- Unit-test JSON-RPC framing and parsing, request/response correlation, notifications, server requests, errors, event normalization, and malformed input.
- Test the lifecycle with a fake app-server executable so tests do not require network access or user credentials.
- Test status states for missing executable, logged-out CLI, ready CLI, and app-server initialization failure.
- Exercise UI event flow: streaming, tool start/result pairing, approval once/session/deny, interruption, new chat, and process exit.
- Manually verify MCP connectivity, editor context, proposed edit review, user-approved evaluation/compile action, folder selection, and thread resume against the supported Codex CLI version.
- Confirm the existing Claude flow remains functional after shared panel/command changes.

## Open Questions

- What minimum Codex CLI/app-server version should Cadre support, and how will Cadre detect protocol compatibility?
- Does the supported app-server version expose all current Cadre tools through `dynamicTools` without experimental protocol flags?
- Does the selected app-server configuration provide a sufficiently restrictive sandbox and approval policy to prevent unreviewed filesystem edits in v1?
- Should Codex's model catalog refresh on every panel open, once per process, or only when the user opens the model picker?
- Should Codex thread IDs persist across Cadre restarts, or should v1 match current in-memory chat continuity?
- Should the panel be renamed to **Assistant** or **AI**, and should the provider dropdown or separate provider tabs be used?
- Which existing Claude-specific command names should remain permanent compatibility aliases?
- Does Agent mode need a separate Codex thread/model configuration, or can it be represented as per-turn instructions in a resumed thread?

## References

- [Codex as a platform: app-server integration overview](https://developers.openai.com/blog/codex-as-a-platform)
- [Codex app-server and ChatGPT plan authentication](https://developers.openai.com/siwc/token-sharing-open-source/codex-app-server)
- [Codex CLI access tokens and saved login authentication](https://developers.openai.com/zh-Hans/docs/enterprise/access-tokens)
- [Connect an MCP server to Codex](https://developers.openai.com/learn/docs-mcp) (background on Codex MCP support; this design uses app-server dynamic tools)
- Cadre's current implementation: `src/ui/claude.lisp`, `src/core/claude/cli.lisp`, `src/core/claude/mcp.lisp`, `src/ui/claude-tools.lisp`, `src/ui/app.lisp`, and `docs/manual/using-claude.md`.
