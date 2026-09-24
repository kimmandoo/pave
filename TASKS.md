# Tasks

## Active implementation plan: Pave coding agent

The full planned feature set is still in progress. Tests must establish behavior per subsystem before a capability is marked complete. Preserve the mobile-specific system prompt, manifest discovery and per-command shell approval.

### Phase 1 — Provider and conversation contracts

- [x] Implemented the OCaml CLI, bounded tool loop, session recovery and initial mobile workspace tools.
- [x] Implemented OpenAI-compatible Chat Completions and Anthropic Messages, buffered and SSE-streaming, with local HTTP/tool-call verification.
- [ ] Implement the model catalog, provider credentials/configuration, OpenAI Responses/Codex, Gemini, and additional provider adapters; preserve provider-specific thinking, usage, errors and multimodal content.
- [ ] Implement every auth-policy provider and its matching transport; prioritize OAuth authorization-code/PKCE and device-code login, token refresh, protected inference and secure credential storage. Adding a provider should change one focused registry descriptor plus a transport only when its wire protocol differs.

### Phase 2 — Session and interactive runtime (in progress)

- [x] Replaced flat session JSON with an append-only parent-linked JSONL journal, migrated existing transcripts, added branch/fork commands and resumed interrupted tool calls with explicit failure results.
- [x] Added manual model-generated compaction with a durable summary boundary; preserved full journal history and recovered incomplete tool results.
- [ ] Implement automatic context budgeting, provider-native compaction, session metadata and full resume semantics beyond the current message journal.
- [ ] Implement cancellable event-driven turns, queued steering and tool-call lifecycle events, then finish the terminal's component model, transcript, composer, overlays and accessibility behavior. Verify real interactive UX with keyboard and resize flows.

### Phase 3 — Tools and IDE integrations

- [ ] Complete the built-in read/edit/grep/glob/bash contracts, approvals, job ownership and specialized tools without claiming that the existing seven simplified tools are equivalent.
- [ ] Implement LSP server lifecycle, navigation, diagnostics and write-through refactors; implement DAP launch/attach, breakpoints, stepping and inspection.

### Phase 4 — Delegation, extensibility and auxiliary packages

- [ ] Implement subagents/worktrees/structured yields and async task/job lifecycle.
- [ ] Implement persistent JS/Python eval kernels, tool bridge, browser/CDP, MCP, skills and extension hooks.
- [ ] Inventory and implement the remaining catalog, native services, TUI widgets, statistics, collaboration, snapshot/compaction and utility features required for the planned product.

### Phase 5 — Maintainability pass (after product capabilities)

- [ ] Review the final dependency graph and group provider, session, agent, tool, terminal and CLI code into clearly owned directories/files. Keep public contracts narrow, remove superseded code paths and update callers, tests, build rules and contributor docs together. Verify CLI, PTY and provider behavior after the move.

### Product completion requirements

- [ ] Profile and bound idle CPU, memory growth, subprocess lifetime and redraws during long sessions; keep keyboard interactions responsive without polling loops or unnecessary allocations.
- [ ] Remove remaining legacy brand mentions from published files and local commit history while retaining required MIT copyright and permission notices in `THIRD_PARTY_NOTICES`.

Do not mark a phase complete merely because its module compiles. The full product plan has not been achieved.
