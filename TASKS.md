# Tasks

## Active implementation plan: Pave coding agent

The full planned feature set is still in progress. Tests must establish behavior per subsystem before a capability is marked complete. Preserve the mobile-specific system prompt, manifest discovery and per-command shell approval.

### Phase 1 — Provider and conversation contracts

- [x] Implemented the OCaml CLI, bounded tool loop, session recovery and initial mobile workspace tools.
- [x] Implemented OpenAI-compatible Chat Completions and Anthropic Messages, buffered and SSE-streaming, with local HTTP/tool-call verification.
- [x] Added ten provider descriptors and six wire payload formats; Anthropic/Codex OAuth PKCE, OpenRouter's state-less PKCE-to-key login and GitHub Copilot's public device-code login use private storage and pinned OAuth inference endpoints. Verified registered routes in isolated local fixtures; Codex replay preserved encrypted reasoning and Gemini 3 replay preserved native thought signatures across tool turns and session restart.
- [ ] Complete the model catalog, remaining provider transports and provider-specific thinking, usage, errors, multimodal content, proprietary gateway state and account entitlements; Gemini 3 signed tool calls now replay correctly, but other Gemini semantics still require parity work.
- [ ] Implement every remaining authentication policy and its matching inference transport. The generic authorization-code and device-code engines exist, but only Anthropic, Codex, OpenRouter and personal GitHub Copilot sign-in are wired to CLI inference. Copilot currently supports only the public Chat endpoint and two pinned model IDs, not Enterprise, Responses, Anthropic or model discovery. A new provider should change one descriptor plus a transport only if its wire protocol differs.

### Phase 2 — Session and interactive runtime (in progress)

- [x] Replaced flat session JSON with an append-only parent-linked JSONL journal, migrated existing transcripts, added branch/fork commands and resumed interrupted tool calls with explicit failure results.
- [x] Added manual model-generated compaction with a durable summary boundary; preserved full journal history and recovered incomplete tool results.
- [x] Added in-session `/login` with real browser callback and private-store grant, and `/model` provider/model selection with preserved conversation; the terminal temporarily leaves alternate-screen mode for browser URLs and restores the transcript afterward. Interactive startup no longer requires credentials before sign-in.
- [ ] Implement automatic context budgeting, provider-native compaction, session metadata and full resume semantics beyond the current message journal.
- [ ] Implement cancellable event-driven turns, queued steering and tool-call lifecycle events, then finish the terminal's component model, transcript, composer, overlays and accessibility behavior. Verify real interactive UX with keyboard and resize flows.

### Phase 3 — Tools and IDE integrations

- [ ] Complete the built-in read/edit/grep/glob/bash contracts, approvals, job ownership and specialized tools without claiming that the existing seven simplified tools are equivalent.
- [ ] Implement LSP server lifecycle, navigation, diagnostics and write-through refactors; implement DAP launch/attach, breakpoints, stepping and inspection.

### Phase 4 — Delegation, extensibility and auxiliary packages

- [ ] Implement subagents/worktrees/structured yields and async task/job lifecycle.
- [ ] Implement persistent JS/Python eval kernels, tool bridge, browser/CDP, MCP, skills and extension hooks.
- [ ] Inventory and implement the remaining catalog, native services, TUI widgets, statistics, collaboration, snapshot/compaction and utility features required for the planned product.

### Phase 5 — Maintainability pass (final review after product capabilities)

- [x] Grouped the current provider, auth, session, agent, tool and terminal modules and their tests into responsibility-based directories without changing public OCaml module names; separated CLI credential commands/selection from runtime orchestration.
- [ ] Revisit the **final** dependency graph after new capabilities land; keep public contracts narrow, split newly oversized files along actual responsibilities, remove superseded code paths and update callers, tests, build rules and contributor docs together. Verify CLI, PTY and provider behavior after the final move.

### Product completion requirements

- [ ] Profile and bound idle CPU, memory growth, subprocess lifetime and redraws during long sessions; keep keyboard interactions responsive without polling loops or unnecessary allocations.
- [x] Removed remaining legacy brand mentions from published files and local commit history while retaining required MIT copyright and permission notices in `THIRD_PARTY_NOTICES`; remote cache copies cannot be erased by Git history rewriting.

Do not mark a phase complete merely because its module compiles. The full product plan has not been achieved.
