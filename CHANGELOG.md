# Changelog

## 2026-09-25

- feat(session): added a bounded, searchable `/tree` journal ancestry picker with parent-linked previews and active-tip highlighting; selection checks out exactly that branch, while Escape preserves the current transcript. A native PTY followed two divergent turns, filtered the abandoned branch, selected it and verified the alternative disappeared; redirected CLI selection also worked.
- test(distribution): published `v0.1.9` on all four native targets; installed public `v0.1.8` into an isolated directory, upgraded it to `v0.1.9` over HTTPS, then verified branch changes repaint authoritative saved history in the shipped PTY.
- fix(tui): reconcile visible transcript with the journal after `/branch` and `/fork`; navigating to an earlier branch no longer leaves abandoned turns and stale `/entries` notices on-screen. Reproduced the stale display before the fix, then navigated both directions and forked in a real saved-session PTY.
- feat(tui): typed slash-command parsing from one command catalog, a prefix-filtered searchable `/` + Tab chooser that inserts without executing or discarding a draft, and descriptive `/help`; retained responsive worker wakeups behind the static palette. Exercised prefix selection, Escape preservation, insertion, full catalog and help in a native PTY.
- feat(tui): `/tools [NAME]` now shows the exact agent-advertised tool set and focused descriptions, reflecting shell enablement without implying sandboxing or bypassing per-command approval. A compact tool list fit the actual 70×18 terminal; redirected CLI confirmed disabled shell tools stay unavailable.
- feat(tui): separated semantic transcript state from painting, with restrained role/tool colors, readable Markdown/code and tool lifecycle blocks, lazy grapheme-safe wrapping, bounded scrollback, accurate interrupted states and `Alt+O` expandable tool results; kept narrow and `NO_COLOR` layouts usable.
- feat(tui): added bounded atomic undo/redo for grapheme editing and bracketed paste, kill/yank and line movement; matched Meta shortcuts to the terminal's actual lower-case escape sequences.
- feat(session): added opt-in `/new` private per-workspace journals and searchable `/resume` with explicit safe-discard confirmation for unsaved conversations; restored visible conversation on reopen without clearing an unsent editor draft.
- test(tui): rendered real 80×24, 30×10 and `NO_COLOR` 40×12 PTYs with local streamed Ollama turns; observed real `read_file` tool result expansion/collapse, Unicode draft undo/redo, private journal permissions, `/resume` history restoration and clean `/quit`.
- test(distribution): published `v0.1.8` on all four native targets; upgraded an isolated real `v0.1.7` installation over HTTPS, confirmed current version and expanded a real `read_file` tool result in the shipped 60×16 PTY.
- test(distribution): published `v0.1.7` on all four native targets and upgraded an isolated public-installed `v0.1.6` binary to `v0.1.7` over HTTPS; checked its native PTY verified-model picker.

- feat(update): added `pave update` for installer-owned macOS/Linux binaries with an embedded local installer, ownership marker and the existing checksum/archive validation; kept custom install directories and rejected unmanaged executables or inherited version/destination overrides.
- docs(project): inventoried reference capabilities as individually actionable, source-indexed tasks across providers, auth, agent/session, tools, terminal, extensibility and distribution without claiming implemented parity.
- test(update): exercised a native installed binary upgrading from a different executable using fake HTTPS release assets; corrupted archive checksum left the previous executable intact, and source-built binaries refused self-update.
- feat(tui): rendered the existing Pave pixel mark as a centered, colored ASCII startup illustration in an empty interactive transcript, with a compact narrow-terminal fallback; removed it on the first message without persisting it.
- test(tui): opened the native terminal in a PTY, observed the ASCII silhouette, resized to a narrow screen, submitted `/help` and verified the startup illustration gave way to the transcript before exiting with `/quit`.
- feat(model): replaced the OpenAI `gpt-4.1-mini` default with the documented coding-focused `gpt-6-sol` and routed GPT-6 models through Responses; kept the separate personal Copilot Chat model guard rather than pretending it supports newer inference routes.
- feat(update): added read-only `pave update --check` with a release-tag version embedded during native packaging, a bounded HTTPS latest-release lookup and explicit rate-limit/metadata failures; source and foreign-managed installs remain ineligible.
- feat(tui): moved interactive model turns to a cancellable event-driven worker, queued follow-up prompts without prematurely submitting them, cleared provisional output on cancellation and kept the editor responsive while streaming.
- feat(tui): added transcript scrollback, wrapped grapheme-aware multiline movement, word editing and reverse history search; replaced text entry for `/login` and `/model` with searchable keyboard pickers, and added a project settings overlay.
- feat(config): loaded typed user/project provider, model, turn and shell-disable settings with precedence and diagnostics; persisted explicit project changes atomically and loaded bounded ancestor/user `AGENTS.md` instructions below the mobile safety prompt.
- feat(provider): added pinned, credential-isolated `--models` discovery for OpenAI, Gemini, personal Copilot and local Ollama without treating discovered model IDs as automatically supported transports.
- feat(tools): added gitignore-aware glob and regex grep plus paginated file reads; made approved foreground shell commands cancellable with process-group cleanup and journal-safe interrupted results.
- test(tui): exercised native PTY logo, model/settings overlays, actual settings persistence and denial, live SSE cancellation with a queued follow-up and removal of provisional rows.
- test(provider): exercised GPT-6 Sol default routing through a local Responses server, four provider model-listing fixtures, live loopback Ollama listing, and an installed tagged update-check fixture with newer/current/older metadata and no writes.
- test(distribution): published `v0.1.6` on all four native targets; upgraded an isolated publicly installed `v0.1.5` binary over real HTTPS, confirmed `pave update --check` reported current and observed its ASCII launch logo in a PTY.
- feat(config): applied path-scoped `.pave/rules` as model-visible system guidance before guarded file mutations and added bounded `SYSTEM.md`/`SYSTEM_TEMPLATE.md`/`APPEND_SYSTEM.md` precedence with explicit CLI overrides; mobile safety and ancestor instructions remain.
- feat(tui): discovered account-listed routable model IDs asynchronously when opening `/model`, kept searchable offline suggestions visible during network waits, cancelled listing on Escape, and moved the model ahead of long workspace paths in the status header.
- fix(provider): recognized service-prefixed `models/gemini-3-*` IDs as signed Gemini 3 tool calls, preserving native thought-signature replay after choosing a discovered ID.
- test(agent): verified withheld scoped writes, replay after instruction exposure, cancellation/journal isolation, real CLI prompt precedence, delayed PTY model-list updates and Gemini signed-call replay with service-prefixed model IDs.

## 2026-09-24

- feat(provider): preserved Gemini 3 model-issued thought signatures in buffered and streamed tool calls, native function-result replay and reopened session journals; rejected missing signatures and mismatched native state rather than fabricating continuation.
- feat(auth): added personal GitHub Copilot HTTPS device-code sign-in with private credential storage, public Chat-only endpoint/model guards and explicit unsupported refresh/Enterprise routes; exposed in-session `/login github-copilot` and `/model github-copilot/gpt-4.1`.
- test(provider): exercised Gemini 3 signed tools and journal replay over local HTTP buffered/SSE scenarios; exercised Copilot device code, private storage, guarded token routing, two-request Chat tool turns and native PTY slash sign-in with fake HTTPS; no live vendor credentials were used.
- fix(tui): allowed a Copilot terminal to start before a model is selected without permitting unsupported model requests or reading a bearer token; verified `/model` rejection and selection in a native PTY.
- test(distribution): published `v0.1.4` across four native release targets and installed the checksum-verified latest arm64 archive into an isolated directory; the shipped binary entered a model-less Copilot TUI, selected a model and exited normally.

- feat(tui): added in-session `/login [PROVIDER]` with a browser callback and terminal suspend/resume that retains the transcript; added `/model [PROVIDER/MODEL_ID]` with route recomputation and preserved conversation across model changes.
- fix(cli): deferred credential resolution until the first model request so users can sign in from an otherwise uncredentialed terminal; prevented unknown slash commands from becoming model prompts and cleared stale custom endpoints on model switches.
- test(tui): exercised the native PTY through slash provider/model selection, browser callback, private key exchange, authenticated turns on two models with shared history, and terminal restoration using isolated fake HTTPS responses; no live vendor account was used.

- refactor(project): grouped library, executable UI and tests by responsibility while preserving public OCaml module names; moved CLI authentication commands and credential selection into a dedicated module.
- docs(project): mapped source/test ownership and provider contribution paths so new transports and browser grants have a single obvious home.

- feat(provider): added account-scoped Codex Responses streaming with encrypted reasoning replay persisted across session reopen, model-bound native output verification and enterprise residency routing; registered OpenRouter's compatible chat endpoint.
- feat(auth): added Codex OAuth PKCE login, account-bound JWT identity, refresh and pinned bearer inference; added OpenRouter's explicit state-less PKCE-to-key browser exchange without weakening standard OAuth state validation.
- test(provider): exercised Codex and OpenRouter browser callbacks, token exchange, refresh, SSE tool turns, reopened-session reasoning replay, vendor endpoint isolation and private key storage through isolated CLI fixtures; no live vendor account was used.
- test(distribution): verified four-platform public CI for the seven-provider milestone and successful publishing of all four `v0.1.2` release builds.

- feat(provider): added registered OpenAI Responses, native Ollama and Google Gemini transports alongside Chat Completions and Anthropic Messages; routed DeepSeek, Groq and Mistral through explicit compatible chat descriptors.
- feat(auth): added Anthropic OAuth PKCE browser/manual login, validated loopback callback, private atomic credential storage, cross-process refresh and bearer inference that rejects custom endpoints.
- fix(provider): rejected inconsistent streamed Responses tool arguments and final assistant output rather than executing a divergent tool call.
- test(provider): exercised all seven CLI provider descriptors against local HTTP fixtures, and an isolated OAuth callback, token exchange, refresh, protected inference and logout with a fake HTTPS subprocess; no live vendor credential was used.
- test(distribution): verified the public `v0.1.1` GitHub Release completed all four native builds and published the release artifacts.

- chore(license): identified Pave as the MIT copyright holder and preserved earlier MIT notices verbatim in the distributed third-party notice file.
- docs(provider): listed only the two working wire protocols and API-key authentication methods; marked OAuth and additional transports as pending.
- docs(project): redesigned the public README around install, use, feature status and contribution paths; replaced the rounded logo with a two-dimensional pixel-art mark.
- docs(project): scheduled a responsibility-based directory and file restructuring after the planned product capabilities were completed.
- chore(project): removed legacy branding from published files, replaced the private repository's commit lineage with a clean root, and made the repository public while retaining MIT notices.
- feat(distribution): added a one-command, checksum-verified native installer and four-platform GitHub Release workflow with bundled license notices.
- feat(tui): added a differential full-screen terminal interface with bounded transcript rows, live text, shell approvals, prompt history and non-TTY fallback.
- fix(tui): preserved fragmented CJK keystrokes, replaced malformed UTF-8, handled interrupted escape sequences and edited grapheme clusters correctly.
- ci(project): added public-repository test workflows, opam metadata, installation instructions, an original logo and contribution/security policies.
- test(distribution): exercised source package installation, a real installed CLI, local release-archive installation and checksum rejection without replacing the installed binary.
- test(tui): exercised real pseudo-terminal CJK input/output, multiline paste, resize, streamed replies, interrupted escape keys and malformed input.

- feat(session): added manual model-generated conversation compaction with durable branch-aware summary replay and unchanged full transcript history.
- test(session): verified kept-turn replay, interrupted tool recovery, branch/fork isolation and live CLI resume after compaction.

- feat(session): replaced flat transcript writes with a private append-only JSONL session tree, branch/fork commands, safe flat-session migration, and persisted interruption results for unfinished tool calls.
- test(session): covered parent-linked branch selection, fork isolation, stale-writer rejection, flat-session migration, and interrupted tool-call recovery.

- feat(provider): ported Anthropic Messages and incremental OpenAI/Anthropic SSE tool-call decoding with bounded transport and terminal-aware cancellation.
- test(provider): covered fragmented SSE frames, paired streamed tool calls, live local HTTP provider errors, and both providers through the mobile CLI.

- feat(agent): ported the core tool-calling coding-agent loop to OCaml with an OpenAI-compatible HTTP adapter, durable sessions, and bounded turns.
- feat(mobile): added mobile project detection, workspace-scoped file tools, interactive shell approval, and a CLI for iOS, Android, Flutter, and React Native repositories.
- test(agent): added real local HTTP tool-call, session-recovery, workspace-boundary, and command-timeout coverage.
