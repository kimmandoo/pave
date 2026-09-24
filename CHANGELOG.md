# Changelog

## 2026-09-24

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
