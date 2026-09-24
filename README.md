<p align="center">
  <img src="assets/pave-mark.svg" width="128" alt="Pave pixel-art logo">
</p>

<h1 align="center">Pave</h1>
<p align="center">A terminal coding agent for iOS, Android, Flutter and React Native projects.<br>Native OCaml · keyboard-first · open source.</p>

<p align="center">
  <a href="https://github.com/kimmandoo/pave/actions/workflows/ci.yml"><img src="https://github.com/kimmandoo/pave/actions/workflows/ci.yml/badge.svg" alt="CI status"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/license-MIT-9af0d3" alt="MIT license"></a>
  <a href="CONTRIBUTING.md"><img src="https://img.shields.io/badge/contributions-welcome-9af0d3" alt="Contributions welcome"></a>
</p>

<p align="center"><a href="#install">Install</a> · <a href="#use">Use</a> · <a href="#features">Features</a> · <a href="#contribute">Contribute</a></p>

> [!NOTE]
> Pave is in active development. The provider/agent core, session journal and interactive terminal work; cancellation, LSP/DAP, subagents, plugins and a model catalog are not yet implemented. See [TASKS.md](TASKS.md) before relying on a planned feature.

## Install

**macOS 15+** (Apple Silicon or Intel) · **Linux glibc 2.35+** (x86-64 or ARM64). No Windows or musl builds yet. [Releases](https://github.com/kimmandoo/pave/releases) provide native binaries; you do not need opam, a compiler or sudo.

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | sh
```

The [installer](install.sh) verifies the release archive against its published SHA-256 manifest, then installs the binary to `~/.local/bin/pave` and its notices to `~/.local/share/licenses/pave`. If prompted, add `~/.local/bin` to your `PATH`. Inspect the script before running it if you prefer not to pipe downloads into a shell.

<details>
<summary>Version pinning, custom destination, upgrade and removal</summary>

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | PAVE_VERSION=v0.1.0 sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | PAVE_INSTALL_DIR=/absolute/path/bin sh
```

Rerun the installer to upgrade. To uninstall, remove `~/.local/bin/pave` and `~/.local/share/licenses/pave` (or your custom paths).

</details>

<details>
<summary>Build from source with opam</summary>

Install Git, curl, opam, a C compiler and build tools. macOS: `brew install opam curl git` plus Xcode Command Line Tools. Ubuntu/Debian: `sudo apt-get install opam curl git build-essential pkg-config m4`. On first use, run `opam init -y`.

```sh
git clone https://github.com/kimmandoo/pave.git
cd pave
opam switch create . 5.5.1 -y
opam install . --deps-only -y
opam install . -y
opam exec -- pave --help
```

The switch belongs to the checkout; prefix commands with `opam exec --` without changing your parent shell. To update, run `git pull --ff-only`, `opam install . --deps-only -y`, then `opam reinstall pave -y`. To remove just the package, run `opam remove pave -y` inside the switch. CI targets OCaml 5.3 and 5.5; the source installation was exercised with 5.5.1.

</details>

## Use

Set `OPENAI_API_KEY` in your environment for the default provider, or use `ANTHROPIC_API_KEY` with `--provider anthropic`. Keep credentials out of checked-in config and session files.

```sh
# Interactive: resize-aware TUI, prompt history and a persistent session.
pave --root /path/to/mobile/repo --session /private/path/pave.jsonl

# One-shot, streaming reply.
pave --root /path/to/mobile/repo --prompt 'Inspect the Android build failure' --stream

# Anthropic: provide a model supported by your account.
pave --provider anthropic --model "$MODEL_ID" --root /path/to/mobile/repo
```

`--endpoint URL` selects a custom completion endpoint; `--model ID` overrides the default OpenAI model (`gpt-4.1-mini`). Anthropic requires an explicit model. Redirected input/output uses a plain line-oriented CLI instead of the full-screen interface.

| In the TUI | Action |
| --- | --- |
| `Enter` · `Shift+Enter` | Send a prompt · insert a newline (bracketed paste also supports multiline) |
| `←` `→` · `↑` `↓` | Move by Unicode grapheme · browse prompt history |
| `Ctrl+C` · `Ctrl+D` | Clear the draft · exit when the draft is empty |
| `/help` · `/entries` | Show commands · list journal message IDs |
| `/branch ID` · `/fork /path/new.jsonl` | Continue from an earlier message · copy the selected conversation |
| `/compact` · `/quit` | Summarize older turns manually · exit |

Sessions are private append-only JSONL journals on creation, **not encrypted**. Keep them outside version control. Reopening a session marks interrupted tool calls as failed rather than rerunning them. `/compact` preserves the full journal; model summarization may fail if the provider's context limit is exceeded.

## Features

| Available | Not yet available |
| --- | --- |
| OpenAI-compatible Chat Completions and Anthropic Messages, buffered or SSE-streamed | OpenAI Responses/Codex, Gemini-native and a full model catalog |
| Mobile manifest detection, workspace file read/search/edit/write, bounded agent turns | LSP/DAP, subagents, extensions and full tool catalog |
| Grapheme-aware CJK input, live transcript, branching sessions and manual compaction | Cancellation/queued typing during an active model turn; automatic compaction |

**Shell safety:** model-requested shell execution is off by default. `--allow-shell` asks for **each** command in an interactive terminal; noninteractive runs deny commands even with the flag. Approved commands are **not sandboxed** and can access files outside the workspace. Check commands before approving them; Pave does not install mobile SDKs, sign apps or deploy to devices for you.

## Contribute

New contributors are welcome—bug reports, TUI polish, provider work and mobile-workspace testing are useful. Start with [open issues](https://github.com/kimmandoo/pave/issues), [the feature plan](TASKS.md) and [design rules](docs/DESIGN_RULES.md).

1. Fork the repository, create a focused branch from `main`, and use the source-install instructions above.
2. Add a behavior-focused regression for a bug. Run `opam exec -- dune runtest --force` and `opam exec -- dune build @install`; check TUI changes in a real terminal or PTY.
3. Update usage/docs and [CHANGELOG.md](CHANGELOG.md) when behavior changes. Open a [pull request](https://github.com/kimmandoo/pave/pulls) with the behavior, checks and platform limitations.

Read [CONTRIBUTING.md](CONTRIBUTING.md) for the full checklist and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for community expectations. Report vulnerabilities **privately** using [SECURITY.md](SECURITY.md), not a public issue. Never attach credentials, private source or session transcripts to reports.

## License

[MIT](LICENSE). Required copyright and permission notices remain in `LICENSE`; linked-library licenses accompany binaries in [THIRD_PARTY_NOTICES](THIRD_PARTY_NOTICES).
