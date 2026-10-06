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

<p align="center"><a href="#install">Install</a> · <a href="#quick-start">Quick start</a> · <a href="#features">Features</a> · <a href="#providers">Providers</a> · <a href="#documentation">Docs</a> · <a href="#contribute">Contribute</a></p>

> [!NOTE]
> Pave is under active development. Provider routes, sign-in paths and the terminal have fixture coverage, **not** blanket live vendor entitlement. Remaining work is tracked in the [ordered backlog](task.md).

## Highlights

- **Mobile-aware** — inventories Xcode, SwiftPM, Gradle, Flutter and React Native/Expo projects, and runs focused checks only with your approval.
- **70+ providers** — OpenAI, Anthropic, Gemini, Codex, Copilot, OpenRouter, Bedrock, Vertex, Ollama and many more, plus your own OpenAI-compatible gateway.
- **Keyboard-first TUI** — streaming output, queued follow-ups, searchable model picker, `@` file attachments and live write previews.
- **Branching sessions** — private append-only journals with `/tree`, `/branch`, `/fork` and journal-safe compaction.
- **Safe by default** — shell is off unless you pass `--allow-shell`, and every command still needs your approval.
- **Extensible** — local skills, declarative commands, plugins, MCP servers, LSP/DAP and opt-in read-only subagents.

## Install

| Platform | Support |
| --- | --- |
| macOS 15+ | Apple Silicon, Intel |
| Linux (glibc 2.35+) | x86-64, ARM64 |
| Windows, musl | Not yet |

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | sh
```

The [installer](install.sh) verifies the SHA-256 checksum and installs to `~/.local/bin/pave` — no opam, compiler or sudo needed. Inspect the script first if you prefer not to pipe into a shell.

```sh
pave update --check  # Check for a newer release
pave update          # Upgrade in place
pave uninstall       # Remove the binary and its license files
```

<details>
<summary>Update safeguards</summary>

- The binary runs its **embedded** checksum-verifying installer, pinned to the resolved latest release tag.
- `--check` changes no files; unavailable or rate-limited GitHub metadata is an error, not a guess.
- An update reports success only after the verified install completes.
- Updates keep the original install directory and the private login store at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json`.
- Binaries installed through `v0.1.4` need one installer rerun to add the native-install marker.

</details>

<details>
<summary>Pin a version or change the destination</summary>

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | PAVE_VERSION=v0.1.41 sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | PAVE_INSTALL_DIR="$HOME/tools/bin" sh
```

`pave update` always targets the latest release at the current location. `pave uninstall` removes only the installer-owned binary, license files and install marker — settings, sessions and credentials stay.

</details>

<details>
<summary>Build from source with opam</summary>

**Prerequisites**

- macOS: `brew install opam curl git` plus Xcode Command Line Tools
- Ubuntu/Debian: `sudo apt-get install opam curl git build-essential pkg-config m4`
- First time only: `opam init -y`

```sh
git clone https://github.com/kimmandoo/pave.git
cd pave
opam switch create . 5.5.1 -y
opam install . --deps-only -y
opam install . -y
opam exec -- pave --help
```

| Task | Command |
| --- | --- |
| Update | `git pull --ff-only && opam install . --deps-only -y && opam reinstall pave -y` |
| Remove | `opam remove pave -y` |

CI targets OCaml 5.3 and 5.5.

</details>

## Quick start

```sh
pave                                   # Opens interactive setup; no credentials needed to start
```

1. Run `/setup` to connect an account or save a default model.
2. Run `/model` to switch models for the current conversation.
3. Start asking about your project.

> [!TIP]
> API-key providers read keys from environment variables only. Keep keys out of checked-in config and session files.

### Common commands

```sh
# Interactive TUI with a persistent session
pave --root /path/to/mobile/repo --session /private/path/pave.jsonl

# One-shot streaming reply
pave --model "$MODEL_SELECTOR" --prompt 'Inspect the Android build failure' --stream

# Discover providers and models
pave --providers
pave --provider openai --models
```

<details>
<summary>Sign-in examples</summary>

```sh
# Anthropic (API key or browser OAuth)
pave --login anthropic
pave --provider anthropic --model "$MODEL_ID" --root /path/to/mobile/repo

# Codex subscription (browser PKCE or device approval)
pave --login openai-codex
pave --login-device openai-codex
pave --provider openai-codex --model "$CODEX_MODEL" --prompt 'Inspect this project'

# OpenRouter (browser exchange, or OPENROUTER_API_KEY)
pave --login openrouter

# GitHub Copilot personal account (device code)
pave --login github-copilot

# Local Ollama
pave --provider ollama --model "$LOCAL_MODEL" --prompt 'Inspect this project'
```

</details>

<details>
<summary>Model selectors</summary>

- Copy a canonical `provider@route[#account]/EXACT_UPSTREAM_ID` selector from `--models` output into `--model`.
- `[listed · API unverified]` means the account lists the ID — **not** that it supports Chat or tools.
- Anthropic OAuth cannot list models without `ANTHROPIC_API_KEY`; type the model ID instead.
- `--context-window auto` reads provider-reported limits where supported; otherwise pass a verified number. See [context budgeting](docs/USAGE.md#context-budgeting).

</details>

### Key shortcuts

| Key | Action |
| --- | --- |
| `Enter` | Send when idle; queue while working without interrupting |
| `Alt+Enter` / `/queue MSG` | Same noninterrupting send/queue behavior |
| `/steer MSG` | Explicitly interrupt and send this message next |
| `Alt+Q` / `/queue` | Manage queued prompts: run next, interrupt-and-run now, edit or cancel |
| `@` | Attach workspace files |
| `/` + `Tab` | Search slash commands |
| `Ctrl+R` | Search prompt history |
| `Alt+O` | Expand the latest tool result |
| `Ctrl+C` · `Ctrl+D` | Interrupt · exit |

On macOS, use `Return` and `Option` in place of `Enter` and `Alt`. See the [full keyboard and slash-command reference](docs/USAGE.md#keyboard-and-slash-command-reference).

## Features

| Area | Available | Not yet |
| --- | --- | --- |
| **Providers** | Seven wire formats, streaming and buffered decoders, reasoning replay, per-model effort selection | Full thinking/usage/multimodal parity and model catalog |
| **Mobile** | Project inventory for Xcode, SwiftPM, Gradle, Flutter, RN/Expo; approved builds/checks with source-location hints; simulator/AVD/ADB inventory | Booting simulators, deploying or running device tests |
| **Tools** | File read/search/edit/write, LSP/DAP, managed worktrees, JS/Python eval, read-only child jobs | Writable parallel workers, browser/CDP |
| **Tasks** | OpenAI embeddings, images, speech, transcription; Cohere rerank | Video generation (OpenAI Videos/Sora retired) |
| **Terminal** | CJK-aware input, cancellable streaming, branching sessions, automatic and native compaction | Tokenizer-exact context limits |

## Providers

Linux and macOS builds expose **71** built-in provider IDs on both architectures.

| Category | Examples |
| --- | --- |
| Major APIs | OpenAI, Anthropic, Google Gemini, xAI, Mistral, DeepSeek, Groq |
| Subscriptions | OpenAI Codex, GitHub Copilot, Kimi Code, Alibaba / Xiaomi / MiniMax plans, Devin |
| Clouds & gateways | Azure OpenAI, Google Vertex, Amazon Bedrock, OpenRouter, Cloudflare, Vercel |
| Local | Ollama, LM Studio, llama.cpp, vLLM |

See [docs/PROVIDERS.md](docs/PROVIDERS.md) for every route, credential, CLI flag and caveat, including [custom OpenAI-compatible providers](docs/PROVIDERS.md#user-defined-openai-compatible-providers).

## Safety

> [!WARNING]
> Approved shell commands are **not sandboxed** and can access files outside the workspace.

- Shell execution is **off** by default; `--allow-shell` still asks for **each** command, even in `yolo` mode.
- Headless runs deny anything that needs approval.
- The default `write` approval mode allows reads and workspace writes; execution needs approval.
- Login storage and session journals are private (0600) but **unencrypted**. `--mask-secrets` redacts known keys locally.
- Pave never installs mobile SDKs, signs apps or deploys to devices for you.

## Documentation

| Document | Contents |
| --- | --- |
| [Usage reference](docs/USAGE.md) | Accounts, setup, TUI, configuration, sessions, context, skills/MCP, tasks, mobile tools |
| [Providers](docs/PROVIDERS.md) | Provider table, route notes, custom providers, caveats |
| [Compaction](docs/compaction.md) | Context compaction behavior and limits |
| [Design rules](docs/DESIGN_RULES.md) | Binding implementation contract |
| [Changelog](CHANGELOG.md) | Release history |

## Contribute

Bug reports, TUI polish, provider work and mobile-workspace testing are all welcome. Start with [open issues](https://github.com/kimmandoo/pave/issues) or the [ordered backlog](task.md).

1. Fork, branch from `main` and build from source.
2. Add a behavior-focused regression, then run:
   ```sh
   opam exec -- dune runtest --force
   opam exec -- dune build @install
   ```
3. Update docs and [CHANGELOG.md](CHANGELOG.md), then open a [pull request](https://github.com/kimmandoo/pave/pulls).

See [CONTRIBUTING.md](CONTRIBUTING.md) and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md). Report vulnerabilities **privately** via [SECURITY.md](SECURITY.md) — never attach credentials, private source or session transcripts.

## License

[MIT](LICENSE). Earlier notices and linked-library terms are in [THIRD_PARTY_NOTICES](THIRD_PARTY_NOTICES); both ship with release binaries.
