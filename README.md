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

<p align="center"><a href="#install">Install</a> · <a href="#use">Use</a> · <a href="#providers">Providers</a> · <a href="#features">Features</a> · <a href="#contribute">Contribute</a></p>

> [!NOTE]
> Pave is in active development. Ten provider descriptors and six wire payload formats, the agent loop, four sign-in paths (three browser flows and one device-code flow), session journal and interactive terminal work in local fixtures. Live credentialed inference, cancellation, LSP/DAP, subagents, plugins and a full model catalog are not yet verified or implemented. See [TASKS.md](TASKS.md) for the remaining work.

## Install

**macOS 15+** (Apple Silicon or Intel) · **Linux glibc 2.35+** (x86-64 or ARM64). No Windows or musl builds yet. [Releases](https://github.com/kimmandoo/pave/releases) provide native binaries; you do not need opam, a compiler or sudo.

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | sh
```

The [installer](install.sh) verifies the release archive against its published SHA-256 manifest, then installs the binary to `~/.local/bin/pave` and its notices and native-install marker to `~/.local/share/licenses/pave`. If prompted, add `~/.local/bin` to your `PATH`. Inspect the script before running it if you prefer not to pipe downloads into a shell. For installer-owned binaries:

```sh
pave update --check  # Compare embedded release version against GitHub's latest published tag; no files changed.
pave update          # Upgrade to the latest release.
```

The native binary executes its **embedded** copy of the checksum-verifying installer; it does not fetch a new shell script. `--check` requires a release built with embedded version metadata (`v0.1.6` or later); an unavailable/rate-limited GitHub API fails with an error rather than guessing. Updates reinstall the latest release, including when you are already up to date. This command preserves a custom install directory, but intentionally ignores `PAVE_VERSION` and `PAVE_INSTALL_DIR` overrides from your environment. A binary installed before the native-install marker was introduced (through `v0.1.4`) needs the one-command installer run **once more** before `pave update` is available. Source/opam installs do not self-update; use the package-manager steps below.

<details>
<summary>Version pinning, custom destination and removal</summary>

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | PAVE_VERSION=v0.1.5 sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | PAVE_INSTALL_DIR="$HOME/tools/bin" sh
```

Use the installer directly to install a **specific published** tag or change destinations; `pave update` always targets the latest release at the binary's current location. To uninstall, remove `~/.local/bin/pave` and `~/.local/share/licenses/pave` (or the equivalent locations under your custom destination).

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

The default provider uses `OPENAI_API_KEY` when making a request. You can enter the interactive terminal **before** configuring a credential, then use `/login` and `/model`; one-shot prompts still require a usable provider. Choose another registered provider with `--provider ID`; `pave --providers` shows configured routes and key variables. `pave --provider ollama --models` queries local model tags; the same `--models` option queries pinned, authenticated listings for OpenAI, Google Gemini and personal Copilot. These are live listing results, not proof that Pave supports every listed inference route: unsupported Copilot routes are labeled. Discovery does not cache results and may fail offline; the `/model` picker uses a separate, limited offline suggestion list and also accepts a typed `PROVIDER/MODEL_ID`. Keep API keys out of checked-in config and session files.

```sh
# Interactive: resize-aware TUI, prompt history and a persistent session.
pave --root /path/to/mobile/repo --session /private/path/pave.jsonl

# One-shot streaming reply; OpenAI defaults to GPT-6 Sol on Responses.
pave --provider openai --prompt 'Inspect the Android build failure' --stream

# Anthropic: API key, or explicit browser-based OAuth login for a subscription.
pave --login anthropic
pave --provider anthropic --model "$MODEL_ID" --root /path/to/mobile/repo

# Codex subscription: separate ChatGPT OAuth and account-scoped Responses API.
pave --login openai-codex
pave --provider openai-codex --model "$CODEX_MODEL" --prompt 'Inspect this project'

# OpenRouter browser exchange yields a durable API key; or set OPENROUTER_API_KEY.
pave --login openrouter
pave --provider openrouter --model "$ROUTER_MODEL" --prompt 'Inspect this project'

# GitHub Copilot personal account: device-code login; public Chat route only.
pave --login github-copilot
pave --provider github-copilot --model gpt-4.1 --prompt 'Inspect this project'

# Local Ollama; pull a model with Ollama before invoking Pave.
pave --provider ollama --model "$LOCAL_MODEL" --prompt 'Inspect this project'
```

Inside a running Pave terminal, `/login` opens a searchable, keyboard-accessible sign-in-provider picker; `/login github-copilot` starts device authorization directly. The browser URL or verification code is printed on the regular terminal while the full-screen UI is suspended, then the previous transcript/editor returns. `/model` opens a searchable suggested-model picker; type any complete `PROVIDER/MODEL_ID` there to select a custom model after route validation, or run `/model github-copilot/gpt-4.1` directly. Escape cancels a picker without sending the draft. Namespaced model IDs keep everything after the first slash, and a bare model ID uses the current provider. Suggestions are **not** live-validated entitlements; `--models` queries supported provider listings separately. Switching models retains visible conversation and drops opaque Codex/Gemini state when the provider or model changes.

For a remote browser, use `pave --login-manual PROVIDER` for browser-based providers and paste the full callback URL; OpenRouter also accepts the authorization code alone because that provider does not echo state. Standard OAuth providers require the correct callback state. **Copilot uses a device code instead:** `--login-manual github-copilot` is unsupported; enter the displayed code at the displayed GitHub verification URL. `pave --logout PROVIDER` removes a stored credential. The private store is **unencrypted** at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` (0700 directory, 0600 file); OpenRouter's browser exchange stores an API key there. An environment API key takes precedence where available. OAuth refresh is locked across processes; browser/device-derived tokens and keys cannot be sent to a custom `--endpoint`.

`--api NAME` selects an explicit registered route; `--endpoint URL` overrides an API-key provider's completion endpoint. OpenAI defaults to [`gpt-6-sol`](https://developers.openai.com/api/docs/models/gpt-6-sol), the current coding-focused GPT-6 model, on the Responses route; [`gpt-6-astra`](https://developers.openai.com/api/docs/models/gpt-6-astra) is a higher-cost flagship selectable with `--model`. `--model ID` overrides the default and is required for one-shot prompts with providers that have no default. Interactive sessions may select a provider/model later with `/model`. Personal Copilot still accepts only its explicitly supported older Chat models and is **not** the OpenAI default. `--models` refuses a custom `--endpoint` to avoid sending a private gateway key to a public listing endpoint. Redirected input/output uses a plain line-oriented CLI with the same slash commands instead of the full-screen interface.

The full-screen TUI initially shows the existing pixel-art Pave mark as colored ASCII art. It is an empty-transcript placeholder, not a journal entry; the first message replaces it. Small terminals show a compact `PAVE` label, while redirected output remains plain text.

Typed settings may be placed in `${XDG_CONFIG_HOME:-~/.config}/pave/settings.json` and `<workspace>/.pave/settings.json`. Supported JSON keys are `default_provider`, `default_model` (requires the matching provider), `max_turns` (1–100) and `disable_shell` (boolean). Explicit CLI defaults override project settings, which override user defaults; `disable_shell: true` in **either** scope prevents `--allow-shell` from granting execution. Invalid, duplicate, oversized or symlinked settings are reported and skipped. `/settings` edits project settings with searchable choices and atomic private-file replacement; changes take effect on the next launch, not midway through an active turn. User and ancestor `AGENTS.md` instructions are appended **after** Pave's mobile safety prompt; bounded relative `@file.md` imports are supported. Path-scoped `.pave/rules/*.md` matching exists in the loader but is **not yet applied** to per-file tool operations. Project instructions are guidance, not a security boundary.

| In the TUI | Action |
| --- | --- |
| `Enter` · `Shift+Enter` | Send a prompt · insert a newline; pasted Enter never submits |
| `←` `→` · `↑` `↓` | Move by Unicode grapheme or wrapped visual row; history at first/last row |
| `Ctrl+P`/`Ctrl+N` · `Ctrl+R` | Explicit older/newer history · incremental reverse search (Enter recalls, Escape cancels) |
| `Ctrl/Alt+←/→` · `Ctrl+W` | Move or delete by word; editor draft stays intact during model output |
| `PgUp`/`PgDn` · `Ctrl+Home`/`Ctrl+End` | Scroll the transcript by page or jump to its beginning/end |
| `Ctrl+C` · `Ctrl+D` | Clear a nonempty draft; with an empty draft cancel the active turn · exit when empty |
| `/login [PROVIDER]` · `/model [PROVIDER/MODEL_ID]` | Search sign-in/model choices (Esc cancels) or select directly |
| `/cancel` · `/settings` | Stop the active request/command; edit typed project defaults for the next launch |
| `/help` · `/entries` | Show commands · list journal message IDs |
| `/branch ID` · `/fork /path/new.jsonl` | Continue from an earlier message · copy the selected conversation |
| `/compact` · `/quit` | Summarize older turns manually · exit |

The input remains responsive during network calls and approved commands. Prompts submitted while a turn runs are queued and appear in the transcript only when their own turn begins; `/cancel` stops the active turn without discarding queued prompts or an unsent draft. Transient streamed text from a cancelled or failed turn is removed. In-memory scrollback keeps the latest 10,000 rows; `--session` journals the full durable conversation.

Sessions are private append-only JSONL journals on creation, **not encrypted**. Keep them outside version control: Gemini 3 native replay may persist model-issued thought text and signatures alongside visible conversation content. Reopening a session marks interrupted tool calls as failed rather than rerunning them. `/compact` preserves the full journal; model summarization may fail if the provider's context limit is exceeded.

## Providers

| Provider | Transport | Authentication | CLI selection |
| --- | --- | --- | --- |
| OpenAI | Chat Completions, Responses | `OPENAI_API_KEY` | `--provider openai`; GPT-6/GPT-5/o-series auto-route to Responses |
| OpenAI Codex subscription | account-scoped Codex Responses | `--login openai-codex` (PKCE; refresh) | `--provider openai-codex --model MODEL_ID` |
| Anthropic | Messages | `ANTHROPIC_API_KEY` or `--login anthropic` | `--provider anthropic --model MODEL_ID` |
| Ollama | native `/api/chat` | none (local server) | `--provider ollama --model MODEL_ID` |
| Google Gemini API | native `generateContent` | `GEMINI_API_KEY` | `--provider google --model MODEL_ID` |
| DeepSeek | Chat Completions | `DEEPSEEK_API_KEY` | `--provider deepseek --model MODEL_ID` |
| Groq | Chat Completions | `GROQ_API_KEY` | `--provider groq --model MODEL_ID` |
| Mistral | Chat Completions | `MISTRAL_API_KEY` | `--provider mistral --model MODEL_ID` |
| OpenRouter | Chat Completions | `OPENROUTER_API_KEY` or `--login openrouter` (PKCE exchanges for API key) | `--provider openrouter --model MODEL_ID` |
| GitHub Copilot (personal github.com) | official Chat Completions endpoint only | `--login github-copilot` (device code, `read:user`) | `--provider github-copilot --model gpt-4.1` or `gpt-4o` |

Interactive Copilot sessions may start with `pave --provider github-copilot` and select `/model github-copilot/gpt-4.1` after entering the terminal; one-shot prompts still require a supported model before any credential is read.

All ten entries completed **isolated CLI fixtures**, not live vendor calls. The Codex scenario exercised a real loopback callback, JWT account routing, refresh, SSE tool turns, encrypted reasoning replay through a reopened session and enterprise residency headers; OpenRouter exercised its state-less PKCE exception, key exchange, stored-key inference and logout. Gemini 3 buffered and SSE scenarios exercised signed tool calls, function-result replay and reopened journals; unsigned Gemini 3 tool calls fail closed. Copilot's GitHub device-code grant exercised private token storage, endpoint/model isolation, Chat tool turns, in-session `/login` and `/model`, and logout with fake HTTPS responses. Auth fixtures intercepted HTTPS with a local subprocess, so public OAuth client registration, live account entitlement, model support and actual vendor responses remain **unverified**. Copilot currently accepts only the personal `https://api.githubcopilot.com/chat/completions` route with `gpt-4.1` or `gpt-4o`; Enterprise, Responses, Anthropic and dynamic model discovery are **not** supported. Other reference auth policies, proprietary gateway transports and full model semantics are still missing. A compatible endpoint does not imply every model feature works.

## Features

| Available | Not yet available |
| --- | --- |
| Six wire payload formats with distinct provider routes; bounded buffered and incremental-stream decoders; model-bound Codex/Gemini native state replay | Most provider-specific thinking/usage/multimodal parity and full model catalog |
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

[MIT](LICENSE), with Pave as the copyright holder. Earlier MIT copyright and permission notices and linked-library terms remain in [THIRD_PARTY_NOTICES](THIRD_PARTY_NOTICES); both files accompany release binaries.
