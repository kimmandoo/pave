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

The default provider uses `OPENAI_API_KEY` when making a request. You can enter the interactive terminal **before** configuring a credential, then use `/login` and `/model`; one-shot prompts still require a usable provider. Choose another registered provider with `--provider ID`; `pave --providers` shows IDs, routes and required key variables. Keep API keys out of checked-in config and session files.

```sh
# Interactive: resize-aware TUI, prompt history and a persistent session.
pave --root /path/to/mobile/repo --session /private/path/pave.jsonl

# One-shot, streaming reply; GPT-5/o-series models select the Responses route.
pave --provider openai --model gpt-5 --prompt 'Inspect the Android build failure' --stream

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

Inside a running Pave terminal, `/login` lists sign-in providers and accepts a provider ID; `/login github-copilot` starts the device-code flow directly. The browser URL or verification code is printed on the regular terminal while the full-screen UI is temporarily suspended, then the previous transcript/editor returns after sign-in. `/model` lists registered providers and prompts for `PROVIDER/MODEL_ID`; `/model github-copilot/gpt-4.1` selects one directly. Namespaced model IDs keep everything after the first slash, and a bare model ID uses the current provider. These are provider routes, **not** a complete or live-validated model catalog. Authentication and provider/model selection are separate; choose a model after login. Switching models retains the visible conversation and drops opaque Codex/Gemini state if the provider or model changes.

For a remote browser, use `pave --login-manual PROVIDER` for browser-based providers and paste the full callback URL; OpenRouter also accepts the authorization code alone because that provider does not echo state. Standard OAuth providers require the correct callback state. **Copilot uses a device code instead:** `--login-manual github-copilot` is unsupported; enter the displayed code at the displayed GitHub verification URL. `pave --logout PROVIDER` removes a stored credential. The private store is **unencrypted** at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` (0700 directory, 0600 file); OpenRouter's browser exchange stores an API key there. An environment API key takes precedence where available. OAuth refresh is locked across processes; browser/device-derived tokens and keys cannot be sent to a custom `--endpoint`.

`--api NAME` selects an explicit registered route; `--endpoint URL` overrides an API-key provider's completion endpoint. `--model ID` overrides the OpenAI default (`gpt-4.1-mini`) and is required for one-shot prompts with providers that have no default. Interactive sessions may select it later with `/model`. Redirected input/output uses a plain line-oriented CLI with the same slash commands instead of the full-screen interface.

| In the TUI | Action |
| --- | --- |
| `Enter` · `Shift+Enter` | Send a prompt · insert a newline (bracketed paste also supports multiline) |
| `←` `→` · `↑` `↓` | Move by Unicode grapheme · browse prompt history |
| `Ctrl+C` · `Ctrl+D` | Clear the draft · exit when the draft is empty |
| `/login [PROVIDER]` · `/model [PROVIDER/MODEL_ID]` | Sign in via browser or device code · choose the next turn's provider/model |
| `/help` · `/entries` | Show commands · list journal message IDs |
| `/branch ID` · `/fork /path/new.jsonl` | Continue from an earlier message · copy the selected conversation |
| `/compact` · `/quit` | Summarize older turns manually · exit |

Sessions are private append-only JSONL journals on creation, **not encrypted**. Keep them outside version control: Gemini 3 native replay may persist model-issued thought text and signatures alongside visible conversation content. Reopening a session marks interrupted tool calls as failed rather than rerunning them. `/compact` preserves the full journal; model summarization may fail if the provider's context limit is exceeded.

## Providers

| Provider | Transport | Authentication | CLI selection |
| --- | --- | --- | --- |
| OpenAI | Chat Completions, Responses | `OPENAI_API_KEY` | `--provider openai`; `--api responses` or GPT-5/o-series auto-route |
| OpenAI Codex subscription | account-scoped Codex Responses | `--login openai-codex` (PKCE; refresh) | `--provider openai-codex --model MODEL_ID` |
| Anthropic | Messages | `ANTHROPIC_API_KEY` or `--login anthropic` | `--provider anthropic --model MODEL_ID` |
| Ollama | native `/api/chat` | none (local server) | `--provider ollama --model MODEL_ID` |
| Google Gemini API | native `generateContent` | `GEMINI_API_KEY` | `--provider google --model MODEL_ID` |
| DeepSeek | Chat Completions | `DEEPSEEK_API_KEY` | `--provider deepseek --model MODEL_ID` |
| Groq | Chat Completions | `GROQ_API_KEY` | `--provider groq --model MODEL_ID` |
| Mistral | Chat Completions | `MISTRAL_API_KEY` | `--provider mistral --model MODEL_ID` |
| OpenRouter | Chat Completions | `OPENROUTER_API_KEY` or `--login openrouter` (PKCE exchanges for API key) | `--provider openrouter --model MODEL_ID` |
| GitHub Copilot (personal github.com) | official Chat Completions endpoint only | `--login github-copilot` (device code, `read:user`) | `--provider github-copilot --model gpt-4.1` or `gpt-4o` |

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
