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
> Pave is in active development. Ten provider descriptors and six wire payload formats, four sign-in paths, an append-only session journal and a cancellable interactive terminal work in local fixtures. Live vendor entitlements, LSP/DAP, subagents, plugins and a full model catalog are not verified or implemented. See [TASKS.md](TASKS.md) for the remaining work.

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

The native binary executes its **embedded** copy of the checksum-verifying installer; it does not fetch a new shell script. `--check` requires a release built with embedded version metadata (`v0.1.6` or later); an unavailable/rate-limited GitHub API fails with an error rather than guessing. Updates validate the latest release tag from GitHub's API and pin both archive and checksum downloads to that tag, including when you are already up to date. This avoids a stale `/latest/download` redirect reinstalling an old version. Updates preserve a custom install directory and the separate private login store at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json`, but intentionally ignore `PAVE_VERSION` and `PAVE_INSTALL_DIR` overrides from your environment. A binary installed before the native-install marker was introduced (through `v0.1.4`) needs the one-command installer run **once more** before `pave update` is available. Source/opam installs do not self-update; use the package-manager steps below.

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

The default provider uses `OPENAI_API_KEY` when making a request. The interactive terminal opens before credentials are configured; use `/login` and `/model` from there. `pave --providers` lists available routes. `--models` queries pinned authenticated OpenAI, Gemini, Anthropic (API key), DeepSeek, Groq, Mistral, Together, Cerebras, Venice, DeepInfra, Fireworks, Baseten, Hugging Face, NanoGPT, Codex, OpenRouter and personal Copilot listings, plus local Ollama, LM Studio, llama.cpp and vLLM hosts. Azure deployment IDs are explicitly configured; its model discovery is not available. No model ID is built into the provider catalog. The cancellable `/model` picker displays discovered account/local IDs; if discovery is unavailable, type a model ID explicitly. A listing does not guarantee account entitlement or every model capability. Keep API keys out of checked-in config and session files.

```sh
# Interactive: resize-aware TUI, prompt history and a persistent session.
pave --root /path/to/mobile/repo --session /private/path/pave.jsonl

# One-shot streaming reply; choose an ID from `pave --provider openai --models`.
pave --provider openai --model "$MODEL_ID" --prompt 'Inspect the Android build failure' --stream

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
pave --provider github-copilot --model "$COPILOT_MODEL" --prompt 'Inspect this project'

# Local Ollama; pull a model with Ollama before invoking Pave.
pave --provider ollama --model "$LOCAL_MODEL" --prompt 'Inspect this project'
```

Inside a running Pave terminal, `/login` opens a searchable keyboard-accessible sign-in picker; `/login github-copilot` starts device authorization. Browser URLs and verification codes appear on the regular terminal while the full-screen UI is suspended, then the previous transcript/editor returns. `/model` loads the selected provider's models asynchronously into a searchable keyboard picker. Pick a listed ID with arrows and Enter, or type `PROVIDER/MODEL_ID` if discovery is unavailable. Escape cancels the outstanding listing without submitting the draft. A bare ID uses the current provider; everything after the first slash belongs to the model ID. Switching models keeps visible conversation and drops opaque Codex/Gemini state when provider or model changes.

For a remote browser, use `pave --login-manual PROVIDER` for browser-based providers and paste the full callback URL; OpenRouter also accepts the authorization code alone because that provider does not echo state. Standard OAuth providers require the correct callback state. **Copilot uses a device code instead:** `--login-manual github-copilot` is unsupported; enter the displayed code at the displayed GitHub verification URL. `pave --logout PROVIDER` removes a stored credential. The private store is **unencrypted** at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` (0700 directory, 0600 file); OpenRouter's browser exchange stores an API key there. An environment API key takes precedence where available. OAuth refresh is locked across processes; browser/device-derived tokens and keys cannot be sent to a custom `--endpoint`.

`--api NAME` selects an explicit registered wire route; `--endpoint URL` overrides an API-key provider's completion endpoint. OpenAI uses Responses unless `--api chat` explicitly requests Chat Completions. No provider ships a default model ID: first-run setup lists models from the selected provider, saved user/project/session defaults are respected, and noninteractive prompts require `--model ID` or a saved default. Personal Copilot uses only its pinned public Chat transport and requires an account-supported Chat model. `--models` refuses a custom `--endpoint` to avoid sending a private gateway key to a public listing endpoint. Redirected input/output uses a plain line-oriented CLI with the same slash commands instead of the full-screen interface.

On a fresh interactive terminal launch without an explicit provider/model/session or existing configured default, Pave opens a keyboard-operated **SETUP** screen before the normal editor. Choose a provider, select a supported model and confirm the default; OAuth-capable providers offer their real sign-in flow, while API-key providers show the required environment variable without collecting or echoing a key. You can skip setup, including a missing-key step, and return with `/setup`; a skipped key cannot be used until its environment variable is set. Choices persist privately under `${XDG_CONFIG_HOME:-~/.config}/pave/` as user defaults and a versioned setup status. Existing configured defaults, explicit CLI selection, resumed `--session` and noninteractive `--prompt` bypass onboarding. `/settings` remains the project-level editor.

Setup uses the same searchable keyboard picker and asynchronous account/local model discovery as `/model`. After authentication, the chooser lists returned live IDs first; use Up/Down and Enter to select one without typing an ID. If discovery is unsupported or fails, the status says why and you may type a route-compatible `PROVIDER/MODEL_ID` explicitly; its entitlement remains unverified. Available discovery covers API-key OpenAI/Google/Anthropic/DeepSeek/Groq/Mistral/Together/Cerebras/Venice/DeepInfra/Fireworks/Baseten/Hugging Face/NanoGPT; local Ollama, LM Studio, llama.cpp and vLLM; signed-in personal Copilot, account-scoped Codex subscription OAuth and OpenRouter's authenticated user-filtered catalog. Anthropic's OAuth sign-in does not currently list models, but an API key can. Together lists only reported chat models; Venice filters tool-capable text models; Fireworks lists complete serverless/tool-capable resource IDs; Hugging Face and NanoGPT filter tool-capable account listings. Anthropic and Fireworks cursor pages are bounded and malformed pages fail rather than showing partial results. Codex listing reuses inference's locked token refresh and sends its bearer/account ID only to pinned ChatGPT backend routes. OpenRouter prefers a configured environment key; otherwise its stored OAuth-exchanged API key is used only on the pinned `/api/v1/models/user` route. Azure requires an explicitly configured resource and deployment; setup cannot discover its deployments. Resize preserves the active choice; wide terminals show step guidance and narrow terminals stay compact.

The full-screen TUI shows the existing pixel-art Pave mark as colored ASCII art when the normal editor has an empty transcript. It is an empty-transcript placeholder, not a journal entry; the first message replaces it. Conversation roles, Markdown headings/lists/code, tool progress and folded tool results use distinct blocks; `Alt+O` expands the latest visible tool result. Typing `/` immediately shows a small, filtered list of actual commands with descriptions; `/re` narrows it, Up/Down moves, Tab or Enter inserts the selected command **without executing**, and Escape closes the hints without losing the draft. Enter again submits; `/help` lists the same catalog. Small terminals show a compact `PAVE` label, `NO_COLOR=1` removes colored text, and redirected output remains plain text.

Bracketed paste stays in the editor until its closing delimiter, inserts at most the remaining 16 KiB of draft capacity as a single undoable edit, converts pasted Tab to a space and preserves newlines without submitting. `Ctrl+Z` undoes the whole paste; `Ctrl+Y` restores it. Search-query paste is limited to its remaining 512-byte capacity.

The header switches from `Working` to the current tool name during a tool call, then back to model work for the next request. Its elapsed-turn timer advances during slow responses and shell approval without polling while idle; shell commands still require a visible command and a separate `y` decision. Idle measured usage replaces activity and elapsed time only after the turn finishes.

Known provider, authentication and tool failures display their message in a readable TUI error block; incomplete streaming answers are removed instead of staying in the transcript as successful turns. Unknown exceptions retain their diagnostic text.

The idle header shows measured cumulative input/output tokens for the selected journal branch, or the current unsaved conversation. It hides the badge while a turn is active and when usage is unavailable; `/usage` gives provider/model details and the untracked-cost caveat.

Typed settings live in `${XDG_CONFIG_HOME:-~/.config}/pave/settings.json` and `<workspace>/.pave/settings.json`. Supported keys: `default_provider`, `default_model` (requires its provider), `max_turns` (1–100), `disable_shell` (boolean). Explicit CLI flags override project defaults, then user defaults; `disable_shell: true` in **either** scope denies `--allow-shell`. Invalid, duplicate, oversized or symlinked files are reported and skipped. `/settings` edits project defaults with atomic private-file replacement for the **next** launch.

User and ancestor `AGENTS.md` files load below the fixed mobile safety prompt, with bounded relative `@file.md` imports. Workspace `.pave/rules/*.md` files with `---`, `paths: src/**/*.swift`, `---` headers apply only to matching file-tool paths. Before a new scoped rule can affect `write_file` or `edit_file`, the first call is **withheld and journaled as unexecuted**; the rule enters the next model request's system context and the model must retry. Unsafe rule imports or target paths fail closed. Project guidance is not a sandbox, and approved shell commands can modify files outside these scoped operations.

For optional prompt customization, use `<workspace>/.pave/SYSTEM.md`, `SYSTEM_TEMPLATE.md`, or `APPEND_SYSTEM.md`; user-level files under `${XDG_CONFIG_HOME:-~/.config}/pave/` are fallback. In each scope, `SYSTEM.md` wins over `SYSTEM_TEMPLATE.md`; project wins over user. `--system-prompt TEXT` or strict `--system-prompt-template FILE` overrides the discovered custom source; these flags conflict. `--append-system-prompt TEXT` overrides discovered append text. Templates support `{{root}}` for the workspace path; unknown placeholders are errors for explicit files and diagnostics with fallback for discovered files. Sources are bounded UTF-8 regular files, read once at launch. The mobile safety prompt and `AGENTS.md` remain in place regardless of a custom override; no tool output becomes system instructions.

| In the TUI | Action |
| --- | --- |
| `Enter` · `Shift+Enter` | Send a prompt · insert a newline; pasted Enter never submits |
| `←` `→` · `↑` `↓` | Move by Unicode grapheme or wrapped visual row; history at first/last row |
| `Ctrl+P`/`Ctrl+N` · `Ctrl+R` | Explicit older/newer history · incremental reverse search (Enter recalls, Escape cancels) |
| `Ctrl/Alt+←/→` · `Ctrl+W` | Move or delete by word; editor draft stays intact during model output |
| `Ctrl+Z`/`Ctrl+Y` · `Ctrl+K`/`Ctrl+U` · `Alt+Y` | Undo/redo a draft edit · kill after/before the cursor · yank killed text; bracketed paste is one undo step |
| `PgUp`/`PgDn` · `Ctrl+Home`/`Ctrl+End` · `Alt+O` | Scroll the transcript, jump to its beginning/end, or expand/collapse the latest visible tool result |
| `Ctrl+C` · `Ctrl+D` | Clear a nonempty draft; with an empty draft cancel the active turn · exit when empty |
| `/` then `Tab` | Search available slash commands; Enter inserts a choice, Escape returns to the unchanged draft |
| `/login [PROVIDER]` · `/model [PROVIDER/MODEL_ID]` | Search sign-in/model choices (Esc cancels) or select directly |
| `/cancel` · `/settings` | Stop the active request/command; edit typed project defaults for the next launch |
| `/tools [NAME]` | List the tools actually offered to the model, or inspect one tool's description; shell availability follows `--allow-shell` and still requires per-command approval |
| `/context` | Inspect the actual model/route, saved branch and retained conversation count; show only provider-reported input/output tokens from OpenAI Responses, Codex subscription Responses, Anthropic Messages, Google Gemini, Ollama and Chat Completions routes that supply complete usage (the official OpenAI Chat stream explicitly requests it), on the selected ancestry or cumulative ephemeral conversation; other requests, context limit and cost remain untracked |
| `/usage` | Inspect only recorded provider-reported tokens; private journals group the selected branch's input/output totals by model, while ephemeral conversations show the combined measured total without claiming per-model provenance |
| `/retry` | Reissue the last user turn only if it made no tool calls; saved sessions retain the prior answer on an abandoned branch, while ephemeral answers are replaced; both requests may incur usage |
| `/hotkeys` | Display actual interactive keyboard shortcuts (including search, word editing, paste and tool expansion); headless CLI does not claim terminal keys work |
| `/new` · `/resume [PATH]` | Create a private, persistent workspace journal; search recent journals or reopen an explicit workspace journal |
| `/help` · `/entries` | Show descriptive commands · list journal message IDs |
| `/tree` · `/branch ID` · `/fork /path/new.jsonl` | Search recent journal ancestry and select a branch · choose an exact entry ID · copy the selected conversation |
| `/compact` · `/quit` | Summarize older turns manually · exit |

The input remains responsive during network calls and approved commands. Prompts submitted while a turn runs are queued and appear in the transcript only when their own turn begins; `/cancel` stops the active turn without discarding queued prompts or an unsent draft. Transient streamed text from a cancelled or failed turn is removed. In-memory scrollback keeps the latest 10,000 logical rows; a saved journal retains the full durable conversation and `/resume` restores its visible history without replacing the current editor draft. Plain startup conversations are ephemeral; `/new` asks before discarding an unsaved one.

Sessions are private append-only JSONL journals on creation, **not encrypted**. `/new` opts into storage at `${XDG_STATE_HOME:-~/.local/state}/pave/sessions/<SHA-256 of canonical workspace path>/<random>.jsonl` (0700 directories, 0600 files). `/resume` lists up to 100 recent journals from the current workspace only, skipping symlinks, foreign owners and permissive files; an explicit path is subject to the same checks. `--session PATH` continues to support an explicitly chosen journal and shows its restored transcript at startup. Selected provider/model changes are recorded as branch-local metadata, not provider messages; `/resume`, `--session` and `/branch` restore the saved selection and status, while explicit `--provider`, `--model`, `--api` or `--endpoint` flags take precedence. Credentials and custom endpoints are never stored as model metadata; a removed provider/route must be overridden explicitly on reopen. Keep journals outside version control: Gemini 3 native replay may persist model-issued thought text and signatures alongside visible conversation content. Reopening a session marks interrupted tool calls as failed rather than rerunning them. `/compact` preserves the full journal; model summarization may fail if the provider's context limit is exceeded.

The `/tree` picker displays parent-linked entries (including model changes), highlights the active tip and searches both preview text and IDs. It bounds the list to the most recent 1,024 entries; `/branch ID` remains available for older entries. Selecting an entry restores only that branch's visible history and saved model; Escape keeps the current branch and draft.

## Providers

| Provider | Transport | Authentication | CLI selection |
| --- | --- | --- | --- |
| OpenAI | Chat Completions, Responses | `OPENAI_API_KEY` | `--provider openai --model MODEL_ID` (Responses by default; `--api chat` for Chat-only models) |
| OpenAI Codex subscription | account-scoped Codex Responses | `--login openai-codex` (PKCE; refresh) | `--provider openai-codex --model MODEL_ID` |
| Anthropic | Messages | `ANTHROPIC_API_KEY` or `--login anthropic` | `--provider anthropic --model MODEL_ID` |
| Ollama | native `/api/chat` | none (local server) | `--provider ollama --model MODEL_ID` |
| Google Gemini API | native `generateContent` | `GEMINI_API_KEY` | `--provider google --model MODEL_ID` |
| DeepSeek | Chat Completions | `DEEPSEEK_API_KEY` | `--provider deepseek --model MODEL_ID` |
| Groq | Chat Completions | `GROQ_API_KEY` | `--provider groq --model MODEL_ID` |
| Mistral | Chat Completions | `MISTRAL_API_KEY` | `--provider mistral --model MODEL_ID` |
| OpenRouter | Chat Completions | `OPENROUTER_API_KEY` or `--login openrouter` (PKCE exchanges for API key) | `--provider openrouter --model MODEL_ID` |
| Together AI | Chat Completions | `TOGETHER_API_KEY` | `--provider together --model MODEL_ID` |
| Cerebras | Chat Completions | `CEREBRAS_API_KEY` | `--provider cerebras --model MODEL_ID` |
| Venice | Chat Completions | `VENICE_API_KEY` | `--provider venice --model MODEL_ID` |
| DeepInfra | Chat Completions | `DEEPINFRA_API_KEY` | `--provider deepinfra --model MODEL_ID` |
| Fireworks AI | Chat Completions | `FIREWORKS_API_KEY` | `--provider fireworks --model MODEL_ID` |
| Hugging Face Inference | Chat Completions | `HF_TOKEN` | `--provider huggingface --model MODEL_ID` |
| NanoGPT | Chat Completions | `NANO_GPT_API_KEY` | `--provider nanogpt --model MODEL_ID` |
| Azure OpenAI (Responses only) | Azure v1 Responses | `AZURE_OPENAI_API_KEY` + `AZURE_OPENAI_ENDPOINT` | `--provider azure --model DEPLOYMENT_ID` |
| Baseten | Chat Completions | `BASETEN_API_KEY` | `--provider baseten --model MODEL_ID` |
| LM Studio (local) | Chat Completions | optional `LM_STUDIO_API_KEY` | `--provider lm-studio --model MODEL_ID` |
| llama.cpp (local) | Chat Completions | optional `LLAMA_CPP_API_KEY` | `--provider llama.cpp --model MODEL_ID` |
| vLLM (local) | Chat Completions | optional `VLLM_API_KEY` | `--provider vllm --model MODEL_ID` |
| GitHub Copilot (personal github.com) | pinned public Chat Completions endpoint | `--login github-copilot` (device code, `read:user`) | `--provider github-copilot --models` then `--model MODEL_ID` |

Interactive Copilot sessions may select a discovered Chat model from `/model`; one-shot prompts require `--model` or a saved default. The pinned provider endpoint rejects unauthorized model IDs.

Local LM Studio, llama.cpp and vLLM use keyless Chat Completions unless their optional `LM_STUDIO_API_KEY`, `LLAMA_CPP_API_KEY` or `VLLM_API_KEY` is set. The respective `LM_STUDIO_BASE_URL`, `LLAMA_CPP_BASE_URL` and `VLLM_BASE_URL` can select a self-hosted private LAN address; only numeric private/loopback addresses and `localhost` are accepted, and a supplied key on plain HTTP travels unencrypted to that selected host. Defaults use loopback ports 1234, 8080 and 8000. The same validated host serves `/v1/models` and `/v1/chat/completions`; provider model discovery ignores `--endpoint`, disables proxies and does not follow redirects. Configure a trusted HTTPS LAN endpoint when sending a key across a network.

Azure Responses requires `AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com` and `AZURE_OPENAI_API_KEY`; `--model DEPLOYMENT_ID` is the exact deployment name supplied by you, not a bundled model ID. Optionally set `AZURE_OPENAI_API_VERSION=v1` or `preview`; without it the `/openai/v1/responses` default version is used. Pave sends the key only to that exact public-cloud Azure resource Responses path and rejects custom/non-Azure hosts; neither Azure Chat Completions nor Azure deployment discovery nor Microsoft Entra authentication is implemented. It never translates a model name into a deployment ID.

These twenty-two entries have **isolated transport/CLI fixtures**, not live vendor calls; the sibling provider registry currently has 83 distinct provider declarations, so 61 remain unimplemented and Pave does **not** yet provide parity. Together, Cerebras, Venice, DeepInfra, Fireworks, Baseten, Hugging Face and NanoGPT completed authenticated local HTTP tool-call/result turns and pinned listing fixtures. Azure v1 Responses completed a native CLI two-request fixture with its explicit deployment, key header, Responses function call, real workspace `read_file` result and final answer; Azure Chat, Entra ID, sovereign clouds and deployment discovery remain unsupported. Local LM Studio, llama.cpp and vLLM completed HTTP model listing and streamed tool-result turns; a shipped keyless LM Studio setup PTY saved a dynamically listed model. The Codex scenario exercised a real loopback callback, JWT account routing, refresh, SSE tool turns, encrypted reasoning replay through a reopened session and enterprise residency headers; OpenRouter exercised its state-less PKCE exception, key exchange, stored-key inference and logout. Gemini buffered and SSE scenarios exercised signed tool calls, function-result replay and reopened journals; unsigned Gemini tool calls fail closed for every model ID, including older models that may not emit `thoughtSignature`. Copilot's GitHub device-code grant exercised private token storage, pinned Chat tool turns, in-session `/login` and `/model`, and logout with fake HTTPS responses. Auth fixtures intercepted HTTPS with a local subprocess, so public OAuth client registration, live account entitlement, model support and actual vendor responses remain **unverified**. Copilot currently uses only the personal `https://api.githubcopilot.com/chat/completions` route; Enterprise, Responses and Anthropic transports are **not** supported. Other reference auth policies, proprietary gateway transports and full model semantics are still missing. A compatible endpoint does not imply every model feature works.

## Features

| Available | Not yet available |
| --- | --- |
| Six wire payload formats with distinct provider routes; bounded buffered and incremental-stream decoders; model-bound Codex/Gemini native state replay | Most provider-specific thinking/usage/multimodal parity and full model catalog |
| Mobile manifest detection, workspace file read/search/edit/write, bounded agent turns | LSP/DAP, subagents, extensions and full tool catalog |
| Grapheme-aware CJK input, cancellable streaming with queued follow-ups, searchable dynamic model picker, branching sessions and manual compaction | Automatic context budgeting/compaction and full structured session resume |

**Shell safety:** model-requested shell execution is off by default. `--allow-shell` asks for **each** command in an interactive terminal; noninteractive runs deny commands even with the flag. Approved commands are **not sandboxed** and can access files outside the workspace. Check commands before approving them; Pave does not install mobile SDKs, sign apps or deploy to devices for you.

## Contribute

New contributors are welcome—bug reports, TUI polish, provider work and mobile-workspace testing are useful. Start with [open issues](https://github.com/kimmandoo/pave/issues), [the feature plan](TASKS.md) and [design rules](docs/DESIGN_RULES.md).

1. Fork the repository, create a focused branch from `main`, and use the source-install instructions above.
2. Add a behavior-focused regression for a bug. Run `opam exec -- dune runtest --force` and `opam exec -- dune build @install`; check TUI changes in a real terminal or PTY.
3. Update usage/docs and [CHANGELOG.md](CHANGELOG.md) when behavior changes. Open a [pull request](https://github.com/kimmandoo/pave/pulls) with the behavior, checks and platform limitations.

Read [CONTRIBUTING.md](CONTRIBUTING.md) for the full checklist and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for community expectations. Report vulnerabilities **privately** using [SECURITY.md](SECURITY.md), not a public issue. Never attach credentials, private source or session transcripts to reports.

## License

[MIT](LICENSE), with Pave as the copyright holder. Earlier MIT copyright and permission notices and linked-library terms remain in [THIRD_PARTY_NOTICES](THIRD_PARTY_NOTICES); both files accompany release binaries.
