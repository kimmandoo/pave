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
> Pave is under active development. Linux and macOS Intel builds expose 71 built-in provider IDs; macOS arm64 builds expose 72 when the Apple Foundation Models helper is present. These cover 70/83 and 71/83 source identities respectively; separate Kimi Code regional routes account for the extra Pave ID, and 13 or 12 source identities remain unmatched. Seven wire payload formats, account sign-in paths, branching session journal and cancellable terminal have fixture coverage—not blanket live vendor entitlement. LSP/DAP, subagents, plugins and a complete model catalog remain open. See [the feature plan](TASKS.md).

## Install

**macOS 15+** (Apple Silicon or Intel) · **Linux glibc 2.35+** (x86-64 or ARM64). No Windows or musl builds yet. [Releases](https://github.com/kimmandoo/pave/releases) provide native binaries; you do not need opam, a compiler or sudo.

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | sh
```

The [installer](install.sh) checks the downloaded archive against its published SHA-256 manifest. It writes:

- The executable to `~/.local/bin/pave` (add that directory to `PATH` if needed).
- Licenses and a native-install marker to `~/.local/share/licenses/pave`.

Inspect the script first if you prefer not to pipe a download into a shell. Installed binaries can update themselves:

```sh
pave update --check  # Compare embedded release version against GitHub's latest published tag; no files changed.
pave update          # Upgrade to the latest release.
```
A successful upgrade prints the version transition (`Updated Pave v0.1.40 → v0.1.41.`). A same-version install says `Reinstalled`; a checksum or installation failure never reports success.

**Update safeguards**

- The native binary runs its **embedded** checksum-verifying installer, not a newly downloaded script. It resolves the latest GitHub release tag and pins both archive and checksum to that tag, avoiding stale `/latest/download` redirects.
- `--check` changes no files and requires embedded version metadata (available since `v0.1.6`). Unavailable or rate-limited GitHub metadata produces an error rather than a guessed result.
- Updates retain the original installation directory and separate private login store at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json`; they ignore environment `PAVE_VERSION` and `PAVE_INSTALL_DIR` overrides.
- For binaries installed through `v0.1.4`, rerun the installer once to add the native-install marker. Source/opam installs use their package manager instead.

<details>
<summary>Version pinning, custom destination and removal</summary>

```sh
curl -fsSL https://raw.githubusercontent.com/kimmandoo/pave/main/install.sh | PAVE_VERSION=v0.1.41 sh
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

Start `pave` for interactive setup; no credentials are needed just to open the terminal. The default provider reads `OPENAI_API_KEY` only when making a request.

1. Use `/setup` to connect an account or choose a saved default; use `/model` to switch the current conversation.
2. Run `pave --providers` for routes or `pave --provider ID --models` for a fresh, pinned, account-scoped listing. Copy a canonical `provider@route[#account]/EXACT_UPSTREAM_ID` selector from the output directly into `--model`; IDs and display labels are distinct, and model rosters are not cached.
3. Treat `[listed · API unverified]` as an account-listed ID, **not** proof of Chat, tool support or compatible wire routes. Anthropic OAuth cannot list models without `ANTHROPIC_API_KEY`; type an Anthropic model ID instead if needed.

Keep API keys out of checked-in config and session files.

```sh
# Interactive: resize-aware TUI, prompt history and a persistent session.
pave --root /path/to/mobile/repo --session /private/path/pave.jsonl

# One-shot streaming reply; paste an exact selector from `pave --provider openai --models`.
pave --model "$MODEL_SELECTOR" --prompt 'Inspect the Android build failure' --stream

# Anthropic: API key, or explicit browser-based OAuth login for a subscription.
pave --login anthropic
pave --provider anthropic --model "$MODEL_ID" --root /path/to/mobile/repo

# Codex subscription: browser PKCE or device approval, with account-scoped Responses.
pave --login openai-codex
pave --login-device openai-codex
pave --provider openai-codex --model "$CODEX_MODEL" --prompt 'Inspect this project'

# OpenRouter browser exchange yields a durable API key; or set OPENROUTER_API_KEY.
pave --login openrouter
pave --provider openrouter --model "$ROUTER_MODEL" --prompt 'Inspect this project'

# GitHub Copilot personal account: device-code login; public Chat route only.
pave --login github-copilot
pave --provider github-copilot --model "$COPILOT_MODEL" --prompt 'Inspect this project'

# Local Ollama; pull a model with Ollama before invoking Pave.
pave --provider ollama --model "$LOCAL_MODEL" --prompt 'Inspect this project'
# `--context-window auto` reads fresh provider-reported limits for exact listed models; supported sources and route checks are documented below.
pave --provider commandcode --api chat --model "$MODEL_ID" --context-window auto --session /private/path/pave.jsonl
# Google, Codex subscription, Devin, OpenRouter and direct Anthropic API also expose source-backed live context metadata; OpenRouter output caps are reported separately.
pave --provider google --model "$GEMINI_MODEL_ID" --context-window auto
# Other routes require a manually supplied, independently verified limit.
pave --provider openai --api responses --model "$MODEL_ID" --context-window "$CONTEXT_WINDOW" --session /private/path/pave.jsonl
```

### Prompts, attachments and shell completions

For a one-shot turn, provide exactly one source: `--prompt`, nonempty redirected stdin (bounded to 1 MiB), or `--prompt-file`. Prompt-file paths are workspace-relative to `--root`, checked, and bounded UTF-8 text; their contents are prompt data, never parsed as slash commands. Empty or conflicting sources fail before a provider request.

Repeat `--image PATH` for checked workspace-relative images. Pave validates file type, MIME signature and size, then rejects routes without native user-media support before authentication or network access.

In the TUI, type `@` to preview attachable workspace files and directories immediately; typing a filename filters the list, including nested fuzzy matches, while `@dir/` narrows it to that directory. Rows show safe filenames, MIME types and byte sizes without reading media payloads into the preview. Use ↑/↓ to choose, Tab or Enter to insert an exact reference, and Esc to dismiss; selecting a directory continues browsing and selecting a file does not submit the prompt. Quoted paths with spaces remain supported. Completion stays inside the checked workspace, honors ignore and symlink boundaries, and excludes unreadable, oversized, invalid-media or non-UTF-8 text files. On submission, text files become labeled prompt text and supported media is sent as native typed content. Code/email occurrences and unresolved safe references remain literal; unsafe paths or invalid files fail closed. Staged media from `--image PATH` or `/attach PATH` appears above the composer with its sanitized filename, MIME type and size; sent and restored messages show the same metadata without exposing payload bytes. Graphical image display remains opt-in with `--terminal-images` in a verified Kitty/iTerm2 terminal.

`--output jsonl` writes ordered turn, text, tool and outcome records to stdout; diagnostics go to stderr. Completed turns exit 0, provider failures 1, tool failures 2 and cancellation 130. Plain text remains the default. Prose shortcuts are opt-in with `--shortcut NAME` and individually disableable with `--disable-shortcut NAME`; available names are `thinkdeep`, `verifyfirst` and `planfirst`. Pasted text, code, paths and tool output are not expanded. TUI headers use sanitized model display names when available and fall back to the upstream ID; labels never change the exact selector.

`pave completions bash|zsh|fish` prints a script generated from CLI/task option metadata. Model and session candidates come only from authorized local state for the effective `--root` workspace (or the current directory); completion does not contact providers. Session paths containing spaces remain selectable.


### Non-chat model tasks

`pave task` invokes task-specific APIs directly; it does not use or add a Chat route, does not affect `pave --providers`, and never creates or modifies a conversation, session, or route identity. Each task requires the exact `--model` ID you choose. Credentials are read only from `OPENAI_API_KEY` for OpenAI tasks and `COHERE_API_KEY` for rerank. No default model, custom endpoint, OAuth credential, or fallback is used. Model access and entitlement are controlled by the provider account; Pave does not infer task support from a model name.

```sh
# OpenAI embeddings: print the validated vector response as JSON.
pave task embed --model "$EMBEDDING_MODEL" --input 'Text to embed'

# OpenAI image generation: write a new PNG under the workspace.
pave task image --model "$IMAGE_MODEL" --prompt 'A red bicycle by the sea' --output bicycle.png

# OpenAI text-to-speech: write a new WAV file.
pave task speak --model "$TTS_MODEL" --input 'Hello from Pave' --voice alloy --output greeting.wav

# OpenAI audio transcription: source paths are workspace-relative.
pave task transcribe --model "$TRANSCRIPTION_MODEL" --file recordings/meeting.wav

# Cohere v2 rerank: repeat --document for every candidate text.
pave task rerank --model "$RERANK_MODEL" --query 'Which passage describes the API?' \
  --document 'First candidate passage' --document 'Second candidate passage' --top-n 1
```

Task input/output limits are enforced locally: embedding text 256 KiB (OpenAI separately enforces its documented model token limits); image prompts 16 KiB with one 1024×1024 PNG (decoded output at most 16 MiB); speech input at most 4096 UTF-8 characters, using a documented built-in voice and WAV output (at most 20 MiB); transcription files must be nonempty and at most 25,000,000 bytes, with `.wav`, `.mp3`, `.mpga`, `.mpeg`, `.m4a`, `.mp4`, `.webm`, `.flac` or `.ogg`; rerank query at most 16 KiB, up to 1000 documents of at most 32 KiB each and 512 KiB combined, with `--top-n` between 1 and the document count. Output paths must be workspace-relative, use `.png`/`.wav` as appropriate, have existing real directories, and name a file that does not already exist. Writes are atomic; symlinks, traversal, malformed media and oversized responses are rejected. The provider account may enforce lower limits.

The pinned operations and wire formats follow the official references: [OpenAI embeddings](https://platform.openai.com/docs/api-reference/embeddings), [OpenAI image generation](https://platform.openai.com/docs/api-reference/images), [OpenAI text-to-speech](https://platform.openai.com/docs/api-reference/audio/createSpeech), [OpenAI audio transcription](https://platform.openai.com/docs/api-reference/audio/createTranscription), and [Cohere v2 rerank](https://docs.cohere.com/reference/rerank). OpenAI's Videos API and Sora were shut down on 2026-09-24; OpenAI documents no replacement, so Pave does not expose video generation. See the [OpenAI video reference](https://platform.openai.com/docs/api-reference/videos) and [OpenAI deprecation notice](https://developers.openai.com/api/docs/deprecations).

### Provider-specific notes

The [provider table](#providers) lists credentials and CLI flags. Additional route caveats:

- **Moonshot / Ollama Cloud:** Moonshot (`MOONSHOT_API_KEY` or `KIMI_API_KEY`) lists models on its global `/v1/models` and uses Chat. Ollama Cloud (`OLLAMA_CLOUD_API_KEY` or `OLLAMA_API_KEY`) uses fixed hosted `/api/tags` and `/api/chat`; the local Ollama route never receives its key.
- **Bedrock Mantle:** `AWS_BEARER_TOKEN_BEDROCK` with `AWS_REGION` selects a validated regional Responses endpoint and bearer-key model listing, separate from SigV4 Bedrock Converse and ConverseStream. A listed model is not guaranteed to support Responses.
- **xAI / NVIDIA:** `XAI_API_KEY` lists account models at `/v1/language-models`; native Chat preserves reported `reasoning_content` on tool replay. `NVIDIA_API_KEY` lists `/v1/models`, but listings omit per-model tool support and hosted schemas vary; Pave does not infer compatibility from names. Both buffer validated completions before displaying text, not incremental SSE.
- **Novita / SiliconFlow:** Novita (`NOVITA_API_KEY`) uses a fixed hosted Chat endpoint, dynamic `/models` and unchanged interleaved reasoning replay. SiliconFlow uses separate global `SILICONFLOW_API_KEY` and China `SILICONFLOW_CN_API_KEY` hosts, text/chat-filtered listings and native Chat; credentials never cross regions. Some models need a vendor-specific thinking switch for tools: no default switch or model-name heuristic is bundled. These routes buffer validated completions.
- **StepFun:** `STEPFUN_API_KEY` is sent only to `api.stepfun.ai`, not the separate `.com` platform. Its `/v1/models` mixes Chat and audio without capability flags; `--models` calls IDs unclassified and `/model` marks them `[listed · API unverified]`. Select only a known compatible ID. Complete StepFun tool calls may end with its documented `finish_reason: "stop"`; other Chat routes retain strict finish validation.
- **Apple Foundation Models:** on macOS 26+ Apple silicon with Apple Intelligence enabled, `apple` uses the OS-managed `default` ID. It is text-only, has no model listing or Pave tools, and is advertised only when its native helper is installed or packaged.
- **Cursor / agent-runtime boundaries:** Cursor's official SDK bridge exposes a model-listing RPC and separate agent-runtime APIs, not a raw completion route; Pave does not present it as generic Chat. GitLab Duo Agent is likewise distinct from the implemented GitLab Duo direct inference APIs and remains unsupported.
- **Copilot:** the supported route is personal `github.com` Chat only. No documented personal Responses/Anthropic route or GitHub Enterprise route is claimed.
- **CoreWeave Serverless:** W&B Inference's fixed host accepts `COREWEAVE_API_KEY` or `WANDB_API_KEY` for native Chat and account `/v1/models`. A CoreWeave control-plane credential is not interchangeable.
- **Alibaba Coding Plan / Token Plan:** `ALIBABA_CODING_PLAN_API_KEY` is the Coding Plan subscription key; select `--api china` or `--api intl` for the matching pinned host and an explicitly supported model. `ALIBABA_TOKEN_PLAN_API_KEY` is separate and reaches only its pinned Beijing Token Plan route. Both plans lack a Pave account-model listing; both may use the `sk-sp-` prefix, but their keys and hosts are not interchangeable. See [Coding Plan](https://help.aliyun.com/en/model-studio/coding-plan-faq) and [Token Plan](https://help.aliyun.com/en/model-studio/token-plan-personal-quick-start).
- **Xiaomi MiMo Token Plan:** obtain the subscription key from the Token Plan console and use the matching provider/region: `xiaomi-token-plan-ams` with `XIAOMI_TOKEN_PLAN_AMS_API_KEY`, `xiaomi-token-plan-cn` with `XIAOMI_TOKEN_PLAN_CN_API_KEY`, or `xiaomi-token-plan-sgp` with `XIAOMI_TOKEN_PLAN_SGP_API_KEY`. These `tp-`/`ttp-` keys are not the pay-as-you-go `XIAOMI_API_KEY`; the Token Plan has no documented model-list endpoint, so select a known supported ID. [Official quick access](https://mimo.mi.com/docs/en-US/tokenplan/Token%20Plan/quick-access).
- **Cloudflare AI Gateway:** configure `CLOUDFLARE_ACCOUNT_ID`, `CLOUDFLARE_GATEWAY_ID` and `CLOUDFLARE_AI_GATEWAY_API_KEY` for that gateway. Configure upstream BYOK credentials or Unified Billing in Cloudflare separately; Pave sends the gateway token only as `cf-aig-authorization`, never as an upstream provider key. Choose a provider-qualified model ID or configured `dynamic/ROUTE`; Pave has no authoritative gateway catalog. [Cloudflare API guide](https://developers.cloudflare.com/ai-gateway/usage/chat-completion/).

These routes passed isolated fake-HTTPS/native workspace tool-result scenarios; no live vendor account entitlement or per-model tool support was verified.

### Accounts and model selection

| Command | Scope |
| --- | --- |
| `/setup` → **Connect account only** | Sign in without changing the current model or saved default; optionally pick a model afterward. |
| `/setup` → **Choose user default** | Guided provider → access → API/model selection, saved for later launches. |
| `/model` | Search models across connected providers, switch the current conversation, and remember the exact model for this workspace on the next interactive launch. |
| `/settings` | Edit project defaults; the workspace's last interactive model choice takes precedence on subsequent launches unless an explicit CLI or session model is selected. |

API-key providers read environment variables, never keys typed into the TUI. Browser URLs and device codes appear on the regular terminal while the full-screen UI is suspended; the transcript and draft return afterward.

In `/model`, use arrows and Enter to choose a discovered ID, or type a canonical `PROVIDER@API[#ACCOUNT]/EXACT_MODEL_ID` selector. Tab browses each provider's loading/ready/unsupported/failure status while successful results stay selectable; unclassified IDs may not support Chat or tools. A bare ID keeps the current provider and API. Saved sessions restore their route; switching models keeps conversation text but retains signed provider state **only** when provider, account, model and API all match. Escape cancels discovery and preserves the draft.

Long model labels are abbreviated only to fit the terminal; the picker still searches and selects the complete ID. Recent selections are stored in private workspace-scoped state under `${XDG_STATE_HOME:-~/.local/state}/pave/sessions/`, separate from conversation journals and user/project defaults. An explicit `--provider`, `--model`, `--api`, `--endpoint`, `--account`, or `--session` overrides the recent choice. Release binaries show their embedded version on the interactive launch screen; a source build shows `source`.

**Sign-in details**

- Remote browser: `pave --login-manual PROVIDER` accepts a full callback URL; OpenRouter also accepts its authorization code alone. GitLab Duo, Devin, Anthropic and browser-based Codex require matching callback state.
- GitLab Duo requires a user-registered `GITLAB_CLIENT_ID` and matching loopback `GITLAB_REDIRECT_URI`; choose its upstream model and API manually. Devin uses its pinned CLI authorization and account-scoped Connect model roster.
- GitHub Copilot and Kilo use device approval: `pave --login PROVIDER`, then open the shown verification URL and enter its code. `--login-manual` is not supported for either; Kilo's public models do not certify Chat/tool support.
- OpenAI Codex also supports `pave --login-device openai-codex`: it prints the fixed verification URL and code, then polls the official device-approval endpoints for up to 15 minutes. The authorization code is exchanged through the same pinned OAuth token endpoint; the stored grant remains bound to the same Codex account and Responses route, with locked refresh. No loopback browser callback is opened.
- `pave --logout PROVIDER` removes every saved sign-in for that provider; add `--account "$ID"` to remove only one. The private store at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` is **unencrypted** (0700 directory, 0600 file); OpenRouter's browser exchange stores an API key. Environment keys take precedence where available. Browser/device-derived credentials cannot be sent to a custom `--endpoint`.
- Google Vertex and Bedrock Converse/ConverseStream use scoped Google ADC or the AWS credential chain; Azure public-cloud routes use a resource API key or the current Azure CLI Entra identity. Apple Foundation Models is local and uses no provider credential.

**Account IDs and local secret masking**

- OAuth sign-ins are stored per provider and selected independently. When a provider returns an account ID, that ID scopes its models. If it returns none, Pave prints and stores a random `pave-local:<hex>` sign-in ID; this is a Pave-local selector, not a provider identity. For headless inference with multiple saved grants, use `--account "$ID"` or an account-scoped model selector. `pave --logout PROVIDER --account "$ID"` removes only that sign-in; `pave --logout PROVIDER` removes every saved sign-in for the provider.

On launch, an exact account in the selected model (including a saved session or workspace's last interactive choice) routes to that grant even when others are saved. A single saved sign-in is selected automatically. If the active model is unscoped and several grants remain, the TUI asks which account should receive the first prompt; cancelling keeps the draft, and the choice is remembered for that workspace and saved session. Headless `--prompt` cannot ask: use `--account "$ID"` or `--model 'PROVIDER@API#ACCOUNT/MODEL'` instead. Pave never guesses between multiple grants by their order or by a shared model ID.
- Environment API keys take precedence over saved OAuth grants where a provider supports both; the TUI shows the precedence. While an environment key is active, an explicit `--account` or account-scoped model selector is rejected rather than silently relabeled; unscoped models use the environment key. The local OAuth file remains **unencrypted** and private (0700 directory, 0600 file); masking does not encrypt it.
- Opt in with `--mask-secrets` to replace the active provider API key and OAuth access token when they occur in local conversation text, saved tool arguments and tool-result text. Tool arguments use collision-safe reversible placeholders for execution; UI/event output uses one-way redaction. This is exact-value matching, not a guarantee against fragments, transformed/derived values or unknown secrets. Images and opaque provider state are not traversed, ambient cloud credentials are not known to the mask, and historical journal entries are not rewritten. Masking is local and does not control provider-side logging or retention.
- No deployable remote authentication broker and trust model are available in this repository; credentials remain in the private local store.

**Routes and network boundaries**

- `--api NAME` selects a registered wire route. OpenAI defaults to Responses; `--api chat` selects Chat Completions.
- `--endpoint URL` works only for routes allowing custom hosts. Bound bearer/ADC/SigV4 routes—including xAI, NVIDIA, Ollama Cloud and Bedrock Mantle—reject untrusted overrides; `--models` rejects custom endpoints so private gateway keys never reach a public listing.
- Completion requests require HTTPS except for loopback or explicitly validated local engines. Credentialed `--endpoint http://remote-host` fails before a request; loopback HTTP remains available for development.
- No remote provider bundles a model ID. Apple Foundation Models uses the OS-managed local ID `default`; other noninteractive prompts need `--model ID` or a saved selection. First-run setup can query supported listings or accept a known manual ID. Personal Copilot needs an account-supported Chat model on its pinned public route. Redirected I/O uses a plain line-oriented CLI with the same slash commands, not the full-screen interface.

### First run

Without an explicit provider/model/session or configured default, an interactive launch opens keyboard-operated **SETUP** before the editor:

1. Pick a provider and access method. OAuth-capable providers offer real sign-in; API-key providers show the environment variable without collecting or echoing its value.
2. Choose an account-listed model or enter a known route-compatible ID, then confirm the default; or skip—including a missing-key step—and return later with `/setup`. Apple Foundation Models uses `default` without a listing. A skipped key is unusable until its environment variable is set.

User defaults and the versioned setup state are private under `${XDG_CONFIG_HOME:-~/.config}/pave/`. Configured defaults, explicit CLI choices, resumed `--session` and noninteractive `--prompt` bypass onboarding. `/settings` edits project defaults.

The searchable setup picker queries the chosen provider asynchronously. Use Up/Down and Enter; `[listed · API unverified]` does not certify Chat or tools. On listing failure, type a route-compatible `PROVIDER/MODEL_ID`; for Apple Foundation Models the ID is `default`. Resize keeps the active choice.

- **Sign-in listings:** Devin's native roster and signed-in Codex, Copilot and OpenRouter are account-scoped. Anthropic OAuth needs `ANTHROPIC_API_KEY` to list models; GitLab Duo has no authoritative non-agentic upstream-model listing and needs an explicit model/API.
- **Key-backed listings:** OpenAI, Google, DeepSeek, Groq, Mistral, Together, Cerebras, Venice, DeepInfra, Fireworks, Baseten, Hugging Face, NanoGPT, AIML API, ai&, Sakana, Abliteration, GMI Cloud, Moonshot, Ollama Cloud, xAI, NVIDIA, Novita, SiliconFlow, CoreWeave, StepFun, local engines and other registered routes. Listings never prove invocation/tool entitlement unless that metadata is supplied. Bounded cursor pages fail closed instead of showing incomplete IDs.

### Terminal experience

- **Conversation:** Pixel-art Pave appears only in an empty transcript; the first message replaces it. Roles, Markdown, tool progress and folded results use distinct blocks. `Option+O` on macOS or `Alt+O` elsewhere toggles the latest visible tool result.
- **Navigation:** Type `/` for filtered slash-command hints (`/re` narrows them). Up/Down selects; Tab inserts. Return/Enter runs an exact command, or inserts a partial match that still needs a second Return/Enter to submit; Escape keeps the draft. Mouse-wheel scrolling moves the transcript without changing the draft. `/help` shows the catalog.
- **Status:** The model row identifies saved versus unsaved sessions; activity switches from `Working` to the running tool and back. Elapsed time advances through slow responses and shell approval without idle polling. Idle usage shows measured branch/conversation input and output tokens when available; `/usage` details provider/account/model/route provenance and reported cache/reasoning counts, never an estimated price.
- **Streaming:** Routes that deliver incremental text pace stream-driven repaints at 60 Hz; a short chunk remains visible by the next frame even if the provider pauses. Completion is shown immediately. Routes that return only a buffered response cannot show text before the provider delivers it.
- **Failures:** Provider, auth and tool errors appear as readable error blocks. Failed or cancelled streaming text is removed; unknown exceptions retain diagnostic text. Approving a visible shell command still requires a separate `y`.
- **Display:** Pickers highlight the active row and keep provider-list errors visible. Narrow terminals show compact `PAVE`; `NO_COLOR=1` removes colors. Redirected I/O uses the plain CLI.
- **Paste:** Bracketed paste waits for its closing delimiter, inserts up to the remaining 16 KiB draft capacity as one undoable edit and converts Tab to space. Newlines do not submit. `Ctrl+Z` undoes the whole paste; `Ctrl+Y` restores it. Search-query paste has a separate 512-byte limit.

### Configuration and safety

Settings live in `${XDG_CONFIG_HOME:-~/.config}/pave/settings.json` (user) and `<workspace>/.pave/settings.json` (project):

| Key | Meaning |
| --- | --- |
| `default_provider`, `default_model`, `default_api` | The model requires its provider; the API must be a registered route for that provider. |
| `max_turns` | Integer from 1 to 100. |
| `disable_shell` | `true` in **either** scope denies `--allow-shell`. |

Explicit CLI flags override session choices, then project and user defaults. A session restores its saved API with its model. Invalid, duplicate, oversized or symlinked settings are reported and skipped; `/settings` atomically replaces private project settings for the **next** launch.

**Tool approvals:** `--approval-mode` overrides the configured mode for one run. The default is `write`: reads and workspace writes are allowed, while execution needs approval. `always-ask` prompts for writes and execution; `yolo` allows ordinary tool tiers. `tools.approval` can set a tool to `allow`, `prompt` or `deny`; deny wins across user/project settings. `/settings` edits the project mode and per-tool overrides for the next launch.

```json
{
  "tools": {
    "approvalMode": "write",
    "approval": {"write_file": "prompt", "run_command": "deny"},
    "commandPatterns": [
      {"match": "rm -rf *", "approval": "deny"},
      {"match": "git status *", "approval": "allow"}
    ]
  }
}
```

Command patterns apply to shell arguments and use `*` for wildcard matching: deny takes precedence over allow, allow matches only a single simple command, and compound commands are checked segment by segment. No mode or allow policy skips Pave's existing prompt for each shell command. Prompt-required actions are denied when no interactive approval surface is available; previews show the tool, tier, impact and arguments, and identify shell execution as unsandboxed.

**Project instructions:** User and ancestor `AGENTS.md` files load below the fixed mobile safety prompt, with bounded relative `@file.md` imports. Workspace `.pave/rules/*.md` scopes paths with frontmatter such as `---`, `paths: src/**/*.swift`, `---`. The first `write_file`/`edit_file` affected by a new rule is **withheld and journaled as unexecuted**; the rule enters the next model request and the model must retry. Unsafe imports/paths fail closed. Project instructions are not a sandbox; approved shell commands can modify files outside these scoped operations.

**Prompt customization:** Place `SYSTEM.md`, `SYSTEM_TEMPLATE.md` or `APPEND_SYSTEM.md` in `<workspace>/.pave/`, with `${XDG_CONFIG_HOME:-~/.config}/pave/` as fallback. `SYSTEM.md` wins over `SYSTEM_TEMPLATE.md` within a scope; project wins over user. `--system-prompt TEXT` and strict `--system-prompt-template FILE` override discovered system content but conflict with each other; `--append-system-prompt TEXT` overrides discovered append content. Templates support `{{root}}`; unknown placeholders fail for explicit files and are diagnosed with fallback for discovered files. Sources are bounded UTF-8 regular files read once at launch. Neither customization nor tool output replaces the mobile safety prompt or `AGENTS.md`.

### Keyboard and slash-command reference

| In the TUI | Action |
| --- | --- |
| `Return` (macOS) · `Enter` (other supported terminals) | Send a prompt; during a turn, interrupt it and steer with that prompt. Pasted Enter never submits. |
| `Option+Return` (macOS) · `Alt+Enter` (other supported terminals) · `/queue MESSAGE` | Queue a follow-up without interrupting the active turn. The slash command also works when the terminal cannot encode modified Enter. |
| `Option+↑` (macOS) · `Alt+↑` (other supported terminals) | Restore the most recent queued prompt into the editor; otherwise navigate prompt history. |
| `←` `→` · `↑` `↓` | Move by Unicode grapheme or wrapped visual row; history at first/last row |
| `Ctrl+P`/`Ctrl+N` · `Ctrl+R` | Explicit older/newer history · incremental reverse search (Return recalls on macOS; Enter elsewhere, Escape cancels) |
| `Ctrl/Option+←/→` (macOS) · `Ctrl/Alt+←/→` (other terminals) · `Ctrl+W` | Move or delete by word; editor draft stays intact during model output |
| `Ctrl+Z`/`Ctrl+Y` · `Ctrl+K`/`Ctrl+U` · `Option+Y` (macOS) / `Alt+Y` (other terminals) | Undo/redo a draft edit · kill after/before the cursor · yank killed text; bracketed paste is one undo step |
| `PgUp`/`PgDn` · mouse wheel · `Ctrl+Home`/`Ctrl+End` · `Option+O` (macOS) / `Alt+O` (other terminals) | Scroll the transcript, jump to its beginning/end, or expand/collapse the latest visible tool result |
| `Ctrl+C` · `Ctrl+D` | Close a picker or cancel account sign-in; in the composer, interrupt a turn without losing the draft or clear a nonempty idle draft; `Ctrl+D` exits when empty. |
| `/` then `Tab` | Search available slash commands; Return/Enter inserts a partial match or runs an exact command; Escape returns to the draft |
| `/setup` · `/model [PROVIDER[@API]/MODEL_ID]` | Connect an account without changing defaults, or configure the user default · choose the active conversation model/API across connected providers |
| `/cancel` · `/settings` | Stop the active request/command; edit typed project defaults for the next launch |
| `/queue MESSAGE` | Queue a follow-up while a turn is active; when idle, send it immediately. |
| `/tools [NAME]` | List the tools actually offered to the model, or inspect one tool's description; shell availability follows `--allow-shell` and still requires per-command approval |
| `/context` | Inspect the actual model/route and selected branch; show provider-reported aggregate and modality usage and, only when explicitly configured, the context-window byte proxy. Limits are never inferred from model names; media payload bytes are counted but modality token cost remains unknown |
| `/usage` | Inspect recorded provider-reported token counts and cache/reasoning/modality details; private journals group the selected branch by provider/account/model/API route, while ephemeral conversations show only a combined measured total. Prices and subscription value are not estimated |
| `/retry` | Reissue the last user turn only if it made no tool calls; saved sessions retain the prior answer on an abandoned branch, while ephemeral answers are replaced; both requests may incur usage |
| `/hotkeys` | Display actual interactive keyboard shortcuts (including search, word editing, paste and tool expansion); headless CLI does not claim terminal keys work |
| `/new` · `/resume [ID|TITLE|PATH]` | Create a private workspace journal; list, search by title/ID, or reopen a same-workspace private journal |
| `/clear` · `/fresh` | Reset model context while preserving journal history/settings · rebuild the local agent from current context without changing the journal |
| `/rename TITLE` · `/label [TEXT]` · `/pin` | Save journal title/entry labels · toggle a journal pin in the private recent-session index |
| `/approval [MODE]` · `/thinking [LEVEL|default]` · `/tool enable|disable NAME` | Persist branch-local approval, thinking metadata and tool availability; thinking metadata does not override provider-specific controls |
| `/attach PATH|clear` | Stage workspace-relative PNG/JPEG/WebP images or WAV/MP3/AAC/OGG/Opus/FLAC/M4A audio and MP4/WebM video for the next prompt (up to 8, 7 MiB per file, 10 MiB combined encoded data); audio/video require direct Gemini or Vertex GenerateContent |
| `/help` · `/entries` | Show descriptive commands · list journal entry IDs and metadata |
| `/tree` · `/branch ID` · `/fork [PATH]` | Search/select parent-linked entries · check out an exact entry ID · fork into a private journal or an explicit new file |
| `/compact` · `/quit` | Summarize older turns manually · exit |

On macOS, `/help` labels Meta as `Option` and Enter as `Return`; other supported terminals show `Alt` and `Enter`. Configure Option to send Escape/Meta in the terminal to use modified shortcuts. `/queue MESSAGE` remains available when it is not configured.


### Sessions and long-running turns

- **During a turn:** Network and approved commands leave the editor responsive. Later prompts queue until their turn starts; `/cancel` stops the active turn without dropping queued prompts or the draft. Failed/cancelled partial text is removed.
- **Scrollback:** Memory retains the newest 10,000 logical rows; a saved journal retains its complete durable history. `/resume` restores that history without replacing the editor draft. New startup conversations are ephemeral; `/new` confirms before discarding an unsaved conversation or staged media attachments.
- **Private journals:** `/new` creates an **unencrypted** append-only JSONL file under `${XDG_STATE_HOME:-~/.local/state}/pave/sessions/<SHA-256 of canonical workspace path>/<random>.jsonl` (0700 directories, 0600 files). `/resume` searches at most 100 recent journals for the current workspace and accepts only private owned regular files, including explicit paths. `/pin` appends a journal metadata event and updates the private recent-list index. `--session PATH` reopens a chosen journal.
- **Context and metadata:** Model/API, approval mode, tool availability, thinking-level metadata and entry labels are typed journal entries, never provider messages. Model/API, approval, tool, thinking and label state follows the selected branch; titles and pins are session-wide. `/thinking` records metadata only and leaves provider/model defaults unchanged. `/clear` appends a reset boundary, preserves earlier journal history and settings, and refuses while tool calls remain unresolved. `/fresh` rebuilds the local agent on the next prompt without writing to the journal.
- **Media and privacy:** Attachments are base64 data stored with the user journal entry, separate from its provider-message record; journals are unencrypted and may contain sensitive image/audio/video data. Pave displays media names/placeholders, never base64. Image-capable routes receive native image fields; direct Gemini and Vertex `GenerateContent` send supported audio/video as `inlineData`; every other route rejects audio/video before authentication or network I/O.
- **Branch metadata:** Provider/model/API changes belong to branches, not provider messages. `/resume`, `--session` and `/branch` restore selected branch settings; explicit `--provider`, `--model`, `--api` or `--endpoint` wins. Credentials and custom endpoints are not stored as model metadata, and a removed route must be overridden on reopen.
- **Recovery and privacy:** Reopening marks interrupted tool calls failed instead of rerunning them. Keep journals out of version control: Gemini 3 replay can persist model-issued thought text and signatures. `/compact` and automatic compaction append branch-local markers; the complete original journal remains intact. Matching signed provider state is retained only on its exact route/model. OpenAI Responses replays opaque route/model-bound compaction state; Anthropic replays signed content only for explicitly capable models on the official API-key route. Other matching signed prefixes fail closed rather than being generically summarized.
- **Tree picker:** `/tree` searches at most 1,024 parent-linked entries by their sanitized previews and IDs, highlights the active tip and keeps older ancestry reachable through `/branch ID`. `/branch` and `/fork` use the selected branch's durable messages and metadata; canceled selection leaves the branch and draft unchanged.

### Context budgeting

Set `--context-window TOKENS` only for the exact initial provider, model, API route and endpoint. Pave never infers limits from model names, and changing provider/model/API/endpoint disables that configured budget. `--context-window auto` fetches fresh pinned model metadata for Command Code (`context_length` plus the exact advertised API route), Google (`inputTokenLimit`), account-scoped OpenAI Codex (`context_window`), Devin (`maxTokens`), OpenRouter's authenticated `/models/user` (`context_length`, with optional `top_provider.max_completion_tokens` kept as output metadata), or Anthropic (`max_input_tokens` from its direct API listing and sole registered Messages route). OpenRouter fields follow its [official model-list reference](https://openrouter.ai/docs/api/api-reference/models/list-all-models-and-their-properties). The reported OpenRouter output maximum caps the heuristic output reserve when it is lower; it is not the context window or a local tokenizer. Anthropic signed compaction additionally requires both listed compaction capabilities and the official API-key endpoint. Nonregistered endpoint overrides, missing limits and unsupported values fail closed. Devin `tokenizerType` is provider-reported display metadata only, not a local tokenizer. `/models`, `/model` and setup pickers show provider-reported context, tokenizer and compaction metadata where available. `/context` shows the token window, output reserve and conservative UTF-8 request-byte proxy separately; the proxy is not a tokenizer or reported token count. Image, audio and video payload bytes contribute to the proxy, while modality token costs remain unknown.

Before each request, Pave trims oversized text tool results only in the provider-facing copy, then summarizes older complete turns in bounded chunks when needed. It keeps the newest user turn and its attachments, preserves tool-call/result adjacency, and appends a branch-local compaction marker only after every summary succeeds; the append-only journal and full tool outputs remain unchanged.

The direct OpenAI API-key Responses route calls `/responses/compact` and replays opaque returned items only for the matching provider/route/model. The direct Anthropic API-key Messages route uses the compaction beta only when the exact live model listing advertises both required capabilities; it stores and replays the signed block first in the message list with the same system prompt and tools. Custom endpoints never receive the signature or beta header. Other matching signed state fails closed rather than being generically summarized. See [compaction behavior and limits](docs/compaction.md).

Without an explicit window, `/compact` has no local byte-proxy preflight; the legacy generic path uses one unbounded summary request, while native OpenAI Responses or capability-advertised Anthropic compaction may be rejected by the provider if its input is too large. A prompt/system/tool schema that already exceeds the byte allowance cannot be compacted without an older safe turn.


## Providers

Use `pave --providers` for the live list. This reference separates wire transport, credentials and model selection; **a listed model is not proof of account access or tool support**.

<details>
<summary>Show provider routes, credentials and CLI examples</summary>

| Provider | Transport | Authentication | CLI selection |
| --- | --- | --- | --- |
| OpenAI | Chat Completions, Responses | `OPENAI_API_KEY` | `--provider openai --model MODEL_ID` (Responses by default; `--api chat` for Chat-only models) |
| OpenAI Codex subscription | account-scoped Codex Responses | `--login openai-codex` (browser PKCE) or `--login-device openai-codex` (device approval) | `--provider openai-codex --model MODEL_ID` |
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
| [Fireworks AI](https://docs.fireworks.ai/guides/reasoning) | Chat Completions; account models filtered for serverless/tool support; IDs do not prove entitlement | `FIREWORKS_API_KEY` | `--provider fireworks --model MODEL_ID`; `/thinking` sends supported effort and preserves reasoning across tool results |
| Hugging Face Inference | Chat Completions | `HF_TOKEN` | `--provider huggingface --model MODEL_ID` |
| NanoGPT | Chat Completions | `NANO_GPT_API_KEY` | `--provider nanogpt --model MODEL_ID` |
| Azure OpenAI | Azure v1 Responses and Chat | Azure API key or Azure CLI Entra identity | `--provider azure --model DEPLOYMENT_ID` (Responses by default; `--api chat` for Chat) |
| AIML API | Chat Completions | `AIMLAPI_API_KEY` | `--provider aimlapi --model MODEL_ID` |
| ai& | Chat Completions | `AIAND_API_KEY` | `--provider aiand --model MODEL_ID` |
| Sakana AI | Responses | `SAKANA_API_KEY` or `FUGU_API_KEY` | `--provider sakana --model MODEL_ID` |
| Abliteration AI | Responses (default), Chat Completions | `ABLITERATION_API_KEY` or `ABLIT_KEY` | `--provider abliteration --model MODEL_ID` |
| GMI Cloud | Chat Completions | `GMI_API_KEY` | `--provider gmi-cloud --model MODEL_ID` |
| Google Vertex AI | Gemini GenerateContent streaming and Anthropic Messages | Google ADC, gcloud impersonation or explicit access token | `--provider google-vertex --model MODEL_ID` (`--api messages` for Claude) |
| Amazon Bedrock | SigV4 Converse and ConverseStream | AWS credential chain | `--provider amazon-bedrock --model MODEL_ID` |
| Baseten | Chat Completions | `BASETEN_API_KEY` | `--provider baseten --model MODEL_ID` |
| LM Studio (local) | Chat Completions | optional `LM_STUDIO_API_KEY` | `--provider lm-studio --model MODEL_ID` |
| llama.cpp (local) | Chat Completions | optional `LLAMA_CPP_API_KEY` | `--provider llama.cpp --model MODEL_ID` |
| vLLM (local) | Chat Completions | optional `VLLM_API_KEY` | `--provider vllm --model MODEL_ID` |
| Apple Foundation Models (macOS arm64) | on-device Foundation Models text | macOS 26+, Apple silicon, Apple Intelligence; no provider key | `--provider apple --model default` |
| GitHub Copilot (personal github.com) | pinned public Chat Completions endpoint | `--login github-copilot` (device code, `read:user`) | `--provider github-copilot --models` then `--model MODEL_ID` |
| Moonshot AI (global) | Chat Completions | `MOONSHOT_API_KEY` or `KIMI_API_KEY` | `--provider moonshot --model MODEL_ID` |
| Ollama Cloud | native hosted Chat | `OLLAMA_CLOUD_API_KEY` | `--provider ollama-cloud --model MODEL_ID` |
| Bedrock Mantle | regional Responses | `AWS_BEARER_TOKEN_BEDROCK` + AWS region | `--provider bedrock-mantle --model MODEL_ID` |
| xAI | native Chat | `XAI_API_KEY` | `--provider xai --model MODEL_ID` |
| NVIDIA hosted NIM | native Chat | `NVIDIA_API_KEY` | `--provider nvidia --model MODEL_ID` |
| Novita AI | native Chat | `NOVITA_API_KEY` | `--provider novita --model MODEL_ID` |
| SiliconFlow global | native Chat | `SILICONFLOW_API_KEY` | `--provider siliconflow --model MODEL_ID` |
| SiliconFlow China | native regional Chat | `SILICONFLOW_CN_API_KEY` | `--provider siliconflow-cn --model MODEL_ID` |
| StepFun international | native Chat; model IDs unclassified | `STEPFUN_API_KEY` | `--provider stepfun --model KNOWN_CHAT_ID` |
| CoreWeave Serverless (W&B Inference) | native Chat | `COREWEAVE_API_KEY` or `WANDB_API_KEY` | `--provider coreweave --model MODEL_ID` |
| Synthetic | native OpenAI-host Chat; model IDs unclassified | `SYNTHETIC_API_KEY` | `--provider synthetic --model KNOWN_CHAT_ID` |
| Z.AI standard API | native Chat, no documented model listing | `ZAI_API_KEY` | `--provider zai --model KNOWN_CHAT_ID` |
| ZenMux | native Chat; model IDs unclassified | `ZENMUX_API_KEY` | `--provider zenmux --model KNOWN_CHAT_ID` |
| Wafer Serverless | native Chat; model IDs unclassified | `WAFER_SERVERLESS_API_KEY` | `--provider wafer-serverless --model KNOWN_CHAT_ID` |
| Baidu Qianfan V2 | native Chat; only `type=chat` IDs listed | `QIANFAN_API_KEY` | `--provider qianfan --model MODEL_ID` |
| Xiaomi MiMo pay-as-you-go | native Chat; model IDs unclassified | `XIAOMI_API_KEY` | `--provider xiaomi --model KNOWN_CHAT_ID` |
| Kilo | native Chat; public model IDs unclassified | `KILO_API_KEY` or `--login kilo` (device approval) | `--provider kilo --model KNOWN_CHAT_ID` |
| [Alibaba Coding Plan](https://help.aliyun.com/en/model-studio/coding-plan-faq) | Region-pinned subscription Chat; no account model listing | `ALIBABA_CODING_PLAN_API_KEY` (`sk-sp-`) | `--provider alibaba-coding-plan --api china or intl --model KNOWN_MODEL_ID` |
| SingularityAPI universal | native Chat; authenticated model IDs unclassified | `SINGULARITYAPI_DEV_API_KEY` | `--provider singularityapi-dev --model KNOWN_CHAT_ID` |
| SingularityAPI reserved | pinned Chat; live account entitlement unverified | `SINGULARITYAPI_TECH_API_KEY` | `--provider singularityapi-tech --model KNOWN_CHAT_ID` |
| OpenCode Zen / Go | distinct pinned Responses / Chat; public model IDs unclassified | `OPENCODE_API_KEY` | `--provider opencode-zen|opencode-go --model KNOWN_MODEL_ID` |
| Charm Hyper | native Chat; keyless public model IDs unclassified | `CHARM_HYPER_API_KEY` or `HYPER_API_KEY` (`sk-hyper-`) | `--provider charm-hyper --model KNOWN_CHAT_ID` |
| Fire Pass | native Chat; manually supplied full router resource | `FIREPASS_API_KEY` (`fpk_`) | `--provider firepass --model accounts/fireworks/routers/ROUTER_ID` |
| Yolo Auto | native Chat; authenticated model IDs unclassified | `YOLO_AUTO_API_KEY` | `--provider yolo-auto --model KNOWN_CHAT_ID` |
| Xiaomi MiMo Token Plan (AMS / CN / SGP) | three independently pinned subscription Chat regions | `XIAOMI_TOKEN_PLAN_AMS_API_KEY`, `_CN_API_KEY`, `_SGP_API_KEY` respectively (`tp-`/`ttp-`) | `--provider xiaomi-token-plan-ams|xiaomi-token-plan-cn|xiaomi-token-plan-sgp --model KNOWN_CHAT_ID` |
| MiniMax Coding Plan (international / China) | independently pinned subscription Chat regions | `MINIMAX_CODE_API_KEY` / `MINIMAX_CODE_CN_API_KEY` respectively (`sk-cp-`) | `--provider minimax-code|minimax-code-cn --model KNOWN_CHAT_ID` |
| Meta Model API | pinned stateless Responses; authenticated model IDs unclassified | `MODEL_API_KEY` or `META_API_KEY` | `--provider meta --model KNOWN_RESPONSES_ID` |
| Vercel AI Gateway | pinned Chat; listed IDs unclassified | `AI_GATEWAY_API_KEY` or `VERCEL_AI_GATEWAY_API_KEY` | `--provider vercel-ai-gateway --model KNOWN_CHAT_ID` |
| Cloudflare AI Gateway | account/gateway-scoped unified Chat; no authoritative catalog | `CLOUDFLARE_AI_GATEWAY_API_KEY` + `CLOUDFLARE_ACCOUNT_ID` + `CLOUDFLARE_GATEWAY_ID` | `--provider cloudflare-ai-gateway --model PROVIDER/MODEL_OR_DYNAMIC/ROUTE` |
| Command Code Studio Provider API | separate Chat, Messages, Responses; explicit route and model | `COMMAND_CODE_API_KEY` or `COMMANDCODE_API_KEY` (Studio key, not GO-plan credential) | `--provider commandcode --api chat|messages|responses --model KNOWN_ROUTE_MODEL_ID` |
| GitLab Duo Direct Access | account-bound token exchange then Anthropic, Responses or Chat proxy | `GITLAB_TOKEN` PAT or `--login gitlab-duo` (registered `GITLAB_CLIENT_ID` + `GITLAB_REDIRECT_URI`) | `--provider gitlab-duo --api messages|responses|chat --model KNOWN_UPSTREAM_MODEL_ID` |
| Devin CLI | pinned protobuf/Connect Chat and account-scoped models | `DEVIN_API_KEY` session token or `--login devin` (PKCE) | `--provider devin --models`, then `--model ACCOUNT_MODEL_ID` |
| [MiniMax API](https://platform.minimax.io/docs/api-reference/models/openai/list-models) (international) | Chat Completions; `/models` returns unclassified IDs | `MINIMAX_API_KEY` | `--provider minimax --models`, then select a listed ID with `--model MODEL_ID` |
| [Cline Pass](https://github.com/cline/cline/blob/main/docs/api/chat-completions.mdx) | Chat Completions; exact full model ID required, no API listing | `CLINE_API_KEY` | `--provider cline-pass --model cline-pass/MODEL_ID` |
| [Alibaba Token Plan](https://help.aliyun.com/en/model-studio/token-plan-personal-quick-start) (Beijing) | Fixed OpenAI-compatible Chat; explicit model ID | `ALIBABA_TOKEN_PLAN_API_KEY` (`sk-sp-`) | `--provider alibaba-token-plan --model KNOWN_MODEL_ID` |
| [Kimi Code](https://www.kimi.com/code/docs/) (international or China) | Pinned regional Chat or Messages; explicit plan model ID | `KIMI_API_KEY` (Kimi Code key for selected region, not Moonshot API) | `--provider kimi-code` or `kimi-code-cn`, `--api chat` or `messages`, `--model KNOWN_MODEL_ID` |
| [Umans Code](https://app.umans.ai/offers/code/docs) | Chat or Messages (Messages default); `/v1/models/info` reports model capabilities | `UMANS_AI_CODING_PLAN_API_KEY` | `--provider umans --models`, then `--model MODEL_ID` |

</details>

### User-defined OpenAI-compatible providers

Configure custom providers only in `${XDG_CONFIG_HOME:-~/.config}/pave/settings.json`. Project `.pave/settings.json` deliberately rejects `custom_providers`; configured endpoints, route names, account IDs, model IDs, and environment-variable names are not secrets.

```json
{
  "custom_providers": [
    {
      "id": "team-gateway",
      "display_name": "Team Gateway",
      "default_route": "chat",
      "routes": [
        {
          "name": "chat",
          "api": "openai-chat",
          "endpoint": "https://gateway.example/v1/chat/completions",
          "account_id": "team-7",
          "api_key_env": "TEAM_GATEWAY_KEY",
          "models_endpoint": "https://gateway.example/v1/models"
        }
      ]
    }
  ]
}
```

- Only OpenAI Chat Completions is supported. Endpoints must be HTTPS; numeric loopback HTTP (`127.0.0.1` or `[::1]`) is allowed for inference only. The optional model-list endpoint must be HTTPS, same-origin, and end in `/models`.
- `api_key_env` names the environment variable whose value is sent as a Bearer key to that fixed route. Omit it for a keyless route. Never put the key itself in settings.
- Configure exactly one of `models_endpoint` or a static `models` array. Static model entries take an exact `id` and optional `display_name` and explicitly declared `tools` boolean; omitted capability data stays unknown. Neither configuration proves account entitlement.
- Select the default route with `pave --provider team-gateway --model MODEL_ID`; use `--api ROUTE_NAME` for another configured route. A changed route configuration requires reselecting a saved model. Built-in provider counts exclude user-defined providers.

### Provider-specific thinking controls

The TUI `/thinking LEVEL` command stores branch-local metadata; a route sends only a control documented for that provider. Fireworks maps `minimal` to `reasoning_effort: "none"` and replays its returned `reasoning_content` with tool results. Alibaba Coding Plan and Token Plan map `none` to `enable_thinking: false`, other supported levels to `true`, and omit the field by default; model support is not inferred from the model ID. See the linked [Fireworks reasoning](https://docs.fireworks.ai/guides/reasoning) and [Alibaba Qwen Code](https://help.aliyun.com/en/model-studio/qwen-code) guidance.

### Deployment and catalog caveats

- **Copilot:** Interactive `/model` can choose a discovered Chat ID; headless prompts need `--model` or a saved default. The pinned endpoint rejects unauthorized model IDs.

- **Local LM Studio / llama.cpp / vLLM:** Keyless Chat by default; optional `LM_STUDIO_API_KEY`, `LLAMA_CPP_API_KEY` or `VLLM_API_KEY`. Matching `*_BASE_URL` variables allow numeric private/loopback addresses or `localhost` (default ports 1234, 8080, 8000). Listing and chat use the same validated host; discovery ignores `--endpoint`, disables proxies and follows no redirects. A key over plain LAN HTTP is unencrypted: use trusted HTTPS across a network.
- **Azure OpenAI:** Set `AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com` and choose the exact deployment ID. Routes are public-cloud v1 Responses by default or Chat with `--api chat`; authenticate with `AZURE_OPENAI_API_KEY` or the current Azure CLI identity. `--models` discovers deployments through Azure Resource Manager in the current subscription and requires management read access; an API key alone cannot list. Pave does not translate model IDs to deployment names or target sovereign clouds.
- **Google Vertex:** Set `GOOGLE_CLOUD_PROJECT`, `GOOGLE_VERTEX_LOCATION` (or documented aliases) and a known publisher model ID. Google ADC supports authorized-user/service-account credentials directly; impersonated credentials use gcloud, while explicit tokens and metadata credentials remain supported. The registered routes are native Gemini GenerateContent and Vertex Claude Messages (`--api messages`); there is no account model-list API, so IDs remain manual. Cloud Code Assist/Gemini CLI identity is not aliased to Vertex, and Antigravity consumer OAuth is prohibited by its terms.
- **Amazon Bedrock:** Set `AWS_REGION` or `AWS_DEFAULT_REGION` and use the AWS credential chain: environment/shared files, cached SSO, web identity/assume-role, ECS/EC2 metadata; `credential_process` is disabled unless `PAVE_AWS_CREDENTIAL_PROCESS=allow`. SigV4 signs only regional Converse/ConverseStream requests. `--models` lists active on-demand text foundations and inference profiles, not Invoke permission; custom endpoints are rejected. Bedrock Mantle remains a separate regional bearer Responses route.
- **Cursor and GitLab Duo Agent:** Cursor's official [bridge protocol](https://github.com/cursor/sdk-bridge#readme) uses Connect over HTTP/1.1; the [service definitions](https://github.com/cursor/sdk-bridge/blob/main/docs/services.md) separate `SdkCursorService.ListModels` from the agent-run lifecycle. It is not a generic raw-completion provider. GitLab Duo Agent's persistent authenticated WebSocket/runtime is separate from GitLab Duo's supported direct Messages/Responses/Chat routes. No unsupported agent login or route is advertised.
- **Personal Copilot:** only the pinned Chat route and account roster are supported; Responses, Anthropic Messages and GitHub Enterprise require separately documented routes, auth and entitlement evidence before registration.
- **Kimi Code:** Use the region-matched Kimi Code subscription key, not a Moonshot Open Platform key. Pave sends the truthful `User-Agent: Pave`; Kimi's [official API guide](https://www.kimi.com/code/docs/) requires the client identity not be impersonated. The route does not establish plan entitlement.
- **Alibaba Token Plan:** The `sk-sp-` route is distinct from Alibaba Coding Plan and the workspace API. Confirm the current Token Plan terms and supported-tool eligibility; a Pave route is not proof of account access or authorization.
- **Umans:** `/v1/models/info` reports the provider's current model/capability data. The listing does not validate the configured key or prove account access; prices remain unknown in Pave.
- **Anthropic prompt cache:** Only the direct `https://api.anthropic.com/v1/messages` API-key route opts into Anthropic's automatic ephemeral prefix cache; OAuth and custom/compatible endpoints do not. The default cache lifetime is five minutes. Cache writes can have different billing, and cache retention terms may differ; check current Anthropic pricing and data-retention terms. Pave records provider-reported cache read/write tokens and does not estimate dollars.

**Coverage and limits**

- Linux/macOS Intel builds list 71 provider IDs across 70 of the 83 source identities (13 unmatched); macOS arm64 adds Apple Foundation Models when its helper is present (72 IDs, 71 source identities, 12 unmatched). The first 15 routes added after v0.1.39 passed native fake-HTTPS `read_file` turns. R3 route, discovery, reasoning-replay and CLI tool-result fixtures passed; fixtures do not prove live entitlement.
- Command Code catalogs describe endpoints but cannot validate a Studio key: choose `--api`. GitLab Duo has no authoritative non-agentic upstream-model roster: supply route and model. Unverified listings stay unclassified in `/model`.
- Alibaba Coding Plan requires an explicit `--api china|intl`; its key is distinct from the Beijing Token Plan key. Xiaomi Token Plan environment keys and endpoints are region-bound, unlike Xiaomi pay-as-you-go. Cloudflare requires a configured account/gateway and uses the gateway-auth header; none of these routes has a Pave account-model listing.
- Cursor's official SDK bridge uses Connect over HTTP/1.1; `SdkCursorService.ListModels` is separate from its agent-run lifecycle, so Cursor has no generic Pave completion route. GitLab Duo Agent's persistent WebSocket/runtime remains unsupported; GitLab Duo Direct Access is a separate supported route. Gemini CLI, Kimi Code device OAuth, Muse, Stencil/Z.AI Coding Plan, xAI subscription OAuth, Perplexity and Copilot Enterprise also remain unavailable pending the documented matching registration/route/transport prerequisites. Google's [Antigravity terms](https://antigravity.google/terms/) prohibit third-party OAuth clients; standard API keys are not mislabeled as subscription grants.

## Features

| Available | Not yet available |
| --- | --- |
| Seven wire payload formats with distinct provider routes; bounded buffered and incremental-stream decoders; model-bound Codex/Gemini state and tested provider reasoning replay | Most provider-specific thinking/usage/multimodal parity and full model catalog |
| Bounded mobile manifest, Xcode shared-scheme, SwiftPM/Gradle, Flutter pubspec and React Native/Expo script/lockfile/observed-host mapping (no SDK or build claim); workspace file read/search/edit/write, bounded agent turns, approved LSP/DAP, read-only child jobs, approved managed worktrees and approved persistent JS/Python eval | Writable parallel workers, skills/plugins/MCP, browser/CDP, and the remaining reference-only tool surfaces |
| Separate task APIs for OpenAI embeddings, image generation, text-to-speech and transcription plus Cohere v2 reranking; these do not add chat routes | OpenAI video generation (Videos/Sora retired; no replacement documented) |
| Grapheme-aware CJK input, cancellable streaming with queued follow-ups, searchable model picker, branching sessions, explicit-window byte-proxy budgeting with counted media payload bytes, automatic/manual journal-safe summaries, native OpenAI Responses and capability-gated Anthropic compaction | Tokenizer-exact context limits and media token estimates where no verified route metadata exists, plus other documented provider-native compaction |

Use `mobile_project` without arguments to inventory stacks. To see a focused inert command preview, supply both the exact reported project `subroot` (an Xcode bundle path for Xcode) and `platform` (`ios` or `android`). A Flutter/React Native host folder is not a separate native app selection; this tool never executes its previews.

**Shell safety:** model-requested shell execution is off by default. `--allow-shell` advertises shell commands but still asks for **each** command in an interactive terminal, even with `--approval-mode yolo` or a per-tool allow; noninteractive runs deny shell execution. Approved commands are **not sandboxed** and can access files outside the workspace. Check the impact preview and exact command before approving it; Pave does not install mobile SDKs, sign apps or deploy to devices for you.

## Contribute

New contributors are welcome—bug reports, TUI polish, provider work and mobile-workspace testing are useful. Start with [open issues](https://github.com/kimmandoo/pave/issues), [the feature plan](TASKS.md) and [design rules](docs/DESIGN_RULES.md).

1. Fork the repository, create a focused branch from `main`, and use the source-install instructions above.
2. Add a behavior-focused regression for a bug. Run `opam exec -- dune runtest --force` and `opam exec -- dune build @install`; check TUI changes in a real terminal or PTY.
3. Update usage/docs and [CHANGELOG.md](CHANGELOG.md) when behavior changes. Open a [pull request](https://github.com/kimmandoo/pave/pulls) with the behavior, checks and platform limitations.

Read [CONTRIBUTING.md](CONTRIBUTING.md) for the full checklist and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for community expectations. Report vulnerabilities **privately** using [SECURITY.md](SECURITY.md), not a public issue. Never attach credentials, private source or session transcripts to reports.

## License

[MIT](LICENSE), with Pave as the copyright holder. Earlier MIT copyright and permission notices and linked-library terms remain in [THIRD_PARTY_NOTICES](THIRD_PARTY_NOTICES); both files accompany release binaries.
