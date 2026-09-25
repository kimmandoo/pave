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
> Pave is under active development. Its 65 provider routes, seven wire payload formats, account sign-in paths, branching session journal and cancellable terminal have fixture coverage—not blanket live vendor entitlement. LSP/DAP, subagents, plugins and a complete model catalog remain open. See [the feature plan](TASKS.md).

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
2. Run `pave --providers` for routes or `pave --provider ID --models` for pinned, account-scoped model listings. No model IDs or wire routes are guessed from names.
3. Treat `[listed · API unverified]` as an account-listed ID, **not** proof of Chat, tool support or compatible wire routes. Anthropic OAuth cannot list models without `ANTHROPIC_API_KEY`; type an Anthropic model ID instead if needed.

Keep API keys out of checked-in config and session files.

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

### Provider-specific notes

The [provider table](#providers) lists credentials and CLI flags. Additional route caveats:

- **Moonshot / Ollama Cloud:** Moonshot (`MOONSHOT_API_KEY` or `KIMI_API_KEY`) lists models on its global `/v1/models` and uses Chat. Ollama Cloud (`OLLAMA_CLOUD_API_KEY` or `OLLAMA_API_KEY`) uses fixed hosted `/api/tags` and `/api/chat`; the local Ollama route never receives its key.
- **Bedrock Mantle:** `AWS_BEARER_TOKEN_BEDROCK` with `AWS_REGION` selects a validated regional Responses endpoint and bearer-key model listing, separate from SigV4 Bedrock Converse. A listed model is not guaranteed to support Responses.
- **xAI / NVIDIA:** `XAI_API_KEY` lists account models at `/v1/language-models`; native Chat preserves reported `reasoning_content` on tool replay. `NVIDIA_API_KEY` lists `/v1/models`, but listings omit per-model tool support and hosted schemas vary; Pave does not infer compatibility from names. Both buffer validated completions before displaying text, not incremental SSE.
- **Novita / SiliconFlow:** Novita (`NOVITA_API_KEY`) uses a fixed hosted Chat endpoint, dynamic `/models` and unchanged interleaved reasoning replay. SiliconFlow uses separate global `SILICONFLOW_API_KEY` and China `SILICONFLOW_CN_API_KEY` hosts, text/chat-filtered listings and native Chat; credentials never cross regions. Some models need a vendor-specific thinking switch for tools: no default switch or model-name heuristic is bundled. These routes buffer validated completions.
- **StepFun:** `STEPFUN_API_KEY` is sent only to `api.stepfun.ai`, not the separate `.com` platform. Its `/v1/models` mixes Chat and audio without capability flags; `--models` calls IDs unclassified and `/model` marks them `[listed · API unverified]`. Select only a known compatible ID. Complete StepFun tool calls may end with its documented `finish_reason: "stop"`; other Chat routes retain strict finish validation.
- **CoreWeave Serverless:** W&B Inference's fixed host accepts `COREWEAVE_API_KEY` or `WANDB_API_KEY` for native Chat and account `/v1/models`. A CoreWeave control-plane credential is not interchangeable.

These routes passed isolated fake-HTTPS/native workspace tool-result scenarios; no live vendor account entitlement or per-model tool support was verified.

### Accounts and model selection

| Command | Scope |
| --- | --- |
| `/setup` → **Connect account only** | Sign in without changing the current model or saved default; optionally pick a model afterward. |
| `/setup` → **Choose user default** | Guided provider → access → API/model selection, saved for later launches. |
| `/model` | Search models across connected providers and switch **this conversation only**. |
| `/settings` | Edit project defaults for the **next launch**. |

API-key providers read environment variables, never keys typed into the TUI. Browser URLs and device codes appear on the regular terminal while the full-screen UI is suspended; the transcript and draft return afterward.

In `/model`, use arrows and Enter to choose a discovered ID, or type `PROVIDER/MODEL_ID`. Unclassified IDs may not support Chat or tools. For incompatible API routes, enter `PROVIDER@API/MODEL_ID` (for example, `/model commandcode@messages/MODEL_ID`) or choose the route in setup/settings. A bare ID keeps the current provider and API. Saved sessions restore their route; switching models keeps conversation text but retains signed provider state **only** when provider, model and API all match. Escape cancels discovery without submitting the draft.

**Sign-in details**

- Remote browser: `pave --login-manual PROVIDER` accepts a full callback URL; OpenRouter also accepts its authorization code alone. GitLab Duo, Devin, Anthropic and Codex require matching callback state.
- GitLab Duo requires a user-registered `GITLAB_CLIENT_ID` and matching loopback `GITLAB_REDIRECT_URI`; choose its upstream model and API manually. Devin uses its pinned CLI authorization and account-scoped Connect model roster.
- GitHub Copilot and Kilo use device approval: `pave --login PROVIDER`, then open the shown verification URL and enter its code. `--login-manual` is not supported for either; Kilo's public models do not certify Chat/tool support.
- `pave --logout PROVIDER` removes a saved credential. The private store at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` is **unencrypted** (0700 directory, 0600 file); OpenRouter's browser exchange stores an API key. Environment keys take precedence where available. Browser/device-derived credentials cannot be sent to a custom `--endpoint`.

**Routes and network boundaries**

- `--api NAME` selects a registered wire route. OpenAI defaults to Responses; `--api chat` selects Chat Completions.
- `--endpoint URL` works only for routes allowing custom hosts. Bound bearer/ADC/SigV4 routes—including xAI, NVIDIA, Ollama Cloud and Bedrock Mantle—reject untrusted overrides; `--models` rejects custom endpoints so private gateway keys never reach a public listing.
- Completion requests require HTTPS except for loopback or explicitly validated local engines. Credentialed `--endpoint http://remote-host` fails before a request; loopback HTTP remains available for development.
- No provider bundles a model ID. Noninteractive prompts need `--model ID` or a saved selection; first-run setup can query models. Personal Copilot needs an account-supported Chat model on its pinned public route. Redirected I/O uses a plain line-oriented CLI with the same slash commands, not the full-screen interface.

### First run

Without an explicit provider/model/session or configured default, an interactive launch opens keyboard-operated **SETUP** before the editor:

1. Pick a provider and access method. OAuth-capable providers offer real sign-in; API-key providers show the environment variable without collecting or echoing its value.
2. Pick an account-listed model and confirm the default, or skip—including a missing-key step—and return later with `/setup`. A skipped key is unusable until its environment variable is set.

User defaults and the versioned setup state are private under `${XDG_CONFIG_HOME:-~/.config}/pave/`. Configured defaults, explicit CLI choices, resumed `--session` and noninteractive `--prompt` bypass onboarding. `/settings` edits project defaults.

The searchable setup picker queries the chosen provider asynchronously. Use Up/Down and Enter; `[listed · API unverified]` does not certify Chat or tools. On listing failure, type a route-compatible `PROVIDER/MODEL_ID`. Resize keeps the active choice.

- **Sign-in listings:** Devin's native roster and signed-in Codex, Copilot and OpenRouter are account-scoped. Anthropic OAuth needs `ANTHROPIC_API_KEY` to list models; GitLab Duo has no authoritative non-agentic upstream-model listing and needs an explicit model/API.
- **Key-backed listings:** OpenAI, Google, DeepSeek, Groq, Mistral, Together, Cerebras, Venice, DeepInfra, Fireworks, Baseten, Hugging Face, NanoGPT, AIML API, ai&, Sakana, Abliteration, GMI Cloud, Moonshot, Ollama Cloud, xAI, NVIDIA, Novita, SiliconFlow, CoreWeave, StepFun, local engines and other registered routes. Listings never prove invocation/tool entitlement unless that metadata is supplied. Bounded cursor pages fail closed instead of showing incomplete IDs.

### Terminal experience

- **Conversation:** Pixel-art Pave appears only in an empty transcript; the first message replaces it. Roles, Markdown, tool progress and folded results use distinct blocks. `Option+O` on macOS or `Alt+O` elsewhere toggles the latest visible tool result.
- **Navigation:** Type `/` for filtered slash-command hints (`/re` narrows them). Up/Down selects, Tab or Return (macOS) / Enter (other supported terminals) inserts **without executing**, Escape keeps the draft, and a second Return/Enter submits. `/help` shows the catalog.
- **Status:** The model row identifies saved versus unsaved sessions; activity switches from `Working` to the running tool and back. Elapsed time advances through slow responses and shell approval without idle polling. Idle usage shows measured branch/conversation input and output tokens when available; `/usage` details models and untracked cost.
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
| `PgUp`/`PgDn` · `Ctrl+Home`/`Ctrl+End` · `Option+O` (macOS) / `Alt+O` (other terminals) | Scroll the transcript, jump to its beginning/end, or expand/collapse the latest visible tool result |
| `Ctrl+C` · `Ctrl+D` | Close a picker or cancel account sign-in; in the composer, interrupt a turn without losing the draft or clear a nonempty idle draft; `Ctrl+D` exits when empty. |
| `/` then `Tab` | Search available slash commands; Return (macOS) / Enter (other terminals) inserts a choice, Escape returns to the unchanged draft |
| `/setup` · `/model [PROVIDER[@API]/MODEL_ID]` | Connect an account without changing defaults, or configure the user default · choose the active conversation model/API across connected providers |
| `/cancel` · `/settings` | Stop the active request/command; edit typed project defaults for the next launch |
| `/queue MESSAGE` | Queue a follow-up while a turn is active; when idle, send it immediately. |
| `/tools [NAME]` | List the tools actually offered to the model, or inspect one tool's description; shell availability follows `--allow-shell` and still requires per-command approval |
| `/context` | Inspect the actual model/route, saved branch and retained conversation count; show only provider-reported input/output tokens from OpenAI Responses, Codex subscription Responses, Anthropic Messages, Google Gemini, Ollama and Chat Completions routes that supply complete usage (the official OpenAI Chat stream explicitly requests it), on the selected ancestry or cumulative ephemeral conversation; other requests, context limit and cost remain untracked |
| `/usage` | Inspect only recorded provider-reported tokens; private journals group the selected branch's input/output totals by model, while ephemeral conversations show the combined measured total without claiming per-model provenance |
| `/retry` | Reissue the last user turn only if it made no tool calls; saved sessions retain the prior answer on an abandoned branch, while ephemeral answers are replaced; both requests may incur usage |
| `/hotkeys` | Display actual interactive keyboard shortcuts (including search, word editing, paste and tool expansion); headless CLI does not claim terminal keys work |
| `/new` · `/resume [PATH]` | Create a private, persistent workspace journal; search recent journals or reopen an explicit workspace journal |
| `/help` · `/entries` | Show descriptive commands · list journal message IDs |
| `/tree` · `/branch ID` · `/fork /path/new.jsonl` | Search recent journal ancestry and select a branch · choose an exact entry ID · copy the selected conversation |
| `/compact` · `/quit` | Summarize older turns manually · exit |

On macOS, `/help` labels Meta as `Option` and Enter as `Return`; other supported terminals show `Alt` and `Enter`. Configure Option to send Escape/Meta in the terminal to use modified shortcuts. `/queue MESSAGE` remains available when it is not configured.


### Sessions and long-running turns

- **During a turn:** Network and approved commands leave the editor responsive. Later prompts queue until their turn starts; `/cancel` stops the active turn without dropping queued prompts or the draft. Failed/cancelled partial text is removed.
- **Scrollback:** Memory retains the newest 10,000 logical rows; a saved journal retains its complete durable history. `/resume` restores that history without replacing the editor draft. New startup conversations are ephemeral; `/new` asks before discarding an unsaved one.
- **Private journals:** `/new` creates an **unencrypted** append-only JSONL file under `${XDG_STATE_HOME:-~/.local/state}/pave/sessions/<SHA-256 of canonical workspace path>/<random>.jsonl` (0700 directories, 0600 files). `/resume` shows at most 100 recent journals for the current workspace, rejecting symlinks, foreign owners and permissive files, including explicit paths. `--session PATH` reopens a chosen journal.
- **Branch metadata:** Provider/model/API changes belong to branches, not provider messages. `/resume`, `--session` and `/branch` restore them; explicit `--provider`, `--model`, `--api` or `--endpoint` wins. Credentials and custom endpoints are not stored as model metadata, and a removed route must be overridden on reopen.
- **Recovery and privacy:** Reopening marks interrupted tool calls failed instead of rerunning them. Keep journals out of version control: Gemini 3 replay can persist model-issued thought text and signatures. `/compact` retains the entire journal, but summarization can fail when the provider context limit is exceeded.
- **Tree picker:** `/tree` shows parent-linked entries and model changes, searches preview text and IDs, highlights the active tip and limits the list to 1,024 recent entries. `/branch ID` selects older entries; Escape leaves the branch and draft intact.

## Providers

Use `pave --providers` for the live list. This reference separates wire transport, credentials and model selection; **a listed model is not proof of account access or tool support**.

<details>
<summary>Show provider routes, credentials and CLI examples</summary>

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
| AIML API | Chat Completions | `AIMLAPI_API_KEY` | `--provider aimlapi --model MODEL_ID` |
| ai& | Chat Completions | `AIAND_API_KEY` | `--provider aiand --model MODEL_ID` |
| Sakana AI | Responses | `SAKANA_API_KEY` or `FUGU_API_KEY` | `--provider sakana --model MODEL_ID` |
| Abliteration AI | Responses (default), Chat Completions | `ABLITERATION_API_KEY` or `ABLIT_KEY` | `--provider abliteration --model MODEL_ID` |
| GMI Cloud | Chat Completions | `GMI_API_KEY` | `--provider gmi-cloud --model MODEL_ID` |
| Google Vertex AI | signed Gemini streaming, configured project/location | Google ADC or `GOOGLE_CLOUD_ACCESS_TOKEN` | `--provider google-vertex --model MODEL_ID` |
| Amazon Bedrock | AWS SigV4 Converse (buffered) | AWS environment/shared profile credentials | `--provider amazon-bedrock --model MODEL_ID` |
| Baseten | Chat Completions | `BASETEN_API_KEY` | `--provider baseten --model MODEL_ID` |
| LM Studio (local) | Chat Completions | optional `LM_STUDIO_API_KEY` | `--provider lm-studio --model MODEL_ID` |
| llama.cpp (local) | Chat Completions | optional `LLAMA_CPP_API_KEY` | `--provider llama.cpp --model MODEL_ID` |
| vLLM (local) | Chat Completions | optional `VLLM_API_KEY` | `--provider vllm --model MODEL_ID` |
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
| Alibaba Coding Plan | region-pinned subscription Chat | `ALIBABA_CODING_PLAN_API_KEY` (`sk-sp-`) | `--provider alibaba-coding-plan --api china|intl --model KNOWN_CHAT_ID` |
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

</details>

### Deployment and catalog caveats

- **Copilot:** Interactive `/model` can choose a discovered Chat ID; headless prompts need `--model` or a saved default. The pinned endpoint rejects unauthorized model IDs.

- **Local LM Studio / llama.cpp / vLLM:** Keyless Chat by default; optional `LM_STUDIO_API_KEY`, `LLAMA_CPP_API_KEY` or `VLLM_API_KEY`. Matching `*_BASE_URL` variables allow numeric private/loopback addresses or `localhost` (default ports 1234, 8080, 8000). Listing and chat use the same validated host; discovery ignores `--endpoint`, disables proxies and follows no redirects. A key over plain LAN HTTP is unencrypted: use trusted HTTPS across a network.
- **Azure Responses:** Set `AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com`, `AZURE_OPENAI_API_KEY`, and exact `--model DEPLOYMENT_ID`. `AZURE_OPENAI_API_VERSION=v1` or `preview` is optional; the default is `/openai/v1/responses`. Only that Azure resource path receives the key. Azure Chat, deployment discovery, Entra auth, custom/non-Azure hosts and model-name-to-deployment translation are unsupported.
- **Google Vertex:** Set `GOOGLE_CLOUD_PROJECT`, `GOOGLE_VERTEX_LOCATION` (or documented aliases) and an explicit publisher model. Authenticate via `gcloud auth application-default login`, workload metadata or `GOOGLE_CLOUD_ACCESS_TOKEN`. Pave derives the regional Google Gemini SSE endpoint and rejects `--endpoint`; no bundled OAuth client or model IDs.
- **Amazon Bedrock:** Set `AWS_REGION` or `AWS_DEFAULT_REGION`, AWS access/secret keys (optionally a session token) or a static shared profile, and a foundation/inference-profile ID. SigV4 signs only regional `/converse`; answers are buffered and custom endpoints rejected. `--models` lists on-demand text foundations, not inference profiles or Invoke rights. Azure, Vertex and Bedrock never forward credentials to caller-supplied hosts.

**Coverage and limits**

- The 65 local routes have isolated wire/tool-result fixtures; the first 15 added after v0.1.39 also passed native fake-HTTPS two-turn `read_file` scenarios. The sibling inventory has 83 identities, leaving 18 unimplemented. Fixtures do **not** prove live entitlement.
- SingularityAPI reserved has no authenticated response proof; Fire Pass needs a full router resource; OpenCode catalogs do not prove plan access. Cloudflare needs an account/gateway with BYOK or Unified Billing and lacks a documented account-specific listing.
- Command Code catalogs describe endpoints but cannot validate a Studio key: choose `--api`. GitLab Duo has no authoritative non-agentic upstream-model roster: supply route and model. Unverified listings stay unclassified in `/model`.
- Alibaba Coding Plan needs `--api china|intl`; Xiaomi and MiniMax keys are region-bound. Cursor bidirectional Connect and GitLab Duo Agent WebSocket remain unsupported.
- Google's [Antigravity terms](https://antigravity.google/terms/) prohibit third-party OAuth clients; xAI has no published reusable subscription OAuth registration. Zhipu Coding Plan excludes unofficial clients. Standard API keys are not mislabeled as plan credentials.

## Features

| Available | Not yet available |
| --- | --- |
| Seven wire payload formats with distinct provider routes; bounded buffered and incremental-stream decoders; model-bound Codex/Gemini native state replay | Most provider-specific thinking/usage/multimodal parity and full model catalog |
| Mobile manifest detection, workspace file read/search/edit/write, bounded agent turns | LSP/DAP, subagents, extensions and full tool catalog |
| Grapheme-aware CJK input, cancellable streaming with queued follow-ups, searchable dynamic model picker, branching sessions and manual compaction | Automatic context budgeting/compaction and full structured session resume |

**Shell safety:** model-requested shell execution is off by default. `--allow-shell` advertises shell commands but still asks for **each** command in an interactive terminal, even with `--approval-mode yolo` or a per-tool allow; noninteractive runs deny shell execution. Approved commands are **not sandboxed** and can access files outside the workspace. Check the impact preview and exact command before approving it; Pave does not install mobile SDKs, sign apps or deploy to devices for you.

## Contribute

New contributors are welcome—bug reports, TUI polish, provider work and mobile-workspace testing are useful. Start with [open issues](https://github.com/kimmandoo/pave/issues), [the feature plan](TASKS.md) and [design rules](docs/DESIGN_RULES.md).

1. Fork the repository, create a focused branch from `main`, and use the source-install instructions above.
2. Add a behavior-focused regression for a bug. Run `opam exec -- dune runtest --force` and `opam exec -- dune build @install`; check TUI changes in a real terminal or PTY.
3. Update usage/docs and [CHANGELOG.md](CHANGELOG.md) when behavior changes. Open a [pull request](https://github.com/kimmandoo/pave/pulls) with the behavior, checks and platform limitations.

Read [CONTRIBUTING.md](CONTRIBUTING.md) for the full checklist and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for community expectations. Report vulnerabilities **privately** using [SECURITY.md](SECURITY.md), not a public issue. Never attach credentials, private source or session transcripts to reports.

## License

[MIT](LICENSE), with Pave as the copyright holder. Earlier MIT copyright and permission notices and linked-library terms remain in [THIRD_PARTY_NOTICES](THIRD_PARTY_NOTICES); both files accompany release binaries.
