# Pave usage reference

Detailed behavior for the interactive TUI, CLI and local extensions. For installation and a quick start, see the [README](../README.md); for provider routes and credentials, see [PROVIDERS.md](PROVIDERS.md).

## Contents

- [Agent execution mode](#agent-execution-mode)
- [Local skills, plugins and MCP](#local-skills-plugins-and-mcp)
- [Prompts, attachments and shell completions](#prompts-attachments-and-shell-completions)
- [Non-chat model tasks](#non-chat-model-tasks)
- [Accounts and model selection](#accounts-and-model-selection)
- [First run](#first-run)
- [Terminal experience](#terminal-experience)
- [Configuration and safety](#configuration-and-safety)
- [Keyboard and slash-command reference](#keyboard-and-slash-command-reference)
- [Sessions and long-running turns](#sessions-and-long-running-turns)
- [Context budgeting](#context-budgeting)
- [Mobile workspace tools](#mobile-workspace-tools)

## Agent execution mode

The default is **one agent**, with at most four contiguous read-only tool calls running concurrently. Writes, shell execution and unknown tools stay exclusive; parallel reads do not create extra model agents.

Read-only child agents are an explicit startup opt-in and require a private saved session:

```sh
pave --root /path/to/mobile/repo --session /private/path/pave.jsonl --enable-subagents
```

This enables `/delegate`, `/plan`, `/advisor`, `/watchdog`, `/loop`, `/autoresearch` and the model's `task` tool. Without the flag these spawning commands are absent from help/completion and denied by the parser; `/tool enable task` cannot override startup admission. Existing `/jobs`, `/wait`, `/cancel-job` and `/artifact` remain available for inspecting saved work.

Each model-requested delegation still requires interactive approval, including in `yolo` mode; an explicit workflow command is the user's launch action. Children have only `read_file`, `list_files`, `glob`, `search` and `grep`, at most six turns, and no shell, writes or nested delegation. Results stay session-owned; use `/jobs` then `/artifact ID` to read them. Child requests may be billed. Their reported usage remains in result text, not the parent's durable `/usage` totals.

## Local skills, plugins and MCP

In the interactive TUI, user skills live in `${XDG_CONFIG_HOME:-~/.config}/pave/skills/NAME/SKILL.md` and project skills in `.pave/skills/NAME/SKILL.md`. A skill begins with `---` metadata (`name`, `description`, optional comma-separated `resources`), a closing `---`, and plain-text instructions. Declarative commands live in `commands/NAME.json` at those same user/project roots and contain `{"name":"review","description":"Review a change","prompt":"Review this change"}`. Project names take precedence; unsafe files, name collisions and oversized content are diagnosed without execution. Use `--disable-user-content` or `--disable-project-content` to omit a source. `/skill:NAME` explicitly activates a skill for later prompts, while `/NAME` inserts a declarative command into the draft without sending it; help and completion show only available entries. Treat both files as untrusted task data.

Only `--local-tools /absolute/private/path/tools.json` opts into custom tools. The user-owned manifest must reside beneath the private Pave config directory, outside the workspace, with mode 0600. It contains `{"version":1,"tools":[{"name":"local_review","description":"Review","parameters":{"type":"object","properties":{"text":{"type":"string","maxLength":4096}},"required":["text"],"additionalProperties":false},"program":"/absolute/executable","arguments":[],"timeoutSeconds":10}]}`. Each invocation needs interactive approval, including in permissive mode; headless calls do not run the program. These child processes are not a sandbox. Optional private `plugins/NAME.json` manifests reference already available skill, command and tool names, using `{"schemaVersion":1,"name":"review-pack","version":"1.0.0","skills":["review"],"commands":["review"],"tools":["local_review"]}`. `/plugin list|enable NAME|disable NAME|reload` manages the private enabled state; no remote installation occurs.

For MCP, put `{"servers":[{"name":"local","command":"/absolute/server","args":[]}]}` in private user `mcp.json` or project `.pave/mcp.json`. A user/project `{"name":"local","deny":true}` disables that name. An HTTP entry uses `{"name":"remote","transport":"http","url":"https://host.example/mcp","bearerSecretRef":"remote-token"}`; referenced secrets live only in a mode-0600 private `mcp-secrets.json` object in the user config directory. Plain HTTP requires explicit `allowLoopbackHttp:true` and a loopback host.

`/mcp list` displays configured servers without connecting and exposes available `/mcp:NAME` suggestions. `/mcp connect NAME` requires interactive approval, then advertises validated tools as `mcp_NAME_TOOL`. `/mcp tools|resources|prompts NAME` inspects live listings; `/mcp read NAME URI` and `/mcp get NAME PROMPT` insert validated, source-attributed untrusted data into the draft for review. `/mcp reload` closes owned connections, reloads config and removes denied server suggestions. Each MCP tool effect requires separate approval. A failed server does not disconnect its siblings. Legacy SSE, marketplace installation and MCP OAuth are not offered without their separate prerequisites.

## Prompts, attachments and shell completions

For a one-shot turn, provide exactly one source: `--prompt`, nonempty redirected stdin (bounded to 1 MiB), or `--prompt-file`. Prompt-file paths are workspace-relative to `--root`, checked, and bounded UTF-8 text; their contents are prompt data, never parsed as slash commands. Empty or conflicting sources fail before a provider request.

Repeat `--image PATH` for checked workspace-relative images. Pave validates file type, MIME signature and size, then rejects routes without native user-media support before authentication or network access.

In the TUI, type `@` to preview attachable workspace files and directories immediately; typing a filename filters the list, including nested fuzzy matches, while `@dir/` narrows it to that directory. Rows show safe filenames, MIME types and byte sizes without reading media payloads into the preview. Use ↑/↓ to choose, Tab or Enter to insert an exact reference, and Esc to dismiss; selecting a directory continues browsing and selecting a file does not submit the prompt. Quoted paths with spaces remain supported. Completion stays inside the checked workspace, honors ignore and symlink boundaries, and excludes unreadable, oversized, invalid-media or non-UTF-8 text files. On submission, text files become labeled prompt text and supported media is sent as native typed content. Code/email occurrences and unresolved safe references remain literal; unsafe paths or invalid files fail closed. Staged media from `--image PATH` or `/attach PATH` appears above the composer with its sanitized filename, MIME type and size; sent and restored messages show the same metadata without exposing payload bytes. Graphical image display remains opt-in with `--terminal-images` in a verified Kitty/iTerm2 terminal.

`--output jsonl` writes ordered turn, text, tool and outcome records to stdout; diagnostics go to stderr. Completed turns exit 0, provider failures 1, tool failures 2 and cancellation 130. Plain text remains the default. Prose shortcuts are opt-in with `--shortcut NAME` and individually disableable with `--disable-shortcut NAME`; available names are `thinkdeep`, `verifyfirst` and `planfirst`. Pasted text, code, paths and tool output are not expanded. TUI headers use sanitized model display names when available and fall back to the upstream ID; labels never change the exact selector.

`pave completions bash|zsh|fish` prints a script generated from CLI/task option metadata. Model and session candidates come only from authorized local state for the effective `--root` workspace (or the current directory); completion does not contact providers. Session paths containing spaces remain selectable.


## Non-chat model tasks

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

## Accounts and model selection

| Command | Scope |
| --- | --- |
| `/setup` → **Connect account only** | Sign in without changing the current model or saved default; optionally pick a model afterward. |
| `/setup` → **Choose user default** | Guided provider → access → API/model selection, saved for later launches. |
| `/model` | Choose a provider, then its API/account when needed, then a model and reported effort for this conversation; remember the exact workspace model for the next interactive launch. |
| `/settings` | Edit project defaults; the workspace's last-used model takes precedence on fresh interactive launches unless an explicit CLI or session model is selected. |

API-key providers read environment variables, never keys typed into the TUI. Browser URLs and device codes appear on the regular terminal while the full-screen UI is suspended; the transcript and draft return afterward.

`/model` opens the provider list first; it fetches only the scope you open. A provider with multiple APIs or accounts asks you to choose one. Type to search that scope's complete fresh model roster; Escape or Tab returns to the provider list. `/model FILTER` searches the active provider/API/account directly. Arrows and Enter choose a discovered ID. The next panel centers on that model: Left/Right selects a reasoning-effort chip, Enter confirms model and effort together, and Escape leaves both unchanged. Only fresh reported levels that the native API can serialize are offered; missing or unsupported metadata offers **Provider default**, not guessed levels. Codex honors the selection on Standard and Lite requests and rechecks the authenticated account's fresh metadata before inference; its Lite listing default remains in effect when no override is selected. Setup/project defaults remain model-only.

The selected model's detail starts with its exact provider/route/account/model selector. Narrow pickers use available rows for this detail before introductory/status prose, while retaining a usable model list. Very long details and explanations show an ellipsis when omitted. Cancelling returns to the existing conversation without changing its model or draft.

Effort changes provider-native computation only; it does not enable Pave subagents. `/thinking LEVEL` remains available for explicit documented controls when a listing does not report selectable levels.

The scoped roster keeps choices visible before lengthy discovery details. Page Up/Down moves by the visible page; Home/End reaches the first/last entry, including scope actions. Effort confirmation shows the model identity, compact selected chips and position/count; the no-override default remains distinct from an explicit level. Resize preserves selection, and bracketed-pasted arrow sequences do not navigate effort controls.

Explicit `PROVIDER@API[#ACCOUNT]/EXACT_MODEL_ID` selectors remain usable; a bare ID keeps the current provider and API. Unclassified IDs do not prove Chat or tool compatibility. Saved sessions restore their route; switching models keeps conversation text but retains signed provider state **only** when provider, account, model and API all match. Escape cancels discovery and preserves the draft.

Long model labels are abbreviated only to fit the terminal; the picker still searches and selects the complete ID. Recent selections are stored in private workspace-scoped state under `${XDG_STATE_HOME:-~/.local/state}/pave/sessions/`, separate from conversation journals and user/project defaults. An explicit `--provider`, `--model`, `--api`, `--endpoint`, `--account`, or `--session` overrides the recent choice. Release binaries show their embedded version on the interactive launch screen; a source build shows `source`.

**Sign-in details**

- Remote browser: `pave --login-manual PROVIDER` accepts a full callback URL; OpenRouter also accepts its authorization code alone. GitLab Duo, Devin, Anthropic and browser-based Codex require matching callback state.
- GitLab Duo requires a user-registered `GITLAB_CLIENT_ID` and matching loopback `GITLAB_REDIRECT_URI`; choose its upstream model and API manually. Devin uses its pinned CLI authorization and account-scoped Connect model roster. Devin transport failures distinguish DNS/connect, TLS, timeout and HTTP causes when known, without displaying response bodies or credentials; timeouts do not automatically replay a completion.
- GitHub Copilot and Kilo use device approval: `pave --login PROVIDER`, then open the shown verification URL and enter its code. `--login-manual` is not supported for either; Kilo's public models do not certify Chat/tool support.
- OpenAI Codex also supports `pave --login-device openai-codex`: it prints the fixed verification URL and code, then polls the official device-approval endpoints for up to 15 minutes. The authorization code is exchanged through the same pinned OAuth token endpoint; the stored grant remains bound to the same Codex account and Responses route, with locked refresh. No loopback browser callback is opened.
- `pave --logout PROVIDER` removes every saved sign-in for that provider; add `--account "$ID"` to remove only one. The private store at `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` is **unencrypted** (0700 directory, 0600 file); OpenRouter's browser exchange stores an API key. Environment keys take precedence where available. Browser/device-derived credentials cannot be sent to a custom `--endpoint`.
- Google Vertex and Bedrock Converse/ConverseStream use scoped Google ADC or the AWS credential chain; Azure public-cloud routes use a resource API key or the current Azure CLI Entra identity. Apple Foundation Models is local and uses no provider credential.

**Account IDs and local secret masking**

- OAuth sign-ins are stored per provider and selected independently. When a provider returns an account ID, that ID scopes its models. If it returns none, Pave prints and stores a random `pave-local:<hex>` sign-in ID; this is a Pave-local selector, not a provider identity. For headless inference with multiple saved grants, use `--account "$ID"` or an account-scoped model selector. `pave --logout PROVIDER --account "$ID"` removes only that sign-in; `pave --logout PROVIDER` removes every saved sign-in for the provider.

On launch, an exact account in the selected model (including a saved session or workspace's last-used model) routes to that grant even when others are saved. A single saved sign-in is selected automatically. If the active model is unscoped and several grants remain, the TUI asks which account should receive the first prompt; cancelling keeps the draft, and the choice is remembered for that workspace and saved session. Headless `--prompt` cannot ask: use `--account "$ID"` or `--model 'PROVIDER@API#ACCOUNT/MODEL'` instead. Pave never guesses between multiple grants by their order or by a shared model ID.
- Environment API keys take precedence over saved OAuth grants where a provider supports both; the TUI shows the precedence. While an environment key is active, an explicit `--account` or account-scoped model selector is rejected rather than silently relabeled; unscoped models use the environment key. The local OAuth file remains **unencrypted** and private (0700 directory, 0600 file); masking does not encrypt it.
- Opt in with `--mask-secrets` to replace the active provider API key and OAuth access token when they occur in local conversation text, saved tool arguments and tool-result text. Tool arguments use collision-safe reversible placeholders for execution; UI/event output uses one-way redaction. This is exact-value matching, not a guarantee against fragments, transformed/derived values or unknown secrets. Images and opaque provider state are not traversed, ambient cloud credentials are not known to the mask, and historical journal entries are not rewritten. Masking is local and does not control provider-side logging or retention.
- No deployable remote authentication broker and trust model are available in this repository; credentials remain in the private local store.

**Routes and network boundaries**

- `--api NAME` selects a registered wire route. OpenAI defaults to Responses; `--api chat` selects Chat Completions.
- `--endpoint URL` works only for routes allowing custom hosts. Bound bearer/ADC/SigV4 routes—including xAI, NVIDIA, Ollama Cloud and Bedrock Mantle—reject untrusted overrides; `--models` rejects custom endpoints so private gateway keys never reach a public listing.
- Completion requests require HTTPS except for loopback or explicitly validated local engines. Credentialed `--endpoint http://remote-host` fails before a request; loopback HTTP remains available for development.
- No remote provider bundles a model ID. Apple Foundation Models uses the OS-managed local ID `default`; other noninteractive prompts need `--model ID` or a saved selection. First-run setup can query supported listings or accept a known manual ID. Personal Copilot needs an account-supported Chat model on its pinned public route. Redirected I/O uses a plain line-oriented CLI with the same slash commands, not the full-screen interface.

## First run

Without an explicit provider/model/session or configured default, an interactive launch opens keyboard-operated **SETUP** before the editor:

1. Pick a provider and access method. OAuth-capable providers offer real sign-in; API-key providers show the environment variable without collecting or echoing its value.
2. Choose an account-listed model or enter a known route-compatible ID, then confirm the default; or skip—including a missing-key step—and return later with `/setup`. Apple Foundation Models uses `default` without a listing. A skipped key is unusable until its environment variable is set.

User defaults and the versioned setup state are private under `${XDG_CONFIG_HOME:-~/.config}/pave/`. Configured defaults, explicit CLI choices, resumed `--session` and noninteractive `--prompt` bypass onboarding. `/settings` edits project defaults.

The searchable setup picker queries the chosen provider asynchronously. Use Up/Down and Enter; `[listed · API unverified]` does not certify Chat or tools. On listing failure, type a route-compatible `PROVIDER/MODEL_ID`; for Apple Foundation Models the ID is `default`. Resize keeps the active choice.

- **Sign-in listings:** Devin's native roster and signed-in Codex, Copilot and OpenRouter are account-scoped. Anthropic OAuth needs `ANTHROPIC_API_KEY` to list models; GitLab Duo has no authoritative non-agentic upstream-model listing and needs an explicit model/API.
- **Key-backed listings:** OpenAI, Google, DeepSeek, Groq, Mistral, Together, Cerebras, Venice, DeepInfra, Fireworks, Baseten, Hugging Face, NanoGPT, AIML API, ai&, Sakana, Abliteration, GMI Cloud, Moonshot, Ollama Cloud, xAI, NVIDIA, Novita, SiliconFlow, CoreWeave, StepFun, local engines and other registered routes. Listings never prove invocation/tool entitlement unless that metadata is supplied. Bounded cursor pages fail closed instead of showing incomplete IDs.

## Terminal experience

- **Conversation:** Pixel-art Pave appears only in an empty transcript; the first message replaces it. Roles, Markdown, tool progress and folded results use distinct blocks. Successful `read_file` calls show one sanitized workspace-relative path and line count each rather than repeating completion text or the first content line; expand an individual call to read its full bounded result. Failures remain visible. Fenced `diff` and printed unified diffs show separate file/hunk, added, removed and context styling; `NO_COLOR` retains their gutters and `+`/`-` markers. Folded command diffs preview the file header; `Option+O` on macOS or `Alt+O` elsewhere expands the full result.
- **Live writes:** `write_file` shows its safe relative filename and line-numbered content while tool arguments are still being generated. The card follows the same call through queued/approval, writing and completion; cancelled, malformed or denied drafts remain explicitly not written. The live tail keeps 16 lines, up to 256 UTF-8 bytes per line, with omitted line/byte counts. Buffered routes show the proposal after their complete response arrives. With `--mask-secrets`, partial argument previews are suppressed; validated content and approval details are redacted before truncation, without changing approved file bytes.
- **Write card details:** Filename, state and progress use separate rows. Omission counts appear only when nonzero; a settled success does not repeat the tool-result text under the card. Expand the existing result details when needed; failures remain visible.
- **Navigation:** Type `/` for filtered slash-command hints (`/re` narrows them). Up/Down selects; Tab inserts. Return/Enter runs an exact command, or inserts a partial match that still needs a second Return/Enter to submit; Escape keeps the draft. Mouse-wheel scrolling moves the transcript without changing the draft. `/help` shows the catalog.
- **Suggestions and search:** Slash/file suggestions show your position in the results and highlight the selected row across the width. Narrow windows keep names visible; wider ones add descriptions or file metadata. The footer distinguishes insertion from running an exact command. An empty picker search shows “No matches” and how to edit the filter or cancel.
- **Reading earlier output:** When you scroll up, the footer shows the visible row range and `Ctrl+End latest`; Ctrl+End returns to the newest output without changing your draft. At the latest output, the footer returns to send/command hints. Shortcut labels are omitted as whole items when the window cannot fit them.
- **Status:** The model row identifies saved versus unsaved sessions; activity switches from `Working` to the running tool and back. Elapsed time advances through slow responses and shell approval without idle polling. Idle usage shows measured branch/conversation input and output tokens when available; `/usage` details provider/account/model/route provenance and reported cache/reasoning counts, never an estimated price.
- **Streaming:** Routes that deliver incremental text pace stream-driven repaints at 60 Hz; a short chunk remains visible by the next frame even if the provider pauses. Completion is shown immediately. Routes that return only a buffered response cannot show text before the provider delivers it.
- **Waiting for a response:** Shared curl streams allow up to 600 s for request upload and the first response byte; only after data starts does the 120 s no-data limit apply. Connection setup remains bounded to 10 s and the whole stream to one hour. Buffered calls retain a 600 s total limit. An upstream service or proxy can impose a shorter limit; failures never automatically replay a possibly accepted request.
- **Follow-ups:** Return/Enter sends immediately when idle and queues while working. The current response continues; queued messages run in order and the header shows their count. Use `/steer MESSAGE` only when you deliberately want to interrupt and run a replacement next. Ctrl+C or `/cancel` stops the active turn, not the remaining queue.
- **Manage queued prompts:** Open `/queue` without text, or `Option+Q` on macOS / `Alt+Q` elsewhere to keep your current draft. The list shows execution order and attachments. Select an item, then choose **Run next** (keep active work), **Run now** (interrupt active work), **Edit in composer**, or **Cancel queued prompt** (leave active work alone). Back is selected initially; Escape returns without changes. Items update live, and an item that already started cannot be cancelled through a stale queue action. Editing preserves attachment bytes and refuses rather than overwriting staged media or overflowing the draft.
- **Failures:** Provider, auth and tool errors appear as readable error blocks. Failed or cancelled streaming text is removed; unknown exceptions retain diagnostic text. Tool and shell effects require their own explicit decision when approval is requested; sending a prompt is not an approval.
- **Display:** Pickers highlight the active row and keep provider-list errors visible. Narrow terminals show compact `PAVE`; `NO_COLOR=1` removes colors. Redirected I/O uses the plain CLI.
- **Paste:** Bracketed paste waits for its closing delimiter, inserts up to the remaining 16 KiB draft capacity as one undoable edit and converts Tab to space. Newlines do not submit. `Ctrl+Z` undoes the whole paste; `Ctrl+Y` restores it. Search-query paste has a separate 512-byte limit.

## Configuration and safety

Settings live in `${XDG_CONFIG_HOME:-~/.config}/pave/settings.json` (user) and `<workspace>/.pave/settings.json` (project):

| Key | Meaning |
| --- | --- |
| `default_provider`, `default_model`, `default_api` | The model requires its provider; the API must be a registered route for that provider. |
| `max_turns` | Integer from 1 to 100. |
| `disable_shell` | `true` in **either** scope denies `--allow-shell`. |

Explicit CLI flags override session choices, then project and user defaults. A session restores its saved API with its model. A fresh interactive launch without explicit selectors or `--session` first restores the workspace's last-used model, ahead of configured defaults. Successful top-level responses—including CLI-selected, configured, resumed and headless models—save the exact provider/account/API/model identity; accepted interactive model selections save immediately too. Failed requests, cancelled selection and merely opening a session do not replace it. Custom routes remain bound to their configuration; stale or insecure saved choices are discarded. Headless launch selection still follows explicit/session/configured defaults, not the recent interactive preference. Invalid, duplicate, oversized or symlinked settings are reported and skipped; `/settings` atomically replaces private project settings for the **next** launch.

**Tool approvals:** `--approval-mode` overrides the configured mode for one run. The default is `write`: reads and workspace writes are allowed, while execution needs approval. `always-ask` prompts for writes and execution; `yolo` allows ordinary tool tiers. `tools.approval` can set a tool to `allow`, `prompt` or `deny`; deny wins across user/project settings. `/settings` explains each mode's effect, preselects the current project choice, identifies inherited/effective settings, and edits defaults for the next launch—not the current action. Escape returns without saving.

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

The dedicated permission screen asks a question about the action (for example *Run this shell command?*), leads with exactly what will run (command, query, URL or path), then explains what happens, with the tool, tier and scope in its subtitle. Buttons sit directly below: **[ n  Deny ]**, **[ y  Allow once ]** and, for `web_search`, `web_fetch`, `write_file`, `edit_file`, `apply_edits`, `ast_edit` and `image_ocr` only, **[ a  Allow all TOOL ]**, which allows later calls of that tool without asking until Pave exits (never saved). Shell, process, device, remote, external-tool and child-agent actions always ask per call. Deny is selected initially; arrows/Tab move, Return/Enter confirms, and `y`/`a`/`n` (or Korean-layout `ㅛ`/`ㅁ`/`ㅜ`) choose directly; Escape or Ctrl+C denies. Other typing and bracketed paste are ignored. If the full preview cannot fit, only Deny is available until you resize. Closing the screen restores your draft and conversation position. Non-TUI terminal prompts accept `y`, `a` (where offered) or anything else to deny. Other yes/no decisions — discarding an unsaved conversation, choosing a model after connecting an account, and `/setup` — use the same button layout with the safe choice first.

The complete heading, scope, all action labels and consequences must fit too—not only the arguments. Narrow screens wrap the explanation and stack full buttons; if there is still insufficient room, shortcuts cannot approve or discard. A denied proposed write is labelled **not written**. Successful file content beginning `Error:` is displayed as a successful read.

If the terminal is too small even for the decision text, the compact screen names that decision and its safe action—for example **k/Esc Keep conversation**—and asks you to resize. It never presents an unavailable approve/discard shortcut as usable.

**Web search providers:** `web_search` tries providers in order and falls back to the next one when a provider fails, reporting the failed providers alongside the results. Without configuration the order is automatic: Exa (`EXA_API_KEY`), Firecrawl (`FIRECRAWL_API_KEY`), Brave (`BRAVE_SEARCH_API_KEY`), Tavily (`TAVILY_API_KEY`), Kagi (`KAGI_API_KEY`) and Jina (`JINA_API_KEY`) when their keys are set, then credential-free DuckDuckGo HTML search. Set `PAVE_WEB_SEARCH_PROVIDER_PRIORITY` (for example `brave,duckduckgo`) to use exactly those providers in that order; listed providers without their key are skipped. Each key is sent only to its own pinned HTTPS endpoint, and the approval screen lists the provider order and which providers receive credentials. Only Brave supports `page` > 0. DuckDuckGo may answer automated traffic with a bot challenge; configure a keyed provider for reliable results. In the conversation, each tool card names what the call acted on, shows a single-line outcome without a redundant expand hint, labels your refusals as **denied by you · not run**, and keeps notes raised during the call inside that card.

Nonzero pages are planned against Brave only; unsupported paging or invalid queries fail before the permission screen. Malformed HTML and provider-declared failures can fall back, while cancellation stops the chain. Failure diagnostics do not reproduce arbitrary remote error payloads.

**Project instructions:** User and ancestor `AGENTS.md` files load below the fixed mobile safety prompt, with bounded relative `@file.md` imports. Workspace `.pave/rules/*.md` scopes paths with frontmatter such as `---`, `paths: src/**/*.swift`, `---`. The first `write_file`/`edit_file` affected by a new rule is **withheld and journaled as unexecuted**; the rule enters the next model request and the model must retry. Unsafe imports/paths fail closed. Project instructions are not a sandbox; approved shell commands can modify files outside these scoped operations.

**Prompt customization:** Place `SYSTEM.md`, `SYSTEM_TEMPLATE.md` or `APPEND_SYSTEM.md` in `<workspace>/.pave/`, with `${XDG_CONFIG_HOME:-~/.config}/pave/` as fallback. `SYSTEM.md` wins over `SYSTEM_TEMPLATE.md` within a scope; project wins over user. `--system-prompt TEXT` and strict `--system-prompt-template FILE` override discovered system content but conflict with each other; `--append-system-prompt TEXT` overrides discovered append content. Templates support `{{root}}`; unknown placeholders fail for explicit files and are diagnosed with fallback for discovered files. Sources are bounded UTF-8 regular files read once at launch. Neither customization nor tool output replaces the mobile safety prompt or `AGENTS.md`.

## Keyboard and slash-command reference

| In the TUI | Action |
| --- | --- |
| `Return` (macOS) · `Enter` (other supported terminals) | Send when idle; queue a follow-up while working, without cancelling the active turn. Pasted Enter never submits. |
| `Option+Return` (macOS) · `Alt+Enter` (other supported terminals) · `/queue MESSAGE` | Same noninterrupting send/queue behavior as Return/Enter. |
| `Option+↑` (macOS) · `Alt+↑` (other supported terminals) | Restore the most recent queued prompt into the editor; otherwise navigate prompt history. Refuses rather than overwriting staged attachments. |
| `Option+Q` (macOS) · `Alt+Q` (other supported terminals) | Open live queue management without submitting or clearing the draft; `/queue` is the slash-command alternative. |
| `←` `→` · `↑` `↓` | Move by Unicode grapheme or wrapped visual row; history at first/last row |
| `Ctrl+P`/`Ctrl+N` · `Ctrl+R` | Explicit older/newer history · incremental reverse search (Return recalls on macOS; Enter elsewhere, Escape cancels) |
| `Ctrl/Option+←/→` (macOS) · `Ctrl/Alt+←/→` (other terminals) · `Ctrl+W` | Move or delete by word; editor draft stays intact during model output |
| `Ctrl+Z`/`Ctrl+Y` · `Ctrl+K`/`Ctrl+U` · `Option+Y` (macOS) / `Alt+Y` (other terminals) | Undo/redo a draft edit · kill after/before the cursor · yank killed text; bracketed paste is one undo step |
| `PgUp`/`PgDn` · mouse wheel · `Ctrl+Home`/`Ctrl+End` · `Option+O` (macOS) / `Alt+O` (other terminals) | Scroll the transcript, jump to its beginning/end, or expand/collapse the latest visible tool result |
| `Ctrl+C` · `Ctrl+D` | Close a picker or cancel account sign-in; in the composer, interrupt a turn without losing the draft or clear a nonempty idle draft; `Ctrl+D` exits when empty. |
| `/` then `Tab` | Search available slash commands; Return/Enter inserts a partial match or runs an exact command; Escape returns to the draft |
| `/setup` · `/model [PROVIDER[@API]/MODEL_ID]` | Connect an account without changing defaults, or configure the user default · choose the active conversation model/API across connected providers |
| `/cancel` · `/settings` | Stop the active request/command; edit typed project defaults for the next launch |
| `/queue [MESSAGE]` | Without text, manage pending prompts: run next, interrupt-and-run now, edit or cancel one. With text, queue while busy or send immediately when idle. |
| `/steer MESSAGE` | Explicitly interrupt the active turn and send this message next, ahead of queued follow-ups; send immediately when idle. |
| `/tools [NAME]` | List the tools actually offered to the model, or inspect one tool's description; shell availability follows `--allow-shell` and still requires per-command approval |
| `/context` | Inspect the actual model/route and selected branch; show provider-reported aggregate and modality usage and, only when explicitly configured, the context-window byte proxy. Limits are never inferred from model names; media payload bytes are counted but modality token cost remains unknown |
| `/usage` | Inspect recorded provider-reported token counts and cache/reasoning/modality details; private journals group the selected branch by provider/account/model/API route, while ephemeral conversations show only a combined measured total. Prices and subscription value are not estimated |
| `/retry` | Reissue the last user turn only if it made no tool calls; saved sessions retain the prior answer on an abandoned branch, while ephemeral answers are replaced; both requests may incur usage |
| `/hotkeys` | Display actual interactive keyboard shortcuts (including search, word editing, paste and tool expansion); headless CLI does not claim terminal keys work |
| `/new` · `/resume [ID|TITLE|PATH]` | Create a private workspace journal; list, search by title/ID, or reopen a same-workspace private journal |
| `/clear` · `/fresh` | Reset model context while preserving journal history/settings · rebuild the local agent from current context without changing the journal |
| `/rename TITLE` · `/label [TEXT]` · `/pin` | Save journal title/entry labels · toggle a journal pin in the private recent-session index |
| `/approval [MODE]` · `/thinking [LEVEL|default]` · `/tool enable|disable NAME` | Persist branch-local approval, thinking and tool availability; adapters translate only documented native thinking controls, not universal model support |
| `/attach PATH|clear` | Stage workspace-relative PNG/JPEG/WebP images or WAV/MP3/AAC/OGG/Opus/FLAC/M4A audio and MP4/WebM video for the next prompt (up to 8, 7 MiB per file, 10 MiB combined encoded data); audio/video require direct Gemini or Vertex GenerateContent |
| `/help` · `/entries` | Show descriptive commands · list journal entry IDs and metadata |
| `/tree` · `/branch ID` · `/fork [PATH]` | Search/select parent-linked entries · check out an exact entry ID · fork into a private journal or an explicit new file |
| `/compact` · `/quit` | Summarize older turns manually · exit |

On macOS, `/help` labels Meta as `Option` and Enter as `Return`; other supported terminals show `Alt` and `Enter`. Configure Option to send Escape/Meta in the terminal to use modified shortcuts. `/queue MESSAGE` remains available when it is not configured.


## Sessions and long-running turns

- **During a turn:** Network and approved commands leave the editor responsive. Later prompts queue until their turn starts; `/cancel` stops the active turn without dropping queued prompts or the draft. Failed/cancelled partial text is removed.
- **Scrollback:** Memory retains the newest 10,000 logical rows; a saved journal retains its complete durable history. `/resume` restores that history without replacing the editor draft. New startup conversations are ephemeral; `/new` confirms before discarding an unsaved conversation or staged media attachments.
- **Private journals:** `/new` creates an **unencrypted** append-only JSONL file under `${XDG_STATE_HOME:-~/.local/state}/pave/sessions/<SHA-256 of canonical workspace path>/<random>.jsonl` (0700 directories, 0600 files). `/resume` searches at most 100 recent journals for the current workspace and accepts only private owned regular files, including explicit paths. `/pin` appends a journal metadata event and updates the private recent-list index. `--session PATH` reopens a chosen journal.
- **Context and metadata:** Model/API, approval mode, tool availability, thinking-level metadata and entry labels are typed journal entries, never provider messages. Model/API, approval, tool, thinking and label state follows the selected branch; titles and pins are session-wide. `/thinking` records this conversation's override; documented native adapters apply it, unsupported routes do not gain a thinking control, and user/project model defaults remain unchanged. `/model` saves model and effort only after final confirmation; canceling either picker writes neither setting. `/clear` appends a reset boundary, preserves earlier journal history and settings, and refuses while tool calls remain unresolved. `/fresh` rebuilds the local agent on the next prompt without writing to the journal.
- **Media and privacy:** Attachments are base64 data stored with the user journal entry, separate from its provider-message record; journals are unencrypted and may contain sensitive image/audio/video data. Pave displays media names/placeholders, never base64. Image-capable routes receive native image fields; direct Gemini and Vertex `GenerateContent` send supported audio/video as `inlineData`; every other route rejects audio/video before authentication or network I/O.
- **Branch metadata:** Provider/model/API changes belong to branches, not provider messages. `/resume`, `--session` and `/branch` restore selected branch settings; explicit `--provider`, `--model`, `--api` or `--endpoint` wins. Credentials and custom endpoints are not stored as model metadata, and a removed route must be overridden on reopen.
- **Recovery and privacy:** Reopening marks interrupted tool calls failed instead of rerunning them. Keep journals out of version control: Gemini 3 replay can persist model-issued thought text and signatures. `/compact` and automatic compaction append branch-local markers; the complete original journal remains intact. Matching signed provider state is retained only on its exact route/model. OpenAI Responses replays opaque route/model-bound compaction state; Anthropic replays signed content only for explicitly capable models on the official API-key route. Other matching signed prefixes fail closed rather than being generically summarized.
- **Tree picker:** `/tree` searches at most 1,024 parent-linked entries by their sanitized previews and IDs, highlights the active tip and keeps older ancestry reachable through `/branch ID`. `/branch` and `/fork` use the selected branch's durable messages and metadata; canceled selection leaves the branch and draft unchanged.

## Context budgeting

Set `--context-window TOKENS` only for the exact initial provider, model, API route and endpoint. Pave never infers limits from model names, and changing provider/model/API/endpoint disables that configured budget. `--context-window auto` fetches fresh pinned model metadata for Command Code (`context_length` plus the exact advertised API route), Google (`inputTokenLimit`), account-scoped OpenAI Codex (`context_window`), Devin (`maxTokens`), OpenRouter's authenticated `/models/user` (`context_length`, with optional `top_provider.max_completion_tokens` kept as output metadata), or Anthropic (`max_input_tokens` from its direct API listing and sole registered Messages route). OpenRouter fields follow its [official model-list reference](https://openrouter.ai/docs/api/api-reference/models/list-all-models-and-their-properties). The reported OpenRouter output maximum caps the heuristic output reserve when it is lower; it is not the context window or a local tokenizer. Anthropic signed compaction additionally requires both listed compaction capabilities and the official API-key endpoint. Nonregistered endpoint overrides, missing limits and unsupported values fail closed. Devin `tokenizerType` is provider-reported display metadata only, not a local tokenizer. `/models`, `/model` and setup pickers show provider-reported context, tokenizer and compaction metadata where available. `/context` shows the token window, output reserve and conservative UTF-8 request-byte proxy separately; the proxy is not a tokenizer or reported token count. Image, audio and video payload bytes contribute to the proxy, while modality token costs remain unknown.

Before each request, Pave trims oversized text tool results only in the provider-facing copy, then summarizes older complete turns in bounded chunks when needed. It keeps the newest user turn and its attachments, preserves tool-call/result adjacency, and appends a branch-local compaction marker only after every summary succeeds; the append-only journal and full tool outputs remain unchanged.

The direct OpenAI API-key Responses route calls `/responses/compact` and replays opaque returned items only for the matching provider/route/model. The direct Anthropic API-key Messages route uses the compaction beta only when the exact live model listing advertises both required capabilities; it stores and replays the signed block first in the message list with the same system prompt and tools. Custom endpoints never receive the signature or beta header. Other matching signed state fails closed rather than being generically summarized. See [compaction behavior and limits](compaction.md).

Without an explicit window, `/compact` has no local byte-proxy preflight; the legacy generic path uses one unbounded summary request, while native OpenAI Responses or capability-advertised Anthropic compaction may be rejected by the provider if its input is too large. A prompt/system/tool schema that already exceeds the byte allowance cannot be compacted without an older safe turn.


## Mobile workspace tools

Use `mobile_project` without arguments to inventory stacks. For framework command previews, supply the exact reported project `subroot` and `platform` (`ios` or `android`); Xcode filenames remain evidence, not runnable scheme choices. A Flutter/React Native host folder is not a separate native app selection; inventory never executes a command.

With `--allow-shell` and a private session on a Mac with Xcode, use `xcode_preflight` in order: `action=schemes` with the exact scanned Xcode bundle `subroot`, `action=destinations` with one returned `scheme`, then `action=build` or `test` with that scheme and a returned iOS Simulator `destination` UUID. Each phase requires a distinct interactive approval; no SDK, simulator runtime or successful build is assumed. Headless runs cannot approve these actions.
An opt-in local macOS Xcode 27 disposable-project run completed an iOS Simulator **build** with signing disabled after separate scheme and destination discovery; it did not boot a simulator, deploy an app or run a simulator test. CI still runs only fake-Xcode safety regressions, not a real simulator build or test.
For an actual compatible device inventory, separately approve `xcode_preflight action=simulators` with that same `subroot` and `scheme` after successful destination discovery. It runs `xcrun simctl list devices available -j` and shows only available iOS Simulator devices also accepted by Xcode for that scheme; UUID, runtime, name and current Booted/Shutdown state are not an authorization to boot, deploy or test. Missing Xcode/runtime and headless approval still produce no available choice.
For Android inventory, select the exact Gradle settings directory as `subroot` and separately approve `android_devices action=avds` (`emulator -list-avds`) and `android_devices action=devices` (`adb devices`) in a private `--allow-shell` interactive session. ADB may start its local server and use your configured ADB identity. Configured AVDs are not running devices or proof of a usable system image; only attached `device`-state emulator serials are shown as ready. Offline/unauthorized transports are unavailable, and physical device serials are withheld. No SDK is installed, emulator booted, serial chosen, or test run by inventory. A manual local SDK run found three configured AVDs and no attached ADB device.
With `--allow-shell` and a private interactive session, `mobile_check` previews and separately approves each focused command. Select `stack=swiftpm, action=discover` on an exact package `subroot`, then `action=run, target=<returned test>`; select `stack=gradle, action=tasks` on an exact settings directory, then `action=run, target=:module:task` from that listing. SwiftPM refuses package dependencies that could resolve implicitly; Gradle uses an installed system `gradle --offline`, not a downloading wrapper. Discovery results are session-bound and invalidated when the selected manifest changes. For `stack=flutter`, use `action=analyze` or `action=test, target=<workspace-relative test/*.dart>`; checks use `--no-pub` and require existing dependencies. For `stack=node`, use an existing RN/Expo `action=test` or `lint` and an observed lockfile; conflicting lockfiles require `manager=npm|pnpm|yarn`. Scripts can execute arbitrary project code; no `npx` or package installation occurs. A missing toolchain returns its real command failure, not a successful check.
Failed approved SwiftPM, Xcode, Gradle, Flutter and RN/Expo checks retain their actual exit and raw bounded output, then highlight checked workspace source locations (`path:line:column`) for Swift, Kotlin/Java, Dart or JS/TS respectively. Gradle hints stay in the selected module; dependency/generated, external, symlinked or malformed paths are not promoted to source locations. Truncated output remains incomplete. Diagnosis runs no extra command and edits no files.


**Shell safety:** model-requested shell execution is off by default. `--allow-shell` advertises shell commands but still asks for **each** command in an interactive terminal, even with `--approval-mode yolo` or a per-tool allow; noninteractive runs deny shell execution. Approved commands are **not sandboxed** and can access files outside the workspace. Check the impact preview and exact command before approving it; Pave does not install mobile SDKs, sign apps or deploy to devices for you.
