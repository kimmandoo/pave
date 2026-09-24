# Security policy

## Supported code

The latest `main` branch receives security fixes. No stable release series is maintained yet.

## Report privately

Do not post credentials, session journals, private repository content, or exploit details in a public issue. Email the maintainer at `mingyu5675@gmail.com` with a minimal reproduction, the affected version/commit, potential impact, and a safe way to contact you. Please allow time to investigate and coordinate a fix before public disclosure. Do not run an exploit against repositories you do not own.

## Runtime trust boundary

Pave sends prompts, selected file contents and tool output to the configured model provider. Session journals are unencrypted; Codex sessions additionally contain provider-issued encrypted reasoning payloads, and Gemini 3 sessions can contain opaque model-issued thought signatures for correct continuation. Keep journals outside version control and protect backups. Workspace file tools restrict paths, but shell commands approved with `--allow-shell` are **not sandboxed** and can access files outside the selected workspace. Shell execution remains disabled by default, and approval is per command in an interactive terminal.

Sign-in is explicit: browser grants (`--login anthropic`, `--login openai-codex`, `--login openrouter`) or GitHub Copilot's `--login github-copilot` public device code. OpenRouter exchanges PKCE for a durable API key rather than a renewable access token. Copilot requests only `read:user`, accepts only public github.com grants without unsupported expiry/refresh semantics, and pins OAuth inference to `https://api.githubcopilot.com/chat/completions` with two explicit model IDs; Enterprise and other Copilot APIs are unavailable. Credentials live outside the workspace in `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` (0700 directory, 0600 file), not the journal. The store is **not encrypted**: protect local account access and backups, and run `pave --logout PROVIDER` to delete a local grant or key (revoke it with the provider separately). Refresh is serialized under a private lock where supported. OAuth-derived credentials cannot be redirected with `--endpoint`; Codex also pins the account ID and enterprise residency from its JWT to the official HTTPS inference endpoint. An explicitly configured API-key provider may use a custom endpoint; treat it as a recipient of the key, prompts and file contents.

Interactive `/login` temporarily exits the alternate screen so browser URLs or a GitHub verification code are visible and copyable in the normal terminal; terminal scrollback may retain them, but Pave does not journal them or the resulting grant. `/model` clears any previous custom endpoint override when switching and discards opaque Codex/Gemini continuation state before sending prior messages to another protocol or model. Model providers still receive the visible conversation context after a switch.
