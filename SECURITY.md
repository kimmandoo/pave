# Security policy

## Supported code

The latest `main` branch receives security fixes. No stable release series is maintained yet.

## Report privately

Do not post credentials, session journals, private repository content, or exploit details in a public issue. Email the maintainer at `mingyu5675@gmail.com` with a minimal reproduction, the affected version/commit, potential impact, and a safe way to contact you. Please allow time to investigate and coordinate a fix before public disclosure. Do not run an exploit against repositories you do not own.

## Runtime trust boundary

Pave sends prompts, selected file contents and tool output to the configured model provider. Session journals are unencrypted; Codex sessions additionally contain provider-issued encrypted reasoning payloads, and Gemini 3 sessions may contain full model-issued native parts, including thought text and signatures, for correct continuation. Keep journals outside version control and protect backups. Workspace file tools restrict paths, but shell commands approved with `--allow-shell` are **not sandboxed** and can access files outside the selected workspace. Shell execution remains disabled by default, and approval is per command in an interactive terminal.

Sign-in is explicit: browser grants (`--login anthropic`, `--login openai-codex`, `--login openrouter`) or GitHub Copilot's `--login github-copilot` public device code. OpenRouter exchanges PKCE for a durable API key rather than a renewable access token. Copilot requests only `read:user`, accepts only public github.com grants without unsupported expiry/refresh semantics, and pins OAuth inference to `https://api.githubcopilot.com/chat/completions` with two explicit model IDs; Enterprise and other Copilot APIs are unavailable. Credentials live outside the workspace in `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` (0700 directory, 0600 file), not the journal. The store is **not encrypted**: protect local account access and backups, and run `pave --logout PROVIDER` to delete a local grant or key (revoke it with the provider separately). Refresh is serialized under a private lock where supported. OAuth-derived credentials cannot be redirected with `--endpoint`; Codex also pins the account ID and enterprise residency from its JWT to the official HTTPS inference endpoint. An explicitly configured API-key provider may use a custom endpoint; treat it as a recipient of the key, prompts and file contents.

Interactive `/login` temporarily exits the alternate screen so browser URLs or a GitHub verification code are visible and copyable in the normal terminal; terminal scrollback may retain them, but Pave does not journal them or the resulting grant. `/model` clears any previous custom endpoint override when switching and discards opaque Codex/Gemini continuation state before sending prior messages to another protocol or model. Model providers still receive the visible conversation context after a switch.

Typed user/project `settings.json` files select defaults and may disable shell tools, but cannot enable them: `--allow-shell` and per-command approval remain required. Project settings and discovered `AGENTS.md` instructions may be repository-controlled; inspect them before trusting a workspace. Relative `@` imports are size/depth-bounded and reject symlinks or escape paths. Path-scoped `.pave/rules` are not yet attached to file-tool operations. Neither settings nor instructions are a sandbox or a substitute for reviewing model-requested actions.

`--models` sends the chosen provider's existing API key or Copilot device grant only to its pinned listing endpoint (or asks unauthenticated local Ollama for tags). It does not follow listing redirects or accept `--endpoint` overrides; external catalog entries are informational and can include model IDs whose inference routes Pave does not implement. A cancelled tool command is terminated with its process group and journaled as interrupted, but commands already run may have had side effects and are **not** rolled back.

## Native self-update

`pave update` runs only from a regular executable named `pave` with a matching `.native-install` marker beside its installed license notices. It runs the `install.sh` text compiled into that executable, not a newly downloaded shell script; it installs to the running binary's own directory and ignores inherited destination/version overrides. The installer fetches only HTTPS GitHub Release assets, verifies the downloaded archive against the release's SHA-256 manifest and checks archive entries before replacing the binary by rename. A failed checksum does not replace the executable. Old native installs need a fresh installer run to acquire the marker; source/opam or other package-manager installs must be updated using their own manager.

`pave update --check` reads a bounded, HTTPS-only GitHub release metadata response, compares its published tag to the version embedded at release build time and does **not** modify the installation. A network/API/rate-limit error is reported rather than interpreted as “up to date”; the release metadata is informational, not a second integrity signature.

The manifest and archive come from the **same release origin**: SHA-256 detects corruption or a mismatch, but it is not an independent signature against a compromised release account. The marker indicates install layout, not a security boundary against someone who can already modify the installation directory. Inspect a release and the installer before first installation, protect the writable binary directory and use `PAVE_VERSION` with the standalone installer when a specific published version is required.
