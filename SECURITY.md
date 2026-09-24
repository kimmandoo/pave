# Security policy

## Supported code

The latest `main` branch receives security fixes. No stable release series is maintained yet.

## Report privately

Do not post credentials, session journals, private repository content, or exploit details in a public issue. Email the maintainer at `mingyu5675@gmail.com` with a minimal reproduction, the affected version/commit, potential impact, and a safe way to contact you. Please allow time to investigate and coordinate a fix before public disclosure. Do not run an exploit against repositories you do not own.

## Runtime trust boundary

Pave sends prompts, selected file contents and tool output to the configured model provider. Session journals are unencrypted; keep them outside version control. Workspace file tools restrict paths, but shell commands approved with `--allow-shell` are **not sandboxed** and can access files outside the selected workspace. Shell execution remains disabled by default, and approval is per command in an interactive terminal.

OAuth sign-in is explicit (`--login anthropic`); credentials live outside the workspace in a private `${XDG_CONFIG_HOME:-~/.config}/pave/oauth.json` (0700 directory, 0600 file), not the journal. The store is **not encrypted**: protect local account access and backups, and run `pave --logout anthropic` to delete the local grant (revoke it with the provider separately). Refresh is serialized under a private lock. OAuth inference refuses `--endpoint` overrides so bearer tokens cannot be redirected to an arbitrary HTTPS host. API-key providers can target a custom endpoint only when explicitly configured; treat that endpoint as a recipient of your key, prompts and file contents.
