# Security policy

## Supported code

The latest `main` branch receives security fixes. No stable release series is maintained yet.

## Report privately

Do not post credentials, session journals, private repository content, or exploit details in a public issue. Email the maintainer at `mingyu5675@gmail.com` with a minimal reproduction, the affected version/commit, potential impact, and a safe way to contact you. Please allow time to investigate and coordinate a fix before public disclosure. Do not run an exploit against repositories you do not own.

## Runtime trust boundary

Pave sends prompts, selected file contents and tool output to the configured model provider. Session journals are unencrypted; keep them outside version control. Workspace file tools restrict paths, but shell commands approved with `--allow-shell` are **not sandboxed** and can access files outside the selected workspace. Shell execution remains disabled by default, and approval is per command in an interactive terminal.
