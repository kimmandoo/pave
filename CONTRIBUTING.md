# Contributing to Pave

Pave targets iOS, Android, Flutter, and React Native repositories. The port is in progress; check `TASKS.md` and `docs/DESIGN_RULES.md` before proposing a capability as complete.

## Find the right module

| Area | Source | Test |
| --- | --- | --- |
| Executable and credentials | `bin/main.ml`, `bin/cli_auth.ml` | `test/auth/`, CLI transport fixtures |
| Native installer and self-update | `install.sh`, `bin/update.ml`, `bin/build/embed_installer.ml`, `bin/dune` | Installed-binary upgrade and corrupt-release smoke |
| Terminal rendering, slash routing and input | `bin/ui/`, `lib/ui/interaction.ml`, `lib/ui/composer.ml` | `test/ui/` |
| Conversation contracts and stream frames | `lib/core/` | `test/core/` |
| Provider registry and HTTP dispatch | `lib/provider/` | `test/provider/` |
| Provider-specific wire encoders/decoders | `lib/provider/transports/` | `test/provider/transports/` |
| Browser/device authentication and private credential store | `lib/auth/` | `test/auth/` |
| Agent loop and mobile prompt | `lib/agent/` | `test/agent/` |
| Durable conversation journal | `lib/session/` | `test/session/` |
| Workspace and shell tools | `lib/tools/` | `test/tools/` |

`lib/dune`, `bin/dune` and `test/dune` use unqualified subdirectories: **moving a file does not change its OCaml module name** (`lib/auth/oauth_flow.ml` remains `Pave.Oauth_flow`). Module names must be unique within a Dune stanza. For an OpenAI-compatible endpoint, add a descriptor in `lib/provider/provider_catalog.ml`; for a distinct protocol, put an encoder/decoder in `lib/provider/transports/` and route it explicitly in `lib/provider/provider.ml`. Provider-specific browser grants belong in `lib/auth/`, with CLI actions in `bin/cli_auth.ml`. Exercise authentication **and** completion/tool turns against an isolated transport fixture before listing a route as working. Keep tests beside the corresponding responsibility, not in a duplicate top-level module hierarchy.

In-session `/login` reuses `bin/cli_auth.ml` and temporarily releases the full-screen terminal in `bin/ui/tui.ml`; never print an authorization URL or device code into the alternate-screen renderer or put tokens in a journal. GitHub device grants belong in `lib/auth/github_copilot_oauth.ml`; pin their inference endpoint and supported model IDs before reading the credential. `/model` parsing and provider-route lookup live in `lib/ui/interaction.ml`, with selector boundaries in `test/ui/test_interaction.ml`. Switching a provider/model rebuilds the agent from the current journal (or retained in-memory messages) and must not forward Codex or Gemini opaque state across models or protocols. An uncredentialed user must be able to enter the UI to sign in before the first model request.

`bin/dune` generates the embedded installer module directly from the checked-in `install.sh`; edit the script once, then verify both the one-command installer and an installed binary's `pave update`. Keep the ownership marker, custom-directory behavior, checksum/archive validation and executable-last replacement aligned. Never invoke a remotely downloaded update script or self-update an opam-managed executable. `TASKS.md` contains the source-indexed parity inventory; unchecked entries are not implemented.

## Report an issue

Open a GitHub issue with the operating system, OCaml version, installation method, terminal type, a minimal reproduction, the expected behavior, and the observed output. Remove API keys, private source code, and session transcripts before sharing logs. For a security issue, use `SECURITY.md` instead of a public issue.

## Propose a change

1. Create a branch from `main`; keep the change focused and include a consumer-visible regression when fixing a bug.
2. Follow `README.md` to install dependencies. Run `opam exec -- dune runtest --force` and `opam exec -- dune build @install` on a supported POSIX host. CLI/TUI changes also need a real terminal or PTY interaction check.
3. Update `README.md` for changed usage, `docs/DESIGN_RULES.md` for intentional contract changes, and add a past-tense `type(scope): description` entry under the current date in `CHANGELOG.md`.
4. Open a pull request describing behavior, verification, platform limitations, and any performance or resource impact. CI runs on macOS and Linux. Do not add credentials or session journals to the repository.

Contributions must be yours to license under the repository's MIT license, with third-party notices retained where required.
