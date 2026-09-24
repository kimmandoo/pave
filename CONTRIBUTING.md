# Contributing to Pave

Pave targets iOS, Android, Flutter, and React Native repositories. The port is in progress; check `TASKS.md` and `docs/DESIGN_RULES.md` before proposing a capability as complete.

## Report an issue

Open a GitHub issue with the operating system, OCaml version, installation method, terminal type, a minimal reproduction, the expected behavior, and the observed output. Remove API keys, private source code, and session transcripts before sharing logs. For a security issue, use `SECURITY.md` instead of a public issue.

## Propose a change

1. Create a branch from `main`; keep the change focused and include a consumer-visible regression when fixing a bug.
2. Follow `README.md` to install dependencies. Run `opam exec -- dune runtest --force` and `opam exec -- dune build @install` on a supported POSIX host. CLI/TUI changes also need a real terminal or PTY interaction check.
3. Update `README.md` for changed usage, `docs/DESIGN_RULES.md` for intentional contract changes, and add a past-tense `type(scope): description` entry under the current date in `CHANGELOG.md`.
4. Open a pull request describing behavior, verification, platform limitations, and any performance or resource impact. CI runs on macOS and Linux. Do not add credentials or session journals to the repository.

Contributions must be yours to license under the repository's MIT license, with third-party notices retained where required.
