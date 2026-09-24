# Troubleshooting

### [2026-09-25] OCaml Unix lacks a no-follow open flag

- **Context / Symptom:** `dune runtest` rejected `Unix.O_NOFOLLOW` as an unbound constructor while building typed user/project settings on the OCaml 5.5.1 switch.
- **Root Cause:** The OCaml `Unix.open_flag` API does not expose that POSIX flag even when the host OS supports it; checking only `Unix.stat` would instead follow a symlink.
- **Solution:** Validated files with `Unix.lstat` before and after a nonblocking `Unix.openfile`, and compared device/inode with `Unix.fstat` before reading; project settings updates reject symlinked lock paths and use a locked private temporary file plus atomic rename.
- **Prevention / Reference:** Do not assume every host `open(2)` flag is represented by the OCaml `Unix.open_flag` constructors; verify against the compiler's actual API and keep symlink checks on both sides of an open.

### [2026-09-24] Fragmented UTF-8 keystrokes disappeared in the terminal

- **Context / Symptom:** A real pseudo-terminal split a Chinese codepoint over separate `Unix.read` calls; the character did not appear in the draft even though each byte reached the process.
- **Root Cause:** Feeding each raw read directly to `Notty.Unescape.input` discarded an incomplete UTF-8 codepoint at the read boundary.
- **Solution:** Added `Terminal_input` to buffer codepoint bytes and escape sequences across reads before forwarding complete events to Notty. Invalid scalar sequences now become replacement characters; a truncated escape sequence times out without swallowing later typing.
- **Prevention / Reference:** Use a real PTY with fragmented byte writes when checking CJK input. A pasted or single-write string does not exercise the read boundary.

### [2026-09-24] Terminal renderer package rejected OCaml 5.5

- **Context / Symptom:** `opam install notty -y` failed: `notty → ocaml < 5.4` conflicted with the project's `ocaml-system = 5.5.1` switch invariant.
- **Root Cause:** The original Notty package had not declared compatibility with OCaml 5.5.
- **Solution:** Installed the maintained `notty-community` package instead. Its `notty-community.unix` renderer and event API supported the existing OCaml 5.5 switch.
- **Prevention / Reference:** Use `opam install notty-community` for this switch; do not relax the switch invariant to downgrade the compiler.

### [2026-09-24] Unbound tool-call record field during OCaml build

- **Context / Symptom:** `dune runtest` failed in the then-flat `lib/agent.ml` (now `lib/agent/agent.ml`) with `Error: Unbound record field name` at `call.name`.
- **Root Cause:** The compiler could not infer the module-qualified `Protocol.tool_call` record type from the `List.iter` lambda at that point.
- **Solution:** Annotated the lambda argument `(call : Protocol.tool_call)` so record-field resolution is unambiguous. Subsequent `dune runtest` passed.
- **Prevention / Reference:** Annotate arguments at module boundaries when OCaml record fields are accessed before type inference has resolved the record type.

### [2026-09-24] Concurrent Dune exec could not locate the project executable

- **Context / Symptom:** Running two `dune exec pave -- ...` smoke scenarios in parallel produced `Warning: As this is not the main instance of Dune it is unable to locate the executable "pave" within this project` and `Error: Program 'pave' not found!` in one process.
- **Root Cause:** The second Dune invocation did not own the workspace build lock and could not resolve the local executable by its public name.
- **Solution:** Ran the OpenAI and Anthropic CLI smoke scenarios sequentially. Both completed their streamed mobile-project tool-call cycles.
- **Prevention / Reference:** Serialize `dune exec` in the same workspace; parallelize independent tests through a single Dune invocation instead.
