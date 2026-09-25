# Troubleshooting

### [2026-09-25] Chat wrappers double-wrapped the multimodal message list

- **Context / Symptom:** Compilation failed in Chat provider wrappers after they wrapped `Protocol.chat_messages_to_json` in another JSON `List`.
- **Root Cause:** The shared helper already returned the complete JSON list value; callers treated it as the list's element sequence and introduced an incompatible nested shape.
- **Solution:** Passed the helper result directly from all 26 Chat-completions wrapper boundaries. The full regression suite then compiled and passed.
- **Prevention / Reference:** Confirm helper return types at provider adapter boundaries before adding wire-level constructors.

### [2026-09-25] Chat image serialization reversed tool-result order

- **Context / Symptom:** `test_protocol` found the first serialized tool result did not retain its text projection when contiguous image-bearing results were grouped for Chat Completions.
- **Root Cause:** The reversed accumulator applied `List.rev_append` to an already reversed result group, inverting provider call order.
- **Solution:** Accumulated each serialized result directly into the reversed output while collecting its image blocks; the final reversal now preserves model call order. The protocol regression, full suite, install build and opam lint passed.
- **Prevention / Reference:** Assert exact wire ordering with results returned in reverse arrival order when serializing concurrent tool calls.


### [2026-09-25] Modified Enter keys did not queue follow-ups

- **Context / Symptom:** A 70×18 PTY sent Kitty's `ESC[13;5u` for Ctrl+Enter during a streaming turn, but no follow-up was queued.
- **Root Cause:** The installed Notty decoder did not recognize Kitty modified-Enter CSI-u sequences. `Terminal_input` also kept ESC followed by C0 Return/Line Feed pending until its escape timeout discarded both bytes.
- **Solution:** Decoded ESC-prefixed Return/Line Feed as Meta+Enter events, mapped Option/Alt+Enter to queued follow-ups, and added `/queue MESSAGE` as an explicit terminal-independent submission boundary. Help now labels Option/Return on macOS and Alt/Enter elsewhere. A local fake-provider PTY verified queue, dequeue, steering, draft retention and retry.
- **Prevention / Reference:** Exercise key bytes through the real TUI PTY and installed decoder; do not assume Kitty CSI-u support. On macOS, configure Option to send Escape/Meta or use `/queue MESSAGE`.

### [2026-09-25] Account handoff lost Ctrl+C and queued type-ahead

- **Context / Symptom:** Ctrl+C during browser sign-in after the TUI released the terminal could terminate Pave instead of restoring the screen. Input queued during the handoff could be replayed by the next picker.
- **Root Cause:** Notty release restores canonical terminal signal mode, where the default OCaml SIGINT disposition exits the process; catching `Sys.Break` at the caller alone cannot intercept that signal. Recreating the Notty terminal also leaves the kernel input queue intact.
- **Solution:** `Tui.suspend` temporarily maps SIGINT to `Sys.Break`, restores the terminal in `Fun.protect`, ignores SIGINT during reinitialization, flushes `TCIFLUSH`, then restores the prior handler. Account flows handle cancellation without exiting; chooser Ctrl+C returns only from the top picker, allowing cancellable model discovery to stop and join. Local 70×18 PTYs verified a canceled Ollama listing, OpenRouter loopback sign-in with a URL-restricted fake `curl`, discarded handoff type-ahead, composer input after both return paths and clean exit.
- **Prevention / Reference:** Exercise suspended terminal flows with a controlling PTY, local callback and local model-listing endpoint; a direct key-decoder test does not verify signal mode or the kernel input queue.

### [2026-09-25] Strict tool schema rejected an overbroad scoped-rules fixture

- **Context / Symptom:** The first `opam exec -- dune runtest --force` after tool argument validation failed in `test_scoped_rules` with an assertion and `Pave.Provider.Provider_error("curl failed (exit status 52)")`.
- **Root Cause:** The local HTTP fixture helper attached both `path` and `content` to every tool call, so `read_file` received a property absent from its `additionalProperties: false` schema. The fixture asserted on the resulting tool error and closed the connection before returning a response.
- **Solution:** The helper now emits only `path` for `read_file`; the isolated scoped-rules executable and full suite passed.
- **Prevention / Reference:** Keep fixture arguments aligned with the exact `Tools.definitions` schema; unknown fields are rejected before execution.


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

### [2026-09-25] Canceled turns leaked queued terminal updates

- **Context / Symptom:** A canceled worker could publish queued messages, streamed deltas, or tool/model phases after cancellation, and a detached producer from the prior turn could be mistaken for events from the next queued follow-up.
- **Root Cause:** `Turn_runner` originally queued untagged notices and checked only the runner's mutable current cancellation flag; the dispatcher had no way to distinguish a prior turn's producer from the active one.
- **Solution:** Tagged notices with a per-turn ID, accepted nonterminal events only from the owning worker thread, gated dispatch on that owner's cancellation flag, and serialized cancellation against successful completion. A normal return after a winning cancel now produces `Cancelled`; terminal outcomes still dispatch once. Red/green tests forced same-turn late notices, detached old-turn notices during a blocked follow-up, and normal return after cancellation. A local HTTP 70×18 PTY confirmed Ctrl+C removed provisional output and suppressed a delayed provider chunk.
- **Prevention / Reference:** Keep terminal mutations on the UI thread, require each event to match its owner ID/thread, and serialize cancel versus completion; do not clear the owner without delivering one terminal outcome.

### [2026-09-25] Concurrent Dune commands contended for the workspace lock

- **Context / Symptom:** Parallel `opam exec -- dune exec ...` invocations produced `Unexpected contents of build directory global lock file (_build/.lock). Expected an integer PID. Found:`; the generated lock file was empty.
- **Root Cause:** Independent Dune processes were launched concurrently in the same workspace and collided on the shared `_build` lock; no Dune process remained after the failure.
- **Solution:** Confirmed there was no active Dune process, removed only the generated stale `_build/.lock`, then ran focused tests, the full suite, install build and opam lint sequentially; all passed.
- **Prevention / Reference:** Run one Dune command at a time in this workspace. Put parallelism inside one Dune invocation instead of launching multiple Dune processes.
