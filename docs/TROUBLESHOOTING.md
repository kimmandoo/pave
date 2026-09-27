# Troubleshooting


### [2026-09-27] R7 integration exposed OCaml binder and record inference errors

- **Context / Symptom:** Initial forced tests/builds reported syntax errors from using `effect` as an identifier, a malformed task-dispatch `try ... with`, missing `Agent.create` callback arguments, and ambiguous artifact/job record fields. A fake OpenAI endpoint TUI smoke was also rejected with `remote endpoint overrides are disabled; define a custom provider in user settings`.
- **Root Cause:** `effect` is reserved in OCaml; overlapping record labels needed explicit types; integration edits omitted optional arguments and the local child-job unit terminator. Pinned remote APIs intentionally reject arbitrary endpoint overrides.
- **Solution:** Renamed binders, restored the task completion helper and `Agent.create` arguments, added explicit record annotations and an erasable unit argument. Reran the TUI smoke through the pinned loopback Ollama endpoint. The full forced suite, install build and `opam lint` passed.
- **Prevention / Reference:** Annotate shared record labels at artifact/job boundaries. Offline provider smokes must use a supported pinned local route or an explicitly configured custom provider, not override a pinned remote API.

### [2026-09-27] Linux x86_64 release test reported a missing OpenSSL signer

- **Context / Symptom:** The first v0.1.48 Linux x86_64 Actions attempt failed in `test_vertex_auth` with `Pave.Vertex_auth.Authentication_error("OpenSSL is required for Google service-account credentials")`; Linux aarch64 and both macOS jobs passed. Rerunning only the failed matrix job passed tests, native build, archive packaging and smoke without source changes; the complete release workflow then succeeded.
- **Root Cause:** Not isolated. `Vertex_auth.service_account_assertion` maps any `Unix.Unix_error` from its signer subprocess to the generic missing-OpenSSL message, so the failing run did not expose the underlying errno. The identical rerun passed.
- **Solution:** Reran the failed job using `gh run rerun 36302372130 --failed`; attempt 2 passed and published the release. No code workaround was added.
- **Prevention / Reference:** If it recurs, preserve the subprocess `Unix_error` rather than attributing every signer failure to an absent OpenSSL executable; inspect the specific job log before retrying.

### [2026-09-27] Appending the Apple helper hit a read-only release binary

- **Context / Symptom:** Release packaging failed at `cat "$helper" >> bundle/pave` with `Permission denied` after copying Dune's native executable.
- **Root Cause:** `cp` preserved the generated `main.exe` mode without the owner-write bit, so the copied binary could execute but could not be extended.
- **Solution:** Temporarily added owner-write permission before appending helper bytes and restored the read-only executable mode afterward. The extracted three-member archive passed `--help`, `--providers`, and the embedded helper's disabled-Apple-Intelligence diagnostic.
- **Prevention / Reference:** When extending a copied native executable, grant owner write only for the append operation, restore its executable mode, and smoke the extracted archive.

### [2026-09-27] Apple model picker had no selectable default

- **Context / Symptom:** The Apple provider appeared in first-run setup, but the model picker reported `Model listing unsupported for apple` and offered no model to save.
- **Root Cause:** The on-device route intentionally has no remote model roster, but discovery had no representation for its known OS-managed `default` ID.
- **Solution:** Added a credential-free local runtime-default discovery record for `apple@chat/default` and made the picker display its platform/readiness requirements without claiming a provider roster. The real setup flow listed and saved the default in isolated temporary configuration at 30×10 and 70×18 PTYs.
- **Prevention / Reference:** No remote listing does not mean there is no usable model selector. Register only the fixed OS-managed ID and label it as a local runtime default, not a provider listing.

### [2026-09-27] macOS cancellation fixture treated TCP reset as a hang

- **Context / Symptom:** The forced suite failed in `test_provider_http` with `Unix_error(ECONNRESET, "read", "")`, then timed out its parent-side disconnect assertion.
- **Root Cause:** The local HTTP fixture assumed a cancelled libcurl request always closed its TCP stream with orderly EOF; on macOS the peer can instead report `ECONNRESET`.
- **Solution:** Treated only `ECONNRESET` as the expected client disconnect in the fixture, while keeping the bounded wait and parent notification assertion. `opam exec -- dune exec test/test_provider_http.exe` passed.
- **Prevention / Reference:** Cancellation tests should accept EOF or connection reset as TCP disconnect outcomes without suppressing timeout, unexpected socket errors, or missing server notification.




### [2026-09-27] Swift helper entry point required library parsing

- **Context / Symptom:** Building the macOS Apple helper failed with `'main' attribute cannot be used in a module that contains top-level code`; Swift pointed at the file-scope constants and requested `-parse-as-library`.
- **Root Cause:** The helper uses an `@main` async entry point alongside file-scope constants, but Dune invoked `swiftc` in its default executable parsing mode.
- **Solution:** Added `-parse-as-library` to the conditional FoundationModels compiler rule. `opam exec -- dune build @install` then built the helper.
- **Prevention / Reference:** Keep `@main` Swift helper sources compiled with `swiftc -parse-as-library` when file-scope declarations are present.


### [2026-09-27] Vertex service-account signature helper stalled

- **Context / Symptom:** The first direct Vertex service-account ADC exchange did not finish while creating its OpenSSL RSA signature; the helper process remained waiting instead of returning a token.
- **Root Cause:** The parent/child pipe lifecycle did not guarantee the expected EOF and could block while moving the signing input/output between OCaml and OpenSSL.
- **Solution:** Replaced the ambiguous subprocess pipe handling with explicit `fork`/`dup2`/`execvp`, nonblocking parent I/O, bounded timeout/cancellation, and process cleanup. `test_vertex_auth` verifies the generated RS256 signature with OpenSSL and exercises auth resolution.
- **Prevention / Reference:** Close unused pipe ends in both processes and treat helper execution as a bounded, cancellable child process; do not rely on implicit EOF from inherited descriptors.

### [2026-09-27] OpenRouter model limits were discarded after parsing

- **Context / Symptom:** `/models/user` parsing found `context_length` and `top_provider.max_completion_tokens`, but fresh exact-model rows lost both fields before `--context-window auto` could use them.
- **Root Cause:** `Model_discovery.discover_raw` routed OpenRouter through the ID-only discovery projection, discarding its typed per-model capability metadata.
- **Solution:** Kept OpenRouter on the typed model-discovery path through capability registration. Missing/null metadata remains unknown, while invalid present limits reject the listing. Model-discovery regressions and a real auto-context TUI fixture proved the exact window and output cap.
- **Prevention / Reference:** Preserve typed model rows through provider discovery when limits/capabilities exist; project IDs only at genuinely ID-only boundaries. See the [OpenRouter model-list reference](https://openrouter.ai/docs/api/api-reference/models/list-all-models-and-their-properties).

### [2026-09-27] Basic JSON numeric parsing used Safe-only variants

- **Context / Symptom:** The new non-chat task response parser did not compile when its `Yojson.Basic.t` branches matched `Intlit`; embedding usage parsing also expected fields absent from the actual response shape.
- **Root Cause:** `Intlit` belongs to the Safe JSON representation, not `Yojson.Basic.t`, and the embedding usage fields were read from the wrong object.
- **Solution:** Added strict numeric parsers for Basic JSON values and read the documented embedding usage fields at their proper response location. Non-chat parser regressions, the forced suite, install build and opam lint passed.
- **Prevention / Reference:** Keep JSON constructors aligned with the selected Yojson module and validate provider usage against its documented envelope rather than borrowing a Safe-parser pattern.

### [2026-09-27] Isolated Codex smoke hid opam state and missed stdin curl config

- **Context / Symptom:** The first isolated CLI smoke set `HOME` to a fresh directory and `opam exec` failed with `Opam has not been initialised, please run opam init`. After retaining the normal home, device login passed but the Codex model-listing smoke reported `request failed or timed out`.
- **Root Cause:** opam used `HOME` to locate its initialized switch. The login transport invoked `curl --config FILE`, while model discovery invoked `curl --config -` and streamed its configuration on stdin; the task-owned fake curl handled only file paths.
- **Solution:** Kept the initialized home and isolated Pave state with `XDG_CONFIG_HOME`. Extended the fake curl fixture to accept both config input modes while allow-listing only pinned auth, token and model endpoints; device login, locked refresh, account-bound listing and account-mismatch refusal then passed.
- **Prevention / Reference:** Isolate application data with `XDG_CONFIG_HOME` without replacing the toolchain's `HOME`; fake-curl fixtures for Pave's HTTP adapters must support both config-file and stdin config invocation.
### [2026-09-27] R4 credential integration exposed compile and TUI feedback failures

- **Context / Symptom:** The auth-store migration build found `Unbound record field content`, incomplete credential pattern matches for `selection_id` and `Account_api_key`, non-erasable optional CLI arguments, and an untyped binding record label. Later runs found a missing binding in configured model selection, a unit-return mismatch, unused picker/settings bindings, a shadowed `path` in `test_oauth_store`, and a forked masking fixture whose child lacked placeholder allocations (`curl` exit 52). A real one-second Copilot device-grant PTY returned to Connect Provider without visible timeout feedback.
- **Root Cause:** Account selectors and the new credential variant changed record inference and exhaustiveness; labeled-only optional functions could not erase their optional arguments; the test helper shadowed a standard JSON utility; the masking fixture forked before dynamic placeholders were registered; `Tui.alert` was overwritten by the immediately reopened chooser.
- **Solution:** Annotated message and binding records, completed match branches and added erasable unit terminators; carried selectors through refresh and added explicit `Cloud_identity` auth for Vertex/Bedrock; fixed the fixture names and seeded child masking state; passed sign-in outcomes via chooser `initial_status`. The forced suite, install build, opam lint and isolated CLI/PTY smokes passed.
- **Prevention / Reference:** Keep provider identity, local selection ID and grant type distinct; type shared record labels and make optional resolver signatures erasable; use chooser status for failures that must remain visible after a modal returns; initialize forked-fixture state after fork when mutations are process-local.

### [2026-09-27] Fireworks pinned route exposed a stale loopback fixture

- **Context / Symptom:** The first forced suite run failed in `test_provider_http` with `Pave.Provider_error("Fireworks API key requires its pinned Chat endpoint")`. After excluding Fireworks from that arbitrary loopback route table, the fixture's later request numbering still failed.
- **Root Cause:** Fireworks now uses a dedicated adapter that rejects endpoint overrides; the older shared fixture replaced each generic provider endpoint with a local HTTP server and assigned mock responses by provider position.
- **Solution:** Removed Fireworks from the loopback provider list, kept its pinned-endpoint fake-curl coverage in `test_r3_routes`, and shifted the later mock response indices. The targeted HTTP fixture, forced suite, install build and opam lint passed.
- **Prevention / Reference:** Keep route-pinned transports on fixed-endpoint fake-curl fixtures rather than arbitrary endpoint-overriding loopback tests.

### [2026-09-27] Chat SSE accepted completion without a terminal finish reason

- **Context / Symptom:** The forced suite showed OpenAI-compatible Chat SSE ending in `[DONE]` without any `finish_reason` could still return a successful, incomplete assistant message. Strict Anthropic envelope checks also exposed provider fixtures and synthetic Command Code/GitLab replay responses that omitted Anthropic's required message type/assistant role.
- **Root Cause:** The Chat stream finalizer checked only for `[DONE]`, not a validated finish reason. The stricter Anthropic parser correctly required a complete `message` envelope, but internal response reconstruction and old fixtures did not preserve that envelope.
- **Solution:** Required `finish_reason` before accepting Chat `[DONE]`; updated Anthropic stream/response fixtures and synthetic replay envelopes to carry `type=message` and `role=assistant`. Added invalid-finish and streamed-tool-side-effect regressions. The forced suite passed.
- **Prevention / Reference:** Treat a transport terminator as framing, not proof of a complete provider response; validate the protocol's semantic terminal event before committing assistant state or dispatching tools.

### [2026-09-27] Redirect errors duplicated their HTTP status

- **Context / Symptom:** The local compatibility regression reported `HTTP 302 (HTTP 302)` instead of its stable generic `HTTP 302`. Its cleanup also asserted that a deliberately killed fixture server exited normally, which masked the original provider assertion with `Finally_raised`.
- **Root Cause:** Generic non-categorized statuses were given a second status suffix, and fixture cleanup conflated expected SIGKILL reaping with a successful server exit.
- **Solution:** Kept generic statuses as `HTTP N`, appending status context only to recognized classifications; reaped the fixture child without asserting normal exit. The local compatibility regression then passed.
- **Prevention / Reference:** Keep generic provider errors backward-compatible and make test cleanup preserve the primary failure rather than replacing it with expected process teardown.

### [2026-09-27] Usage provenance screen failed the install build

- **Context / Symptom:** `opam exec -- dune build @install` reported a usage-branch syntax error, then rejected the aggregation pattern because `Session.usage_by_route` returns `(route_key, usage)` map bindings rather than flattened tuple rows. The first combined detail row also wrapped a token label at 100 columns.
- **Root Cause:** The usage footer was not appended to the complete journal/ephemeral match expression, and the fold destructured a map binding as a four-field row.
- **Solution:** Appended the common unknown-price footer after the full match, destructured `(key, usage)` bindings, and rendered cache/reasoning details as separate short rows. The forced suite, install build, opam lint and a 100×24 PTY `/usage` smoke passed with provider/account/route/model, totals, cache/reasoning fields and unknown-cost disclaimer.
- **Prevention / Reference:** Match the actual collection's key/value type at UI aggregation boundaries, and exercise usage output with real terminal dimensions rather than relying on compilation alone.

### [2026-09-26] Anthropic compaction integration did not compile

- **Context / Symptom:** `opam exec -- dune build @install` reported a syntax error in `Model_discovery.discover`, `Unbound value parse` in the new Anthropic compaction helper, then an undefined `retrieved_at` field in the listing-source record.
- **Root Cause:** The provider-specific Command Code and Devin model projections were missing their typed `Result.map` wrappers; the response parser used by `complete` is local to that function; and the newly populated source record omitted its required retrieval timestamp.
- **Solution:** Restored typed model-row projections, supplied `retrieved_at`, and called `Anthropic_wire.parse_compaction_response` directly from the native compaction helper. `opam exec -- dune build @install` passed.
- **Prevention / Reference:** Keep discovery projections explicit for each provider row type, include all source-provenance fields, and do not reuse function-local parsing helpers across module-level functions.

### [2026-09-26] R1 model identity migration left stale build assumptions

- **Context / Symptom:** `opam exec -- dune build @install` rejected model-picker status strings where the TUI required `string option`; later failures reported `string option` where `Option.value` required `string`, an unbound route `name`, and old tuple patterns with extra fields.
- **Root Cause:** CLI and picker consumers still treated account selection and context/capability targets as the previous tuple/string representation after they became optional account IDs and canonical `Model_identity.t` values. Shared record labels also left coordinator snapshots inferred as requests.
- **Solution:** Matched `Some`/`None` account precedence explicitly, annotated route/snapshot/identity values, and rendered exact canonical identities in context and capability status. The install build passed.
- **Prevention / Reference:** Migrate every consumer when replacing tuple identity with a typed record; annotate ambiguous record values and distinguish `string option` from `string` fallbacks.

### [2026-09-26] Public model listings rejected configured inference keys

- **Context / Symptom:** Production model discovery for OpenCode Zen, OpenCode Go, and Charm Hyper failed even though their pinned model endpoints were public and the configured API key was used only for inference.
- **Root Cause:** `Anonymous` discovery policy accepted only a missing credential; passing a configured `Api_key` caused rejection before the public listing transport, which intentionally omitted authorization headers.
- **Solution:** Accepted a valid configured API key for anonymous discovery without forwarding it or binding it as an account. The listing remains provider-wide; transport fixtures assert no authorization header and production discovery asserts provider-listing provenance.
- **Prevention / Reference:** Keep listing access policy distinct from inference credential configuration; public listing adapters must ignore configured secrets rather than send them.

### [2026-09-26] Strict duplicate rejection exposed stale success fixtures

- **Context / Symptom:** The first forced suite run failed in Google pagination, local Chat Completions, and public provider listing tests after duplicate model IDs became invalid.
- **Root Cause:** Several fixtures still used repeated IDs as successful rows to pin the previous deduplication/overwrite behavior, including a duplicate across Google pages.
- **Solution:** Replaced duplicate rows in valid success fixtures with distinct IDs and kept duplicates only in explicit invalid-response assertions. Focused provider regressions and the forced full suite passed.
- **Prevention / Reference:** Treat duplicate IDs as a rejected listing response; test pagination with unique success rows and a separate explicit duplicate failure.

### [2026-09-26] Manual compaction could leave the retained prompt over budget

- **Context / Symptom:** Inspection found that an explicitly bounded manual summary could fit its own summary request while the resulting system prompt, tools, summary and newest turn still exceeded the configured prompt allowance. The automatic path checked this projected context; `/compact` did not.
- **Root Cause:** Manual compaction appended its journal marker immediately after summarization without estimating the projected post-compaction request.
- **Solution:** Added provider-facing tool-text trimming and a projected-context check against the active system prompt and tool schemas before `Session.compact`. A local TUI scenario returned an oversized fixed-prompt failure, kept the journal prefix unchanged, wrote no compaction marker and exited cleanly.
- **Prevention / Reference:** Validate the summary plus retained current turn against the active request contract before committing a branch-local marker; a bounded summary request alone does not prove the next model request fits.

### [2026-09-26] Command Code model route annotations were omitted

- **Context / Symptom:** A fake pinned model listing returned `supported_endpoints` such as `/chat/completions` and `/responses`, but `--models` printed the reported context window without any API-route names.
- **Root Cause:** The provider advertises relative route identifiers while Pave's registered routes store full HTTPS request URLs; consumers compared the two strings directly.
- **Solution:** Added an exact Command Code route-identifier mapping and used it for CLI annotations, model-picker route filtering and `--context-window auto` validation. Unknown route values remain unmatched. The Command Code route regression, fake-provider CLI checks and 90×24 TUI picker smoke passed.
- **Prevention / Reference:** Compare provider route identifiers through a provider-specific exact mapping; never treat a relative API path as equal to a canonical request URL.

### [2026-09-26] Typed model discovery helper was declared after its caller

- **Context / Symptom:** `opam exec -- dune build @install` failed with `Unbound value discover_generic_models` while typed generic listings were added.
- **Root Cause:** OCaml resolves module-level function names in source order; the ID-only dispatcher called the newly extracted typed helper before its definition.
- **Solution:** Moved the shared typed HTTP-listing helper before the dispatcher and projected IDs at the legacy boundary. `opam exec -- dune build @install` then passed.
- **Prevention / Reference:** Keep source helpers before their consumers; preserve one typed row parser and project IDs only at ID-only call sites.

### [2026-09-25] Minimal explicit context window could not fit fixed prompt

- **Context / Symptom:** A headless request with `--context-window 8192` failed before provider I/O because the byte proxy's 4,096-byte prompt allowance was smaller than Pave's system instructions and tool schemas. The old error incorrectly attributed this to the current user turn.
- **Root Cause:** The configured model window is a token count, but Pave deliberately compares a conservative UTF-8 byte proxy against its prompt allowance; the minimum accepted window does not guarantee that fixed prompt content fits.
- **Solution:** Changed the fail-closed diagnostic to identify the system instructions, tool schemas or current prompt as the possible cause. A post-fix local CLI run returned that exact explanation without contacting the provider.
- **Prevention / Reference:** `/context` shows the byte proxy and token allowance separately. Set an explicit larger window for the exact route or reduce prompt/tool context; model names do not select limits.

### [2026-09-25] Codex image serializer inferred the wrong record type

- **Context / Symptom:** `opam exec -- dune runtest --force` failed to compile `lib/provider/transports/codex_wire.ml` with `This expression has type attachment; There is no field arguments within type attachment`.
- **Root Cause:** The `List.iter` and pending-call mapper destructured tuples without fixing the first element's record type; annotating the containing list alone did not resolve OCaml's field inference.
- **Solution:** Annotated tuple-bound call values as `tool_call` in both serializers. `opam exec -- dune build lib/pave.cma` then completed.
- **Prevention / Reference:** Annotate tuple-bound record values directly at wire-serialization boundaries when inference remains ambiguous.

### [2026-09-25] Managed session filenames diverged from journal IDs

- **Context / Symptom:** The private-store regression found that setting a managed session title did not make it searchable, and pinning could reject a session created by the store.
- **Root Cause:** `Session_store.create` and `fork` generated the filename ID separately from the session header ID, while title and pin ownership checks require them to match.
- **Solution:** Added managed session constructors that derive private journal filenames from the generated header ID. `opam exec -- dune exec test/test_session_store.exe` passed title search, pin/unpin, fork lineage, and title inheritance checks.
- **Prevention / Reference:** Derive a managed journal's filename and header identity from the same generated ID.

### [2026-09-25] Exact slash commands selected autocomplete instead of running

- **Context / Symptom:** In a 70×18 PTY, typing `/new` and pressing Return only accepted the visible command hint; the session did not start until another submit action.
- **Root Cause:** `Tui.read` handled visible slash hints before submission even when the draft exactly matched a command.
- **Solution:** Exact command matches now submit on Return while partial prefixes still autocomplete. The PTY exercised `/new`, `/pin`, `/tree`, `/fork`, `/resume`, `/clear`, `/fresh`, and `/quit` without trailing spaces.
- **Prevention / Reference:** Preserve completion for partial commands and execute exact matches on Return; verify this through a real TUI PTY.

### [2026-09-25] System Dune did not use project switch dependencies

- **Context / Symptom:** Running `dune build @install` directly failed with `Library "yojson" not found`, although the repository's opam switch contained Yojson.
- **Root Cause:** The shell selected Homebrew Dune at `/opt/homebrew/bin/dune` without activating the repository's `_opam` switch environment.
- **Solution:** Ran the build through `opam exec -- dune build @install`; it then completed successfully.
- **Prevention / Reference:** Run Dune commands as `opam exec -- dune ...` so compiler and libraries come from the project switch.

### [2026-09-25] Multiline approval preview passed a control character to Notty

- **Context / Symptom:** The first 70×18 fake-provider PTY failed when a tool approval opened with `Invalid_argument("Notty: control character: U+0A, \"\\n\"")`.
- **Root Cause:** Approval layout passed the newline-separated preview body to Notty's single-line text measurement function.
- **Solution:** Split the preview into lines and measured each wrapped line independently. A regression now checks multiline approval row counts; the 70×18 write and shell approval PTY then rendered and denied both actions without side effects.
- **Prevention / Reference:** Never pass line-feed characters to `Notty.I.string`; split terminal content before measuring its display width.

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
