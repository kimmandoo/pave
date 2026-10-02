# Troubleshooting

### [2026-10-03] `dup3`/`pipe2` weak symbols crash OCaml processes built against the macOS 27 SDK

- **Context / Symptom:** After rebuilding the local `_opam` switch on macOS 25.x with Xcode's MacOSX 27 SDK, every test that spawned a managed process failed with `Pave.Workspace_process.Error("process launcher exited before establishing a session")`. A minimal `Unix.fork ()` + `Unix.dup2` child died with `SIGBUS` (wait status `sig 10`); `Unix.pipe ~cloexec:true` showed the same.
- **Root Cause:** `configure`'s `AC_CHECK_FUNC` probes `dup3`/`pipe2`/`accept4`, all newly declared in the macOS 27 SDK under `__API_AVAILABLE(macos(27.0) …)`. Autoconf link tests succeed because the symbols are weak imports, so OCaml 5.5.1 compiles the `HAS_DUP3`/`HAS_PIPE2`/`HAS_ACCEPT4` code paths; at runtime on macOS < 27 the weak address is nil and the first call crashes.
- **Solution:** Rebuild the switch with the probes forced off: `ac_cv_func_dup3=no ac_cv_func_pipe2=no opam reinstall ocaml-compiler ocaml-base-compiler`. Verify `_opam/lib/ocaml/caml/s.h` shows `/* #undef HAS_DUP3 */` and `/* #undef HAS_PIPE2 */`.
- **Prevention / Reference:** Any OCaml switch built with a newer SDK on an older macOS must pin `ac_cv_func_*=no` for every `__API_AVAILABLE(macos(27.0))` libc addition. This is a toolchain/environment problem, not a repo bug — do not "fix" `workspace_process.ml` for it.

### [2026-10-02] `pave hub` subcommand rejected its own name as an argument

- **Context / Symptom:** `pave hub --session FILE --port N` exited with `unexpected argument: hub` and printed the hub usage line.
- **Root Cause:** `Arg.parse` was called on raw `Sys.argv`; unlike `Arg.parse_argv`, it does not skip `argv[0]`-relative dispatch, so the literal `hub` token arrived as an anonymous argument.
- **Solution:** Parse hub options via `Arg.parse_argv (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))`.
- **Prevention / Reference:** Any argv-dispatched subcommand that reuses `Arg.parse` must slice off the subcommand token first; smoke-test the subcommand path, not just the flag grammar.

### [2026-10-02] `Session_store.fork` rejected empty journals after `until=` bound was added

- **Context / Symptom:** `test_session_rewind` failed with `Invalid_response("invalid session journal: nothing to fork before that bound")` on a header-only journal.
- **Root Cause:** The new `?until` implementation guarded `fork_header` with `copied = [] -> invalid`, which also rejected legitimate header-only forks (`until = None`).
- **Solution:** Apply the empty-guard only when `until` is `Some _`: `if until <> None && copied = [] then invalid`.
- **Prevention / Reference:** New optional bounds must not change the default (`None`) path; test both bounded and unbounded forks.

### [2026-10-02] Provider retry backoff stalled the test suite

- **Context / Symptom:** `dune runtest` ran to the 300 s timeout after transient-retry logic was added to `Provider.post_json`/`post_stream`. Fixtures never hit real network errors, but a fixture returning HTTP 429/5xx triggered `Unix.sleepf` backoffs inside the loop.
- **Root Cause:** Retry delays ran unconditionally; injected `Test.curl_helper` fixtures still paid real wall-clock sleeps.
- **Solution:** `retry_delay_seconds` returns `0.0` when `Test.curl_helper` is set, preserving retry counts while removing sleeps under fixtures.
- **Prevention / Reference:** Any time-based backoff added to shared transport paths must bypass real sleeps when `Provider.Test.curl_helper` is injected.

### [2026-10-01] Web-search unit tests launched a real browser on CI

- **Context / Symptom:** `test_web_search` passed locally but failed in CI at `runtest` with `Failure("web search: DuckDuckGo bot challenge is explained was accepted")` — a case expecting `search` to raise instead completed successfully.
- **Root Cause:** The test fakes `~http` but `Web_search.plan`'s `find_browser` still probes real filesystem paths for Chromium. CI runners ship Chrome, so the browser-rendered Ecosia engine joined the provider chain, ignored the fake transport entirely, and ran a real headless browser with real network access — returning results where the test expected exhaustion. Host-dependent hermeticity break.
- **Solution:** `test_web_search.ml` now wraps `Web_search.search` with a default `~find_browser:(fun () -> None)` so no test can reach a real browser unless it explicitly injects `find_browser`/`render` (the Ecosia fixture tests do).
- **Prevention / Reference:** Any `~http`-faked engine test must also pin `find_browser` and `render`; injected transports do not cover browser-rendered providers.

### [2026-10-01] Headless Chrome on WSL refuses loopback when the server is IPv4-only

- **Context / Symptom:** While verifying the new `browser` tool, `Page.navigate` to `http://127.0.0.1:<port>` and `http://localhost:<port>` failed with `net::ERR_CONNECTION_REFUSED` even though `curl` to the same socket succeeded and a Python `http.server` was listening. `https://example.com` worked from the same browser.
- **Root Cause:** Not a sandbox block: on this WSL2 host Chrome resolves `localhost` and reaches loopback via IPv6 (`::1`), while `python3 -m http.server` bound only `0.0.0.0` (IPv4). A server bound to `::` with `IPV6_V6ONLY` off answered all of `[::1]`, `localhost`, and `127.0.0.1`. Separately, `--remote-debugging-pipe` is unsupported by `chrome-headless-shell` (it exits with `Remote debugging pipe file descriptors are not open` even when fds 3/4 are correctly established).
- **Solution:** Verified the full CDP path against a dual-stack loopback server (`http://localhost:8473`) — navigate/observe/evaluate/screenshot and `modelContext` `list_tools`/`call_tool` all worked. No code change was needed.
- **Prevention / Reference:** For loopback page servers under WSL2, bind `::` with `IPV6_V6ONLY=0` (e.g. `socketserver.TCPServer` with `address_family = AF_INET6`), not `0.0.0.0`.

### [2026-10-01] Most credential-free search engines refuse even a headless browser

- **Context / Symptom:** Adding browser-backed engines from the reference provider set: with curl, Ecosia returned HTTP 403 and Startpage 303. With Chrome for Testing `chrome-headless-shell` 154 (`--dump-dom`), Google returned an "unusual traffic" page, Startpage an Anubis proof-of-work page, Mojeek an ALTCHA captcha, Bing no results, and DuckDuckGo HTML an `anomaly` challenge. Only Ecosia rendered real results (10 `organic-result` articles, repeatable).
- **Root Cause:** These engines fingerprint headless automation or require interactive challenges; the reference implementation relies on stealth patches and puppeteer interaction that Pave does not reproduce.
- **Solution:** Implemented only Ecosia as a browser-rendered engine (sandbox on, fresh deleted profile, fixed URL, 30 s, 4 MiB DOM cap) and kept DuckDuckGo on its plain HTML POST, which still works without a browser. Challenge pages fail that engine and the chain moves on.
- **Prevention / Reference:** Re-probe an engine with `chrome-headless-shell --headless --dump-dom URL` before adding it; do not add stealth/evasion flags. A scratch dune executable created right after `dune runtest` sometimes failed with `I/O error: ... .cmi: No such file`; rebuilding the same target once succeeded.

### [2026-10-01] Devin discarded healthy long responses at a fixed two-minute deadline

- **Context / Symptom:** After successful file tools, the user received `Devin Connect transport failed: request timed out (remote acceptance unknown)`. A local real-curl response emitting binary chunks every five seconds reproduced the transport cutoff at 120.13 seconds with one request.
- **Root Cause:** The independent Devin executor applied `max-time = 120` to completion as well as unary metadata RPCs and waited for a temporary response file to finish. It could not distinguish prefill, active progress and response inactivity. Shared provider streams already had longer phase-specific budgets.
- **Solution:** Used bounded binary pipe receipt with a three-byte HTTP-status suffix, a 600-second first-data allowance, a resetting 120-second inactivity allowance and a 3,600-second total completion cap. Metadata retains 120 seconds and all connections retain 10 seconds. Safe transport failures now name the RPC. No automatic replay or incomplete Connect response acceptance was introduced.
- **Prevention / Reference:** The same real-curl workload completed after 130.14 seconds with all 494 bytes and one request. Subprocess regressions cover first-byte/idle/total deadlines, continued progress, delayed readable EOF, cancellation/reaping, size rejection during receipt and fragmented binary/status bytes; existing routed two-turn tool-result replay passed. This reproduces a client-side failure mode, not the exact upstream account/request; no authenticated Devin inference was performed.

### [2026-10-01] Web fetch confused source size with preview size

- **Context / Symptom:** Fetching the reported LINE `page-data.json` produced `HTTPS request failed or timed out`. The public source returned HTTP 200 and 510,997 bytes during diagnosis; the old fetch configuration allowed only 65,536 downloaded bytes.
- **Root Cause:** `max_bytes` bounded both input transfer and rendered output, and curl size failures were collapsed into a generic timeout message. JSON documents were also passed through HTML conversion, which could strip markup inside JSON string values.
- **Solution:** Separated the 1 MiB input ceiling from the at-most-65,536-byte preview, returned explicit `truncated` metadata with UTF-8-safe content, preserved structured JSON-looking source text and classified allowlisted transport failures without reflecting remote errors. Oversized input, private hosts, redirects and cancellation still fail.
- **Prevention / Reference:** Executed the real `Tools.web_fetch` path against `https://designsystem.line.me/page-data/LDSG/components/buttons/action-button-en/page-data.json`: received a 65,536-byte preview with `truncated=true`, the Action Button content and intact HTML strings. Truncated JSON is not a complete parseable document. Regressions cover larger ignored HTML, exact JSON preservation, Unicode truncation, long table cells and over-limit downloads.

### [2026-10-01] DuckDuckGo empty pages and challenges were conflated

- **Context / Symptom:** The reported searches returned either `DuckDuckGo returned no usable results` or HTTP 202. A diagnostic GET returned a 202 `anomaly-modal` challenge. The real native form POST returned HTTP 200 with a genuine empty-result page: a single-quoted `no-results` class inside a multi-class `result--no-result` container.
- **Root Cause:** HTTP status rejection ran before challenge classification; the scraper treated no usable rows as failure and matched exact double-quoted class strings instead of HTML class membership.
- **Solution:** Recognized empty pages as successful empty results/citations, reused checked attribute parsing for quote/order-independent class membership and links, and classified 202 as blocked/deferred without publishing its body. Only previously configured/approved fallback providers may be tried; unknown/malformed pages remain failures.
- **Prevention / Reference:** Deterministic regressions cover captured empty-page class variants, single-quoted multi-class result links, JSON-provider empty lists and 202 fallback without reflecting challenge tokens. DuckDuckGo can still challenge external requests; use a configured supported search API key or a known source URL rather than inventing results.

### [2026-10-01] A cancelled pending read was correctly reported as unexecuted

- **Context / Symptom:** The user also saw `turn cancelled before this tool ran; do not assume it executed` on `read_file` after web failures.
- **Root Cause:** This is the scheduler's cancellation result, not a file-read failure. A normal web tool error does not itself set the turn-cancellation flag; the supplied log does not identify what requested cancellation.
- **Solution:** Kept cancellation safety unchanged. An actual Agent/loopback-provider smoke verified both cases: a failed web tool followed by a successful read and second model request; and explicit cancellation after that failure, producing an aborted/unexecuted read with no second model request.
- **Prevention / Reference:** Do not suppress the abort, execute cancelled work or label it successful to hide this message. Existing tool-call/result pairing and cancellation regressions remain in place.

### [2026-10-01] An extracted release archive lacked the native-install marker

- **Context / Symptom:** Running `pave update --check` directly from the verified v0.1.78 archive returned `native-install marker missing; rerun install.sh once to enable self-update (opam installs must use opam)`.
- **Root Cause:** Archive extraction is not installation. The updater deliberately requires the marker and license layout created by `install.sh`; the release archive correctly contains only the executable and two legal files.
- **Solution:** Ran the actual installer against the latest public release with `PAVE_INSTALL_DIR` under a disposable directory and `PAVE_VERSION` unset, then ran the installed binary's update check. It reported `Pave v0.1.78 is up to date.` No guard was bypassed and the user's installation remained unchanged.
- **Prevention / Reference:** Verify archive/CLI/TUI behavior on extracted files, but use a real isolated installation to verify self-update discovery.

### [2026-10-01] Narrow decision hints and selected model details were misleading

- **Context / Symptom:** The actual 24×12 discard dialog said `n/Esc deny`, although its safe choice was Keep conversation on `k`. At 36×18, a truncated model row had no selected-item detail despite spare space; introductory prose ended abruptly.
- **Root Cause:** The insufficient-room fallback hard-coded tool-approval language, detail rows had a width-only admission threshold and model identity followed long capability metadata. Intro rows used raw clipping.
- **Solution:** Derived compact hints from the actual safe option, budgeted the gutter, admitted useful detail after reserving roster rows and moved the exact selector first. Shortened introductions explicitly with ellipses.
- **Prevention / Reference:** Actual CLI/tmux acceptance covered 24×7/12 safe decisions, 36×18 selection changes, 100×28 to 18×8 resize/scroll, color and NO_COLOR, cancellation, and exact Korean/combining-character draft bytes received by a loopback model.

### [2026-10-01] Unchanged drafts and ASCII transcript reflow allocated repeatedly

- **Context / Symptom:** A native PTY benchmark of 1,000 unchanged paints with a 16,000-byte draft allocated about 1.18 GB and used 0.492 CPU seconds. Normal ASCII streaming repeatedly allocated Unicode segmentation state.
- **Root Cause:** Paint, hint-room and resize paths independently recomputed identical composer wrapping. Transcript wrapping always used grapheme segmentation, even when every byte was printable ASCII.
- **Solution:** Kept one TUI-owned wrapping result under immutable draft identity and field width, without caching the general Composer measurement API. Printable-ASCII transcript wrapping uses fixed one-byte clusters; all other text keeps the original grapheme path. Used a nonallocating ASCII predicate and shared cluster callback to avoid introducing a Unicode-path allocation regression.
- **Prevention / Reference:** The same draft paint benchmark fell to 18.3 MB and 0.0074 CPU seconds; a Unicode draft fell from 566 MB/0.405 s to 16.3 MB/0.0075 s. Cache equivalence held across 70 edit/cursor/paste/undo/history/width cases. Streaming visual digests were identical for ASCII, long lines and Unicode; measurements are controlled local workloads, not universal latency guarantees.

### [2026-10-01] Full history cleanup required branch and tag replacement

- **Context / Symptom:** Removing identifiers from the current tree and latest message was insufficient: earlier commits, release tags and dependency-update branches still retained historical source references.
- **Root Cause:** Git objects remain reachable through every branch/tag that still points into the old graph. A prior local filter run also left metadata that prompted whether the new rewrite should continue its old mapping.
- **Solution:** After explicit user authorization, fetched the full branch/tag scope, retained expected remote object IDs, committed tested repairs and ran a new whole-history filter without pruning topology. Answered No to continuation of the unrelated old filter run. The filter expired old reflogs/objects; its intentional removal of `origin` was followed by restoring the known SSH remote. Atomically force-pushed all branch/tag refs with individual leases and verified a fresh remote mirror, including server-owned PR refs.
- **Prevention / Reference:** Re-clone other checkouts rather than merging old history back. Existing published binary assets and third-party clones remain separate artifacts; ordinary Git force-push cannot guarantee removal from server caches. Keep required legal notices intact.

### [2026-10-01] Consent text was clipped while shortcuts still approved

- **Context / Symptom:** A real 36×35 terminal displayed only Deny plus a clipped scope/consequence, but `a` granted all later writes. A 24×12 discard prompt also hid its irreversible-action context.
- **Root Cause:** The allow gate budgeted the argument body but not the full heading, scope, button labels or consequences.
- **Solution:** Shared rendering and approval geometry now wraps consent text, stacks full controls when needed and includes all consent rows plus the active timer. Insufficient space locks every non-safe choice.
- **Prevention / Reference:** Actual CLI/tmux checks covered 36×35, 24×12 and 120×40, resize locking, bracketed paste, process-lifetime write grants, mandatory approval of both later shell calls, default denial and denied/not-written outcomes.

### [2026-10-01] Successful tool content looked like an execution failure

- **Context / Symptom:** Reading a real file beginning `Error:` displayed a failed tool card even though the file read succeeded.
- **Root Cause:** Prepared execution flattened errors and successful content into the same block list; the agent and TUI inferred status from a text prefix.
- **Solution:** Prepared/direct tool execution now returns a typed result. Agent settlement and rewind eligibility preserve it, and typed UI events do not reinterpret the content.
- **Prevention / Reference:** The streamed agent lifecycle regression and actual CLI fixture retain error-like file bytes while showing a successful read. Obsolete literal source-text assertions in unrelated lifecycle scenarios were removed; call pairing, cancellation and availability assertions remain.

### [2026-10-01] Search fallback and provider diagnostics lost failure boundaries

- **Context / Symptom:** Runtime probes observed an unclosed search anchor raising `Invalid_argument`, a 1,025-byte title rejected after normalization, Kagi error-plus-data accepted, raw Firecrawl error text echoed, and nested Responses errors reduced to a generic failure.
- **Root Cause:** HTML substring bounds, truncation-marker accounting and provider-specific failure-envelope checks were incomplete; Responses diagnostic lookup missed nested and status-less terminal errors.
- **Solution:** Bounded anchor extraction, UTF-8-safe total title limits and explicit safe search failures retain fallback behavior. Responses/Codex select diagnostic fields before output validation and reject error-bearing completed envelopes. Empty assistant text is omitted from tool-call replay.
- **Prevention / Reference:** Search runtime probes passed after repair; a real curl/loopback Responses SSE failure reported message and code with exactly one request. Replay serialization retained call/result adjacency without an empty assistant item. Linking standalone OCaml smoke drivers required `digestif.c` rather than the virtual `digestif` package.

### [2026-09-30] Additional prompts cancelled work and approval arrows rejected actions

- **Context / Symptom:** The user reported `Turn cancelled` after entering a follow-up. In an actual loopback-provider TUI, pressing Down at a `write_file` approval produced `Error: tool approval denied` without an explicit denial.
- **Root Cause:** Composer Return dispatched steering while modified Return dispatched follow-up, despite both looking like ordinary submission. Approval was appended to the transcript above an editable-looking composer and treated most keys—including arrows—as rejection.
- **Solution:** Unified ordinary/modified Return on FIFO submission and introduced explicit `/steer MESSAGE` for interruption. Added `/queue` / Option+Q management for per-item cancellation, noninterrupting priority, interrupt-and-run-now and draft/media restoration. Stable IDs prevent duplicate text or stale selections from targeting another item. Replaced transcript approval with a draft-preserving modal, safe denial default, explicit one-action choices, inert stray typing/paste, navigation keys, and one geometry calculation shared by rendering and the allow gate. Kept next-launch policy editing visibly separate.
- **Prevention / Reference:** `test_keybindings`, `test_turn_runner`, `test_interaction` and approval-fit boundary tests cover the contracts. Actual PTY checks exercised duplicate-item cancellation, priority and interruption, stale action removal, media restoration, staged-media protection, approval navigation, Korean input, paste, denial/allow, NO_COLOR and shrink/grow locking. Removed surplus queue-indicator whitespace so an eight-cell count remains readable at 18 columns. Kept mandatory shell warnings inside the settings chooser's two visible intro rows. Resize clears obsolete approval guidance once the full preview is visible.

### [2026-09-30] Stream inactivity protection timed out before the first response

- **Context / Symptom:** The user reported a timeout between model request and response. A controlled real curl POST with a five-second first-byte delay failed at 3.04 s with exit 28 under a scaled one-second low-speed guard; the same request completed at 5.03 s without that guard and with the same eight-second total cap.
- **Root Cause:** `speed-time` / `speed-limit` measures average transfer speed and runs before response-body arrival. It is not a post-response inactivity timer, so upload/prefill/remote queueing was charged to the 120 s stream-idle budget. Generic curl exit 28 was also labelled too narrowly.
- **Solution:** Removed low-speed policing and enforced phase-aware deadlines in the cancellable reader: existing 600 s first-response budget, then existing 120 s since the last received body byte, while retaining the ten-second connect and one-hour total limits, completion grace and byte bounds. Ready bytes/EOF win at deadline boundaries. Errors distinguish first-byte wait, post-byte stall and generic connection/total timeout; requests are never automatically replayed.
- **Prevention / Reference:** `test_provider_http` covers first-byte versus idle expiration, progressing streams, deadline-boundary bytes/EOF and cancellation. The actual CLI completed a loopback request whose first response was delayed 125 s (125.03 s, exit 0, exactly one request). The user's exact vendor/proxy failure was not available; upstream timeouts and genuine post-byte stalls can still fail appropriately.

### [2026-09-30] Successfully used initial or resumed models did not become last-used

- **Context / Symptom:** The user reported that the most recently used model was not retained for the next launch.
- **Root Cause:** Recent-model persistence occurred only in the interactive selection path; successful use of an initial CLI/configured model or a restored journal did not update it.
- **Solution:** Captured the resolved exact model identity in the top-level agent and saved it after a valid assistant response, including tool-call responses and headless use. Explicit accepted interactive selections still save immediately; session browsing, failed calls and background agents do not. Compared on-disk state under the lock to avoid rewriting unchanged identities.
- **Prevention / Reference:** The real-CLI regression covers replacement, failed provider/preflight preservation, headless-default precedence, and registered custom account/route/fingerprint resume. Its first resume fixture incorrectly supplied `--endpoint`, which intentionally overrides journal selection; the corrected fixture resumes a registered route with no explicit selectors.

### [2026-09-30] Embedded smoke harness stalled while managing CLI subprocesses

- **Context / Symptom:** The embedded evaluation kernel became unresponsive during subprocess/PTY diagnostics. An initial isolated PTY harness also stopped observing queued requests while waiting without reading terminal output.
- **Root Cause:** [INFERENCE] The embedded harness could block on inherited open stdin or its threaded process/fork interaction; its exact kernel stall was not established as a product fault. Independently, a PTY producer can block when the harness waits for HTTP events without draining terminal output.
- **Solution:** Moved smoke scenarios to a disposable external Python process, supplied `DEVNULL` for headless stdin, used `openpty` plus `Popen` rather than a threaded in-kernel fork, and drained PTY output during request-admission waits. Kept every fixture in an isolated workspace/config/state directory.
- **Prevention / Reference:** Use explicit request-admission synchronization before changing fixture behavior, and keep terminal output draining while waiting for asynchronous turns. Do not count an unsynchronized delayed-response launch as timeout verification.

### [2026-09-30] Static picker searches went blank with selection hints

- **Context / Symptom:** In the real 30×10 CLI TUI, filtering `/settings` to an unmatched string left an empty body and still showed `↑↓ ↵ select · Esc cancel`; the title's match count was clipped. Slash hints also clipped `/model` argument syntax and the key guide at the right edge.
- **Root Cause:** Empty-result space and messages were reserved only for dynamic model pickers. Titles and suggestion rows were assembled as long strings and cropped, while the footer assumed a selectable row existed in static pickers.
- **Solution:** Reserved empty-result space for both static and dynamic lists, paired empty searches with edit/cancel hints, budgeted title/count separately, and used responsive suggestion columns and whole-item keyboard hints.
- **Prevention / Reference:** PTY smoke covered empty/recovered filters at 30×10 and 18×8, command/file selection and insertion, color/NO_COLOR highlighting, exact-command versus partial insertion, scroll-to-latest and queued-turn completion.

### [2026-09-30] Narrow composer retained a partial box after resize

- **Context / Symptom:** An actual CLI PTY resized from 80 columns to 11×10 showed a bare input gutter and straight top rule, but retained the rounded bottom border. Input chrome changed inconsistently across the width breakpoint.
- **Root Cause:** Only the input rows and top rule used `composer_boxed`; the bottom border was unconditional at heights of six rows or more. Resize also reused equal cached row images although a terminal emulator may have reflowed the physical cells.
- **Solution:** Replaced the box with width-independent horizontal rules and a separate metadata row, used a stable two-cell gutter (omitted below four columns), and invalidated the physical-screen/cursor cache on resize. Kept the existing four-row chrome budget and short-terminal fallback.
- **Prevention / Reference:** Actual CLI PTY checks covered 120×24, 80×16, 30×10, 18×3, 11×10, 4×6 and 1×1, color/NO_COLOR, slash hints and active/completed replies. Korean, combining-character and multiline draft bytes reached a loopback provider unchanged after shrink/grow cycles.

### [2026-09-30] Tool approvals were denied without the user refusing

- **Context / Symptom:** A `web_fetch` approval card appeared and then settled as `Error: tool approval denied` although the user meant to approve. Reproduced in tmux by sending `ㅛ` to the approval card.
- **Root Cause:** With a Korean input method active, the y key sends `ㅛ` (U+315B), and approval treated every key other than ASCII `y`/`Y` as a denial. Separately, `enqueue_ui_event` queues an event before writing the wake byte, so the UI thread could handle the event first and later read the leftover byte as a bare `Wake`. The approval loop's catch-all denied on that `Wake`, and `choose`/`read` raised `invalid_arg` when it arrived without an `on_wake` callback.
- **Solution:** Bound `ㅛ` to Approve and made other non-ASCII keys show a switch-input hint. `next_input` now drops a wake unless the caller's own wake fd is readable. Approval ignores `Wake`, and pickers ignore wakes that have no callback. Modified jamo resolve as their Latin shortcut keys, and line prompts use `Approval.confirmed_answer`.
- **Prevention / Reference:** Test modal key handling with non-ASCII input, for example `tmux send-keys -l "ㅛ"`, and never let a catch-all branch treat non-key events as a user decision.

### [2026-09-30] Parallel read-only tool batches ran slower than serial

- **Context / Symptom:** A batch of `search` + `grep` took about 300 ms when run one after another but 600-700 ms through `Tool_scheduler`, whose shared calls run on system threads.
- **Root Cause:** OCaml 5 system threads in one domain share the runtime lock. A tree walk releases and reacquires it on every `readdir`/`stat`/`read`, so two concurrent walks forced a thread handoff per syscall. Moving the work to extra domains is unsafe here because `Unix.fork` (MCP clients, workspace processes, auth helpers) fails while another domain runs.
- **Solution:** Added a reentrant `scanning` lock in `lib/tools/tools.ml` around the tree-walking tools. Waiting on a `Mutex` releases the runtime lock, so walks take turns without contention while cheap calls such as `read_file` still overlap. The scheduler also became a sliding window with immediate in-order settlement.
- **Prevention / Reference:** Benchmark parallel batches against serial execution, not only single calls; a parallel batch should never be slower than the serial sum.

### [2026-09-30] Long streamed replies were killed after 120 seconds

- **Context / Symptom:** A healthy stream from a local vLLM fixture sending one token per second ended after token 118 with `Transport error: provider stream timed out after response data (stream idle or total request timeout)`.
- **Root Cause:** `post_stream` reused the buffered request's curl options, including `max-time 120`, so a stream's total duration was capped at two minutes on top of its idle check.
- **Solution:** Gave `curl_options` an explicit `max_seconds`: buffered requests use 600 s, while streams use a 120 s idle limit (`speed-time`/`speed-limit`) plus a one-hour total cap. The timeout message now names the applicable limits.
- **Prevention / Reference:** Exercise long streams against a loopback fixture (for example 130 tokens at 1 s each) rather than only short replies; the same fixture confirmed the 130-second stream completes.

### [2026-09-30] Composer header lost its border when the model name was shortened

- **Context / Symptom:** At about 60 columns with a long model name, the composer's top row rendered as a bare `────` line with no corners and no model header, while the sides and bottom still drew a box.
- **Root Cause:** The header identity was laid out to exactly `cols - 6` cells. When the model name had to be shortened, the image filled that budget completely, so `composer_top` computed a fill of 0 and fell back to the plain line.
- **Solution:** Reserved one fill cell (`cols - 7`) for the identity, dropped the workspace path when it would get fewer than 8 cells, and gave the model name 28 cells of priority before the path is shown.
- **Prevention / Reference:** Check header PTY captures at several widths (36/50/60/75/100/140) with a long model ID, not only the default 100-column width.

### [2026-09-30] Manual native UI smoke linked mixed interface generations

- **Context / Symptom:** A standalone native panel link launched while Dune was rebuilding dependencies failed with `make inconsistent assumptions over interface Pave__Sse`.
- **Root Cause:** The manual linker read an old `Tui.cmi` and newly rebuilt provider/core interfaces before the complete executable dependency graph had settled.
- **Solution:** Waited for the single full Dune build/test invocation to finish before linking the throwaway driver from one coherent set of objects. No production API compatibility shim or package change was added.
- **Prevention / Reference:** Serialize manual native object linking after Dune, including implementation-only changes that rebuild inferred interfaces.

### [2026-09-30] Intel macOS release exposed a pre-send cancellation assumption

- **Context / Symptom:** Release run `36663417694` for immutable tag `v0.1.72` stopped on Intel macOS with `Failure("a failed partial send disposes without a second blocked shutdown write")`. The other three native platforms passed; no release was published.
- **Root Cause:** The regression armed cancellation 50 ms before document preparation and assumed every error meant a partial native write. `send_raw` can correctly cancel before entering the transport, leaving a healthy server eligible for its two-second graceful shutdown. The hosted log did not identify which send phase occurred. A native local pre-send cancellation reproduced the invalid `<1s` assertion with one predicate call and 2.130-second graceful cleanup.
- **Solution:** Made the controlled server acknowledge receipt of the large frame header through an atomically renamed PID marker. Cancellation now follows that observed phase; the deadline case allows preparation before exercising backpressure. Replaced the incidental one-second cleanup assertion with observable direct-child termination/reaping and rejection of further operations on the closed manager. Both actual native paths disposed in 0.000 seconds locally; the configured deadline still bounded the stalled request.
- **Prevention / Reference:** The downloaded public diagnostic archive matched its GitHub-reported SHA-256. Keep failed tags immutable; validate the corrected regression on the next four-platform release, not by moving `v0.1.72`. No production timeout or exception was suppressed.

### [2026-09-30] Local model picker rejected an overridden native server address

- **Context / Symptom:** A real `/model` PTY fetched one model from an `LM_STUDIO_BASE_URL` loopback fixture, but showed `0 route-compatible listed IDs · 1 excluded`. The permanent picker regression failed before the repair at `test/ui/test_model_picker.ml:186`.
- **Root Cause:** Selection resolved the native route through `Provider_catalog.route`, but `Model_discovery.model_supports_endpoint` compared that current address against the descriptor's static default endpoint.
- **Solution:** Resolved each registered route through the existing factory before endpoint comparison, and migrated CLI/picker API facts to the same current route. Kept exact endpoint, provider, account and fresh-listing admission; no model-name heuristic or arbitrary-host exception was added.
- **Prevention / Reference:** The regression passed after repair. Actual native PTYs fetched only the current scope on entry, no hidden model list while browsing scopes, and one explicitly selected second scope; accepted models reached the next real loopback HTTP request. Escape from effort preserved a byte-identical journal; accepting unknown support cleared the prior thinking override.

### [2026-09-30] Codex ignored the conversation's selected reasoning effort

- **Context / Symptom:** `/thinking` and the agent retained a level, but the Codex completion branch never passed it to the native request constructor. Lite used only the fresh listing default; Standard omitted the requested effort. Account discovery also discarded `supported_reasoning_levels`.
- **Root Cause:** The Codex request API had no thinking argument and discovery projected those account rows without reasoning metadata.
- **Solution:** Preserved bounded unique reported levels; admitted only exact tokens supported by the native route, rechecked account/model support before inference and serialized explicit effort for Standard/Lite while retaining the Lite default when no override is chosen. A selected-model chip panel now confirms model and effort before any settings mutation.
- **Prevention / Reference:** Native HTTPS fixtures exercised Standard high, Lite high/ultra, listing default and absent/empty/restricted/malformed/account-isolated metadata. Actual colored and monochrome effort PTYs exercised explicit selection, default, cancellation, unknown metadata and 100×28→40×12→18×8 resize; their accepted values reached the actual Codex wire constructor. These are controlled fixtures, not live account entitlement. [Official model metadata schema](https://github.com/openai/codex/blob/rust-v0.146.1/codex-rs/models-manager/models.json).

### [2026-09-30] Standalone native TUI smoke needed a concrete Digestif backend

- **Context / Symptom:** Linking a throwaway native UI driver with `ocamlfind ... -package digestif` failed with `No implementation provided for ... Digestif`.
- **Root Cause:** The package exposes the shared interface; the manual link did not select an implementation as the normal Dune build does.
- **Solution:** Linked the isolated OCaml 5.5.1 driver against `digestif.c` and the already built project/UI objects. No production dependency or build rule changed.
- **Prevention / Reference:** Prefer the normal Dune build; manual native smoke drivers must select the concrete backend and matching compiler. The linked driver exercised the real terminal effort panel and Codex serializer, then was removed.

### [2026-09-30] Masked writes exposed restored secrets in approval previews

- **Context / Symptom:** A native PTY with `--mask-secrets` exposed a synthetic `LM_STUDIO_API_KEY` in the completed write-approval surface, even though the exact approved file write itself succeeded.
- **Root Cause:** Agent execution restored tool arguments, then the write-approval formatter truncated/quoted raw proposed content before display redaction. Redacting arbitrary partial fragments also cannot safely hide secrets split across chunks.
- **Solution:** Suppressed unvalidated streamed previews under an active mask, redacted complete proposed content and displayed paths before truncation/quoting, and redacted typed approval fields at the consumer boundary. Execution still receives the original approved arguments.
- **Prevention / Reference:** Native approval/denial/cancel/truncated/masked PTY scenarios passed. The masked scenario retained exact Korean/emoji/secret file bytes while the synthetic secret never appeared in terminal output; streamed drafts remain UI-only, not journal or headless JSONL data.

### [2026-09-30] Subprocess regression fixtures depended on timing instead of lifecycle

- **Context / Symptom:** Full integration encountered the command-cancellation assertion `cancelled && elapsed < 2.` and `Google ADC sh command timed out` in a descendant-held-stdout fixture. Earlier runs of the same paths had passed; no production ADC failure was established.
- **Root Cause:** Cancellation admission used wall-clock delay and pinned total elapsed time. The ADC fixture used an uncoordinated background sleep to try to place stdout closure after leader reaping, without enforcing that lifecycle order.
- **Solution:** Requested command cancellation only after actual child output and asserted cancellation, not a machine-speed threshold. Replaced the ADC sleep race with a private FIFO released only after the direct leader had been reaped; the child then closed its inherited stdout. Removed incidental executable/environment/opaque-key format pins.
- **Prevention / Reference:** Synchronize on observable state transitions; bounded fixture deadlines prevent hangs but are not performance assertions. Live TUI responsiveness was measured separately with a held 872,018-byte write draft: input plus resize 0.020 seconds and cancellation 0.021 seconds, with one request and no file mutation.

### [2026-09-30] Fresh model listings lost capabilities or overstated route compatibility

- **Context / Symptom:** Mistral's mixed-task roster included embedding and tool-disabled models; five generic listing filters discarded reported capabilities. Gemini's output-token limit was missing. OpenAI's ID-only mixed-task roster appeared selectable on both Chat and Responses despite lacking route/tool evidence.
- **Root Cause:** Some adapters projected response objects to IDs too early, Mistral used the generic parser, and OpenAI inherited the generic route-compatible classification. Duplicate JSON members could also hide conflicting catalog data.
- **Solution:** Preserved filtered raw metadata/provenance, required Mistral's documented chat and function-calling flags, retained Gemini's output limit, rejected duplicate members recursively, and marked OpenAI listings API-unverified. Explicit model/route selectors remain usable; no model-name heuristic or stale fallback was introduced.
- **Prevention / Reference:** Each listing/picker open performs fresh discovery. [Mistral model capabilities](https://docs.mistral.ai/api/endpoint/models), [Gemini model limits](https://ai.google.dev/api/models), and [OpenAI's ID-only model roster](https://developers.openai.com/api/reference/resources/models/methods/list.md) describe distinct contracts; fixtures do not establish live account entitlement.

### [2026-09-30] Discovery wake saturation blocked cancellation

- **Context / Symptom:** A standalone coordinator saturated its notification pipe after about 65,540 scopes; closing it could wait indefinitely for the blocked worker.
- **Root Cause:** Worker notifications used a blocking write even though results were already retained under the coordinator mutex.
- **Solution:** Made wake writes nonblocking and coalesced saturated notifications without dropping outcomes. The bounded child regression completed and polled 100,000 scopes, then closed successfully.
- **Prevention / Reference:** Wake bytes are hints, not the result store; producers must not block cancellation while reporting an already-retained result.

### [2026-09-30] Failed jobs and interrupted artifacts escaped persistence limits

- **Context / Symptom:** A 48,011-byte failed-job diagnostic could not be parsed after resume. Two same-process writers accepted 257 artifacts against a 256-item cap. Orphan data, temporary files and corrupt metadata could evade retained-byte accounting.
- **Root Cause:** Failure summaries were bounded only by the parser, POSIX record locks did not serialize threads in one process, and quota inventory counted only valid metadata.
- **Solution:** Normalized every terminal diagnostic to bounded UTF-8 with an explicit truncation marker; serialized the existing record-lock transaction with a process mutex; conservatively counted actual retained data, staging and corrupt/unrecognized files under that lock. Failed candidates remove only their own temporary files.
- **Prevention / Reference:** Exercise resume and exactly-once failed delivery, same/cross-process item and byte boundaries, and sparse interrupted-publication fixtures. Crash leftovers consume quota; they are not silently deleted or adopted.

### [2026-09-30] Compaction rejection lost billed usage and terminal exit left workers waiting

- **Context / Symptom:** A successful 12-input/3-output summary followed by HTTP 500 lost its usage marker; an oversized 17-input/5-output summary also lost usage despite a validated response. Ctrl+D during a held provider request waited for the remote response.
- **Root Cause:** CLI usage was flushed only after the whole compaction committed; turn-runner shutdown joined workers without first cancelling them or releasing approval waits.
- **Solution:** Recorded each validated usage callback immediately and independently of context publication. Shutdown now cancels the active turn, releases approvals, joins it and drains terminal outcomes without dispatching queued work.
- **Prevention / Reference:** Native automatic/manual rejection fixtures retained exact usage once and unchanged context. The held-response PTY exited in 0.114 seconds without releasing the response; neither path automatically retried.

### [2026-09-30] LSP disposal and full stdin pipes escaped request deadlines

- **Context / Symptom:** Terminating an LSP leader left descendants holding its pipes. A server that stopped reading a 256 KiB request prevented cancellation/deadline checks and left a pending request.
- **Root Cause:** LSP launch owned only the leader PID, and synchronous pipe writes could block before the receive loop enforced its deadline.
- **Solution:** Reused owned process-group launch/disposal and interruptible nonblocking writes with the absolute request deadline. A failed partial frame disposes the owned group and settles pending requests once; writer-lock waits and cancellation notifications are bounded.
- **Prevention / Reference:** Native fixtures retained a signal-resistant descendant and filled server stdin. Cancellation/deadline completed in 0.201/0.222 seconds with zero pending requests; repeated disposal remained safe.

### [2026-09-30] Exact-boundary edits and fragmented DAP headers were rejected

- **Context / Symptom:** A valid simultaneous edit at the 1 MiB ceiling was rejected when an early insertion was offset by a later deletion. A maximum-size DAP frame failed when its header delimiter arrived in fragments.
- **Root Cause:** Edit admission capped intermediate construction rather than the final simultaneous result; DAP's incomplete-buffer budget omitted the partial/full four-byte delimiter.
- **Solution:** Calculated the final removed/replacement size before construction and separately budgeted delimiter framing without widening header/body ceilings.
- **Prevention / Reference:** Boundary regressions accepted net-zero edits and delimiter fragments of zero through three bytes, while rejecting real growth without mutation.

### [2026-09-30] Buffered completions downloaded beyond their response ceiling

- **Context / Symptom:** A controlled 32 MiB local HTTP response made the native CLI retain all 33,554,432 response bytes in a temporary file before reporting `completion response exceeds 16 MiB`.
- **Root Cause:** `Provider.post_json` let curl finish downloading to disk and checked the size only when reading the completed file.
- **Solution:** Received buffered bodies through a bounded pipe consumer, retained only the existing 16 MiB body allowance plus curl's three-byte status, and applied curl's declared-size limit before receipt. Oversize exceptions use the existing owned-child kill/reap path; no response temporary file is created.
- **Prevention / Reference:** Exercise both declared-length and chunked success/error bodies, the exact ceiling and one-byte overflow; preserve cancellation, no automatic replay and the normal tool-result round trip.

### [2026-09-30] Devin Connect failures hid their transport cause

- **Context / Symptom:** A model turn ended with `Error: Devin Connect transport failed`, leaving DNS, TLS, timeout and a malformed response indistinguishable. A valid subprocess response could also fail when the three-byte curl HTTP status arrived in separate pipe reads.
- **Root Cause:** `Devin_binary_http.run` assumed one `Unix.read` returned all three status bytes and discarded nonzero curl exit codes. The provider reduced every transport error to the same message; non-2xx Connect responses lost allowlisted structured error codes.
- **Solution:** Read the entire bounded status through EOF, validated a three-digit HTTP code, mapped allowlisted curl exits and HTTP/Connect codes to safe diagnostics without echoing secrets, bodies or headers, and retained cancellation and no automatic completion replay. A subprocess fixture split status bytes and checked successful replies, timeout, TLS, HTTP and cancellation paths.
- **Prevention / Reference:** Do not equate pipe read boundaries with message boundaries; test a real child-process boundary with fragmented writes. A timeout cannot prove whether the remote server accepted a completion. The user's particular remote failure was not identified without its curl exit code or HTTP response.

### [2026-09-30] Tail reads failed on lines outside the selected result

- **Context / Symptom:** Reading `tail.txt:-1` failed with an output-limit error when an earlier line had 65,537 bytes, even though the only requested line was short.
- **Root Cause:** The tail scanner buffered every line and rejected oversized text before evicting lines outside the requested tail.
- **Solution:** Tracked oversized tail lines without storing additional bytes and raised the output-limit error only when a selected tail line remained oversized. The fixture accepts the final short line via `:-1` and rejects `:-2`, which includes the oversized line.
- **Prevention / Reference:** Preserve the overall scan cap, NUL rejection and cancellation checks; test both exclusion and inclusion of the oversized line.

### [2026-09-30] Repeated file reads crowded the transcript

- **Context / Symptom:** Consecutive `read_file` results appeared as repeated tool titles, `completed · N lines · collapsed` rows and unhelpful `---` first-line previews, without showing which file had been read.
- **Root Cause:** All tools shared the same heading/status/first-content preview layout, and live tool-start events carried only a name and call ID.
- **Solution:** Passed a sanitized workspace-relative read target on the typed start event and rendered successful reads as one file-and-line-count row, keeping per-call expansion and restored history. Failed and aborted calls kept explicit error status and content. Colored and `NO_COLOR` PTY paints showed distinct file names without the `---` preview.
- **Prevention / Reference:** Keep generic tool/diff cards and file-read failure visibility as controls in transcript and TUI tests; do not surface absolute paths or URL query strings in the compact title.

### [2026-09-29] Standalone shell command ignored installer environment

- **Context / Symptom:** A v0.1.70 installation meant for a private release-upgrade directory instead printed `Installed pave to /Users/mingyu/.local/bin/pave`; the expected private executable was absent.
- **Root Cause:** The command runner ignored its `env` argument for a standalone command (reporting `Ignored env: service-only, and no service name was given`), so `PAVE_INSTALL_DIR` and `PAVE_VERSION` never reached `install.sh`. The installer used its default destination.
- **Solution:** Passed `PAVE_VERSION=v0.1.70 PAVE_INSTALL_DIR=/absolute/private/bin` as inline shell assignments to `sh install.sh`; the separately installed binary then reported the expected providers and v0.1.70 as current.
- **Prevention / Reference:** For one-off shell commands, pass required variables in the command itself and verify the installer's printed destination before invoking an isolated binary. The default user installation was also written during this session; its previous state was not recorded.

### [2026-09-29] Printed unified diffs lost their patch semantics in the TUI

- **Context / Symptom:** A raw `run_command` diff displayed `Status: exit 0` as its collapsed preview and parsed `- old` as a Markdown bullet when expanded; fenced `diff` output rendered all lines as identical code. A focused failure-before transcript test reproduced the missing addition/deletion distinction.
- **Root Cause:** `Transcript_view` applied ordinary Markdown list parsing to raw tool lines and one undifferentiated `Code` style to every fenced line. Tool previews selected the first nonempty line before seeing the diff file header.
- **Solution:** Classified sanitized unified-diff headers, hunks, changes, context and metadata in the existing transcript path; kept the literal `+`/`-` markers, reset raw diff state at boundaries and preferred file headers in collapsed command previews. Tinted diff rows in the TUI while retaining monochrome gutters. Focused tests and colored/`NO_COLOR` PTY paints displayed both an expanded raw diff and a fenced assistant diff.
- **Prevention / Reference:** Keep ordinary Markdown lists and non-diff code fences as controls; verify wrapped, streamed and collapsed/expanded diff rows through `test_transcript_view` and the opt-in `PAVE_REAL_DIFF_TUI=1` PTY smoke.

### [2026-09-29] Xcode 27 simulator destinations appeared empty despite installed runtimes

- **Context / Symptom:** On a macOS arm64 host with Xcode 27.0 and available iOS 26.5 simulators, the opt-in M08 disposable-project run returned `Xcode destination discovery: exit 0` but `Available iOS Simulator IDs: none`. A checked failure-before parser test reproduced the empty result using Xcode's actual `Destinations compatible with the "MobileFixture" scheme:` heading. After discovery worked, the fixture's first build exited 65 with `Build input file cannot be found: .../MobileFixture.app/Info.plist`.
- **Root Cause:** The parser recognized only older `Available destinations` / `Ineligible destinations` headings. The disposable xcodegen app fixture also omitted `GENERATE_INFOPLIST_FILE: YES`, so its Info.plist was never produced.
- **Solution:** Accepted both heading pairs, still selecting only UUIDs in the compatible iOS Simulator section. Enabled generated Info.plist in the disposable fixture and added a separate interactive confirmation for each real preflight command. An explicitly approved scheme probe, destination probe and non-signing simulator build then returned `Xcode build: exit 0`; CI continues to run fake-Xcode tests only.
- **Prevention / Reference:** Keep Xcode 27 and legacy headings, physical/placeholder/incompatible rows, missing-Xcode refusal and manual real-tool execution separate. One later manual destination probe temporarily returned no IDs before a subsequent approved run succeeded; its exact output was not retained, so no cause was assigned to that transient result.

### [2026-09-29] macOS release tests rejected the system `/var` alias

- **Context / Symptom:** v0.1.69 Release run 36542708863 passed Linux and failed both macOS jobs at `Run tests`. CI run 36550181657 identified `test_account_routing` failing with `Plugin registry: plugin directory path contains symlink or non-directory: /var`.
- **Root Cause:** macOS maps root-owned `/var` to `/private/var`; the plugin registry rejected every symlink ancestor, including this system-owned alias of the private temporary configuration directory.
- **Solution:** Accepted a root-owned directory symlink only beneath a root-owned non-group/world-writable parent when its target is a root-owned directory. User-owned symlinks still fail closed. Captured Dune output as a failure artifact in CI and release jobs for future platform-specific diagnosis.
- **Prevention / Reference:** Keep the user-owned symlink denial regression and verify the real macOS matrix before publishing a release.

### [2026-09-29] Release matrix rejected an OCaml 5.5 reserved identifier

- **Context / Symptom:** v0.1.68 Release run 36538729396 failed every platform at `Run tests`. A local CI-matched OCaml 5.5.1 switch reproduced `File "bin/main.ml", line 1123, characters 27-33: Error: Syntax error` on `let mcp_approve server effect =`; the local development compiler was OCaml 5.2.1.
- **Root Cause:** `effect` was parsed as a keyword by the release compiler but had been used as a parameter name in the new MCP approval callback. The earlier local 5.2.1 build did not detect the incompatibility.
- **Solution:** Renamed the parameter to `action` without changing approval behavior, then ran the full tests and install build with the release's OCaml 5.5.1/no-compression switch.
- **Prevention / Reference:** Check release-bound changes under the workflow compiler version before tagging; public GitHub job logs require sign-in, so an exact local compiler switch provides actionable parser diagnostics.

### [2026-09-29] Idle MCP connection approval was routed through an inactive turn

- **Context / Symptom:** The live `/mcp connect demo` PTY returned `MCP approval denied` without showing an approval prompt, even though an interactive TUI was active.
- **Root Cause:** The MCP callback used `Turn_runner.approve_tool` whenever a runner object existed. The idle runner rejected an approval not owned by an active model turn; `/mcp connect` originates on the UI thread, while later model-issued tool effects originate on the worker thread.
- **Solution:** Routed UI-thread connection approvals directly through `Tui.confirm_tool` and retained turn-owned `Turn_runner.approve_tool` for worker-thread tool effects. A live fake stdio server prompted before launch and listed its tool after `y`; a loopback Streamable HTTP server observed no requests before approval and then initialize, initialized notification and tools/list.
- **Prevention / Reference:** Preserve the distinction between interactive command consent and turn-owned tool consent when integrating new transports.

### [2026-09-29] Private plugin registry rejected a disposable smoke config

- **Context / Symptom:** A live PTY launch with an isolated R13 fixture exited before the TUI with `Plugin registry: plugin directory is not private and user-owned: /tmp/pave-r13-smoke-p8UOkC/config/pave`.
- **Root Cause:** `mkdir -p -m 700` applied the mode only to the final directory, leaving intermediate config directories mode 0755. The plugin registry correctly rejected a non-private config root.
- **Solution:** Set owner-only mode 0700 on the config directory and its `pave` child, then reran the PTY and observed the logo, MCP listing, explicit skill activation and draft-only prompt command insertion.
- **Prevention / Reference:** Create each user config ancestor with private permissions before writing plugin manifests; keep rejecting world-readable registry roots.

### [2026-09-29] Responses stream required a redundant item-done event

- **Context / Symptom:** A complete OpenAI Responses event fixture with an item-added event, text delta and full `response.completed.output` failed with `invalid Responses stream: completed output message mismatch` when `response.output_item.done` was omitted. A complete function-call fixture failed similarly.
- **Root Cause:** The final-envelope validator required each added item to have a preceding item-done event even though the final output item provided the full matching content.
- **Solution:** Validated unfinished streamed items against their corresponding complete final output item. Text/argument delta mismatches, changed IDs, malformed tools and missing final items still fail.
- **Prevention / Reference:** Preserve missing-item-done positive cases and mismatched-delta negatives in `test/provider/transports/test_openai_responses_stream.ml`; a complete final array is authoritative, not a substitute for an absent final response.

### [2026-09-29] Anthropic stream accepted a missing terminal event

- **Context / Symptom:** A closed Anthropic text or tool block followed by a stop reason but no `message_stop` could finish successfully. The transport also marked that partial stream finished, risking an early close before a delayed terminal event.
- **Root Cause:** `Anthropic_stream.finish` and `is_finished` required a terminal reason but not the protocol's explicit `message_stop`.
- **Solution:** Required `message_stop` before success or finished status; separately delivered terminal events complete normally. Incomplete streams retain no reported usage.
- **Prevention / Reference:** Exercise text, tool and delayed-terminal cases in `test/provider/transports/test_anthropic_stream.ml`.

### [2026-09-29] Codex completion required redundant output events

- **Context / Symptom:** A Codex turn showed `invalid completion response: invalid Codex stream: missing completed output item`. A local event fixture reproduced that exact error when `response.output_item.added` and a complete `response.completed.output` arrived without an intermediate item-done event. Another fixture reproduced it with completed item-done events and an empty final output array. The user's raw vendor event trace was unavailable.
- **Root Cause:** The stream parser required both a per-item completion event and the corresponding final output-array entry whenever an item-added event had been seen, even when one complete source was authoritative.
- **Solution:** Validated an item against a nonempty completed output array when its item-done event was absent; reconstructed an omitted/empty array only when every streamed item was completed. Kept delta/final mismatches, incomplete items and mismatched output counts invalid. A pinned-account fake-HTTPS Codex tool turn exercised the missing item-done case.
- **Prevention / Reference:** Keep real completion-envelope and streamed-item variants in `test/provider/transports/test_codex_stream.ml`; never accept a final response with neither complete items nor a complete output array.

### [2026-09-29] Codex model eligibility accepted malformed metadata

- **Context / Symptom:** A fixture model row with `supported_in_api: "false"` was offered as selectable by discovery, and the direct Codex request-format lookup did not reject it. This is a verified parser defect; no live account listing was available to establish whether it caused the user's reported model retrieval issue.
- **Root Cause:** Both paths rejected JSON `false` but implicitly treated every other value, including a string, null or integer, as supported.
- **Solution:** Required an absent or boolean eligibility field. JSON `false` remained excluded from discovery and rejected for inference; malformed fields now invalidate the discovery listing or request-format lookup without inventing models.
- **Prevention / Reference:** Check malformed and valid eligibility rows in both discovery and wire-format regressions; retain pinned endpoint and account headers.

### [2026-09-29] Release verification host lacked GitHub CLI

- **Context / Symptom:** `gh run list` failed with `error: command not found: gh` after pushing the release tag.
- **Root Cause:** The local WSL environment did not have the GitHub CLI installed.
- **Solution:** Queried the public GitHub Actions and Releases REST endpoints with `curl` and `jq`; no local CLI installation was needed.
- **Prevention / Reference:** For public release verification, use `https://api.github.com/repos/kimmandoo/pave/actions/workflows/release.yml/runs` and `/releases/tags/v0.1.66` when `gh` is unavailable.

### [2026-09-29] macOS mobile tests compared symlinked temporary paths

- **Context / Symptom:** CI run 36512074826 failed both macOS matrix jobs at `Run tests` while Ubuntu passed; unauthenticated job logs returned HTTP 403. The focused mobile tests had constructed temporary roots under macOS `/var`, then compared those literal paths against workspace helpers returning canonical `/private/var` paths.
- **Root Cause:** macOS `/var` aliases `/private/var`; path equality in disposable fixtures was not comparing canonical paths. The mobile execution helper also needed to normalize its workspace root before manifest hashing.
- **Solution:** Normalized temporary fixture roots with `Unix.realpath`, normalized the selected `mobile_check` root, and split macOS focused tests into named CI steps for diagnosis. In run 36513165905 both macOS jobs passed their focused and full test suites; the 5.5.1 job subsequently reached the separate real Xcode acceptance step.
- **Prevention / Reference:** Compare canonical paths in tests that assert resolved workspace locations. CI real-toolchain failures should surface a named step and bounded GitHub annotation rather than require private job logs.

### [2026-09-29] Windows Flutter launcher could not run under WSL

- **Context / Symptom:** `flutter --version` on the Linux workstation called `/mnt/c/src/flutter/bin/internal/shared.sh` and failed with `$'\\r': command not found` (exit 127). `swift`, `gradle` and `xcodebuild` were also absent from PATH.
- **Root Cause:** PATH pointed to a Windows checkout of the Flutter SDK whose shell scripts have CRLF line endings; it is not a usable Linux Flutter toolchain. The other platform binaries were not installed locally.
- **Solution:** Kept platform execution cards open and configured disposable real-toolchain acceptance on hosted macOS/Linux CI. Local npm execution established only the RN/Expo card; no fake Flutter/Swift/Gradle/Xcode success was reported.
- **Prevention / Reference:** Provision a Linux Flutter SDK on Linux or use the hosted Flutter setup action. Run `flutter --version` on the target OS before claiming a Flutter check.

### [2026-09-29] Dune build reported `Unbound module "Pave"` on an unmodified tree

- **Context / Symptom:** After an extra-warnings build into a separate `--build-dir` with `DUNE_CACHE=disabled`, `dune build @install` in the repo failed on every `pave__*` module with `File "command line", line 1: Error: Unbound module "Pave"`; `_build/default/lib/.pave.objs/byte/pave.cmi` was missing while `pave.ml-gen` existed. A stashed (clean) tree failed the same way.
- **Root Cause:** The default `_build` tree was left inconsistent (library alias module not rebuilt); source was not at fault.
- **Solution:** `dune clean && dune build @install` restored a green build.
- **Prevention / Reference:** Run side builds from a copy of the tree, or run `dune clean` before trusting a failing `_build` after experimenting with `--build-dir`/`--workspace`.

### [2026-09-29] WSL host had no OCaml toolchain and rejected the Ollama PTY endpoint override

- **Context / Symptom:** `opam: command not found` on the WSL host (no `_opam`, no `dune`, no sudo). A PTY fixture launched with `--provider ollama --endpoint http://127.0.0.1:18434/api/chat` displayed `Error: remote endpoint overrides are disabled; define a custom provider in user settings`.
- **Root Cause:** The host had never been provisioned for OCaml. The local Ollama route intentionally forbids endpoint overrides, so a fixture must listen on its pinned `127.0.0.1:11434`.
- **Solution:** Installed the static opam 2.3.0 binary to `~/.local/bin`, ran `opam init --bare --disable-sandboxing`, created switch `pave-dev` (OCaml 5.2.1) and installed `dune yojson notty-community uutf uuseg uucp digestif`. Ran the loopback Ollama fixture on port 11434 with no `--endpoint`, inside tmux for screen captures.
- **Prevention / Reference:** `export PATH=$HOME/.local/bin:$PATH; eval $(opam env --switch pave-dev --set-switch)` before `dune`. Use `tmux capture-pane -p -e` to verify colors; do not `pkill -f` a pattern that also matches the invoking shell command.

### [2026-09-29] Groovy Gradle inventory omitted the first included module

- **Context / Symptom:** The bounded mobile inventory reported `:legacy-lib:shared` but omitted `:legacy-app` from `include ':legacy-app', ':legacy-lib:shared'`.
- **Root Cause:** The bare-include parser consumed its first literal while matching the Groovy form, then split only the remaining tokens.
- **Solution:** Preserved that first argument, limited inferred modules to top-level unconditional literal includes, and kept dynamic, conditional and unsupported includes unresolved. Regression coverage exercised Groovy multi-argument includes, a static include after interpolation, and misleading method/conditional calls; the tool test passed without executing Gradle.
- **Prevention / Reference:** Keep positive coverage for every supported include spelling and pair dynamic/conditional cases with a later static include so scanning cannot silently stop early.

### [2026-09-29] TUI mouse wheel left transcript scroll unchanged

- **Context / Symptom:** Vertical mouse-wheel input did not move the interactive transcript.
- **Root Cause:** The terminal was created with mouse reporting disabled and the input loop discarded all mouse events.
- **Solution:** Enabled terminal mouse reporting and mapped only wheel-up/down events to the existing bounded transcript scroll. A real 100×30 PTY with a loopback Ollama fixture showed wheel-up moving from later transcript markers to earlier ones, then wheel-down restoring the later viewport; clicks and unrelated input remained non-scrolling.
- **Prevention / Reference:** Verify the emitted SGR mouse protocol and visible viewport transition in a real PTY; a decoder-only unit test does not prove terminal reporting is enabled.

### [2026-09-29] Short streamed replies paused until the activity heartbeat

- **Context / Symptom:** In a real PTY, a local SSE fixture emitted two short text deltas 3 ms apart and then paused; the second delta did not appear for 771 ms, despite already arriving over HTTP.
- **Root Cause:** `Tui.delta` skipped a repaint when a delta arrived inside the 60 Hz frame interval but did not schedule a later frame. The only remaining timeout was the once-per-second activity heartbeat. Newline-containing chunks bypassed the 60 Hz limit and could instead trigger excessive repaints.
- **Solution:** Tracked pending stream paint, combined its frame deadline with the activity timeout, and cleared it after any repaint; newline chunks use the same frame pacing, while turn completion still flushes immediately. The same real PTY showed the formerly delayed delta after 14 ms, and a 600-delta multiline SSE run kept its sampled display lag under 16 ms.
- **Prevention / Reference:** Test a stream that emits a sub-frame delta then stalls before completion. A frame-rate check alone misses an unscheduled final partial frame.

### [2026-09-29] At-file attachments were invisible until Tab

- **Context / Symptom:** Typing `@` in the interactive editor showed no attachable file choices, although pressing Tab opened a path chooser; staged `/attach` media previews did not solve this selector gap.
- **Root Cause:** The inline hint list only handled slash commands. Workspace file completion lived behind the explicit Tab callback, so it never painted while composing an `@` reference.
- **Solution:** Reused checked workspace candidates in an inline `@` selector, filtered files through bounded UTF-8 or media-signature inspection, and kept Tab/Enter insertion, directory chaining and Escape dismissal separate from prompt submission. Verified a real PTY displayed choices before Tab and sent a selected image as native provider content.
- **Prevention / Reference:** Smoke the *interactive composer before submission*, not only the staged attachment display or completion modal; assert that invalid and escaping paths are absent.

### [2026-09-29] Nested at-file search rejected macOS temporary workspaces

- **Context / Symptom:** The nested-file regression failed with `Pave.Workspace_path.Error("path escapes workspace: .")` when searching a temporary workspace.
- **Root Cause:** The fuzzy search received a workspace root beneath macOS `/var`, whose canonical path is `/private/var`; the checked path walker compares canonical paths to its root argument.
- **Solution:** Canonicalized the root before calling the existing bounded fuzzy search. The nested image query and exact mention insertion then passed both regression and a real PTY smoke.
- **Prevention / Reference:** Pass `Workspace_path.root_path root` to checked recursive traversals, including in fixtures created under `/var`.

### [2026-09-28] Nested Xcode projects were omitted from mobile inventory

- **Context / Symptom:** `mobile_project` listed nested Gradle settings but omitted a valid `ios/App.xcodeproj/project.pbxproj` in the same workspace.
- **Root Cause:** The directory visitor sequenced two OCaml `if` expressions without isolating their branches; the `.xcodeproj` test was parsed within the `.xcworkspace` branch and never ran for ordinary project directories.
- **Solution:** Made Xcode workspace and project checks mutually exclusive, recorded the actual Xcode manifest path, and verified nested iOS plus Android manifests together in a real workspace-tool fixture.
- **Prevention / Reference:** Parenthesize side-effecting conditional branches and assert each supported directory kind in the same filesystem regression.

### [2026-09-28] R12 media and mention completion accepted mismatched inputs

- **Context / Symptom:** AAC ADTS bytes saved with an `.mp3` extension were accepted as MP3. Completing `@src/a` to a filename containing commas, quotes or a terminal period produced a reference that was not parsed as that filename. A quoted mention could span a newline, and an inline-code mention crossing lines could be attached.
- **Root Cause:** The MP3 signature recognized any `FF Ex` sync prefix, including AAC headers; completion did not quote punctuation recognized as mention delimiters. Quoted references and inline-code state searched or reset across line boundaries incorrectly.
- **Solution:** Checked the MPEG Layer III header bits before accepting raw MP3 frames, quoted punctuation-bearing completions, confined quoted references to a line, and retained inline-code state across lines. Preserved email-like and quoted references while expanding opt-in prose shortcuts.
- **Prevention / Reference:** Round-trip completed paths through the mention parser and attachment loader; verify mismatched media signatures, incomplete quotes and code spans at line boundaries.

### [2026-09-28] Generated shell completions omitted option values

- **Context / Symptom:** Fish offered no `jsonl` for `--output j` or voices for `task speak --voice a`; Bash offered global flags after `update --` and did not complete voices. Bash model completion after `ollama@ch` also needed candidates relative to Readline's `@` word break.
- **Root Cause:** Fish registered enum candidates separately from required option arguments, task completion treated option values like flags, and Bash returned whole model selectors even when Readline replaced only the suffix after `@`.
- **Solution:** Registered required Fish options with their choices, consumed pending Bash/Zsh task values, constrained subcommand flags, and adapted Bash model candidates to Readline word boundaries. Added executable-generated Bash/Fish completion scenarios for values, prefixes, workspace roots and dash-prefixed arguments.
- **Prevention / Reference:** Source generated scripts and exercise actual shell completion functions instead of asserting script text; use a workspace root containing spaces.


### [2026-09-28] Paste provenance rewrote the wrong shortcut token

- **Context / Symptom:** Typing `thinkdeep`, moving to the start and bracket-pasting `thinkdeep ` marked the original typed token as pasted while the inserted token could be expanded.
- **Root Cause:** Paste tracking inferred an inserted span from the longest common prefix/suffix after the edit; repeated text made that diff ambiguous. Inline-code detection also restarted on each line even when a backtick span crossed a newline.
- **Solution:** Tracked the actual insertion range while the paste was active, normalized grapheme boundaries, and carried an inline-code closing position across lines. Focused composer and shortcut regressions passed with repeated text, identical replacements, combining marks and multiline inline code.
- **Prevention / Reference:** Preserve input provenance at the edit boundary; a content diff cannot identify which copy of repeated text came from a paste.

### [2026-09-28] Workspace mentions lost punctuation and missing-path completion failed

- **Context / Symptom:** `@src/notes.md.` stayed literal even though `src/notes.md` existed; Tab after `(@src/no` did not offer a match, and completing `@src/not-yet/` raised a filesystem error.
- **Root Cause:** The unquoted parser treated terminal periods as part of a filename, completion tokenization did not recognize punctuation that the reference parser accepted, and the walker was called for absent/non-directory parents.
- **Solution:** Trimmed trailing unquoted periods without consuming the sentence punctuation, aligned completion boundaries with mention parsing, and returned an empty result for missing/non-directory parents. The focused mention regression passed.
- **Prevention / Reference:** Exercise a reference in ordinary sentence punctuation and both existing and missing completion directories.

### [2026-09-28] Fish completion evaluated dynamic candidates while sourcing

- **Context / Symptom:** The R12 generator emitted unquoted `-a (pave __complete ...)` for model/session values, attempting to evaluate `commandline -ct` while loading the completion script instead of when completing a value.
- **Root Cause:** Fish evaluates an unquoted command substitution in the command invoking `complete`. Fish's `complete -a` deliberately accepts a **quoted** substitution string and evaluates it later for each completion; the previous session's source-only diagnosis reversed these two stages.
- **Solution:** Quoted the complete `-a` substitution so candidate lookup is deferred, replaced `__fish_use_subcommand` with predicates based on the actual first subcommand, and passed an explicitly selected workspace root to local candidate lookup. Installed Fish for verification: `fish -n` parsed the script and `complete -C` offered global flags after `--provider`, task operations and task-specific flags. Bash/Zsh syntax and Bash completion scenarios also passed.
- **Prevention / Reference:** Verify evaluation timing in the [fish `complete` documentation](https://fishshell.com/docs/current/cmds/complete.html), not from general shell quoting rules alone.

### [2026-09-28] TUI path completion rejected a canonical macOS workspace

- **Context / Symptom:** Completing a path below a temporary workspace failed with `Workspace_path.Error("path escapes workspace: src")` on macOS.
- **Root Cause:** `Unix.realpath` canonicalizes Darwin temporary paths from `/var/folders/...` to `/private/var/folders/...`; `Tools.walk` requires its workspace root to be canonical, but file-mention completion passed the lexical root.
- **Solution:** Canonicalized the root with `Workspace_path.root_path` before walking completion candidates. The focused file-mention test then completed paths under the canonical root while preserving traversal and symlink checks.
- **Prevention / Reference:** Pass canonical workspace roots to `Workspace_path.checked_path` and `Tools.walk`; macOS temporary-directory aliases expose lexical/canonical mismatches.

### [2026-09-28] macOS rejected a malformed UTF-8 filename fixture

- **Context / Symptom:** The file-completion test stopped at `Sys_error(".../invalid-\\255.txt: Illegal byte sequence")` before candidate filtering ran.
- **Root Cause:** The macOS filesystem/path layer refused to create a filename containing invalid UTF-8 bytes.
- **Solution:** Kept the malformed-name candidate assertion on filesystems that permit the fixture and skipped only the exact `Illegal byte sequence` creation failure. Other filesystem errors still fail the test.
- **Prevention / Reference:** Make malformed-byte path fixtures conditional on filesystem support; do not treat an OS-level `EILSEQ` as a path-completion result.

### [2026-09-28] Scoped Devin model failed when multiple accounts were saved

- **Context / Symptom:** Launching `--model 'devin@connect#ACCOUNT/MODEL'` with two saved Devin grants failed before opening the UI: `Error: multiple saved accounts for devin; pass --account or select an account-scoped model`.
- **Root Cause:** Startup eagerly resolved an unscoped discovery credential and configured default before applying the selected model's account identity. Even a valid scoped selector or saved workspace model could not bypass that premature ambiguous lookup.
- **Solution:** Resolved the explicit or saved model account first, inferred only a unique grant, and deferred genuinely ambiguous interactive selections to a chooser before prompt submission. Passed the bound account through automatic context-window discovery too. The selected account is saved with the model; cancelling restores the draft. Headless requests still require `--account` or a scoped selector. An isolated two-grant regression and real TUI verified scoped launch, account selection, credential isolation and fail-closed headless behavior without vendor traffic.
- **Prevention / Reference:** Never resolve an accountless credential before checking the exact selected model identity; model availability and account ordering are not proof of which credential may receive a prompt.

### [2026-09-28] A termination signal was swallowed while a command chooser was open

- **Context / Symptom:** During the unsaved-conversation confirmation opened by `/new`, `SIGTERM` dismissed the chooser but left the interactive process running after five seconds rather than exiting and restoring the terminal.
- **Root Cause:** The interactive command dispatch caught every exception from a slash command, including the TUI's `Terminal_signal`, and routed it through the ordinary user-facing error reporter. The outer terminal-cleanup handler never received it.
- **Solution:** Re-raised terminal signals from command error reporting. An isolated 52×14 PTY reopened the unsaved `/new` confirmation, sent `SIGTERM`, observed exit 143, and confirmed canonical/input/echo modes were restored. Darwin can set the transient `PENDIN` bit when restoring termios; compare functional modes rather than requiring that bit to match.
- **Prevention / Reference:** Exercise shutdown with an active chooser, not only from the idle editor; a user-facing command error handler must never turn a terminal signal into an ordinary notice.

### [2026-09-28] A selected model reverted after relaunch

- **Context / Symptom:** An interactive `/model` selection sent the chosen ID to local Chat inference, but closing Pave and relaunching in the same workspace restored the configured default instead.
- **Root Cause:** Model switches updated the current agent and optional conversation journal but never recorded a workspace-scoped choice when the conversation had no saved journal. User/project setup defaults were the only startup fallback.
- **Solution:** Stored the exact provider/account/route/upstream-ID tuple and custom-route revision in an owned, atomically replaced private workspace state file after successful interactive selection. Fresh interactive launches restore it unless an explicit CLI/account/session selector wins; missing, malformed, stale custom-route and insecure state falls back to settings. An isolated LM Studio PTY reproduced `initial` instead of `picked-local-model` before the change, then relaunched with `picked-local-model` and sent that exact ID to `/v1/chat/completions`; an explicit `--model initial` and another workspace retained their own defaults.
- **Prevention / Reference:** A displayed model label is not a model identity. Test a completed selection across process restart, account/route preservation, CLI precedence and private state permissions, not just within one conversation.

### [2026-09-28] Hosted CI repeated metadata and helper build work

- **Context / Symptom:** The four-job main CI matrix linted the same opam metadata on every host/compiler combination and rebuilt the macOS helper after `@install`; hosted jobs still spent substantial time on cold dependency installation and compiler setup.
- **Root Cause:** Static opam lint is platform-independent, and `@install` already includes the native helper. Separate compiler/OS test jobs and native release jobs cover different contracts; removing full suites or relying on an unpinned shared dependency cache would weaken validation without a demonstrated speedup.
- **Solution:** Kept the four compiler/OS test jobs and native package/extracted-binary safeguards, ran `opam lint` only once on Ubuntu/OCaml 5.5.1, and reused `@install` for the helper's `--help`/provider smoke. `actionlint` passed. Cold opam dependency downloads, compiler setup and forced tests remain the dominant costs; no broad CI time reduction is claimed.
- **Prevention / Reference:** Compare job-step timings before removing coverage; preserve serial `dune runtest --force -j 1` because parallel hosted fixture failures were previously reproduced.

### [2026-09-28] A listed model was not usable for the selected inference route

- **Context / Symptom:** Model selection could offer a Codex row marked `supported_in_api: false` and a Devin row explicitly marked `supportsToolCalls: false`; selecting either for an agent turn failed before the expected answer. A Devin router with no `modelFeatures` field failed the local tool preflight despite upstream allowing tools by default.
- **Root Cause:** Codex discovery ignored the account listing's API support flag, Devin discovery represented omitted feature metadata as unknown instead of enabled, and the picker did not exclude Devin models explicitly unable to accept Pave's tools.
- **Solution:** Excluded Codex API-disabled rows from account discovery and explicitly tool-disabled Devin rows from the interactive picker; treated omitted Devin router features as tool-capable while retaining explicit false as denied. An account-list regression failed before the Codex filter; discovery, picker and routed two-turn Devin fixtures passed afterward.
- **Prevention / Reference:** The upstream Devin router treats an absent `modelFeatures` field as enabled; do not conflate absence with an explicit false. Live listing is not an inference authorization or uptime check.

### [2026-09-28] Slash suggestions swallowed ordinary editing keys

- **Context / Symptom:** With `/model` suggested, pressing Backspace left `/mo` unchanged. Word deletion, cursor movement and undo were also unavailable while the inline hint focus was active.
- **Root Cause:** The hint keymap handled only selection, insertion and dismissal, while composer editing keys were looked up exclusively under composer focus.
- **Solution:** Applied hint-specific bindings first, then fell back to the composer keymap for otherwise unhandled hint keystrokes. A real 70×18 PTY showed `/mo` become `/m` while `/model` remained suggested; keybinding regressions covered plain/modified erase, movement, undo and modal Enter precedence. Removed duplicate shortcut text from the hint footer and shortened compact navigation hints.
- **Prevention / Reference:** Test an actual draft while a live hint overlay owns focus; modal selection must not replace ordinary draft editing.

### [2026-09-28] Codex account model rejected a standard Responses request

- **Context / Symptom:** Selecting `openai-codex@responses#.../gpt-6-luna` produced `Request error: invalid provider request (HTTP 400)`. The old request path used standard top-level Responses tools for every Codex model.
- **Root Cause:** The official Codex model metadata marked `gpt-6-luna` with `use_responses_lite: true`. That protocol uses a developer `additional_tools` namespace, developer instructions, all-turn reasoning context and a Lite request header rather than standard Responses tools.
- **Solution:** Read the exact signed-in account's model format through a bounded pinned HTTPS listing before inference and serialized either Standard or Lite accordingly. Kept native tool replay and existing saved state compatible; rejected absent/unknown formats before posting. An isolated two-turn fake-HTTPS fixture verified account A's Lite request, account B isolation and a redacted HTTP 400 with its diagnostic.
- **Prevention / Reference:** Inspect `use_responses_lite` in the authenticated model listing rather than guessing from a model name. A fake account listing cannot establish live entitlement; upstream 400 responses may still indicate access or provider failures.

### [2026-09-28] Devin Gemini router rejected nullable tool schemas

- **Context / Symptom:** Devin Connect returned `invalid_argument: an internal error occurred (trace ID: …)` after a model turn. A router assignment can send otherwise valid Pave tool schemas containing `type: ["string","null"]` to a Gemini backend.
- **Root Cause:** The upstream Devin Gemini tool adapter rejects JSON Schema `type` arrays for nullable tool parameters and reports an opaque Connect error instead of a field-specific validation message. A server-side internal error without tools cannot be attributed to this schema mismatch.
- **Solution:** Normalized nullable type unions to a single type plus `nullable: true` for an actual Gemini-assigned ID or a directly selected Gemini model with an opaque assigned ID; retained required fields, non-Gemini schemas and signed replay. Sanitized Connect error diagnostics while keeping the failure and trace ID visible. A routed two-turn fixture verified the transmitted schema, tool result and error trailer.
- **Prevention / Reference:** Check both the selected model and router-assigned backend for Gemini schema constraints; do not suppress Connect failures or claim all vendor-side traces are client errors.

### [2026-09-28] Active row flickered and full draft rejected selected paste

- **Context / Symptom:** A delayed model PTY painted eighteen spinner frames and eighty-one full-row erasures in 2.2 seconds. With a 16,384-byte draft, bracket-pasting a replacement over selected text silently dropped it.
- **Root Cause:** The 125 ms animation cadence redrew the activity row before elapsed seconds changed; both the TUI row clear and Notty's leading erase blanked it before each paint. Paste capacity subtracted the entire draft without crediting bytes that the selected replacement removed.
- **Solution:** Advanced activity at most once per second, rendered it before clearing only trailing cells even in a three-row terminal, and clipped long phase names at grapheme boundaries to retain the timer. Computed paste capacity after removing selected bytes and reported truncation on excess. Real PTYs displayed consecutive `Thinking · 0s/1s/2s` frames with zero pre-text row erasures, accepted the selected replacement, displayed the truncation notice, completed the local SSE answer, and retained an unsent draft across 100×24→30×3→70×18 resize and cancellation.
- **Prevention / Reference:** Inspect emitted ANSI bytes as well as the logical row diff; optimized renderers can insert an implicit pre-text erase. Exercise bracketed paste near the byte limit through a real terminal, including an active selection.

### [2026-09-28] Published narrow TUI hid the completed answer

- **Context / Symptom:** The checksum-verified v0.1.58 Darwin arm64 executable rendered `Thinking · 0s/1s/2s` in a real 30×3 PTY but did not display a successfully completed local SSE answer; the activity row simply became empty. This escaped the 24-row answer smoke.
- **Root Cause:** The compact rendering branch reserved footer and composer rows but filled every spare row with blank padding instead of the existing transcript layout. A three-row terminal had one available row after activity ended.
- **Solution:** Rendered the latest visible transcript lines (or active chooser title) in compact spare rows, respecting scroll and leaving the footer/editor intact. Kept the v0.1.58 tag immutable and shipped v0.1.59; the extracted Darwin arm64 binary displayed `PTY smoke complete` in the first row of a 30×3 terminal without resizing.
- **Prevention / Reference:** Exercise the *extracted release binary* at minimum supported viewport sizes through completion, not just during the spinner phase; verify both active and idle screen contents.

### [2026-09-28] NVM-installed JavaScript runtime was not found

- **Context / Symptom:** The persistent-evaluation regression reported that Node.js was unavailable even though Node 24 was installed in the active `NVM_BIN` directory.
- **Root Cause:** Runtime discovery covered fixed system and Homebrew candidates but omitted the active NVM installation.
- **Solution:** Added the absolute `$NVM_BIN/node` candidate without widening the evaluator's child-process environment. The focused persistent-evaluation test passed.
- **Prevention / Reference:** Exercise runtime discovery with supported version managers while keeping runtime lookup separate from the evaluator's explicit environment allowlist.


### [2026-09-28] Terminal Enter decoded as Ctrl+M

- **Context / Symptom:** A real PTY initially accepted prompt text but never submitted it: `Notty.Unescape` emitted bare carriage return as `ASCII M` with `Ctrl`, while the composer submits only `Enter`. After CR/LF normalization, bracket-pasted CRLF still arrived as two line breaks.
- **Root Cause:** `Notty.Unescape` preserves CR as a control character, and the terminal's raw-mode `ICRNL` flag translated CR to LF before the decoder, making pasted CRLF indistinguishable from two LF bytes.
- **Solution:** Normalized physical CR/LF to `Enter`, coalesced LF after CR, preserved ESC+CR as Meta+Ctrl+M, and disabled `ICRNL`, `INLCR` and `IGNCR` while the TUI owns the terminal. Decoder regressions passed; a real 52×14 PTY submitted bracket-pasted CRLF as exactly one draft newline and restored original termios on SIGTERM.
- **Prevention / Reference:** Test literal input bytes through an actual PTY as well as the decoder; inspect termios flags because the PTY line discipline can transform bytes before the decoder sees them.


### [2026-09-28] Darwin signal exit status used OCaml's abstract signal ID

- **Context / Symptom:** A PTY restored terminal attributes on `SIGTERM` but exited with status `117` instead of the conventional `143`; `Sys.sigterm` evaluated to `-11` on the macOS OCaml runtime. Unhandled external `SIGINT` also bypassed cleanup.
- **Root Cause:** `Sys.sig*` values are OCaml runtime signal identifiers on Darwin, not positive POSIX numbers suitable for exit statuses; external `SIGINT` had no cleanup handler.
- **Solution:** Mapped handled signals to POSIX numbers before raising the terminal exception (`INT=2`, `HUP=1`, `QUIT=3`, `TERM=15`, and `TSTP=18` on Darwin/`20` on Linux). Real PTYs verified SIGINT status 130, SIGTERM 143 and SIGTSTP 146, each with termios restored.
- **Prevention / Reference:** Keep `Sys.signal` identifiers separate from the POSIX number added to exit status, and exercise cleanup for external signals through a real PTY.

### [2026-09-28] Kitty image frames omitted the graphics introducer

- **Context / Symptom:** `test_terminal_image` received `ESC _ a=T,...` while a Kitty graphics APC requires `ESC _ G a=T,...`; image frames and deletion commands were not protocol-compliant.
- **Root Cause:** The shared APC wrapper emitted the generic `ESC _` prefix but omitted Kitty's required `G` introducer.
- **Solution:** Changed the Kitty APC wrapper to emit `ESC _ G` for image chunks and clear operations. Exact framing, chunk-boundary and deletion tests passed.
- **Prevention / Reference:** Verify terminal image protocols with byte-exact fixtures for opening introducers, payload chunks and closing delimiters.

### [2026-09-28] DAP fixture assigned message sequences during list construction

- **Context / Symptom:** The DAP regression failed with `DAP adapter sequence is duplicate or out of order` while its fake adapter emitted `initialized`, responses and stopped events.
- **Root Cause:** The fixture incremented the adapter sequence counter from calls embedded in list construction and append expressions, leaving sequence assignment dependent on expression evaluation order rather than explicit wire order.
- **Solution:** Constructed each event/response frame in sequential `let` bindings before assembling the outgoing list. The fragmented DAP lifecycle test passed.
- **Prevention / Reference:** Keep stateful message-ID allocation separate from list/tuple construction and verify IDs in actual transport order.

### [2026-09-28] Hosted process-listener readiness fixture timed out

- **Context / Symptom:** Main CI `36333579512` passed three OCaml jobs, including macOS 5.3.0 after the ADC reaping fix, but macOS 5.5.1 failed the bare assertion at `test/tools/test_workspace_process.ml:183` while waiting for a Python listener's log and loopback port.
- **Root Cause:** The fixture allowed only three seconds for a cold Python startup and port/log observation while the child exited after five seconds. The failed run reported no child status/output, so the exact reason readiness was false is unknown; the narrow lifetime and timeout made the fixture sensitive to hosted scheduling.
- **Solution:** Extended the test listener lifetime to fifteen seconds, allowed eight seconds for both readiness conditions, and made any future failure report its child termination and captured output. The production readiness contract and both required checks remained unchanged.
- **Prevention / Reference:** Keep a test service alive longer than its startup deadline and report status/output on failed readiness, rather than relying on a bare assertion.

### [2026-09-28] Model chooser mixed available and unverified IDs

- **Context / Symptom:** `/model` displayed static suggestions, route actions and manually typed model IDs alongside live model listings, even when those models were not usable on the selected account/API route.
- **Root Cause:** The dynamic chooser merged suggestion and custom-entry sources with discovery results; a successful public catalog or OS default was also not evidence of route compatibility, credentials or device readiness.
- **Solution:** Restricted selectable model rows to fresh, route/account-matched listings, kept setup Back/Skip as separate controls, and reported skipped/failed scopes as status instead. Excluded unclassified IDs, public listings without required inference keys and OS defaults without a readiness probe. Kept explicit CLI/session model selection available for known IDs.
- **Prevention / Reference:** Test model eligibility separately from listing parsing and exercise `/model` in a real PTY with only a local provider; listing success does not guarantee inference permission.

### [2026-09-28] Approval became unusable when the terminal shrank

- **Context / Symptom:** A pending shell/tool approval immediately returned denial when a resize made the full preview no longer fit, without telling the operator why. Accepting an incomplete preview would be unsafe.
- **Root Cause:** `Tui.confirm_review` treated any non-fitting `Resize` as a denial and did not reserve space for the active row or shortcut hints when calculating review visibility.
- **Solution:** Kept the pending approval across resize, displayed a compact resize hint and ignored `y` while the complete preview was clipped; after restoring a fitting viewport, the prompt became actionable again. A real loopback-model PTY smoke shrank from 80×24 to 28×10, confirmed `y` could not run the command, restored 80×24 and completed the approved tool.
- **Prevention / Reference:** Check the complete review area against all reserved UI rows on every approval key and resize, not only when the prompt opens.

### [2026-09-27] R8 native archives acquired a dynamic zstd dependency

- **Context / Symptom:** Extracted v0.1.50 Darwin arm64 and x86_64 binaries linked `/opt/homebrew/opt/zstd/lib/libzstd.1.dylib` and `/usr/local/opt/zstd/lib/libzstd.1.dylib`. The Intel Rosetta smoke failed with `dyld: Library not loaded` on this arm64 host without Intel Homebrew zstd; v0.1.49's Intel binary linked only `libSystem`. Hosted v0.1.50 smoke passed because its runners had zstd installed.
- **Root Cause:** The release compiler's default OCaml 5.5.1 compression support caused compiler-libs native stubs to link against external libzstd; the runner's build dependencies masked the release binary's runtime requirement.
- **Solution:** Selected `ocaml-variants.5.5.1+options,ocaml-option-no-compression` for release builds and added extracted-binary `otool -L`/`ldd` checks that reject libzstd. Corrective v0.1.51 workflow `36326312932` attempt 2 passed all four native test/build/archive/runtime-dependency/smoke jobs and published. Downloaded checksums and exact archive members passed; extracted Darwin arm64 and Intel `otool -L` listed only `libSystem`, and local `--help`/`--providers` smokes passed natively and under Rosetta.
- **Prevention / Reference:** Check extracted native binaries' dynamic dependencies on every release target; `--help` on a build runner does not prove portability when its build toolchain installed additional shared libraries.

### [2026-09-27] Hosted matrix child-process fixtures failed intermittently

- **Context / Symptom:** Hosted workflows `36308330776` and `36308330884` reported AWS credential, OpenSSL signer and curl helper failures. Serial CI `36309981578` failed `test_vertex_wire`; removing a redundant test-side fork passed `36310428334`. v0.1.49/v0.1.51 release jobs and docs-only CI `36327731436` intermittently failed `test_vertex_auth`. After v0.1.52 shipped, docs-only CI `36330225537` failed macOS `test_vertex_anthropic_api` and Linux `test_vertex_wire` with generic ADC helper messages. With safe errno diagnostics in place, CI `36332964471` failed `test_vertex_auth` on both macOS versions with `Google ADC openssl subprocess failed (waitpid: No child processes)`. After the Vertex fix, CI `36334107797` passed Linux and macOS 5.3 but failed macOS 5.5 `test_bedrock_wire` with generic `could not start AWS credential_process`.
- **Root Cause:** Both `Vertex_auth.execute` and `Aws_auth.run_child` called `waitpid` on every loop iteration until both stdout EOF and the exit status were observed. If a direct child exited before the output pipe closed, the first wait reaped it and a subsequent wait raised `ECHILD`. Vertex surfaced that errno; AWS mapped every parent-side `Unix_error` to a generic missing helper message. The original test-side fork was independently unnecessary.
- **Solution:** Serialized hosted forced suites, removed the redundant test-side fork, retained safe ADC syscall/errno diagnostics and made both helpers wait only while the child's status is unknown, continuing to drain stdout afterward. Added a delayed-EOF descendant regression for each helper. No retry, test skip or fake fallback was added.
- **Prevention / Reference:** A child PID may be waited only once; output EOF and child exit are separate lifecycle conditions. Preserve exact syscall/errno without credential paths for any future helper failures, and test delayed EOF after child reaping.

### [2026-09-27] R7 integration exposed OCaml binder and record inference errors

- **Context / Symptom:** Initial forced tests/builds reported syntax errors from using `effect` as an identifier, a malformed task-dispatch `try ... with`, missing `Agent.create` callback arguments, and ambiguous artifact/job record fields. A fake OpenAI endpoint TUI smoke was also rejected with `remote endpoint overrides are disabled; define a custom provider in user settings`.
- **Root Cause:** `effect` is reserved in OCaml; overlapping record labels needed explicit types; integration edits omitted optional arguments and the local child-job unit terminator. Pinned remote APIs intentionally reject arbitrary endpoint overrides.
- **Solution:** Renamed binders, restored the task completion helper and `Agent.create` arguments, added explicit record annotations and an erasable unit argument. Reran the TUI smoke through the pinned loopback Ollama endpoint. The full forced suite, install build and `opam lint` passed.
- **Prevention / Reference:** Annotate shared record labels at artifact/job boundaries. Offline provider smokes must use a supported pinned local route or an explicitly configured custom provider, not override a pinned remote API.

### [2026-09-27] TUI progress callbacks hid elapsed tool status

- **Context / Symptom:** A 30×10 `NO_COLOR` PTY showed `Tool: run_command · 0 B` clipped before the elapsed time after a command progress event.
- **Root Cause:** `Tui.tool_updated` replaced the active tool label with cumulative received-byte counts; the extra text exceeded the compact activity row.
- **Solution:** Kept progress counts out of the visible activity row so its typed phase and elapsed timer survive tool updates. A delayed loopback Chat PTY verified `Thinking · 1s`, `Tool: run_command · 2s`, resize, successful completion and an idle screen with no output polling.
- **Prevention / Reference:** Treat byte counts as event metadata, not activity text; verify active progress in both narrow and wide `NO_COLOR` terminals.

### [2026-09-27] Darwin workspace aliases failed canonical cwd checks

- **Context / Symptom:** The process-tool fixture rejected workspace cwd `.` with `Workspace_path.Error("path escapes workspace: .")`; the managed-worktree fixture compared `/var/folders/...` with Git's `/private/var/folders/...` path. R9 LSP fixture responses also referenced an unopened document when its URI used the noncanonical alias, and the DAP child-cwd comparison expected that alias.
- **Root Cause:** macOS resolves `/var` through `/private/var`, while process checks, test URIs and a DAP cwd assertion retained the noncanonical temporary-root spelling.
- **Solution:** Canonicalized workspace roots and test fixture roots with `Unix.realpath` before constructing process, Git, LSP and DAP path expectations. The affected focused tests and the forced suite passed.
- **Prevention / Reference:** Compare and validate workspace paths only after canonicalizing the root on Darwin.

### [2026-09-27] Workspace process wrappers needed explicit OCaml argument contracts

- **Context / Symptom:** Initial R8 integrations failed to compile when process wrappers passed a string cwd or optional timeout value to the inferred API; labeled-only optional functions also triggered argument-erasure warnings, and shared record labels needed type annotations.
- **Root Cause:** `Workspace_process` represents cwd as `string option`, accepts timeouts as scalar integers, and OCaml optional arguments need a trailing unit when only labeled arguments follow; several new records reused field labels.
- **Solution:** Passed `~cwd:(Some path)` and scalar `~timeout_seconds`, added trailing `()` to affected functions/callsites, and annotated ambiguous records. The full forced suite and install build then passed.
- **Prevention / Reference:** Treat inferred optional-argument types and erasability as part of the module API; annotate shared record labels and verify integration callsites.

### [2026-09-27] Linux PTY master close terminated its session leader

- **Context / Symptom:** R8 release workflow `36321096728` failed `test_workspace_process` on Linux x86_64 and aarch64; the `/bin/cat` PTY test did not report `Exited 0` after its input and output completed.
- **Root Cause:** Closing the PTY master sends `SIGHUP` to the controlling terminal's foreground process group on Linux, including the Python launcher/session leader. The launcher died before returning the child status; Darwin did not exhibit this finalization behavior.
- **Solution:** Ignored `SIGHUP` immediately before the launcher's final master close, then restored its default disposition before re-raising a child `SIGHUP`. Isolated Linux PTY exit and macOS process tests passed; after fix commit `23dfc55`, hosted release retry `36323658937` passed all four native test/build/package/extracted-smoke jobs and publication. The full local forced suite and install build also passed.
- **Prevention / Reference:** Treat PTY master close as a hangup event for the session's foreground process group; verify process exit and signal propagation on both Linux and Darwin.

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

### [2026-09-30] Models looped on `read_file` rejecting defaulted `offset` and `line`

- **Context / Symptom:** A Pave session repeated `read_file docs/WORK_CHECKPOINT.md` many times, each failing with `Error: use either line or offset, not both`, then told the user it could not read the file.
- **Root Cause:** `read_text_page` rejected any call where `line` was set and `offset` was present at all, even `offset: 0`. Models (especially local OpenAI-compatible ones) commonly fill every optional schema field with its default, so a plain read arrived as `offset: 0, line: 1` and could never succeed.
- **Solution:** Treat `offset: 0` and `line: 1` as the defaults they are; reject only a nonzero offset combined with a line above 1, with a message telling the model to retry with one of them. Schema descriptions now state the defaults. Regression cases were added to `test/tools/test_tools.ml`.
- **Prevention / Reference:** Mutually exclusive optional tool arguments must tolerate echoed default values. `dune test test/tools` does not run `test_tools` (it is declared in `test/dune`); run `dune test` to exercise it.

### [2026-10-01] Headless browser calls died after five seconds of silence

- **Context / Symptom:** `browser` `evaluate`/`call_tool` on a page promise that settled after more than five seconds failed with `browser connection timed out waiting for data`, although the operation allowed up to 120 s. Destroyed iframes also kept stale execution-context ids.
- **Root Cause:** `Workspace_browser.read_exact` applied its five-second idle limit to the first two bytes of every WebSocket frame, but CDP sends nothing while an awaited promise is pending. The reverse context index stored `session\000session\000frame` while the forward table used `session\000frame`, so `executionContextDestroyed` never removed anything. A browser that failed to exec also left its `pave-browser-*` profile directory behind.
- **Solution:** The idle limit applies only inside a frame; waiting for the next message uses the 120 s operation bound plus slack, with cancellation still polled every 250 ms. The reverse index stores the forward key, and the profile is removed when spawn fails. Regression tests `test_long_silent_response` and `test_context_destroyed` failed before the fix.
- **Prevention / Reference:** Keep liveness timeouts separate from per-operation deadlines when a protocol answers asynchronously. `dune exec test/test_workspace_browser.exe` covers both.

### [2026-10-02] Retry wrapper swallowed streaming transport errors

- **Context / Symptom:** After the transient-retry change, a streamed request whose curl exited with an unlisted status (for example 23) returned as if finished, and an exhausted retry on exit 7 or a first-byte timeout reported `missing HTTP response status` instead of the transport cause.
- **Root Cause:** The `post_stream` exception handlers returned a boolean "retry?" and the fall-through path then read the dumped headers; a handler that answered `false` or ran out of attempts never re-raised the original `Provider_error`. Separately, the sanitizer cleared its duplicate-id rewrite queue per call, so two same-id calls in one turn lost the first call's pending entry and the results swapped.
- **Solution:** Handlers raise the original error unless a retry is both applicable and still allowed; the rewrite queues are cleared once per assistant turn. `test_provider_retry` and `test_sanitize` case 10 failed before the fix. `publish_web` also needed `Workspace_process.release_finished` because finished job records keep their id.
- **Prevention / Reference:** When an exception handler decides "retry or not", make the not-retry arm re-raise explicitly rather than falling through to shared post-processing. `test_devin_binary_http` is timing-sensitive and failed intermittently under load (4 s total-deadline case); rerun it alone before treating a failure as a regression.

