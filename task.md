# Tasks — ordered remaining work

**Audit:** 2026-10-03, continuing the existing mobile backlog. This is the single active backlog. Only unfinished, independently verifiable work belongs here. Completed M01–M24 and EX01–EX10 were removed; acceptance history remains in [the mobile plan](docs/MOBILE_DEVELOPMENT_PLAN.md), [changelog](CHANGELOG.md) and Git. Existing file tools, LSP/DAP, approved processes/eval/network tools, read-only jobs, local extensions/MCP, compact read cards and Devin status diagnostics are not new implementation tasks.

## How to execute this list

- Work top-to-bottom within P1 → P2 → P3 → P4. A named dependency must finish first; a missing external gate blocks only that card. Skip gated cards rather than inventing credentials, device readiness, registry contracts or hosted services. Complete the remaining mobile core capabilities in their listed order before returning to unrelated backlog cards.
- Each checkbox is one deliverable, not an entire subsystem. Letter suffixes split an old card; the unsuffixed ID is a scope family, not another checkbox. `Depends: none` means no unfinished prerequisite, not permission to ignore existing contracts.
- **Evidence** describes inspected source or an existing plan, not an executed failure. Cards labeled **diagnostic** must first reproduce the suspected behavior; close with evidence if not reproduced, rather than inventing a fix. Other new gaps are source-observed and still require failing-before/passing-after proof when implemented.
- **Accept** is future acceptance, not a claim that this audit ran it. Runtime changes require focused behavioral checks plus actual CLI/tool/PTY smoke. Real Xcode simulator acceptance stays manual-only, never CI. No model/tool request is automatically replayed, and every shell/device command retains separate informed approval.
- Keep [design rules](docs/DESIGN_RULES.md) binding. Remove a finished checkbox after recording verification in the changelog/checkpoint; never retain `[x]` history here. Planning does not authorize new products, trust boundaries, releases or live account spending.

## P1 — Existing-path safety and reliability

- [ ] **AU01 — Cancel in-turn credential refresh**
  **Evidence:** [refresh transport](lib/auth/oauth_flow.ml#L313-L360) blocks on read/wait; [credential resolution](bin/cli_auth.ml#L275-L296) has no turn-cancellation parameter. **Deliver:** propagate cancellation through refresh and reap its owned subprocess; define the outcome of ambiguous remote token rotation without automatic retry.
  **Accept:** one successful refresh retains account/grant binding; cancelling a stalled refresh promptly releases ownership and sends zero inference requests, without logging tokens.
  **Depends:** none.
  **Gate:** none; controlled token endpoint.

- [ ] **AU02 — Bound credential-lock acquisition**
  **Evidence:** [store locking](lib/auth/oauth_store.ml#L404-L435) uses blocking mutex/file locks. **Deliver:** interruptible, deadline-bounded acquisition while retaining nested locking and atomic refresh guarantees.
  **Accept:** two processes serialize updates without losing accounts; a cancelled/expired waiter exits without refreshing, unlocking another holder or replacing credentials.
  **Depends:** AU01.
  **Gate:** none.

- [ ] **WK07 — Report recoverable partial LSP application**
  **Evidence:** [apply_edit_preview](lib/tools/workspace_lsp.ml#L807-L850) prechecks then writes sequentially, with per-file callbacks but no structured partial result. **Deliver:** reproduce a later-target failure and expose exact applied/unchanged targets plus existing guarded recovery records; do not claim whole-batch atomicity.
  **Accept:** two successful files record once; a forced second-write failure reports the first change, preserves later concurrent user edits and permits only hash-checked, approved recovery.
  **Depends:** none.
  **Gate:** none; controlled filesystem fault, not user files.

- [ ] **PL04a — Bound queued prompt admission**
  **Evidence:** [follow-up/steering queues](lib/ui/turn_runner.ml#L247-L299) admit submissions without aggregate item/byte limits. **Deliver:** bounded admission counting retained prompts and attachments, including dequeue/reinsert handling.
  **Accept:** a held-turn PTY preserves queue order; overflow retains the draft/attachments, drops no accepted work and does not cancel the active turn for rejected steering.
  **Depends:** none.
  **Gate:** none; local streaming fixture.

- [ ] **PL04b — Bound event backlog and terminal starvation**
  **Evidence:** [runner notices](lib/ui/turn_runner.ml#L119-L149) and [UI queue](bin/ui/tui.ml#L153-L215) have unbounded aggregate admission; delta batching already exists. **Deliver:** first measure sustained-stream latency/retention, then enforce a documented queue capacity and per-pump fairness bound.
  **Accept:** a sustained-stream PTY keeps ordered output and one settlement within the recorded input/resize latency bound; saturated cancellation/shutdown cannot deadlock producers or lose tool outcomes.
  **Depends:** none.
  **Gate:** none; starvation magnitude is diagnostic, not yet reproduced.

- [ ] **PL06 — Bound installer asset transfers**
  **Evidence:** [fetch](install.sh#L83-L89) has no connect/total deadline or byte cap. **Deliver:** distinct manifest/archive transfer budgets used by standalone install and the embedded updater, retaining HTTPS/checksum/executable-last publication.
  **Accept:** valid controlled assets install; stalled/oversized transfers fail within bounds, preserve the previous executable hash and remove staging files. Download caps do not imply an extracted-size cap.
  **Depends:** none.
  **Gate:** none; disposable install directories.

- [ ] **PL03a — Enforce complete packaged dependency policy**
  **Evidence:** [release dependency check](.github/workflows/release.yml#L101-L110) rejects libzstd specifically, not every forbidden library. **Deliver:** target-specific allowed-system-library checks for executable/helper artifacts on the four current targets.
  **Accept:** extracted artifacts launch without toolchain runtime paths; a deliberately linked non-system library other than zstd and a missing dependency both fail packaging with clear diagnostics.
  **Depends:** none.
  **Gate:** native current-target CI runners; no current broken artifact is asserted.

- [ ] **PL07 — Automate installed updater transactions in CI**
  **Evidence:** [release smoke](.github/workflows/release.yml#L89-L135) extracts/runs binaries, but does not exercise an installed updater transaction. **Deliver:** a disposable controlled-release harness for actual install/check/update/uninstall, invoked by CI.
  **Accept:** custom-directory upgrade preserves unrelated files and user state; corrupt checksum, link/unexpected archive member, invalid marker and failed publication preserve the executable. Verify metadata state on partial publication; inherited destination/version overrides cannot redirect update.
  **Depends:** PL06.
  **Gate:** packaged current-target binaries; no public release required.


## P2 — Mobile completion and focused existing-product improvements

M01–M24 are satisfied prerequisites. The remaining mobile core capabilities are delivered end-to-end here; their active acceptance contracts live in the [mobile plan](docs/MOBILE_DEVELOPMENT_PLAN.md).
AS01 and AO01 have been completed with session-bound approved app lifecycles and bounded screen observation; only the remaining six core capabilities are open.


- [ ] **UI01 — Control and verify mobile UI**
  **Deliver:** approved tap, swipe, text input and back actions bound to the selected device, with a post-action observation.
  **Accept:** the real emulator fixture changes from a known pre-state to the expected accessible state; denial, out-of-bounds coordinates and failed commands do not claim a transition.
  **Depends:** AO01.
  **Gate:** interactive disposable app on an approved emulator/simulator.

- [ ] **BR01 — Persist and replay mobile bug scenarios**
  **Deliver:** private, versioned scenario records of exact app/device identity, steps and assertions, with explicit replay.
  **Accept:** a saved deterministic scenario replays the same steps and reports the first failed assertion; mismatched app/device identity, missing records and cancelled actions stop replay without implicit retries.
  **Depends:** UI01.
  **Gate:** disposable deterministic mobile fixture.

- [ ] **LD01 — Diagnose mobile runtime logs and crashes**
  **Deliver:** bounded Android logcat/ANR and iOS crash/dSYM-or-mapping evidence tied to the selected app/session.
  **Accept:** fixture crashes and ANR evidence identify the owning app and useful source/symbol location when verified; truncated, foreign, malformed or unsymbolicated evidence remains explicitly incomplete.
  **Depends:** AS01.
  **Gate:** disposable platform runtime and available local symbols.

- [ ] **FV01 — Verify edits against a real mobile build**
  **Deliver:** connect the guarded source edit snapshot to focused build/test results and source-linked diagnostics for the same app session.
  **Accept:** one deliberate fixture defect fails before repair and passes afterward; stale snapshots, build failures and incomplete output cannot be reported as verified.
  **Depends:** AS01.
  **Gate:** installed platform toolchain and disposable project.

- [ ] **VR01 — Compare mobile screenshot regressions**
  **Deliver:** save and compare bounded screenshots with exact device, OS, locale, theme and declared dynamic-region metadata.
  **Accept:** unchanged captures compare equal and a controlled pixel change is reported outside masked dynamic regions; mismatched metadata or incomplete captures are not compared as valid baselines.
  **Depends:** AO01.
  **Gate:** deterministic simulator/emulator screen.

- [ ] **MD01 — Complete the mobile TUI dashboard**
  **Deliver:** expose selected sessions, live observation, approved controls, scenario replay, diagnostics and verification/visual results in the TUI.
  **Accept:** a real PTY creates/selects a session, observes it, performs a separately approved action and shows its settled state; cancellation preserves session/draft state and never grants effects.
  **Depends:** AO01, UI01, BR01, LD01, FV01, VR01.
  **Gate:** interactive PTY and selected disposable emulator/simulator.

- [ ] **PG02 — Diagnose reserved-slot permission failures**
  **Evidence:** [Singularity reserved route](lib/provider/transports/singularity_tech_api.ml) lacks slot-specific classification; [HTTP errors](lib/provider/provider.ml) already have generic permission handling. **Deliver:** distinguish documented inactive-reservation denial from a bad key on this exact route only.
  **Accept:** a controlled inactive-slot 403 retains route/account and sends one completion only; ordinary 401/403 never acquire an invented reservation diagnosis.
  **Depends:** none.
  **Gate:** identifiable reservation error evidence; do not classify every 403 as an inactive slot.

- [ ] **PG03 — Accept the Hugging Face fallback key**
  **Evidence:** [CLI key resolution](bin/cli_auth.ml#L143-L174) lacks `HUGGINGFACE_HUB_TOKEN`. **Deliver:** reuse primary-first resolution for inference and pinned listing.
  **Accept:** `HF_TOKEN` wins; fallback alone works, missing keys fail, and neither credential reaches an unrelated origin.
  **Depends:** none.
  **Gate:** none; fixtures only.

- [ ] **PG04 — Accept the Beijing Token Plan fallback key**
  **Evidence:** [CLI key resolution](bin/cli_auth.ml#L143-L174) lacks `BAILIAN_TOKEN_PLAN_API_KEY`. **Deliver:** use it only when `ALIBABA_TOKEN_PLAN_API_KEY` is absent on the existing Beijing route.
  **Accept:** the primary wins; fallback cannot redirect to Coding Plan/workspace hosts and no-key failure remains explicit.
  **Depends:** none.
  **Gate:** none; fixtures only.

- [ ] **CT03 — Protect recent results in existing request pruning**
  **Evidence:** [trim_tool_results](lib/agent/context_budget.ml#L111-L132) already makes request-only copies but trims every oversized text result. **Deliver:** prioritize old eligible results and protect latest-turn call/result payloads; preserve signed replay and images.
  **Accept:** older text measurably shrinks requests while latest results and resumed journal bytes remain intact; protected context that cannot fit is refused rather than silently truncated.
  **Depends:** none.
  **Gate:** none; existing pruning is not missing and does not depend on a new recovery UI.

- [ ] **WK08 — Forward typed MCP image results**
  **Evidence:** [MCP validation](lib/extensions/mcp_client.ml#L480-L497) accepts images, but [CLI integration](bin/main.ml#L1220-L1234) rejects all nontext blocks. **Deliver:** preserve bounded validated Text/Image blocks through existing canonical tool-result handling.
  **Accept:** approved text/image order survives a supported route and history; bad base64/MIME/magic or unsupported route is rejected before provider network, without payloads in UI/logs.
  **Depends:** none.
  **Gate:** none; controlled MCP/provider peers.

- [ ] **SJ05 — Persist child usage under its original identity**
  **Evidence:** [child workflow](bin/main.ml#L1727-L1759) puts usage in a success string, not durable typed markers. **Deliver:** owner-thread, idempotent delivery of each validated child usage record tied to original job/provider/account/route/model.
  **Accept:** usage survives child failure/cancellation and resume exactly once, even after parent model/session switch; no inferred counts or attribution to the new active identity.
  **Depends:** none.
  **Gate:** none.

- [ ] **CT02 — Recover explicitly after provider context rejection**
  **Evidence:** [pre-request compaction](bin/main.ml#L1598-L1722) already exists; post-rejection recovery is not a distinct action. **Deliver:** an explicit user recovery path reusing compaction with unchanged ancestry and signed-state validation, not automatic replay.
  **Accept:** a controlled context-size rejection can be compacted then resumed only by explicit action; interruption, unresolved calls or signed mismatch preserve original history and duplicate no tool/request.
  **Depends:** CT03.
  **Gate:** none; local near-limit fixture, not live account inference.

- [ ] **RC01a — Correlate public tool events with opaque IDs**
  **Evidence:** [JSONL events](bin/main.ml#L1053-L1078) publish name/state but omit per-call correlation; [CLI tests](test/test_cli_prompt.ml#L330-L369) correctly prohibit raw provider IDs. **Deliver:** a bounded public local tool-instance ID across start/update/settle/abort, preserving current one-shot JSONL behavior.
  **Accept:** two same-name shared reads each correlate with one terminal event; raw IDs, arguments/results, credentials, attachments and opaque replay remain absent.
  **Depends:** none.
  **Gate:** none.

- [ ] **RC01b — Define multi-turn local event ownership**
  **Evidence:** [current emitter](bin/main.ml#L23-L59) is one-shot and uses process-local `turn-1`. **Deliver:** versioned local session/turn/approval-request ownership and redaction, reusing the typed runner rather than a second lifecycle.
  **Accept:** two turns and a cancelled approval have distinct ordered owners; late events cannot attach to the next turn, and an event alone never grants an effect.
  **Depends:** RC01a.
  **Gate:** none; non-TTY prompt-required effects still deny.

- [ ] **AG01a — Validate typed child outcomes**
  **Evidence:** [jobs](lib/session/session_jobs.ml) already bound concurrency and deliver owner artifacts, but results are strings. **Deliver:** bounded schema-validated child success/failure values without replacing the existing job system.
  **Accept:** valid structured output survives owner delivery once; schema mismatch, oversize and partial failure remain explicit failures rather than successful text artifacts.
  **Depends:** none.
  **Gate:** none; read-only children and delegation approval remain mandatory.

- [ ] **AG01b — Propagate parent cancellation to owned children**
  **Evidence:** [job cancellation](lib/session/session_jobs.ml) is per-job; the original AG01 requires parent ownership through partial failure. **Deliver:** explicit parent/child lifetime binding for typed batches.
  **Accept:** parent cancellation settles each owned child once; an unrelated session's jobs survive, with no orphaned process or stale success delivery.
  **Depends:** AG01a.
  **Gate:** none.

- [ ] **WK06 — Read owner-scoped typed child-result URIs**
  **Evidence:** [reader dispatch](lib/tools/workspace_reader.ml#L832-L845) has artifact/HTTPS support, not `agent://`. **Deliver:** bounded nested field access to owned typed child results, reusing private artifact ownership.
  **Accept:** valid nested values retain types; foreign-session, missing, oversized and invalid selectors fail closed without leaking artifact content.
  **Depends:** AG01a.
  **Gate:** none.

- [ ] **WK01a — Add single-file hashline anchors**
  **Evidence:** [snapshot editor](lib/tools/workspace_edit.ml#L118-L170) uses exact text and whole-file SHA256, not line anchors. **Deliver:** bounded content-hash anchors on the existing guarded writer.
  **Accept:** a unique current anchor changes only its intended span; stale/ambiguous anchors preserve the file and do not fall back to approximate matching.
  **Depends:** none.
  **Gate:** none.

- [ ] **WK01b — Define and enforce multi-file anchor commit safety**
  **Evidence:** [LSP application](lib/tools/workspace_lsp.ml#L807-L850) is sequential, so generic multi-file atomicity is not an existing guarantee. **Deliver:** a guarded multi-file edit transaction with explicit crash/failure semantics; no second unguarded writer.
  **Accept:** a conflict in any target prevents the planned batch; injected publication failure has documented recoverable state without overwriting concurrent user edits. Never advertise crash-atomic filesystem rename across files unless actually established.
  **Depends:** WK01a, WK07.
  **Gate:** reviewed transaction/rollback contract before advertising all-or-none edits.

- [ ] **WK03a — Preview bounded merge-conflict regions**
  **Evidence:** [editor](lib/tools/workspace_edit.ml) guards snapshots but does not select merge markers. **Deliver:** exact-file/hash previews of well-formed conflict blocks, distinguishing missing base and malformed nesting.
  **Accept:** a supported block shows only its real ours/theirs/base ranges; malformed/ambiguous markers remain unresolved without modifying bytes.
  **Depends:** none.
  **Gate:** none.

- [ ] **WK03b — Apply one approved conflict resolution**
  **Evidence:** [existing snapshot writes](lib/tools/workspace_edit.ml) can host a selected resolution. **Deliver:** a separately approved choice bound to one preview/hash.
  **Accept:** the selected block changes while surrounding bytes survive; denial, stale content, invalid choice or missing requested base writes nothing.
  **Depends:** WK03a.
  **Gate:** none.

- [ ] **WK04a — Plan exact commit hunks without staging**
  **Evidence:** [worktree commits](lib/tools/workspace_git.ml#L295-L335) operate on whole approved paths. **Deliver:** inert snapshot-bound hunk plans with explicit dependencies and cycle rejection.
  **Accept:** a valid plan orders only selected hunks; cyclic, stale or overlapping plans leave index/worktree untouched.
  **Depends:** none.
  **Gate:** none.

- [ ] **WK04b — Execute an approved hunk commit plan**
  **Evidence:** [existing owned commits](lib/tools/workspace_git.ml) do not execute dependency-ordered hunk plans. **Deliver:** distinct staging/commit authorization for the exact plan while preserving the preexisting index.
  **Accept:** the commit contains only selected hunks; cancellation/conflict leaves unrelated staged user hunks and lockfiles untouched and creates no commit.
  **Depends:** WK04a.
  **Gate:** none; explicit per-effect approval.

- [ ] **WK05 — Offer typed clarification choices in the TUI**
  **Evidence:** [tool schemas](lib/tools/tools.ml) and [interaction](lib/ui/interaction.ml) lack structured model-requested choice input. **Deliver:** a bounded multiple-choice clarification overlay, distinct from effect authorization.
  **Accept:** selection returns the typed value; paste cannot select/approve, cancellation restores the draft, and headless use returns unavailable without granting effects.
  **Depends:** none.
  **Gate:** none; actual PTY acceptance required.

- [ ] **SH01a — Project a redacted selected-branch export**
  **Evidence:** [session history](lib/session/session.ml) and [artifacts](lib/session/session_artifact.ml) are not a consented share projection. **Deliver:** a bounded inspectable export representation with branch/provenance and explicit exclusions.
  **Accept:** only selected-branch visible content appears; hidden credentials, media bytes and opaque provider state are excluded, without mutating journal data.
  **Depends:** none.
  **Gate:** explicit export consent; unknown secrets cannot be claimed automatically detected.

- [ ] **SH01b — Write a consented local transcript export**
  **Evidence:** [session ownership](lib/session/session.ml) provides source data but no export command. **Deliver:** an approved no-overwrite owned-file writer for the inspected projection, with no network transmission.
  **Accept:** the exact reviewed export is written; cancellation, existing target or unsafe path preserves journal/target and transmits nothing.
  **Depends:** SH01a.
  **Gate:** none.

- [ ] **LD01 — Record exact completion timing**
  **Evidence:** [usage markers](lib/session/session.ml#L42-L45) contain usage/provenance, not start/first-text/finish/error timing. **Deliver:** optional measured timing markers for the exact request identity.
  **Accept:** streamed TTFT and terminal timestamps survive resume; buffered first-text and unreported token counts remain unknown, with no timing inferred from transcript length.
  **Depends:** none.
  **Gate:** none.

- [ ] **LD02 — Aggregate cross-session local statistics**
  **Evidence:** [usage_by_route](lib/session/session.ml#L703-L731) already totals selected-branch usage. **Deliver:** private project/model/day CLI/JSON aggregation with explicit fork/off-branch semantics and optional timing, not a second per-session usage command.
  **Accept:** reference sessions aggregate without duplicate billing; absent usage/prices/premium counts stay unknown and another project's private data is not exposed by default.
  **Depends:** SJ05, LD01.
  **Gate:** none.

- [ ] **PL04c — Measure long-session manager retention**
  **Evidence:** [session managers](bin/main.ml#L947-L1008) live until shutdown; transcript bounds already exist. **Deliver:** measure repeated session switching, owned subprocess cleanup, idle CPU and retained memory in a real PTY; repair only demonstrated unbounded retention.
  **Accept:** record reproducible numeric limits and enforce them under sustained switching/resize/cancel; dirty jobs/user sessions are not destroyed merely to reduce memory.
  **Depends:** PL04a, PL04b.
  **Gate:** none; **diagnostic**, not a proven memory leak.

- [ ] **PL03b — Verify packaged capability advertising**
  **Evidence:** [release helper smoke](.github/workflows/release.yml#L114-L134) already covers conditional Apple support, but not a complete target capability matrix. **Deliver:** exact packaged availability checks for existing terminal imaging/LSP/DAP/native helper capabilities per current target.
  **Accept:** available helpers execute from an extracted package; unavailable capabilities are not advertised as ready, without removing ordinary tools solely because optional servers are not configured.
  **Depends:** PL03a.
  **Gate:** native current-target runners; do not reopen completed Apple packaging.

## P3 — Incremental optional local capabilities

These are retained product plans, not source-proven defects. No new effect surface is enabled before its own approval/ownership acceptance passes.

- [ ] **WK02a — Read bounded explicit public GitHub references**
  **Evidence:** [reader URI dispatch](lib/tools/workspace_reader.ml#L832-L845) rejects issue/PR schemes. **Deliver:** explicit repository-scoped issue/PR/diff resolution with pinned provenance and bounded pagination, reusing network approval.
  **Accept:** the requested public issue/PR is read exactly; malformed/foreign refs, incomplete pages and private unauthorized refs stay unavailable without credential forwarding.
  **Depends:** none.
  **Gate:** controlled GitHub fixture for implementation; network approval for real reads.

- [ ] **WK02b — Bind private GitHub reads to an authorized identity**
  **Evidence:** [OAuth routing](bin/cli_auth.ml) grants provider access, not arbitrary private GitHub access. **Deliver:** explicit identity/repository-scoped private reads, never borrowing Copilot credentials implicitly.
  **Accept:** the authorized identity reads only its requested allowed repository; revoked, cross-origin and cross-identity requests fail before forwarding credentials.
  **Depends:** WK02a.
  **Gate:** authorized GitHub registration/identity and repository for real acceptance.

- [ ] **AG02a — Define editing-child trust and merge policy**
  **Evidence:** [design rules](docs/DESIGN_RULES.md) deliberately restrict children to read-only tools. **Deliver:** an explicit reviewed rule change covering owned worktrees, per-worker writes/exec, secrets, cancellation and merge approval.
  **Accept:** enumerate permitted/denied effects and dirty-worktree handling; no child write capability is advertised before approval of this contract.
  **Depends:** AG01b.
  **Gate:** explicit product/trust decision; planning alone does not amend read-only policy.

- [ ] **AG02b — Execute one isolated editing child**
  **Evidence:** [jobs](lib/session/session_jobs.ml) and [worktrees](lib/tools/workspace_git.ml) already own resources separately. **Deliver:** one child bound to its approved worktree/policy, with no inherited secret or shell grant.
  **Accept:** only the owned tree changes; denied paths and commands cannot run, and cancellation preserves dirty user trees.
  **Depends:** AG02a.
  **Gate:** approved trust contract and individual shell-command approval.

- [ ] **AG02c — Integrate separately reviewed child changes**
  **Evidence:** the retained AG02 scope requires two independent editing workers, not just worktree creation. **Deliver:** approved integration of exact child snapshots with conflict reporting.
  **Accept:** two disjoint children merge only after review; overlapping/stale changes surface conflict and never overwrite parent or dirty user worktrees.
  **Depends:** AG02b.
  **Gate:** approved merge policy; no automatic commit.

- [ ] **AG03 — Inspect and steer a live owned child**
  **Evidence:** [job commands](bin/main.ml#L2634-L2673) already list/wait/cancel; bounded live transcript/steering is residual. **Deliver:** owner-scoped TUI inspection with exact reported usage and steering of the selected active child.
  **Accept:** parent inspects/cancels the intended child; stale or foreign job IDs cannot steer a new session or expose hidden effects.
  **Depends:** AG01b, SJ05.
  **Gate:** none; children remain read-only unless AG02 is independently authorized.

- [ ] **AG04 — Run an explicitly enabled continuous read-only advisor**
  **Evidence:** [workflow dispatch](bin/main.ml) has one-shot advisor jobs. **Deliver:** a separate bounded advisor context with visible request/cost control, main-turn correlation and immediate disable.
  **Accept:** a concern annotates only its originating turn; disable/cancel stops later requests and never invokes mutating tools or invents cost estimates.
  **Depends:** AG01b, SJ05.
  **Gate:** explicit user enablement and usage consent.

- [ ] **AG05 — Apply no-replay stream interruption rules**
  **Evidence:** [session guidance](lib/session/session.ml) stores branch-local rules, not automatic stream matchers. **Deliver:** a reviewed no-side-effect interruption boundary and visible reason for an enabled matcher.
  **Accept:** a controlled match cancels once with intact journal; after any tool effect, continuation requires a new explicit user turn and never replays the effect.
  **Depends:** none.
  **Gate:** explicit product decision to enable automatic matching; guidance alone is not authorization.

- [ ] **RC02a — Parse bounded correlated stdio RPC frames**
  **Evidence:** [one-shot CLI JSONL](bin/main.ml#L23-L59) is output, not a command transport. **Deliver:** versioned bounded input framing/correlation and read-only session/model inspection.
  **Accept:** valid IDs receive matching responses; malformed/oversized frames recover locally without merging accounts or invoking a tool.
  **Depends:** RC01b.
  **Gate:** none.

- [ ] **RC02b — Run owned prompt/steer/abort over RPC**
  **Evidence:** [turn runner](lib/ui/turn_runner.ml) already supplies lifecycle semantics. **Deliver:** map correlated RPC actions to one owned runner with bounded output/backpressure and EOF cancellation.
  **Accept:** two requests cannot steal each other's events; clean EOF and abort settle once, and headless prompt-required effects remain denied.
  **Depends:** RC02a, PL04a, PL04b.
  **Gate:** none.

- [ ] **RC03a — Specify remote-host approval trust**
  **Evidence:** [design rules](docs/DESIGN_RULES.md) currently require interactive per-effect approval. **Deliver:** a reviewed authenticated approver/session binding, expiry, disconnect-denial and exact-command preview contract before changing that rule.
  **Accept:** threat scenarios reject absent/stale/foreign approvers; no connection-wide shell grant or implementation can bypass the current non-TTY denial.
  **Depends:** RC02b.
  **Gate:** explicit design-contract approval.

- [ ] **RC03b — Implement one authenticated approval exchange**
  **Evidence:** [approval ownership](lib/ui/turn_runner.ml) supplies local requests, not a remote authenticated exchange. **Deliver:** exact-effect request/response bound to the approved remote-host contract.
  **Accept:** one live approved request authorizes only its command; replay, timeout or disconnect denies writes/exec/device effects and cannot approve another owner.
  **Depends:** RC03a.
  **Gate:** authorized host/approver fixture and approved contract.

- [ ] **RC04a — Select a stable ACP client contract**
  **Evidence:** [CLI](bin/main.ml) has no ACP editor adapter. **Deliver:** pin a published protocol version and actual client, mapping editor buffers, cancellation and approval to existing RPC semantics.
  **Accept:** a documented compatibility matrix identifies supported/unsupported messages and a reproducible client handshake; no ACP claim from a protocol-name alias.
  **Depends:** RC02b, RC03b.
  **Gate:** a published stable ACP contract and available real editor client.

- [ ] **RC04b — Complete one approved ACP editor edit**
  **Evidence:** RC04a defines the new client boundary. **Deliver:** the mapped buffer/edit/cancel lifecycle over existing RPC, not an independent agent loop.
  **Accept:** a real client observes exactly one approved edit; disconnect denies pending effects and unsaved buffer conflicts preserve user content.
  **Depends:** RC04a.
  **Gate:** selected editor/client runtime.

- [ ] **BR02 — Attach only explicitly selected relay tabs**
  **Evidence:** no existing tool in [tool dispatch](lib/tools/tools.ml) authorizes access to personal browser tabs. **Deliver:** opt-in loopback-authenticated user-installed relay with exact selected-tab identity and detach.
  **Accept:** only a selected tab is inspectable; unrelated logged-in tabs, revoked pairing and disconnected relay cannot be adopted.
  **Depends:** BR01b.
  **Gate:** actual reviewed relay implementation and explicit installation/pairing.

- [ ] **BR03a — Establish native window-control consent**
  **Evidence:** [native services](lib/tools/native_services.ml) are not desktop authority. **Deliver:** trusted local helper identity, disposable-window selection, permission/revocation lifecycle and a reviewed host-effect contract.
  **Accept:** missing OS permission or revoked selection yields no capture/input capability; workspace/browser consent alone never grants desktop access.
  **Depends:** none.
  **Gate:** supported native helper/platform and explicit design approval.

- [ ] **BR03b — Capture an approved window and accessibility tree**
  **Evidence:** BR03a defines a selected-window boundary. **Deliver:** separately approved bounded screenshot and accessibility reads tied to that window.
  **Accept:** only the selected disposable window is observed; headless, revoked, stale-window and other-app requests disclose nothing.
  **Depends:** BR03a.
  **Gate:** native capture/accessibility permissions.

- [ ] **BR03c — Control approved native input and clipboard effects**
  **Evidence:** [native clipboard support](lib/tools/native_services.ml) must not imply unrestricted input authority. **Deliver:** distinct exact-effect confirmations for selected-window input and clipboard operations.
  **Accept:** the reviewed disposable action occurs once; cancellation/revocation/window changes prevent input and unrelated clipboard contents are not read implicitly.
  **Depends:** BR03a.
  **Gate:** native input/clipboard permissions and explicit per-effect consent.

- [ ] **LD03a — Store explicitly consented local facts**
  **Evidence:** [private session storage](lib/session/session.ml) is history, not an opt-in recall bank. **Deliver:** project/global fact scopes with inspect/edit/delete and explicit retention policy, treating facts as untrusted context.
  **Accept:** an approved fact can be edited/deleted; deletion removes it from retrieval and another project cannot read it by default.
  **Depends:** none.
  **Gate:** explicit recall opt-in; no automatic extraction from private conversations.

- [ ] **LD03b — Search and export consented facts**
  **Evidence:** LD03a defines owned facts, not retrieval or portability. **Deliver:** bounded lexical search, retention enforcement and approved local export using the same scopes.
  **Accept:** expired/deleted facts are absent; search/export cannot cross project scope or silently transmit data, and recall text never becomes trusted instructions.
  **Depends:** LD03a.
  **Gate:** none.

- [ ] **LD06a — Run isolated versioned evaluation fixtures**
  **Evidence:** [existing tests](test/dune) are not an opt-in end-user evaluation runner. **Deliver:** one versioned edit/tool fixture format and disposable-workspace runner recording environment and explicit expected behavior.
  **Accept:** repeated valid and deliberately invalid edits produce the expected distinct outcomes; a failed tool stays failed and user workspaces/credentials remain untouched.
  **Depends:** none.
  **Gate:** explicit execution consent; no hosted VM gateway assumed.

- [ ] **LD06b — Compare recorded evaluation runs**
  **Evidence:** LD06a produces attributable fixture results. **Deliver:** bounded local comparison by input/version/environment with raw failure provenance, not unsupported quality scores.
  **Accept:** repeated identical fixtures remain comparable; different environments/inputs are labeled and missing/error results cannot become passes.
  **Depends:** LD06a.
  **Gate:** none.

- [ ] **LD05a — Capture one consented voice prompt**
  **Evidence:** [non-chat APIs](lib/provider/non_chat.ml) and [task CLI](bin/task_cli.ml) provide transcription, not an interactive microphone lifecycle. **Deliver:** explicit mic permission, bounded recording, approved transcription and draft-only insertion.
  **Accept:** one recording yields an unsent draft; denial/cancel stops capture, cleans owned recordings and sends no phantom agent turn.
  **Depends:** none.
  **Gate:** available local mic/runtime and authorized supported transcription route.

- [ ] **LD05b — Play one consented spoken response**
  **Evidence:** [task CLI](bin/task_cli.ml) has fixed speech APIs, not an interactive speaker flow. **Deliver:** explicit voice choice and bounded cancellable playback separate from microphone capture.
  **Accept:** the selected response/voice plays only after consent; denial/cancel leaves no continuing playback, hidden recording or model download.
  **Depends:** LD05a.
  **Gate:** speaker/runtime and authorized supported speech route.

## P4 — Contract, service and platform gates

Preserve these scopes without inventing availability. Research cards can conclude **unavailable**, which keeps dependent implementation blocked; it does not constitute delivered inference, OAuth, sharing or platform support.

- [ ] **CT01a — Verify one exact-route tokenizer contract**
  **Evidence:** [context budget](lib/agent/context_budget.ml) is a byte proxy; [discovery](lib/provider/model_discovery.ml) already supplies several exact-route window limits. **Deliver:** select one named supported route/model and document an independently sourced tokenizer/version mapping and freshness boundary.
  **Accept:** known reference vectors and identity/source provenance are available; missing/cross-account mapping and image-token costs remain unknown rather than invented.
  **Depends:** none.
  **Gate:** documented tokenizer/source for a named route, not a model-family guess.

- [ ] **CT01b — Use the verified tokenizer for only that route**
  **Evidence:** CT01a supplies the exact mapping; [native token counting](lib/tools/native_tokenizer.ml) is not automatically provider budgeting. **Deliver:** one bounded budgeting integration retaining the byte proxy elsewhere.
  **Accept:** reference requests match the approved vectors; stale/version/account/route mismatch falls back honestly and never claims exact media costs.
  **Depends:** CT01a.
  **Gate:** verified tokenizer assets/license and route mapping.

- [ ] **PG01a — Establish reserved-lane roster authority**
  **Evidence:** [Singularity .tech adapter](lib/provider/transports/singularity_tech_api.ml#L1-L6) has no documented model-list contract. **Deliver:** find a documented pinned per-key roster or record precise unavailability.
  **Accept:** evidence distinguishes two reservation identities; a .dev/global roster or inactive-slot 403 cannot prove .tech access.
  **Depends:** none.
  **Gate:** provider documentation and authorized attributable samples.

- [ ] **PG01b — Add reservation-scoped discovery**
  **Evidence:** PG01a must establish the actual endpoint/fields. **Deliver:** one pinned per-key listing adapter with existing cancellation and selector rules.
  **Accept:** distinct keys expose only their exact IDs; auth/inactive reservation failures remain unavailable and never switch to .dev or a global catalog.
  **Depends:** PG01a.
  **Gate:** positive roster contract and authorized live acceptance account.

- [ ] **PG05a — Verify Cline suggested-catalog provenance**
  **Evidence:** [provider inventory](docs/PROVIDER_MODEL_DISCOVERY_INVENTORY.md#L22) records manual full router IDs and no model-list API. **Deliver:** establish a current pinned `recommended-models` contract or explicit unavailable result.
  **Accept:** provenance distinguishes suggestions from account entitlement; no guessed `/v1/models` endpoint or borrowed provider list.
  **Depends:** none.
  **Gate:** current documented source.

- [ ] **PG05b — Show Cline suggestions outside selectable account models**
  **Evidence:** PG05a supplies suggestions, not eligibility. **Deliver:** bounded separately labeled suggestions while preserving manual full-ID inference and the picker contract.
  **Accept:** malformed/unavailable source stays unclassified; suggestions never appear as entitlement-verified choices or displace the explicit router ID.
  **Depends:** PG05a.
  **Gate:** documented pinned source.

- [ ] **PG06a — Verify attributable Copilot premium usage**
  **Evidence:** [usage type](lib/core/protocol.ml#L3-L12) has token/cache/modality fields, not a verified premium counter. **Deliver:** document the exact reported field/header, semantics and personal account/route attribution with a redacted sample.
  **Accept:** a genuine count is distinguishable from token usage/quota; missing field, Enterprise identity and price remain unknown.
  **Depends:** none.
  **Gate:** documented provider field and authorized attributable evidence.

- [ ] **PG06b — Persist reported premium counters**
  **Evidence:** PG06a defines the counter contract. **Deliver:** optional exact-identity premium markers and branch-aware display without estimated prices.
  **Accept:** reported values survive branch/resume once; unrelated accounts and missing fields remain unknown, never inferred from requests or tokens.
  **Depends:** PG06a.
  **Gate:** verified counter semantics.

- [ ] **PG07a — Establish xAI API-key Responses contract**
  **Evidence:** [provider catalog](lib/provider/provider_catalog.ml) currently advertises xAI Chat. **Deliver:** document official endpoint, request/state/tool semantics and model eligibility for a separate API-key Responses route.
  **Accept:** attributable native tool/reasoning examples define replay; undocumented fields or subscription OAuth are not inferred.
  **Depends:** none.
  **Gate:** official current contract and authorized samples.

- [ ] **PG07b — Add the distinct xAI Responses route**
  **Evidence:** PG07a defines the native protocol. **Deliver:** separately selected request/response/tool-result adapter with account/route-bound opaque state.
  **Accept:** a native two-turn fixture preserves tool/reasoning state; Chat stays unchanged and cross-route/account replay is rejected before network.
  **Depends:** PG07a.
  **Gate:** documented contract; live acceptance requires explicit account authorization.

- [ ] **PG08a — Establish Z.AI Messages contract**
  **Evidence:** [Z.AI route](lib/provider/provider_catalog.ml) is Chat, not a Coding Plan alias. **Deliver:** document a distinct official Messages endpoint, credential scope and tool-result contract.
  **Accept:** native request/response evidence is attributable; absent documentation cannot be replaced with guessed Anthropic compatibility or Zhipu credentials.
  **Depends:** none.
  **Gate:** official endpoint/account contract.

- [ ] **PG08b — Add the separately selected Z.AI Messages route**
  **Evidence:** PG08a supplies the protocol and identity. **Deliver:** native Messages tool continuation without altering current Chat selectors.
  **Accept:** tool IDs round-trip in a two-turn fixture; Chat IDs never silently switch API and another plan/account key is rejected.
  **Depends:** PG08a.
  **Gate:** verified contract; authorized account for live acceptance.

- [ ] **EX11a — Define trust for one actual package registry**
  **Evidence:** [plugin registry](lib/extensions/plugin_registry.ml) only references already discovered local capabilities. **Deliver:** reviewed registry identity, package digest, activation and rollback policy for remote fetching.
  **Accept:** a specific registry/package format is attributable; project-owned sources cannot silently authorize executable activation.
  **Depends:** none.
  **Gate:** actual trusted registry and explicit trust-policy approval.

- [ ] **EX11b — Install one explicitly approved pinned package**
  **Evidence:** EX11a defines package trust. **Deliver:** bounded approved fetch/install with exact source/digest and inert-by-default activation.
  **Accept:** the chosen digest installs; tampered/unavailable/redirected package installs nothing and never activates code automatically.
  **Depends:** EX11a.
  **Gate:** approved registry/package.

- [ ] **EX11c — Upgrade and roll back an owned package**
  **Evidence:** EX11b supplies an owned installation. **Deliver:** separately approved version transition and recoverable rollback without modifying foreign plugins.
  **Accept:** a valid upgrade/rollback retains provenance; failure preserves the prior usable version and project data cannot approve activation.
  **Depends:** EX11b.
  **Gate:** verified old/new package versions and trust policy.

- [ ] **EX12 — Support one required legacy-SSE MCP server**
  **Evidence:** [MCP HTTP](lib/extensions/mcp_http.ml#L125-L158) handles Streamable HTTP SSE responses, not the legacy endpoint-event transport. **Deliver:** only the pinned legacy transport required by a documented real server.
  **Accept:** that server initializes/calls; disconnect settles once, foreign endpoint events leak no credential and no redirected-host reconnect occurs.
  **Depends:** none.
  **Gate:** actual legacy-SSE-only server and documented protocol need; otherwise do not add it.

- [ ] **EX13a — Verify MCP OAuth registration and scope**
  **Evidence:** [MCP config](lib/extensions/mcp_config.ml) supports private bearer references, not managed grants. **Deliver:** establish server-advertised metadata, an authorized registered client and exact scopes/origin.
  **Accept:** a concrete authorized registration is recorded without secrets; missing registration offers no login and provider grants are not borrowed.
  **Depends:** none.
  **Gate:** authorized registered client and real server metadata.

- [ ] **EX13b — Store a server-bound MCP login**
  **Evidence:** EX13a defines client/server ownership. **Deliver:** the approved login and private credential binding for one server, separate from model-provider identity.
  **Accept:** the scoped grant reaches only that server; wrong-origin redirect/token and missing consent yield no tool call.
  **Depends:** EX13a.
  **Gate:** authorized registration/server.

- [ ] **EX13c — Refresh and revoke the MCP grant safely**
  **Evidence:** EX13b supplies a managed grant, not its renewal lifecycle. **Deliver:** bounded cancellable locked refresh and explicit revocation behavior.
  **Accept:** one valid renewal preserves server scope; revoked/cancelled/wrong-origin grants invoke no tool and expose no credential.
  **Depends:** EX13b, AU02.
  **Gate:** server-supported authorized renewal/revocation contract.

- [ ] **SH02a — Specify a deployable read-only collaboration relay**
  **Evidence:** [local events](bin/main.ml) are not a hosted collaboration service. **Deliver:** an actual user-operated relay contract with authenticated encrypted invites, bounded retention and host-only execution.
  **Accept:** deployment/identity/expiry are demonstrable; no share URL is advertised without a real service and no guest can approve effects.
  **Depends:** RC03b, SH01b.
  **Gate:** deployable user-operated relay and reviewed sharing trust model.

- [ ] **SH02b — Deliver bounded guest backfill and reconnect**
  **Evidence:** SH02a defines the relay and consent boundary. **Deliver:** invited read-only event delivery with bounded replay cursors and reconnect.
  **Accept:** reconnect shows only consented redacted content once; guest writes/steering, expired invite and disconnect cannot authorize effects or expose approval secrets.
  **Depends:** SH02a.
  **Gate:** deployed authorized relay.

- [ ] **SH03a — Validate owner-operated issue webhook events**
  **Evidence:** [worktrees](lib/tools/workspace_git.ml) do not constitute an authenticated webhook service. **Deliver:** signature, replay/idempotency and repository/identity checks before scheduling any work.
  **Accept:** one verified event becomes one inert job request; invalid signature/duplicate/foreign repository produces no command, PR or secret-bearing log.
  **Depends:** WK02b, RC03b.
  **Gate:** deployable owner-operated endpoint and isolated webhook credentials.

- [ ] **SH03b — Execute one approved issue-to-PR workflow**
  **Evidence:** SH03a admits events; editing-worker and repository approvals remain separate. **Deliver:** isolated credentials/worktree and reviewed PR publication through the authorized host, not a public autonomous bot.
  **Accept:** the approved event yields only its intended change/PR; cancellation, duplicate delivery and failed approval create no extra effects or leaked credentials.
  **Depends:** SH03a, AG02c.
  **Gate:** deployed owner-operated service and explicit host approvals.

- [ ] **SH04 — Provide an invited read-only browser guest**
  **Evidence:** SH02b provides a relay stream, not a guest UI. **Deliver:** bounded authenticated browser view of the exact consented projection.
  **Accept:** invited guests see permitted backfill; expired/view-only links cannot steer, approve, or see secret-bearing effects, and absent relay advertises no URL.
  **Depends:** SH02b.
  **Gate:** real relay and separately scoped guest UI deployment.

- [ ] **LD04 — Evaluate snapshot-based compaction without replacing history**
  **Evidence:** [current compaction](lib/agent/context_compaction.ml) uses bounded text summaries. **Deliver:** opt-in comparison against persisted bounded image snapshots on one verified image-capable route, not an automatic cutover.
  **Accept:** branch/resume retains original transcript/artifacts; unsupported modality/budget refuses visibly and measured comparisons do not claim unreported image-token costs.
  **Depends:** CT01b, LD06b.
  **Gate:** verified image-capable route, budget evidence and explicit evaluation consent.

- [ ] **LD07a — Establish one authenticated realtime voice contract**
  **Evidence:** [fixed non-chat APIs](lib/provider/non_chat.ml) do not implement realtime WebRTC. **Deliver:** select an actually supported provider/runtime with bounded media-session, authentication and consent semantics.
  **Accept:** a documented connection/stop lifecycle exists; unsupported credentials/runtime expose no realtime feature or implicit model download.
  **Depends:** LD05b.
  **Gate:** supported authorized realtime provider and local media runtime.

- [ ] **LD07b — Run one bounded consented realtime voice session**
  **Evidence:** LD07a defines the selected transport. **Deliver:** separate realtime consent, bounded capture/playback and deterministic disconnection cleanup.
  **Accept:** an authorized disposable session exchanges real media; disconnect/revocation stops microphone transmission and no unreceived response is claimed spoken.
  **Depends:** LD07a.
  **Gate:** supported authenticated provider/runtime and microphone permission.

- [ ] **PL01a — Establish native Windows feasibility**
  **Evidence:** [installer targets](install.sh) and [release matrix](.github/workflows/release.yml#L21-L32) are POSIX-only. **Deliver:** native x64 OCaml/dependency, terminal and approved-process compatibility evidence on Windows, naming required changes.
  **Accept:** a native build/launch baseline or precise blockers are recorded; WSL execution is not Windows support and unsupported effects remain unavailable.
  **Depends:** none.
  **Gate:** actual Windows x64 runner/toolchain.

- [ ] **PL01b — Verify a native Windows prompt/cancel artifact**
  **Evidence:** PL01a identifies the native compatibility changes. **Deliver:** Windows executable/process/terminal behavior with native extracted-artifact acceptance.
  **Accept:** prompt, denied/approved process and cancellation restore terminal state; no POSIX-only path/process assumptions or orphan child remain.
  **Depends:** PL01a.
  **Gate:** native Windows runner and dependency compatibility.

- [ ] **PL01c — Add Windows install/update packaging**
  **Evidence:** [current installer](install.sh) has no native Windows lifecycle. **Deliver:** Windows-owned package, notices, checksums, install/update/uninstall and failure-preserving publication before advertising support.
  **Accept:** extracted native package installs/upgrades; failed update preserves the prior executable and uninstall retains unrelated data.
  **Depends:** PL01b, PL07.
  **Gate:** native Windows release runner and reviewed installer format.

- [ ] **PL02a — Build and execute a real musl x64 artifact**
  **Evidence:** [Linux release jobs](.github/workflows/release.yml#L21-L32) use GNU/Linux runners. **Deliver:** separate musl toolchain/native execution and dependency evidence for x64; keep the artifact unadvertised until distribution selection is ready.
  **Accept:** it launches on a clean musl target with truthful library diagnostics; a GNU-linked binary is never labeled musl.
  **Depends:** PL03a.
  **Gate:** actual musl x64 build and runtime environment.

- [ ] **PL02b — Build and execute a real musl arm64 artifact**
  **Evidence:** [current matrix](.github/workflows/release.yml) has no musl arm64 target. **Deliver:** independent arm64 musl build, dependency inspection and native execution evidence.
  **Accept:** a clean arm64 musl target launches the extracted executable; x64 or GNU smoke cannot substitute for native target proof.
  **Depends:** PL03a.
  **Gate:** actual musl arm64 build and runtime environment.

- [ ] **PL02c — Migrate libc selection and release manifests safely**
  **Evidence:** [manifest validation](install.sh#L91-L110) and release publication assume exactly four assets. **Deliver:** explicit GNU/musl selection and compatible manifest/update transition, preserving old embedded installers.
  **Accept:** each libc selects its verified artifact; legacy four-entry clients keep working, unsupported hosts fail closed and checksum/update/uninstall checks pass before advertising musl.
  **Depends:** PL02a, PL02b, PL07.
  **Gate:** verified musl artifacts and reviewed backward-compatible publication layout.

## Audit coverage and scope disposition

| Surveyed area | Evidence inspected | Backlog result |
| --- | --- | --- |
| Provider/auth/discovery/context | `lib/provider/`, `lib/auth/`, `bin/cli_auth.ml`, context integration and provider/auth fixtures | AU01–AU02; retained PG scopes; narrowed CT01–CT03 to actual residuals; IO01 verified and removed |
| Agent/journal/config/core | `lib/agent/`, `lib/session/`, `lib/config/`, `lib/core/` and lifecycle/usage/artifact fixtures | SJ05; typed/cancelled child residuals, not replacement job storage; SJ01–SJ04 verified and removed |
| Workspace/mobile/extensions | `lib/tools/`, `lib/extensions/`, tool/local-content fixtures and mobile plan | WK07–WK08, MB01; smaller mobile/edit/device cards; external MCP gates retained |
| CLI/TUI/distribution | `bin/`, `lib/ui/`, UI/CLI fixtures, installer/updater and CI/release workflows | queue/fairness and release-boundary cards; correlated events rather than duplicate JSONL |
| Product/maintainer docs | README, contributor/security/design rules, discovery inventory, changelog/checkpoint | canonical backlog links and implemented-feature wording corrected; historical evidence retained |

This was a source-and-fixture survey, not a line-by-line security certification, live provider test, device acceptance run or proof that every proposed risk reproduces. File links identify the audit baseline; re-read current code before implementation.

**PL05 retired:** the old “final dependency cutover” umbrella had no bounded independently useful deliverable. Do not schedule a whole-project rewrite. Make only the module-boundary changes required by a named behavioral card, migrating every caller/test/build rule/document together; behavior and packaged smoke remain that card's acceptance. This retirement is not a claim that maintainability work is complete.

**Implemented portions removed, residual scope retained:** CT01 already has exact-route window discovery; CT02 already has guarded proactive compaction and conservative recovery; CT03 already has request-only pruning; AG01 already has bounded concurrent jobs/idempotent delivery; AG03 already has list/wait/cancel; RC01 already has ordered redacted one-shot JSONL; PL03 already has conditional Apple packaging. Their remaining cards above are narrower, not blanket reimplementations.

**Not queued as fake integrations:** `web` is search, `typesafe` is a judge route and `local` is a catalog seed. Existing configurable compatible endpoints already cover user-owned LiteLLM. Cursor SDK and GitLab Duo Agent require separate agent-runtime contracts, not generic completion aliases. Do not add prohibited Antigravity OAuth or undocumented/unregistered Gemini CLI, Qwen Portal, Kimi device, xAI subscription, Zhipu plan or Muse login choices. Personal Copilot is not Enterprise. Missing vendor registration, entitlement, model-list API or hosted relay remains a gate, not a guessed endpoint or placeholder implementation.
