# Tasks — current project backlog

**Baseline:** 2026-10-04 · `main` at `c637eb4` · documentation audit, not runtime certification.

This is the single active backlog. It separates source-confirmed remaining work from proposed mobile features and externally gated product plans. Completed work is not an open checkbox. Historical acceptance remains in [the mobile plan](docs/MOBILE_DEVELOPMENT_PLAN.md), [changelog](CHANGELOG.md) and [checkpoint](docs/WORK_CHECKPOINT.md).

## Current capabilities — do not reimplement

| Area | Implemented baseline | Remaining boundary |
| --- | --- | --- |
| Mobile project understanding | Swift/Xcode/SwiftPM, Android Gradle, Flutter, RN/Expo inventory; focused separately approved checks; checked source diagnostics; sensitive mobile-config review | Dynamic build/config evidence is not inferred; dependencies/toolchains are not installed implicitly |
| Mobile app sessions | Private selected app/device identity; build/install/launch/stop; `/mobile` dashboard; emulator/simulator inventory and tests; separately approved owned AVD/Simulator boot, readiness, cancellation and shutdown are implemented | No physical devices or runtime/image downloads; real disposable-device acceptance requires separate approval; Flutter/RN device workflows are not equivalent to native app-session support |
| Observe/control/replay | Android screenshots/accessibility, tap/swipe/text/back and persisted one-step scenarios; iOS screenshots | iOS accessibility/control/replay remains unavailable |
| Diagnostics and verification | App-scoped logs/crashes/Android ANR, guarded-source verification, strict masked screenshot baselines | Symbol-file presence is not symbolication; visual decoding uses a bounded in-tree PNG implementation, but native Linux packaging remains unverified |
| Web previews | `publish_web` and `/publish`, per-prefix private identities, official Portal auto-setup after approval | Local app server must already listen; no alternate tunnel, automatic public exposure or shell-profile modification |
| General agent tools | Guarded file edits, LSP/DAP, approved owned processes/eval/network/browser tools, local plugins/MCP, read-only child jobs, journal/branch/compaction, redacted one-shot JSONL | Do not schedule these entire subsystems as new features; only residual cards below |

M01–M24, AS01, AO01, UI01, mobile BR01/LD01, FV01, VR01 and MD01 are completed baseline IDs. Mobile **LD01** means diagnostics; new **ST01** means request timing. Browser BR01a/BR01b are also historical completed work, not open dependencies.

## Execution and acceptance rules

- Recommended order: **P0 reliability → P1 mobile specialization → P2 focused general improvements → P3 optional local capabilities → P4 external gates**. Within a tier, respect named dependencies; skip a blocked card rather than guessing availability. Mobile backend research may run in parallel with independent reliability work.
- Each checkbox is one deliverable. `Evidence` is inspected source or a retained plan, not a reproduced runtime bug. P0 and the mobile baseline received a fresh source audit; other retained cards must be revalidated against current symbols before implementation. File links intentionally omit stale line numbers.
- P1 cards are **new feature proposals**, not promises that the feature already works or authorization to execute it. Gates requiring a product/trust decision need an intentional [design-rule](docs/DESIGN_RULES.md) update first. A research result of unavailable keeps dependent implementation blocked.
- Acceptance is future work: behavior regression plus actual changed CLI/tool/PTY surface. A mock can validate safety/parsing, never certify a real SDK/device/backend. Xcode/device acceptance is manual, disposable and separately approved; CI does not silently boot devices or install dependencies.
- Reuse existing ownership, cancellation, approval and artifact paths. No automatic model/tool replay, global device authority, borrowed credentials, implicit SDK/helper/package install, signing, physical deployment or hosted relay. Portal's explicitly approved setup is its existing narrow exception, not permission for mobile installs.
- Remove a completed checkbox only after recording its real verification. Commit per session; maintain the checkpoint. This planning-only refresh changed no implementation or product safety contract.

## P0 — Source-confirmed safety and reliability

- [x] **AU01 — Cancel in-turn credential refresh**
  **Evidence:** [OAuth transport](lib/auth/oauth_flow.ml) propagates turn cancellation through curl and reaps the owned subprocess; [credential resolution](bin/cli_auth.ml) passes the turn cancellation token. `test_oauth_flow` verifies account/grant binding on rotation, zero transport calls for pre-cancel, one in-flight ambiguous attempt without exposing the rotated token, and child-process reaping. `test_provider_http` verifies cancellation during credential resolution sends no inference request. **Deliver:** propagate cancellation through refresh and reap its owned subprocess; define the outcome of ambiguous remote token rotation without automatic retry.
  **Accept:** one successful refresh retains account/grant binding; cancelling a stalled refresh promptly releases ownership and sends zero inference requests, without logging tokens.
  **Depends:** none.
  **Gate:** none; controlled transport, owned-child and loopback inference fixtures.

- [x] **AU02 — Bound credential-lock acquisition**
  **Evidence:** [store locking](lib/auth/oauth_store.ml) now supports interruptible, deadline-bounded nested locking. `test_oauth_store` verifies four concurrent writers serialize 48 updates, cancelled/expired waiters do not enter the critical section, a cancelled waiter does not release another process's lock, and nested locking succeeds. **Deliver:** interruptible, deadline-bounded acquisition while retaining nested locking and atomic refresh guarantees.
  **Accept:** two processes serialize updates without losing accounts; a cancelled/expired waiter exits without refreshing, unlocking another holder or replacing credentials.
  **Depends:** AU01.
  **Gate:** none.

- [x] **WK07 — Report recoverable partial LSP application**
  **Evidence:** [apply_edit_preview](lib/tools/workspace_lsp.ml) reports structured per-target outcomes and guarded recovery callbacks. `test_workspace_lsp` forces a later-target conflict after one write, verifies exact applied/unchanged paths, preserves the concurrent user edit, and records only the successful write. **Deliver:** reproduce a later-target failure and expose exact applied/unchanged targets plus existing guarded recovery records; do not claim whole-batch atomicity.
  **Accept:** two successful files record once; a forced second-write failure reports the first change, preserves later concurrent user edits and permits only hash-checked, approved recovery.
  **Depends:** none.
  **Gate:** none; controlled filesystem fault, not user files.

- [x] **PL04a — Bound queued prompt admission**
  **Evidence:** [follow-up/steering queues](lib/ui/turn_runner.ml) admit submissions without aggregate item/byte limits. **Deliver:** bounded admission counting retained prompts and attachments, including dequeue/reinsert handling.
  **Accept:** a held-turn PTY preserves queue order; overflow retains the draft/attachments, drops no accepted work and does not cancel the active turn for rejected steering.
  **Depends:** none.
  **Gate:** none; local streaming fixture.

- [x] **PL04b — Bound event backlog and terminal starvation**
  **Evidence:** [runner notices](lib/ui/turn_runner.ml) and [UI queue](bin/ui/tui.ml) each cap at 4,096 events / 4 MiB, reserve 256 events / 1 MiB for terminal and input work, and yield after 128 pumped events. **Deliver:** measured sustained-stream retention and enforce these documented capacities/fairness limits.
  **Accept:** a sustained-stream PTY keeps ordered output and one settlement within the recorded ≤3 s input/resize latency bound; saturated cancellation/shutdown cannot deadlock producers or lose tool outcomes.
  **Depends:** none.
  **Gate:** none; a controlled 12,000-event loopback stream observed a 40.5 ms resize-plus-input response on macOS arm64.

- [x] **PL06 — Bound installer asset transfers**
  **Evidence:** [standalone installer](install.sh) and the embedded updater use 1 MiB manifest / 512 MiB archive caps with 10 s connect and 180 s total deadlines. **Deliver:** distinct manifest/archive transfer budgets used by standalone install and the embedded updater, retaining HTTPS/checksum/executable-last publication.
  **Accept:** valid controlled assets install; stalled/oversized transfers fail within bounds, preserve the previous executable hash and remove staging files. Download caps do not imply an extracted-size cap.
  **Depends:** none.
  **Gate:** none; stalled and oversized local HTTPS fixture transfers preserved the prior install and removed staging.

- [ ] **PL03a — Enforce complete packaged dependency policy**
  **Evidence:** The [release workflow](.github/workflows/release.yml) validates extracted executables/helpers with [native target dependency rules](test/distribution/check_release_dependencies.sh). Run #88 passed tests/builds on all four targets and package/updater gates on both Linux targets and macOS Intel. macOS arm64's loader-based FoundationModels check passed, exposing the missing system Swift-runtime allowlist entry. The checker now loader-validates `/usr/lib/swift/libswift*.dylib` and retains framework/non-system/missing-library controls, including a missing Swift-runtime fixture. **Deliver:** target-specific allowed-system-library checks for executable/helper artifacts on the four current targets.
  **Accept:** extracted artifacts launch without toolchain runtime paths; a deliberately linked non-system dependency and a missing dependency both fail packaging with clear diagnostics.
  **Depends:** none.
  **Gate:** Native four-target release-runner acceptance must pass with the loader-based framework check before completion.

- [ ] **PL07 — Automate installed updater transactions in CI**
  **Evidence:** The [release workflow](.github/workflows/release.yml) invokes [installed_update.py](test/distribution/installed_update.py). Run #87's three updater failures came from the controlled server's missing latest-release API route. The repaired harness serves controlled metadata, deliberately tests fallback lookup and checks unchanged installed hashes. Local complete transactions passed; run #88 passed native transactions on Linux x86_64/AArch64 and macOS Intel. macOS arm64 still awaits package-smoke acceptance of its system Swift runtime. **Deliver:** exercise native install/check/update/uninstall transactions against a controlled release in CI.
  **Accept:** custom-directory upgrade preserves unrelated files and user state; corrupt checksum, link/unexpected archive member, invalid marker and failed publication preserve the executable. Verify metadata state on partial publication; inherited destination/version overrides cannot redirect update.
  **Depends:** PL06.
  **Gate:** The repaired transaction harness must pass on all four native targets before completion; no public release required.

## P1 — Proposed mobile-specialized features

Prioritize platform gaps (MX01–MX04), accessibility/visual review (MX05–MX08), scenario depth (MX09–MX11), diagnosis/performance (MX12–MX16), framework workflows (MX17–MX18) and evidence/dashboard integration (MX19–MX20). Independent cards can proceed without unavailable iOS tooling. Extend existing tools, not a second mobile agent loop.

- [ ] **MX01 — Owned emulator/simulator boot and shutdown**
  **Evidence:** [lib/tools/workspace_android_devices.ml](lib/tools/workspace_android_devices.ml) — Inventory does not manage device lifetime. **Deliver:** Add separately approved boot, readiness and shutdown for one explicitly selected existing AVD/simulator; record process/device ownership.
  **Accept:** A disposable device becomes ready and shuts down only when owned; cancellation reaps owned launchers, and pre-existing devices survive.
  **Depends:** none.
  **Gate:** Installed runtime/image; no SDK/image download, erase or physical device support.

- [ ] **MX02 — iOS accessibility backend contract**
  **Evidence:** [lib/tools/workspace_mobile_observe.ml](lib/tools/workspace_mobile_observe.ml) generates a fixed public-API XCTest UI-test host/runner, exports and validates its structured attachment, bounds output to 10,000 nodes, depth 128 and 1 MiB, creates private temporary project artifacts, and refuses to invoke `xcodebuild` unless `simctl` still reports the exact target UUID Booted. [lib/tools/tools.ml](lib/tools/tools.ml) gates capture on an already-running app and the exact Owned Simulator lifecycle identity; unbound iOS sessions retain prior behavior. Tests cover tree/attachment validation, private project modes and unbound capability refusal, but no Xcode, Simulator or device command was run.
  **Deliver:** Use a separately approved temporary Native XCTest runner against only the selected running app on the exact Owned Simulator; disclose helper provenance, build/install/launch and activation effects (including foregrounding or possible reactivation after a race), permissions/deployment constraints and temporary paths. Never mutate user project sources or fabricate a tree.
  **Accept:** A supported backend reads only the selected disposable app; absent backend/binding yields explicit unavailable, never a fabricated tree.
  **Depends:** MX01 for the Owned Simulator lifecycle binding; ordinary unbound iOS sessions remain supported.
  **Gate:** Real supported macOS/Xcode runner and an explicitly approved disposable Simulator capture remain unverified; keep this card unchecked until the returned tree is observed.

- [ ] **MX03 — Approved iOS semantic UI actions**
  **Evidence:** [lib/tools/workspace_mobile_control.ml](lib/tools/workspace_mobile_control.ml) — Control currently supports Android only. **Deliver:** Implement app-bound tap, text and scroll through the MX02 backend with fresh semantic identifiers and exact per-action approval.
  **Accept:** One approved action changes the selected simulator app; stale/ambiguous targets, denial and cancellation cause no input; separately observe the result.
  **Depends:** MX02.
  **Gate:** Real simulator/backend; no implicit helper installation or physical-device input.

- [ ] **MX04 — iOS bug-scenario replay**
  **Evidence:** [lib/tools/workspace_mobile_scenario.ml](lib/tools/workspace_mobile_scenario.ml) — Stored scenarios and accessibility assertions are Android-only. **Deliver:** Extend versioned scenario identity and one-step replay to the verified iOS backend, preserving failure-stop and separate fresh observation approval.
  **Accept:** A disposable iOS scenario reaches its asserted state; platform/build mismatch and first failed assertion stop without retry.
  **Depends:** MX03.
  **Gate:** Real iOS app/backend; existing Android records remain valid.

- [ ] **MX05 — Accessibility findings from Android trees**
  **Evidence:** [lib/tools/workspace_mobile_observe.ml](lib/tools/workspace_mobile_observe.ml) — Parsed nodes are observations, not accessibility findings. **Deliver:** Add bounded rule-based findings for evidenced missing labels, duplicate ambiguous controls and undersized touch targets when density is known; retain node/rule provenance.
  **Accept:** A seeded fixture produces exact rule/node findings and a corrected fixture clears them; unknown density, contrast, focus order and screen-reader behavior remain unknown.
  **Depends:** none.
  **Gate:** Current approved Android tree; runtime capture still asks separately.

- [ ] **MX06 — Portable visual comparison on Linux**
  **Evidence:** [lib/tools/workspace_mobile_visual.ml](lib/tools/workspace_mobile_visual.ml) bounds encoded PNG input, dimensions and decompressed output; its bounded in-tree chunk/pixel parser uses pure-OCaml `decompress.zl` 1.6.0 (MIT) and retains masks/metadata checks. Release notices include Decompress, Checkseum and Optint; Optint 0.3.0's source license says MIT despite ISC opam metadata. Dynamic-Huffman, Paeth, checksum and size-limit fixtures passed in run #87 on all four native targets. Linux and macOS Intel package smokes passed; macOS arm64 package smoke exposed the separately repaired dyld-cache dependency check. **Deliver:** provide a bounded maintained decoder usable on supported Linux packages, with dependency/license review, retaining current masks and metadata checks.
  **Accept:** The same PNG pairs compare identically on macOS/Linux; malformed/oversized images fail without changing baselines, and packaged decoder runs on native targets.
  **Depends:** none.
  **Gate:** Four-target tests and packaged smoke must validate this decoder revision; no Linux artifact acceptance is claimed until the updated native package smoke passes.

- [ ] **MX07 — Reviewable visual regression reports**
  **Evidence:** [lib/tools/workspace_mobile_visual.ml](lib/tools/workspace_mobile_visual.ml) — Current comparison reports exact masked pixel differences, not a review report. **Deliver:** Return bounded baseline/current/difference artifacts with differing regions and explicit operator-selected tolerance; version comparison settings.
  **Accept:** A seeded layout defect yields the expected region; identical images pass, mismatched environments fail, and tolerances never silently hide changes.
  **Depends:** none.
  **Gate:** Validated captures; Linux execution depends on MX06.

- [ ] **MX08 — Explicit locale/theme/orientation experiments**
  **Evidence:** [lib/tools/workspace_mobile_visual.ml](lib/tools/workspace_mobile_visual.ml) — Baseline environment fields are operator-declared; no controlled environment transition exists. **Deliver:** Preview and separately approve one supported simulator/emulator setting change, record observed capability/state and restore only owned changes without overwriting later user changes.
  **Accept:** A disposable app is captured under the selected setting and restored safely; unsupported setting or failed restore is reported, not assumed.
  **Depends:** none.
  **Gate:** Platform-specific documented commands; design amendment for device-setting effects.

- [ ] **MX09 — Selected-app deep-link exercises**
  **Evidence:** [lib/tools/workspace_mobile_run.ml](lib/tools/workspace_mobile_run.ml) — Launch uses normal app launch, not a deep-link action. **Deliver:** Add exact URL/component preview and separate approval for a verified selected-app deep link; reject implicit external-app delegation.
  **Accept:** A disposable app handles its declared URL and fresh observation verifies the destination; invalid/foreign handler or denied action opens nothing.
  **Depends:** none.
  **Gate:** Known app link registration and real selected emulator/simulator.

- [ ] **MX10 — App lifecycle and state-restoration scenarios**
  **Evidence:** [lib/tools/workspace_mobile_run.ml](lib/tools/workspace_mobile_run.ml) — App stop/launch exists, but background/resume/process-recreation experiments do not. **Deliver:** Add individually approved supported background/resume/process-death steps with explicit data-loss consequences and scenario records.
  **Accept:** A disposable app restores or visibly loses seeded state after the chosen transition; denied/destructive reset has no effect and foreign apps are untouched.
  **Depends:** none.
  **Gate:** Platform capability evidence; no implicit data clear or uninstall.

- [ ] **MX11 — Permission-state test scenarios**
  **Evidence:** [lib/tools/workspace_sensitive.ml](lib/tools/workspace_sensitive.ml) — Source permission guards do not grant runtime permission-test authority. **Deliver:** Inventory supported selected-app permission states and separately approve one exact grant/revoke transition with prior-state evidence.
  **Accept:** A disposable app handles denial/grant as observed; unsupported permissions stay unavailable, and restore cannot overwrite a later user change.
  **Depends:** none.
  **Gate:** Reviewed runtime-permission contract and emulator-only acceptance.
  **Decision:** User chose to keep runtime permission transitions unavailable on 2026-10-04; do not implement grant/revoke effects without a new design decision.

- [ ] **MX12 — Android crash deobfuscation**
  **Evidence:** [lib/tools/workspace_mobile_diagnostics.ml](lib/tools/workspace_mobile_diagnostics.ml) — Mapping presence is reported without deobfuscation. **Deliver:** Bind crash/build identity to an existing verified mapping and approved installed retrace tool; preserve raw and transformed provenance.
  **Accept:** A seeded obfuscated crash resolves expected frames; mismatched/missing mapping refuses attribution and no tool/dependency is downloaded.
  **Depends:** none.
  **Gate:** Real compatible retrace runtime and build-bound mapping.

- [ ] **MX13 — iOS crash retrieval and symbolication**
  **Evidence:** [lib/tools/workspace_mobile_diagnostics.ml](lib/tools/workspace_mobile_diagnostics.ml) — iOS captures log excerpts and dSYM presence, not actual crash retrieval/symbolication. **Deliver:** Retrieve one bounded selected-app simulator crash and separately approve symbolication with matching binary/dSYM UUID and architecture.
  **Accept:** A disposable crash resolves known frames; foreign report, UUID/architecture mismatch or absent symbols remains unresolved with raw provenance.
  **Depends:** none.
  **Gate:** Real macOS crash artifact, matching dSYM and supported symbolicator.

- [ ] **MX14 — Measured Android launch/frame/memory profile**
  **Evidence:** [lib/tools/workspace_mobile_diagnostics.ml](lib/tools/workspace_mobile_diagnostics.ml) — Diagnostics lack a performance measurement workflow. **Deliver:** Add separately approved bounded app-scoped measurements using documented available Android tools; record units, warm/cold conditions, sample window and completeness.
  **Accept:** A disposable known-slow fixture exposes the measured regression; unsupported counters, truncated samples and unrelated PID data cannot become a pass.
  **Depends:** none.
  **Gate:** Real ready emulator and available platform tools; no inferred battery/energy scores.

- [ ] **MX15 — Measured iOS Simulator performance capture**
  **Evidence:** [lib/tools/workspace_mobile_diagnostics.ml](lib/tools/workspace_mobile_diagnostics.ml) — No app-scoped Instruments/xctrace workflow exists. **Deliver:** Select installed supported trace templates and bound one approved simulator capture/export with app/process identity and raw artifact provenance.
  **Accept:** A disposable workload yields actual supported counters; missing templates/failed exports report unavailable, and simulator values are not physical-device energy claims.
  **Depends:** none.
  **Gate:** Installed Xcode trace tool, supported template and real simulator.

- [ ] **MX16 — Offline and network-failure mobile experiments**
  **Evidence:** [lib/tools/workspace_mobile_scenario.ml](lib/tools/workspace_mobile_scenario.ml) — Scenarios currently cover input and tree assertions, not network conditions. **Deliver:** Define one explicit emulator-only network control boundary and separately approved transition/restore; avoid adopting personal proxies or trusting installed certificates.
  **Accept:** A disposable app demonstrates its offline recovery with fresh observations; cancellation restores only owned settings and no credentials/traffic are captured implicitly.
  **Depends:** none.
  **Gate:** Reviewed platform network-control contract; supported commands, no implicit MITM.
  **Decision:** User chose to keep network-disruption experiments unavailable on 2026-10-04; do not implement emulator-wide network effects without a new design decision.

- [ ] **MX17 — Flutter device integration tests**
  **Evidence:** [lib/tools/workspace_flutter_focus.ml](lib/tools/workspace_flutter_focus.ml) — Flutter checks support analysis and targeted test/*.dart, not device integration_test. **Deliver:** Discover an existing integration test and bind an exact installed Flutter device identity to the selected app session; preview separate approved execution without pub get.
  **Accept:** A real disposable integration test executes on the selected device and reports actual assertions; absent dependencies/test/device stays failure, not a unit-test substitute.
  **Depends:** none.
  **Gate:** Installed Flutter/dependencies, ready emulator; cross-framework device identity contract.

- [ ] **MX18 — Owned React Native/Expo development server**
  **Evidence:** [lib/tools/workspace_node_scripts.ml](lib/tools/workspace_node_scripts.ml) — RN/Expo supports declared test/lint scripts, not a managed Metro lifecycle. **Deliver:** Start an existing declared development script after approval as a session-owned process, verify readiness, show host exposure and provide approved stop.
  **Accept:** A disposable existing project serves through the owned process; cancel/session exit stops it, port conflict is explicit, and no npx/install/prebuild runs.
  **Depends:** none.
  **Gate:** Installed dependencies and declared script; public publishing is separately approved via Portal.

- [ ] **MX19 — Build-bound mobile verification report**
  **Evidence:** [lib/tools/workspace_mobile_run.ml](lib/tools/workspace_mobile_run.ml) — Lifecycle/verification/visual/scenario evidence exists in separate stores and outputs. **Deliver:** Assemble a bounded local report for one exact source/build/app/device identity, retaining failed/skipped/incomplete checks and links to owned artifacts.
  **Accept:** A report distinguishes passed/failed/not-run checks and refuses cross-build evidence; no screenshot bytes, unknown secrets or private logs are exported without consent.
  **Depends:** SH01a.
  **Gate:** Explicit local export consent; no hosted service or quality score.

- [x] **MX20 — Mobile dashboard capability-aware actions**
  **Evidence:** [bin/main.ml](bin/main.ml), [lib/tools/workspace_mobile_dashboard.ml](lib/tools/workspace_mobile_dashboard.ml), and [test/tools/test_workspace_mobile_dashboard.ml](test/tools/test_workspace_mobile_dashboard.ml). The dashboard enables only baseline-complete actions; MX02/05/08/09/10/14/17 proposals remain visibly disabled until their live-platform acceptance is recorded, with the exact current session or acceptance reason. `opam exec -- dune build bin/main.exe test/test_workspace_mobile_dashboard.exe test/test_tui_ux.exe && opam exec -- dune exec test/test_workspace_mobile_dashboard.exe && opam exec -- dune exec test/test_tui_ux.exe` passed. Actual Android/iOS Pave PTYs used a fixture provider and fake inventory executables: selecting Android Accessibility audit showed the MX05 gate without another provider request or device command; selecting iOS accessibility tree showed the MX02 and not-running reasons without another provider request, Xcode test or Simulator effect. The fake inventory commands were limited to `adb devices`, Xcode list/destinations and `simctl list`; this verifies dashboard behavior only, not native SDK/device acceptance.
  **Deliver:** Integrate only completed MX actions into /mobile using existing runner approvals; show unsupported/gated actions with exact reason and keep drafts on cancel.
  **Accept:** Actual Android/iOS PTYs expose only usable actions; selecting an unavailable backend executes nothing and every device effect retains its own approval.
  **Depends:** none.
  **Gate:** Implement incrementally after each corresponding MX card, not an empty new dashboard.

## P2 — Focused existing-product improvements

Retained unfinished scopes are not all fresh runtime diagnoses. Provider-specific evidence gates remain binding; existing timing, pruning, jobs, JSONL and editing features are prerequisites, not missing systems.

- [ ] **PG02 — Diagnose reserved-slot permission failures**
  **Evidence:** [Singularity reserved route](lib/provider/transports/singularity_tech_api.ml) lacks slot-specific classification; [HTTP errors](lib/provider/provider.ml) already have generic permission handling. **Deliver:** distinguish documented inactive-reservation denial from a bad key on this exact route only.
  **Accept:** a controlled inactive-slot 403 retains route/account and sends one completion only; ordinary 401/403 never acquire an invented reservation diagnosis.
  **Depends:** none.
  **Gate:** identifiable reservation error evidence; do not classify every 403 as an inactive slot.

- [ ] **PG03 — Accept the Hugging Face fallback key**
  **Evidence:** [CLI key resolution](bin/cli_auth.ml) lacks `HUGGINGFACE_HUB_TOKEN`. **Deliver:** reuse primary-first resolution for inference and pinned listing.
  **Accept:** `HF_TOKEN` wins; fallback alone works, missing keys fail, and neither credential reaches an unrelated origin.
  **Depends:** none.
  **Gate:** none; fixtures only.

- [ ] **PG04 — Accept the Beijing Token Plan fallback key**
  **Evidence:** [CLI key resolution](bin/cli_auth.ml) lacks `BAILIAN_TOKEN_PLAN_API_KEY`. **Deliver:** use it only when `ALIBABA_TOKEN_PLAN_API_KEY` is absent on the existing Beijing route.
  **Accept:** the primary wins; fallback cannot redirect to Coding Plan/workspace hosts and no-key failure remains explicit.
  **Depends:** none.
  **Gate:** none; fixtures only.

- [ ] **CT03 — Protect recent results in existing request pruning**
  **Evidence:** [trim_tool_results](lib/agent/context_budget.ml) already makes request-only copies but trims every oversized text result. **Deliver:** prioritize old eligible results and protect latest-turn call/result payloads; preserve signed replay and images.
  **Accept:** older text measurably shrinks requests while latest results and resumed journal bytes remain intact; protected context that cannot fit is refused rather than silently truncated.
  **Depends:** none.
  **Gate:** none; existing pruning is not missing and does not depend on a new recovery UI.

- [ ] **WK08 — Forward typed MCP image results**
  **Evidence:** [MCP validation](lib/extensions/mcp_client.ml) bounds structurally valid image blocks but does not establish MIME/magic agreement; [CLI integration](bin/main.ml) rejects all nontext blocks. **Deliver:** preserve bounded validated Text/Image blocks through existing canonical tool-result handling.
  **Accept:** approved text/image order survives a supported route and history; bad base64/MIME/magic or unsupported route is rejected before provider network, without payloads in UI/logs.
  **Depends:** none.
  **Gate:** none; controlled MCP/provider peers.

- [ ] **SJ05 — Persist child usage under its original identity**
  **Evidence:** [child workflow](bin/main.ml) puts usage in a success string, not durable typed markers. **Deliver:** owner-thread, idempotent delivery of each validated child usage record tied to original job/provider/account/route/model.
  **Accept:** usage survives child failure/cancellation and resume exactly once, even after parent model/session switch; no inferred counts or attribution to the new active identity.
  **Depends:** none.
  **Gate:** none.

- [ ] **CT02 — Recover explicitly after provider context rejection**
  **Evidence:** [pre-request compaction](bin/main.ml) already exists; post-rejection recovery is not a distinct action. **Deliver:** an explicit user recovery path reusing compaction with unchanged ancestry and signed-state validation, not automatic replay.
  **Accept:** a controlled context-size rejection can be compacted then resumed only by explicit action; interruption, unresolved calls or signed mismatch preserve original history and duplicate no tool/request.
  **Depends:** CT03.
  **Gate:** none; local near-limit fixture, not live account inference.

- [ ] **RC01a — Correlate public tool events with opaque IDs**
  **Evidence:** [JSONL events](bin/main.ml) publish name/state but omit per-call correlation; [CLI tests](test/test_cli_prompt.ml) correctly prohibit raw provider IDs. **Deliver:** a bounded public local tool-instance ID across start/update/settle/abort, preserving current one-shot JSONL behavior.
  **Accept:** two same-name shared reads each correlate with one terminal event; raw IDs, arguments/results, credentials, attachments and opaque replay remain absent.
  **Depends:** none.
  **Gate:** none.

- [ ] **RC01b — Define multi-turn local event ownership**
  **Evidence:** [current emitter](bin/main.ml) is one-shot and uses process-local `turn-1`. **Deliver:** versioned local session/turn/approval-request ownership and redaction, reusing the typed runner rather than a second lifecycle.
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
  **Evidence:** [reader dispatch](lib/tools/workspace_reader.ml) has artifact/HTTPS support, not `agent://`. **Deliver:** bounded nested field access to owned typed child results, reusing private artifact ownership.
  **Accept:** valid nested values retain types; foreign-session, missing, oversized and invalid selectors fail closed without leaking artifact content.
  **Depends:** AG01a.
  **Gate:** none.

- [ ] **WK01a — Add single-file hashline anchors**
  **Evidence:** [snapshot editor](lib/tools/workspace_edit.ml) uses exact text and whole-file SHA256, not line anchors. **Deliver:** bounded content-hash anchors on the existing guarded writer.
  **Accept:** a unique current anchor changes only its intended span; stale/ambiguous anchors preserve the file and do not fall back to approximate matching.
  **Depends:** none.
  **Gate:** none.

- [ ] **WK01b — Define and enforce multi-file anchor commit safety**
  **Evidence:** [LSP application](lib/tools/workspace_lsp.ml) is sequential, so generic multi-file atomicity is not an existing guarantee. **Deliver:** a guarded multi-file edit transaction with explicit crash/failure semantics; no second unguarded writer.
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
  **Evidence:** [worktree commits](lib/tools/workspace_git.ml) operate on whole approved paths. **Deliver:** inert snapshot-bound hunk plans with explicit dependencies and cycle rejection.
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

- [ ] **ST01 — Record exact completion timing**
  **Evidence:** [usage markers](lib/session/session.ml) contain usage/provenance. Stage and tool elapsed durations already persist; exact request start/first-text/finish/error timing is still absent. **Deliver:** optional measured timing markers for the exact request identity.
  **Accept:** streamed TTFT and terminal timestamps survive resume; buffered first-text and unreported token counts remain unknown, with no timing inferred from transcript length.
  **Depends:** none.
  **Gate:** none.

- [ ] **LD02 — Aggregate cross-session local statistics**
  **Evidence:** [usage_by_route](lib/session/session.ml) already totals selected-branch usage. **Deliver:** private project/model/day CLI/JSON aggregation with explicit fork/off-branch semantics and optional timing, not a second per-session usage command.
  **Accept:** reference sessions aggregate without duplicate billing; absent usage/prices/premium counts stay unknown and another project's private data is not exposed by default.
  **Depends:** SJ05, ST01.
  **Gate:** none.

- [ ] **PL04c — Measure long-session manager retention**
  **Evidence:** [session managers](bin/main.ml) live until shutdown; transcript bounds already exist. **Deliver:** measure repeated session switching, owned subprocess cleanup, idle CPU and retained memory in a real PTY; repair only demonstrated unbounded retention.
  **Accept:** record reproducible numeric limits and enforce them under sustained switching/resize/cancel; dirty jobs/user sessions are not destroyed merely to reduce memory.
  **Depends:** PL04a, PL04b.
  **Gate:** none; **diagnostic**, not a proven memory leak.

- [ ] **PL03b — Verify packaged capability advertising**
  **Evidence:** [release helper smoke](.github/workflows/release.yml) already covers conditional Apple support, but not a complete target capability matrix. **Deliver:** exact packaged availability checks for existing terminal imaging/LSP/DAP/native helper capabilities per current target.
  **Accept:** available helpers execute from an extracted package; unavailable capabilities are not advertised as ready, without removing ordinary tools solely because optional servers are not configured.
  **Depends:** PL03a.
  **Gate:** native current-target runners; do not reopen completed Apple packaging.

## P3 — Optional local capabilities

Retained product plans. None is enabled before its own ownership, approval and changed-surface acceptance passes. Editing children, personal-tab access and native desktop control require separate trust decisions.

- [ ] **WK02a — Read bounded explicit public GitHub references**
  **Evidence:** [reader URI dispatch](lib/tools/workspace_reader.ml) rejects issue/PR schemes. **Deliver:** explicit repository-scoped issue/PR/diff resolution with pinned provenance and bounded pagination, reusing network approval.
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
  **Evidence:** [job commands](bin/main.ml) already list/wait/cancel; bounded live transcript/steering is residual. **Deliver:** owner-scoped TUI inspection with exact reported usage and steering of the selected active child.
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
  **Evidence:** [one-shot CLI JSONL](bin/main.ml) is output, not a command transport. **Deliver:** versioned bounded input framing/correlation and read-only session/model inspection.
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
  **Depends:** none; the isolated headless browser is an implemented baseline.
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

## P4 — External contract, service and platform gates

Research can establish precise unavailability. Undocumented vendor endpoints, absent registered clients, unavailable native runners or imaginary services are not implementation claims. These cards are lower priority than local mobile workflows.

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
  **Evidence:** [Singularity .tech adapter](lib/provider/transports/singularity_tech_api.ml) has no documented model-list contract. **Deliver:** find a documented pinned per-key roster or record precise unavailability.
  **Accept:** evidence distinguishes two reservation identities; a .dev/global roster or inactive-slot 403 cannot prove .tech access.
  **Depends:** none.
  **Gate:** provider documentation and authorized attributable samples.

- [ ] **PG01b — Add reservation-scoped discovery**
  **Evidence:** PG01a must establish the actual endpoint/fields. **Deliver:** one pinned per-key listing adapter with existing cancellation and selector rules.
  **Accept:** distinct keys expose only their exact IDs; auth/inactive reservation failures remain unavailable and never switch to .dev or a global catalog.
  **Depends:** PG01a.
  **Gate:** positive roster contract and authorized live acceptance account.

- [ ] **PG05a — Verify Cline suggested-catalog provenance**
  **Evidence:** [provider inventory](docs/PROVIDER_MODEL_DISCOVERY_INVENTORY.md) records manual full router IDs and no model-list API. **Deliver:** establish a current pinned `recommended-models` contract or explicit unavailable result.
  **Accept:** provenance distinguishes suggestions from account entitlement; no guessed `/v1/models` endpoint or borrowed provider list.
  **Depends:** none.
  **Gate:** current documented source.

- [ ] **PG05b — Show Cline suggestions outside selectable account models**
  **Evidence:** PG05a supplies suggestions, not eligibility. **Deliver:** bounded separately labeled suggestions while preserving manual full-ID inference and the picker contract.
  **Accept:** malformed/unavailable source stays unclassified; suggestions never appear as entitlement-verified choices or displace the explicit router ID.
  **Depends:** PG05a.
  **Gate:** documented pinned source.

- [ ] **PG06a — Verify attributable Copilot premium usage**
  **Evidence:** [usage type](lib/core/protocol.ml) has token/cache/modality fields, not a verified premium counter. **Deliver:** document the exact reported field/header, semantics and personal account/route attribution with a redacted sample.
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
  **Evidence:** [MCP HTTP](lib/extensions/mcp_http.ml) handles Streamable HTTP SSE responses, not the legacy endpoint-event transport. **Deliver:** only the pinned legacy transport required by a documented real server.
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
  **Evidence:** [installer targets](install.sh) and [release matrix](.github/workflows/release.yml) are POSIX-only. **Deliver:** native x64 OCaml/dependency, terminal and approved-process compatibility evidence on Windows, naming required changes.
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
  **Evidence:** [Linux release jobs](.github/workflows/release.yml) use GNU/Linux runners. **Deliver:** separate musl toolchain/native execution and dependency evidence for x64; keep the artifact unadvertised until distribution selection is ready.
  **Accept:** it launches on a clean musl target with truthful library diagnostics; a GNU-linked binary is never labeled musl.
  **Depends:** PL03a.
  **Gate:** actual musl x64 build and runtime environment.

- [ ] **PL02b — Build and execute a real musl arm64 artifact**
  **Evidence:** [current matrix](.github/workflows/release.yml) has no musl arm64 target. **Deliver:** independent arm64 musl build, dependency inspection and native execution evidence.
  **Accept:** a clean arm64 musl target launches the extracted executable; x64 or GNU smoke cannot substitute for native target proof.
  **Depends:** PL03a.
  **Gate:** actual musl arm64 build and runtime environment.

- [ ] **PL02c — Migrate libc selection and release manifests safely**
  **Evidence:** [manifest validation](install.sh) and release publication assume exactly four assets. **Deliver:** explicit GNU/musl selection and compatible manifest/update transition, preserving old embedded installers.
  **Accept:** each libc selects its verified artifact; legacy four-entry clients keep working, unsupported hosts fail closed and checksum/update/uninstall checks pass before advertising musl.
  **Depends:** PL02a, PL02b, PL07.
  **Gate:** verified musl artifacts and reviewed backward-compatible publication layout.

## Scope reconciliation and next work

- Preserved all 90 existing unfinished cards; renamed general timing `LD01` to `ST01` and migrated its `LD02` dependency. Added 20 mobile proposals, `MX01`–`MX20`.
- Removed obsolete line anchors and the dangling active `BR01b` prerequisite; BR02 references the completed isolated-browser baseline. Removed the stale MB01 coverage reference and misleading “remaining mobile core” introduction.
- Fresh audit confirmed blocking credential refresh/locks, unbounded aggregate prompt/event admission, sequential LSP application, unbounded installer fetch budgets and incomplete package dependency/update smoke. Existing stage timing, text coalescing, request-only pruning and job ownership were not relisted as absent.
- **Default next implementation:** AU01. **First mobile-specific implementation:** MX01; MX02 backend research can proceed independently. This refresh does not start either task.
- PL05 remains retired: no whole-project rewrite or unbounded dependency cutover. Make only module changes required by a named behavior card.
- No fake integration backlog: configurable compatible endpoints cover user-operated LiteLLM; Cursor SDK/GitLab Duo Agent need distinct runtime contracts. No prohibited Antigravity OAuth, undocumented subscription login, borrowed provider catalog or guessed remote service. Missing entitlement/registration remains a gate.
