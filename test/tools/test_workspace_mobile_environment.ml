module Run = Pave.Workspace_mobile_run
module Env = Pave.Workspace_mobile_environment

let expect label condition = if not condition then failwith label
let rejects label action =
  try ignore (action ()); failwith ("mobile environment accepted " ^ label)
  with Env.Error _ -> ()

let session ?(platform = Run.Android) ?(device = "emulator-5554") () =
  { Run.id = "mobile-1"; root = "/tmp/mobile"; subroot = "/tmp/mobile";
    platform; device; app_id = "dev.example"; app_path = "app.apk";
    scheme = None; variant = None; activity = None; ios_device_binding = None;
    state = Run.Running; screen_size = None }

let () =
  let app = session () and build_hash = "sha256:build-a" in
  let cancelled = ref false in
  let state = ref "Night mode: no" in
  let changes = ref [] in
  let observe ~command:_ ~timeout_ms:_ ~max_output_bytes:_ ~cancelled:_ = !state in
  let run ~command ~timeout_ms:_ ~max_output_bytes:_ ~cancelled:_ =
    changes := command :: !changes;
    state := "Night mode: yes";
    ""
  in
  let plan = Env.preview app ~build_hash ~setting:(Env.Theme Env.Dark)
      ~before_output:"Night mode: no" in
  expect "plan binds exact build identity" (plan.build_hash = build_hash);
  expect "theme command preview is emulator/app bound"
    (String.starts_with ~prefix:("adb -s " ^ Filename.quote "emulator-5554" ^ " shell ")
      (Env.command_preview plan));
  rejects "different selected build" (fun () ->
    Env.execute app ~build_hash:"sha256:build-b" ~approved:true
      ~cancelled ~run ~observe plan);
  rejects "denied approval before command" (fun () ->
    Env.execute app ~build_hash ~approved:false ~cancelled ~run ~observe plan);
  expect "denial and build mismatch run no command" (!changes = []);
  let owned = Env.execute app ~build_hash ~approved:true ~cancelled ~run ~observe plan in
  expect "approved effect runs" (List.length !changes = 1);
  expect "approved effect has exact target" (!state = "Night mode: yes");
  expect "restore recovers prior state" (Env.restore app ~build_hash ~approved:true
    ~cancelled ~run:(fun ~command ~timeout_ms:_ ~max_output_bytes:_ ~cancelled:_ ->
      changes := command :: !changes; state := "Night mode: no"; "") ~observe owned = Env.Restored);
  expect "restoration used only owned effect" (!state = "Night mode: no");
  let preexisting = Env.preview app ~build_hash ~setting:(Env.Theme Env.Light)
      ~before_output:"Night mode: no" in
  let count = List.length !changes in
  let _ = Env.execute app ~build_hash ~approved:true ~cancelled ~run ~observe preexisting in
  expect "pre-existing target is left untouched" (List.length !changes = count);
  rejects "inexact theme value" (fun () ->
    Env.preview app ~build_hash ~setting:(Env.Theme Env.Dark) ~before_output:"night");
  rejects "invalid locale value" (fun () ->
    Env.preview app ~build_hash ~setting:(Env.Locale "en;bad")
      ~before_output:"Locales for app dev.example: []");
  rejects "unsupported iOS" (fun () ->
    Env.preview (session ~platform:Run.Ios ()) ~build_hash
      ~setting:(Env.Theme Env.Dark) ~before_output:"Night mode: no");
  cancelled := true;
  let before_cancel = List.length !changes in
  rejects "cancellation before transition" (fun () ->
    Env.execute app ~build_hash ~approved:true ~cancelled ~run ~observe plan);
  expect "cancelled transition runs no command" (List.length !changes = before_cancel);
  cancelled := false;
  state := "Night mode: no";
  let owned = Env.execute app ~build_hash ~approved:true ~cancelled ~run ~observe plan in
  state := "Night mode: no";
  expect "later user change prevents restore"
    (Env.restore app ~build_hash ~approved:true ~cancelled ~run ~observe owned = Env.Already_changed);
  state := "Night mode: no";
  let owned = Env.execute app ~build_hash ~approved:true ~cancelled ~run ~observe plan in
  let failed = Env.restore app ~build_hash ~approved:true ~cancelled ~observe
    ~run:(fun ~command:_ ~timeout_ms:_ ~max_output_bytes:_ ~cancelled:_ -> "") owned in
  expect "failed restore is reported while ownership remains reusable"
    (match failed with Env.Restore_failed _ -> true | _ -> false);
  expect "failed restore retains the ownership token"
    (Env.restore app ~build_hash ~approved:true ~cancelled ~observe
      ~run:(fun ~command:_ ~timeout_ms:_ ~max_output_bytes:_ ~cancelled:_ ->
        state := "Night mode: no"; "") owned = Env.Restored);
  print_endline "workspace mobile environment: ok"
