exception Error of string

let fail message = raise (Error message)

module Run = Workspace_mobile_run

type theme = Light | Dark
type orientation = Portrait | Landscape

type environment_setting =
  | Locale of string
  | Theme of theme
  | Orientation of orientation

type value = Locale_value of string | Theme_value of theme
  | Orientation_value of bool * int

type plan = {
  session_id : string;
  device : string;
  app_id : string;
  build_hash : string;
  setting : environment_setting;
  before : value;
  target : value;
  observe_command : string;
  change_command : string;
  restore_command : string;
}

type ownership = { plan : plan }
type restore_result = Restored | Already_changed | Restore_failed of string

let max_output_bytes = 4096
let timeout_ms = 10_000
let ensure_output output =
  if String.length output > max_output_bytes then fail "mobile environment output exceeded its limit";
  output

let adb session remote =
  "adb -s " ^ Filename.quote session.Run.device ^ " shell " ^ Filename.quote remote

let check_session session =
  if session.Run.platform <> Run.Android then fail "mobile environment experiments are Android-emulator only";
  if not (Workspace_android_devices.emulator_serial session.Run.device) then
    fail "mobile environment experiments require the exact selected Android emulator";
  if session.Run.state <> Run.Running then
    fail "mobile environment experiments require the selected running app"

let valid_locale value =
  String.length value <= 64 &&
  (value = "" ||
   List.for_all (fun tag ->
     tag <> "" &&
     (match tag.[0] with 'a'..'z' | 'A'..'Z' -> true | _ -> false) &&
     String.for_all (function
       | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true | _ -> false) tag)
     (String.split_on_char ',' value))

let setting_plan session ~setting ~before =
  check_session session;
  let app = Filename.quote session.Run.app_id in
  let p, target, observe_command, change_command, restore_command =
    match setting, before with
    | Locale desired, Locale_value previous when valid_locale desired && valid_locale previous ->
        let locale = Filename.quote desired and old = Filename.quote previous in
        (setting, Locale_value desired,
         adb session ("cmd locale get-app-locales " ^ app ^ " --user current"),
         adb session ("cmd locale set-app-locales " ^ app ^ " --user current --locales " ^ locale),
         adb session ("cmd locale set-app-locales " ^ app ^ " --user current --locales " ^ old))
    | Theme desired, Theme_value previous ->
        let set = function Light -> "no" | Dark -> "yes" in
        (setting, Theme_value desired, adb session "cmd uimode night",
         adb session ("cmd uimode night " ^ set desired), adb session ("cmd uimode night " ^ set previous))
    | Orientation desired, Orientation_value (auto, rotation) ->
        let target_rotation = match desired with Portrait -> 0 | Landscape -> 1 in
        let set auto rotation = "settings put system accelerometer_rotation " ^
          (if auto then "1" else "0") ^ " && settings put system user_rotation " ^ string_of_int rotation in
        (setting, Orientation_value (false, target_rotation),
         adb session "settings get system accelerometer_rotation; settings get system user_rotation",
         adb session (set false target_rotation), adb session (set auto rotation))
    | Locale _, _ -> fail "locale state must be observed and validated before preview"
    | Theme _, _ -> fail "theme state must be observed and validated before preview"
    | Orientation _, _ -> fail "orientation state must be observed and validated before preview" in
  { session_id = session.Run.id; device = session.Run.device; app_id = session.Run.app_id;
    build_hash = ""; setting = p; before; target; observe_command; change_command;
    restore_command }

let parse plan output =
  let output = ensure_output output in
  let lines = String.split_on_char '\n' (String.trim output) |> List.map String.trim in
  let malformed () = fail "mobile environment state was not an exact supported value" in
  match plan.setting with
  | Locale _ ->
      let prefix = "Locales for " ^ plan.app_id ^ " for user " in
      let value = match lines with
        | [line] when String.starts_with ~prefix line ->
            let rest = String.sub line (String.length prefix)
              (String.length line - String.length prefix) in
            (match String.index_opt rest ' ' with
             | Some offset ->
                 let user = String.sub rest 0 offset in
                 let raw = String.sub rest offset (String.length rest - offset) in
                 if user = "" || String.length user > 10 ||
                    not (String.for_all (function '0'..'9' -> true | _ -> false) user) ||
                    not (String.starts_with ~prefix:" are [" raw) ||
                    not (String.ends_with ~suffix:"]" raw) then malformed ();
                 String.sub raw 6 (String.length raw - 7)
             | None -> malformed ())
        | _ -> malformed () in
      if not (valid_locale value) then malformed ();
      Locale_value value
  | Theme _ ->
      (match lines with ["Night mode: yes"] -> Theme_value Dark
       | ["Night mode: no"] -> Theme_value Light | _ -> malformed ())
  | Orientation _ ->
      (match lines with
       | [auto; rotation] when (auto = "0" || auto = "1") &&
           List.mem rotation ["0"; "1"; "2"; "3"] ->
           Orientation_value (auto = "1", int_of_string rotation)
       | _ -> malformed ())

let preview session ~build_hash ~setting ~before_output =
  (* State observation itself is a bounded, read-only command. *)
  check_session session;
  if build_hash = "" || String.length build_hash > 256 then
    fail "mobile environment plan requires a bounded selected-build identity";
  let skeleton = match setting with
    | Locale _ -> Locale_value ""
    | Theme _ -> Theme_value Light
    | Orientation _ -> Orientation_value (false, 0) in
  let preliminary = setting_plan session ~setting ~before:skeleton in
  let before = parse preliminary before_output in
  let plan = setting_plan session ~setting ~before in
  { plan with build_hash }

let observation_command session ~setting =
  let before = match setting with
    | Locale _ -> Locale_value ""
    | Theme _ -> Theme_value Light
    | Orientation _ -> Orientation_value (false, 0) in
  (setting_plan session ~setting ~before).observe_command

let command_preview plan = plan.change_command

let same_session session ~build_hash plan =
  session.Run.id = plan.session_id && session.Run.device = plan.device &&
  session.Run.app_id = plan.app_id && build_hash = plan.build_hash &&
  session.Run.platform = Run.Android &&
  Workspace_android_devices.emulator_serial session.Run.device

let run_bounded ~run ~cancelled command =
  if !cancelled then fail "mobile environment action cancelled";
  let output = run ~command ~timeout_ms ~max_output_bytes ~cancelled in
  ensure_output output

let execute session ~build_hash ~approved ~cancelled ~run ~observe plan =
  if not approved then fail "mobile environment effect requires separate explicit approval";
  if not (same_session session ~build_hash plan) then
    fail "mobile environment plan belongs to another app session or build";
  if !cancelled then fail "mobile environment action cancelled";
  let current = observe ~command:plan.observe_command ~timeout_ms ~max_output_bytes ~cancelled |> parse plan in
  if current <> plan.before then fail "mobile environment pre-state changed since preview";
  if plan.before = plan.target then { plan }
  else begin
    ignore (run_bounded ~run ~cancelled plan.change_command);
    if !cancelled then fail "mobile environment action cancelled after transition";
    let after = observe ~command:plan.observe_command ~timeout_ms ~max_output_bytes ~cancelled |> parse plan in
    if after <> plan.target then fail "mobile environment transition did not reach the approved exact state";
    { plan }
  end

let restore session ~build_hash ~approved ~cancelled ~run ~observe ownership =
  let plan = ownership.plan in
  if not approved then fail "mobile environment restore requires separate explicit approval";
  if not (same_session session ~build_hash plan) then
    fail "mobile environment ownership belongs to another app session or build";
  if plan.before = plan.target then Restored
  else if !cancelled then Restore_failed "restore cancelled"
  else try
    let current = observe ~command:plan.observe_command ~timeout_ms ~max_output_bytes ~cancelled |> parse plan in
    if current <> plan.target then Already_changed
    else begin
      ignore (run_bounded ~run ~cancelled plan.restore_command);
      if !cancelled then Restore_failed "restore cancelled" else
      let restored = observe ~command:plan.observe_command ~timeout_ms ~max_output_bytes ~cancelled |> parse plan in
      if restored = plan.before then Restored else Restore_failed "restore did not recover the exact prior state"
    end
  with Error message -> Restore_failed message

