exception Error of string

let fail message = raise (Error message)

type platform = Android | Ios
type state = Selected | Built | Installed | Running | Stopped

type session = {
  id : string;
  root : string;
  subroot : string;
  platform : platform;
  device : string;
  app_id : string;
  app_path : string;
  scheme : string option;
  variant : string option;
  activity : string option;
  mutable screen_size : (int * int) option;
  mutable state : state;
}

type manager = {
  lock : Mutex.t;
  sessions : (string, session) Hashtbl.t;
  mutable next_id : int;
  mutable closed : bool;
}

let create_manager () = {
  lock = Mutex.create ();
  sessions = Hashtbl.create 8;
  next_id = 0;
  closed = false;
}

let with_lock manager action =
  Mutex.lock manager.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock manager.lock) action

let state_name = function
  | Selected -> "selected"
  | Built -> "built"
  | Installed -> "installed"
  | Running -> "running"
  | Stopped -> "stopped"
let platform_name = function Android -> "android" | Ios -> "ios"

let valid_token ~label ~maximum ~allow token =
  if token = "" || String.length token > maximum ||
     not (String.for_all allow token) then fail ("invalid " ^ label)

let app_id platform value =
  let allowed = match platform with
    | Android -> (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' -> true
        | _ -> false)
    | Ios -> (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '-' -> true
        | _ -> false) in
  valid_token ~label:"mobile app identifier" ~maximum:255 ~allow:allowed value;
  let segments = String.split_on_char '.' value in
  let valid_segment segment =
    String.length segment > 0 &&
    let valid_head = match platform with
      | Android ->
          let first = segment.[0] in
          (first >= 'a' && first <= 'z') ||
          (first >= 'A' && first <= 'Z') || first = '_'
      | Ios -> allowed segment.[0] in
    valid_head && String.for_all allowed segment in
  if List.length segments < 2 || not (List.for_all valid_segment segments) then
    fail "invalid mobile app identifier";
  value
let artifact_path ~must_exist ~root ~platform relative =
  if not (Filename.is_relative relative) then
    fail "app artifact must be workspace-relative";
  let absolute =
    try Workspace_path.checked_path root relative with
    | Unix.Unix_error (Unix.ENOENT, _, _) when not must_exist ->
        let rec nearest existing =
          try ignore (Workspace_path.checked_path root existing)
          with
          | Unix.Unix_error (Unix.ENOENT, _, _) when existing <> "." ->
              nearest (Filename.dirname existing)
          | Workspace_path.Error message -> fail message
          | Unix.Unix_error _ -> fail "app artifact ancestor cannot be inspected" in
        nearest (Filename.dirname relative);
        Filename.concat root relative
    | Workspace_path.Error _ -> fail "app artifact path is outside the workspace" in
  let kind = try Some (Unix.lstat absolute).Unix.st_kind with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | Unix.Unix_error _ -> fail "app artifact cannot be inspected" in
  let suffix = match platform with
    | Android -> Filename.check_suffix relative ".apk"
    | Ios -> Filename.check_suffix relative ".app" in
  let valid_kind = match platform, kind with
    | Android, Some Unix.S_REG | Ios, Some Unix.S_DIR -> true
    | _, None -> not must_exist
    | _ -> false in
  if not suffix || not valid_kind then
    fail (if platform = Android then "Android app artifact must be a regular .apk file"
      else "iOS app artifact must be a regular .app directory");
  absolute

let select manager ~root ~subroot ~platform ~device ~app_id:bundle ~app_path
    ~scheme ~variant ~activity ~device_ready ~scheme_ready =
  let platform = match platform with
    | "android" -> Android | "ios" -> Ios
    | _ -> fail "mobile session platform must be android or ios" in
  if not device_ready then fail "selected device is not in the current approved compatible inventory";
  if platform = Ios && not scheme_ready then
    fail "selected iOS scheme and simulator are not in the current approved discovery inventory";
  let bundle = app_id platform bundle in
  let root =
    try Workspace_path.root_path root with Workspace_path.Error message -> fail message in
  let _artifact = artifact_path ~must_exist:false ~root ~platform app_path in
  let scheme = if platform = Ios then (
    let value = Option.value ~default:"" scheme in
    valid_token ~label:"Xcode scheme" ~maximum:128
      ~allow:(fun ch -> Char.code ch >= 32 && Char.code ch < 127) value;
    Some value) else None in
  let variant = Option.map (fun value ->
    valid_token ~label:"Android build variant" ~maximum:64
      ~allow:(function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false) value;
    value) variant in
  let activity = Option.map (fun value ->
    valid_token ~label:"Android launch activity" ~maximum:256
      ~allow:(function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '$' | '/' -> true | _ -> false)
      value;
    if not (String.starts_with ~prefix:(bundle ^ "/") value) ||
       String.length value <= String.length bundle + 1 then
      fail "Android launch activity must be a component in the selected app"
    else value) activity in
  if platform = Ios && Option.is_some activity then
    fail "iOS app sessions do not accept an Android launch activity";
  with_lock manager (fun () ->
    if manager.closed then fail "mobile session manager is closed";
    if manager.next_id = max_int then fail "mobile session ID space exhausted";
    manager.next_id <- manager.next_id + 1;
    let id = Printf.sprintf "mobile-%d" manager.next_id in
    let session = { id; root; subroot; platform; device; app_id = bundle;
      app_path; scheme; variant; activity; state = Selected; screen_size = None } in
    Hashtbl.add manager.sessions id session;
    session)

let lookup manager id =
  if manager.closed then fail "mobile session manager is closed";
  match Hashtbl.find_opt manager.sessions id with
  | Some session -> session
  | None -> fail "unknown mobile session ID"

let get manager id = with_lock manager (fun () -> lookup manager id)

let sessions manager = with_lock manager (fun () ->
  if manager.closed then fail "mobile session manager is closed";
  Hashtbl.fold (fun _ session rows -> session :: rows) manager.sessions []
  |> List.sort (fun left right ->
    let length_order = Int.compare (String.length right.id) (String.length left.id) in
    if length_order <> 0 then length_order else String.compare right.id left.id))

let command action session =
  let quote = Filename.quote in
  match action, session.platform with
  | "install", Android -> "adb -s " ^ quote session.device ^ " install -r " ^
      quote (artifact_path ~must_exist:true ~root:session.root
        ~platform:session.platform session.app_path)
  | "install", Ios -> "xcrun simctl install " ^ quote session.device ^ " " ^
      quote (artifact_path ~must_exist:true ~root:session.root
        ~platform:session.platform session.app_path)
  | "launch", Android ->
      "adb -s " ^ quote session.device ^ " shell am start -W " ^
      (match session.activity with
       | Some activity -> "-n " ^ quote activity
       | None -> "-a android.intent.action.MAIN" ^
           " -c android.intent.category.LAUNCHER -p " ^ quote session.app_id)
  | "launch", Ios -> "xcrun simctl launch " ^ quote session.device ^ " " ^ quote session.app_id
  | "stop", Android -> "adb -s " ^ quote session.device ^ " shell am force-stop " ^
      quote session.app_id
  | "stop", Ios -> "xcrun simctl terminate " ^ quote session.device ^ " " ^ quote session.app_id
  | _ -> fail "mobile session action must be install, launch or stop"

let can_transition action state = match action, state with
  | "install", (Selected | Built | Installed | Stopped)
  | "launch", (Installed | Running | Stopped)
  | "stop", Running -> true
  | _ -> false

let state_after action = match action with
  | "install" -> Installed | "launch" -> Running | "stop" -> Stopped
  | _ -> fail "mobile session action must be install, launch or stop"

let execute manager ~approved ~run ~action ~id =
  if not approved then fail "mobile device action requires explicit interactive approval";
  let session = with_lock manager (fun () ->
    let session = lookup manager id in
    if not (can_transition action session.state) then
      fail (Printf.sprintf "cannot %s mobile session while it is %s"
        action (state_name session.state));
    session) in
  let command = command action session in
  let output = run ~root:session.root ~command in
  with_lock manager (fun () ->
    let current = lookup manager id in
    current.state <- state_after action;
    current.screen_size <- None);
  output
let mark_built manager ~id =
  with_lock manager (fun () ->
    let session = lookup manager id in
    ignore (artifact_path ~must_exist:true ~root:session.root
      ~platform:session.platform session.app_path);
    if session.state = Running then fail "cannot rebuild a running mobile app";
    session.state <- Built;
    session.screen_size <- None;
    session)

let set_screen_size manager ~id ~width ~height =
  if width <= 0 || height <= 0 then fail "mobile screenshot dimensions must be positive";
  with_lock manager (fun () ->
    let session = lookup manager id in
    if session.state <> Running then fail "screen dimensions require a running mobile session";
    session.screen_size <- Some (width, height))

let clear_screen_size manager ~id =
  with_lock manager (fun () ->
    let session = lookup manager id in
    session.screen_size <- None)

let render session =
  Printf.sprintf "%s · %s · %s · app %s · device %s · %s · artifact %s%s%s%s"
    session.id (platform_name session.platform) (state_name session.state)
    session.app_id session.device
    (match session.scheme with Some value -> "scheme " ^ value | None -> "")
    session.app_path
    (match session.variant with Some value -> " · variant " ^ value | None -> "")
    (match session.activity with Some value -> " · activity " ^ value | None -> "")
    (match session.scheme with Some _ -> "" | None -> "")

let close_manager manager = with_lock manager (fun () ->
  manager.closed <- true;
  Hashtbl.clear manager.sessions)
