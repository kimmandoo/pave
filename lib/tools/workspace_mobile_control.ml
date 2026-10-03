type action =
  | Tap of { x : int; y : int }
  | Swipe of { x1 : int; y1 : int; x2 : int; y2 : int; duration_ms : int }
  | Text of string
  | Back

exception Error of string
let fail message = raise (Error message)

let check_coordinate ~label ~extent value =
  if value < 0 || value >= extent then
    fail (Printf.sprintf "%s coordinate %d is outside the observed screen extent 0..%d"
      label value (extent - 1))

let command (session : Workspace_mobile_run.session) ~screen_size action =
  if session.state <> Workspace_mobile_run.Running then
    fail "mobile UI control requires a running app session";
  if session.platform <> Workspace_mobile_run.Android then
    fail "mobile UI control is currently available only for Android sessions";
  let observed_size () = match screen_size with
    | Some (width, height) when width > 0 && height > 0 -> width, height
    | Some _ -> fail "observed mobile screen dimensions are invalid"
    | None -> fail "capture a screenshot of the running session before coordinate-based UI control" in
  let remote = match action with
    | Tap { x; y } ->
        let width, height = observed_size () in
        check_coordinate ~label:"x" ~extent:width x;
        check_coordinate ~label:"y" ~extent:height y;
        Printf.sprintf "input tap %d %d" x y
    | Swipe { x1; y1; x2; y2; duration_ms } ->
        let width, height = observed_size () in
        check_coordinate ~label:"start x" ~extent:width x1;
        check_coordinate ~label:"start y" ~extent:height y1;
        check_coordinate ~label:"end x" ~extent:width x2;
        check_coordinate ~label:"end y" ~extent:height y2;
        if duration_ms < 1 || duration_ms > 10_000 then
          fail "swipe duration must be between 1 and 10000 milliseconds";
        Printf.sprintf "input swipe %d %d %d %d %d" x1 y1 x2 y2 duration_ms
    | Text text ->
        if String.length text = 0 || String.length text > 512 then
          fail "mobile text input must contain 1..512 bytes";
        String.iter (fun ch ->
          let code = Char.code ch in
          if code < 32 || code = 127 then fail "mobile text input cannot contain control characters";
          if ch = '%' then fail "mobile text input cannot contain percent characters; Android reserves %s for spaces"
        ) text;
        let encoded = String.concat "%s" (String.split_on_char ' ' text) in
        "input text " ^ Filename.quote encoded
    | Back -> "input keyevent KEYCODE_BACK"
  in
  "adb -s " ^ Filename.quote session.device ^ " shell " ^ Filename.quote remote
