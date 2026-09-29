exception Error of string

let fail message = raise (Error message)

let lines output =
  String.split_on_char '\n' output |> List.map (fun line ->
    let length = String.length line in
    if length > 0 && line.[length - 1] = '\r' then
      String.sub line 0 (length - 1)
    else line)

let printable value =
  value <> "" && String.length value <= 128 &&
  String.for_all (fun ch -> Char.code ch >= 32 && Char.code ch < 127) value

let avds output =
  let names = List.filter ((<>) "") (lines output) in
  let valid name =
    printable name &&
    String.for_all (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' | '-' -> true
      | _ -> false) name in
  if List.length names > 100 || not (List.for_all valid names) then
    fail "emulator returned unsafe, duplicate or oversized AVD names";
  let names = List.sort String.compare names in
  let rec duplicate = function
    | first :: (second :: _ as rest) -> first = second || duplicate rest
    | _ -> false in
  if duplicate names then
    fail "emulator returned unsafe, duplicate or oversized AVD names";
  names

type state = Ready | Offline | Unauthorized | Unavailable

type device = { serial : string; state : state; emulator : bool }

let emulator_serial serial =
  let prefix = "emulator-" in
  if not (String.starts_with ~prefix serial) then false
  else
    let length = String.length serial - String.length prefix in
    (length = 4 || length = 5) &&
    String.for_all (function '0'..'9' -> true | _ -> false)
      (String.sub serial (String.length prefix) length)

let adb_devices output =
  let rec header = function
    | [] -> fail "adb returned no device-list header"
    | "List of devices attached" :: rest -> rest
    | ("* daemon started successfully" | "") :: rest -> header rest
    | line :: rest when String.starts_with ~prefix:"* daemon not running; starting now at " line ->
        header rest
    | _ -> fail "adb returned an unexpected device-list prefix" in
  let rows = List.filter ((<>) "") (header (lines output)) in
  if List.length rows > 100 then fail "adb returned too many devices";
  let seen = Hashtbl.create 16 in
  List.map (fun row ->
    match String.split_on_char '\t' row with
    | [serial; status] when printable serial &&
        String.for_all (function
          | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | ':' | '-' | '_' -> true
          | _ -> false) serial && printable status ->
        if Hashtbl.mem seen serial then fail "adb returned duplicate device IDs";
        Hashtbl.add seen serial ();
        let state = match status with
          | "device" -> Ready
          | "offline" -> Offline
          | "unauthorized" -> Unauthorized
          | _ -> Unavailable in
        { serial; state; emulator = emulator_serial serial }
    | _ -> fail "adb returned a malformed device row") rows
  |> List.sort (fun a b -> String.compare a.serial b.serial)
