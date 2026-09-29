exception Error of string

let fail message = raise (Error message)

type discovery = {
  root : string;
  bundle : string;
  manifest_hash : string;
  schemes : string list;
  mutable destinations : (string * string list) list;
}

let schemes output =
  let json = try Yojson.Basic.from_string output
    with Yojson.Json_error _ -> fail "xcodebuild returned invalid scheme JSON" in
  let section = match json with
    | `Assoc [("project", `Assoc fields)]
    | `Assoc [("workspace", `Assoc fields)] -> fields
    | _ -> fail "xcodebuild returned no unique project/workspace section" in
  let schemes = match List.assoc_opt "schemes" section with
    | Some (`List rows) -> List.map (function
        | `String scheme when scheme <> "" && String.length scheme <= 128 &&
          String.for_all (fun char -> Char.code char >= 32 &&
            Char.code char < 127) scheme -> scheme
        | _ -> fail "xcodebuild returned an unsafe scheme") rows
    | _ -> fail "xcodebuild returned no scheme list" in
  if List.length schemes > 100 ||
     List.length (List.sort_uniq String.compare schemes) <> List.length schemes
  then fail "xcodebuild returned duplicate or oversized schemes";
  schemes

let destinations output =
  let uuid id =
    String.length id = 36 &&
    String.for_all (fun char ->
      (char >= '0' && char <= '9') ||
      (char >= 'a' && char <= 'f') ||
      (char >= 'A' && char <= 'F') || char = '-') id &&
    List.for_all (fun offset -> id.[offset] = '-') [8; 13; 18; 23] in
  let available = ref false and results = ref [] in
  String.split_on_char '\n' output |> List.iter (fun line ->
    let line = String.trim line in
    if String.starts_with ~prefix:"Available destinations" line ||
       String.starts_with ~prefix:"Destinations compatible with " line then
      available := true
    else if String.starts_with ~prefix:"Ineligible destinations" line ||
            String.starts_with ~prefix:"Destinations incompatible with " line then
      available := false
    else if !available && String.length line >= 2 &&
      line.[0] = '{' && line.[String.length line - 1] = '}' then (
      let body = String.sub line 1 (String.length line - 2) in
      let fields = String.split_on_char ',' body |> List.filter_map (fun item ->
        match String.index_opt item ':' with
        | None -> None
        | Some colon ->
            Some (String.trim (String.sub item 0 colon),
              String.trim (String.sub item (colon + 1)
                (String.length item - colon - 1)))) in
      if List.assoc_opt "platform" fields = Some "iOS Simulator" then
        match List.assoc_opt "id" fields with
        | Some id when uuid id -> results := id :: !results
        | _ -> ()));
  let results = List.sort_uniq String.compare !results in
  if List.length results > 100 then fail "too many Xcode destinations";
  results

type simulator = {
  runtime : string;
  id : string;
  name : string;
  state : string;
}

let compatible_simulators ~destinations output =
  let json = try Yojson.Basic.from_string output
    with Yojson.Json_error _ -> fail "simctl returned invalid device JSON" in
  let runtimes = match json with
    | `Assoc fields ->
        (match List.assoc_opt "devices" fields with
         | Some (`Assoc runtimes) when
             List.length fields = List.length (List.sort_uniq String.compare
               (List.map fst fields)) &&
             List.length runtimes = List.length (List.sort_uniq String.compare
               (List.map fst runtimes)) -> runtimes
         | _ -> fail "simctl returned no unique device map")
    | _ -> fail "simctl returned no device map" in
  let prefix = "com.apple.CoreSimulator.SimRuntime.iOS-" in
  let seen = Hashtbl.create 32 and results = ref [] and count = ref 0 in
  List.iter (fun (runtime, devices) ->
    if String.starts_with ~prefix runtime then (
      let version = String.sub runtime (String.length prefix)
        (String.length runtime - String.length prefix) in
      if version = "" || String.length version > 32 ||
         not (String.for_all (function '0'..'9' | '-' -> true | _ -> false)
           version) then fail "simctl returned an invalid iOS runtime";
      let version = String.map (function '-' -> '.' | ch -> ch) version in
      let devices = match devices with
        | `List rows -> rows | _ -> fail "simctl returned an invalid device list" in
      List.iter (fun device ->
        incr count;
        if !count > 100 then fail "simctl returned too many iOS devices";
        let fields = match device with
          | `Assoc fields when
              List.length fields = List.length (List.sort_uniq String.compare
                (List.map fst fields)) -> fields
          | _ -> fail "simctl returned an invalid device" in
        let field name = List.assoc_opt name fields in
        match field "isAvailable" with
        | Some (`Bool false) -> ()
        | Some (`Bool true) ->
            let id = match field "udid" with
              | Some (`String id) -> id
              | _ -> fail "simctl returned a device without an ID" in
            if Hashtbl.mem seen id then fail "simctl returned duplicate device IDs";
            Hashtbl.add seen id ();
            (match field "state" with
             | Some (`String ("Booted" | "Shutdown" as state))
               when List.mem id destinations ->
                 let name = match field "name" with
                   | Some (`String name) when name <> "" &&
                       String.length name <= 80 &&
                       String.for_all (fun ch -> Char.code ch >= 32 &&
                         Char.code ch < 127) name -> name
                   | _ -> id in
                 results := { runtime = version; id; name; state } :: !results
             | _ -> ())
        | _ -> fail "simctl returned an invalid availability flag") devices)) runtimes;
  List.sort (fun a b -> compare (a.runtime, a.id) (b.runtime, b.id))
    !results
