exception Error of string

let fail message = raise (Error message)

(* Session-bound discovery fingerprint: [Complete digest] covers the exact
   bundle manifest plus every bounded in-scope project input (shared scheme
   files and manifests/shared schemes of directly referenced .xcodeproj
   bundles). [Unresolved] marks any input that cannot be resolved inside the
   workspace or hashed within bounds; it must never store verified choices. *)
type fingerprint =
  | Complete of string
  | Unresolved

type discovery = {
  root : string;
  bundle : string;
  fingerprint : fingerprint;
  schemes : string list;
  mutable destinations : (string * string list) list;
  (* Per-scheme compatible simulator UUIDs from a successful simctl
     inventory; absent or empty means the inventory did not run or found
     no device, never permission to test. *)
  mutable simulators : (string * string list) list;
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

(* Bounded in-scope project inputs that a fingerprint can cover: the selected
   bundle manifest, its shared scheme files, and the manifests plus shared
   schemes of directly referenced .xcodeproj bundles. Arbitrary executable
   project code (build phases, settings, sources) is intentionally not
   fingerprinted. *)
let max_fingerprint_inputs = 300

let has_xcodeproj text = Filename.check_suffix text ".xcodeproj"

let is_delimiter char =
  char = ' ' || char = '\t' || char = '\r' || char = '\n' ||
  char = ';' || char = ',' || char = '{' || char = '}' ||
  char = '(' || char = ')' || char = '='

(* Quoted and bare tokens in OpenStep plist data. Comments are opaque. *)
let quoted_value source at =
  let n = String.length source and quote = source.[at] in
  let rec find j =
    if j >= n then None
    else if source.[j] = quote then
      Some (String.sub source (at + 1) (j - at - 1), j + 1)
    else find (j + 1) in
  find (at + 1)

let token_end source at =
  let n = String.length source in
  let rec find j =
    if j >= n || is_delimiter source.[j] || source.[j] = '"' then j
    else find (j + 1) in
  find at

(* Literal .xcodeproj members of a contents.xcworkspacedata manifest. *)
let workspace_project_locations source =
  let n = String.length source and locations = ref [] in
  let rec scan i =
    if i + 9 <= n then (
      if (i = 0 || source.[i - 1] = ' ' || source.[i - 1] = '\t' ||
          source.[i - 1] = '\r' || source.[i - 1] = '\n' ||
          source.[i - 1] = '<' || source.[i - 1] = '\'' ||
          source.[i - 1] = '"') &&
         (String.sub source i 9 = "location=" ||
          String.sub source i 9 = "location ") then
        let j = ref (i + 8) in
        while !j < n && (source.[!j] = ' ' || source.[!j] = '\t' ||
                         source.[!j] = '\r' || source.[!j] = '\n') do
          incr j
        done;
        if !j < n && source.[!j] = '=' then (
          incr j;
          while !j < n && (source.[!j] = ' ' || source.[!j] = '\t' ||
                           source.[!j] = '\r' || source.[!j] = '\n') do
            incr j
          done;
          if !j < n && (source.[!j] = '"' || source.[!j] = '\'') then
            match quoted_value source !j with
            | Some (value, next) ->
                if has_xcodeproj value then locations := value :: !locations;
                scan next
            | None -> scan (!j + 1)
          else scan (!j + 1))
        else scan (i + 1)
      else scan (i + 1)) in
  scan 0;
  List.sort_uniq String.compare !locations

(* Literal `path = ...;` values ending in .xcodeproj inside project.pbxproj
   data. Referenced projects list their subproject under both name and path;
   only the path locates the manifest. *)
let pbxproj_project_locations source =
  let n = String.length source and locations = ref [] in
  let read_path i =
    (* i sits just after "path"; allow spaces, '=' and spaces. *)
    let j = ref i in
    while !j < n && (source.[!j] = ' ' || source.[!j] = '\t' ||
                     source.[!j] = '\r' || source.[!j] = '\n') do
      incr j
    done;
    if !j < n && source.[!j] = '=' then (
      incr j;
      while !j < n && (source.[!j] = ' ' || source.[!j] = '\t' ||
                       source.[!j] = '\r' || source.[!j] = '\n') do
        incr j
      done;
      if !j < n && source.[!j] = '"' then
        match quoted_value source !j with
        | Some (value, next) ->
            if has_xcodeproj value then locations := value :: !locations;
            next
        | None -> !j + 1
      else
        let stop = token_end source !j in
        let value = String.sub source !j (stop - !j) in
        if has_xcodeproj value then locations := value :: !locations;
        stop)
    else i in
  let rec scan i =
    if i + 4 <= n then
      if source.[i] = '"' then
        (match quoted_value source i with
         | Some (_, next) -> scan next
         | None -> scan n)
      else if source.[i] = '/' && i + 1 < n && source.[i + 1] = '*' then
        let rec finish j =
          if j + 1 < n then
            if source.[j] = '*' && source.[j + 1] = '/' then j + 2
            else finish (j + 1)
          else n in
        scan (finish (i + 2))
      else if (i = 0 || is_delimiter source.[i - 1]) &&
              String.sub source i 4 = "path" &&
              (i + 4 >= n || is_delimiter source.[i + 4]) then
        scan (read_path (i + 4))
      else scan (i + 1) in
  scan 0;
  List.sort_uniq String.compare !locations

(* Normalize a bundle-directory-relative reference; [None] when it escapes the
   workspace root or is not a plain relative path. *)
let normalize_project_path ~base location =
  if location = "" || location.[0] = '/' || location.[0] = '~' ||
     String.contains location '\000' then None
  else
    let base_parts = List.filter (fun part -> part <> "" && part <> ".")
      (String.split_on_char '/' base) in
    let rec combine acc = function
      | [] -> Some acc
      | part :: rest when part = "" || part = "." -> combine acc rest
      | ".." :: rest ->
          (match acc with
           | _ :: tail -> combine tail rest
           | [] -> None)
      | part :: rest -> combine (part :: acc) rest in
    match combine (List.rev base_parts)
            (String.split_on_char '/' location) with
    | Some parts -> Some (String.concat "/" (List.rev parts))
    | None -> None

(* Fingerprint the selected bundle manifest, every bounded regular shared
   scheme file under its xcshareddata/xcschemes directory, and the manifests
   plus shared schemes of directly referenced .xcodeproj bundles. A missing,
   escaping, unreadable, non-regular or oversized input degrades the result to
   [Unresolved] so it can never back verified choices. *)
let fingerprint ~root ~bundle =
  let limit = Workspace_path.max_write_bytes in
  let inputs = ref [] and unresolved = ref false in
  let degrade () = unresolved := true in
  let add_input label contents =
    inputs := (label, Digestif.SHA256.(to_hex (digest_string contents)))
      :: !inputs;
    if List.length !inputs > max_fingerprint_inputs then degrade () in
  let read_file relative =
    try
      let absolute = Workspace_path.checked_path root relative in
      let stat = Unix.lstat absolute in
      if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_size > limit then None
      else Some (Workspace_path.read_bounded absolute limit)
    with Unix.Unix_error _ | Workspace_path.Error _ | Sys_error _ -> None in
  let add_file label relative =
    match read_file relative with
    | Some contents -> add_input label contents
    | None -> degrade () in
  let rec add_shared_schemes owner =
    let directory = Filename.concat owner "xcshareddata/xcschemes" in
    let names =
      try
        let absolute = Workspace_path.checked_path root directory in
        if (Unix.lstat absolute).Unix.st_kind = Unix.S_DIR then
          Some (Sys.readdir absolute)
        else None
      with Unix.Unix_error (Unix.ENOENT, _, _) -> Some [||]
         | Unix.Unix_error _ | Workspace_path.Error _ | Sys_error _ -> None in
    (match names with
     | None -> degrade ()
     | Some names ->
         Array.sort String.compare names;
         Array.iter (fun name ->
           if String.length name > String.length ".xcscheme" &&
              Filename.check_suffix name ".xcscheme" then
             add_file ("scheme:" ^ directory ^ "/" ^ name)
               (Filename.concat directory name)) names)
  and add_referenced location =
    match normalize_project_path ~base:(Filename.dirname bundle) location with
    | Some member_bundle ->
        add_file ("project:" ^ member_bundle ^ "/project.pbxproj")
          (Filename.concat member_bundle "project.pbxproj");
        add_shared_schemes member_bundle
    | None -> degrade () in
  let manifest = if Filename.check_suffix bundle ".xcworkspace" then
      Filename.concat bundle "contents.xcworkspacedata"
    else Filename.concat bundle "project.pbxproj" in
  (match read_file manifest with
   | Some contents ->
       add_input ("manifest:" ^ manifest) contents;
       add_shared_schemes bundle;
       if Filename.check_suffix bundle ".xcworkspace" then
         List.iter (fun location ->
           match String.split_on_char ':' location with
           | ("group" | "container") :: (_ :: _ as rest) ->
               add_referenced (String.concat ":" rest)
           | _ ->
               (* absolute:, developer: or otherwise unresolvable .xcodeproj
                  locations cannot be fingerprinted inside the workspace. *)
               degrade ()) (workspace_project_locations contents)
       else
         List.iter add_referenced (pbxproj_project_locations contents)
   | None -> degrade ());
  if !unresolved then Unresolved
  else
    let canonical = !inputs |>
      List.sort (fun (a, _) (b, _) -> String.compare a b) |>
      List.map (fun (label, hash) -> label ^ "\n" ^ hash) |>
      String.concat "\n" in
    Complete Digestif.SHA256.(to_hex (digest_string canonical))
