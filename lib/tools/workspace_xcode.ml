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
    if String.starts_with ~prefix:"Available destinations" line then
      available := true
    else if String.starts_with ~prefix:"Ineligible destinations" line then
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
