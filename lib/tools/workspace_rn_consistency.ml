(* Read-only React Native/Expo consistency evidence: literal JS/TS bridge
   references are paired with observed native declarations inside the selected
   package's ios/ or android/ host root, and supported static Expo app.json
   values are compared with literal native configuration. Nothing is
   executed, evaluated, installed or prebuilt; dynamic or absent evidence
   stays unknown. The caller supplies the already bounded, workspace-scoped
   inventory (files and directories observed by the mobile walk) so this
   module never widens the scan. *)

exception Error of string

let fail message = raise (Error message)

let starts_with text prefix =
  let n = String.length text and m = String.length prefix in
  n >= m && String.sub text 0 m = prefix

let ends_with = Filename.check_suffix

let contains_sub text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub text i m = fragment || loop (i + 1)) in
  loop 0

let relative_label path = if path = "" then "." else path

let is_module_name text =
  let valid c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
    (c >= '0' && c <= '9') || c = '_' || c = '$' || c = '.' in
  let rec all i = i >= String.length text || (valid text.[i] && all (i + 1)) in
  String.length text > 0 && all 0 &&
  not (text.[0] >= '0' && text.[0] <= '9')

(* File kinds this checker can read. app.config.* is classified before the
   generic .js/.ts extensions and .kts before .ts so Kotlin scripts and
   executable Expo configuration are never mistaken for JS source. *)
type kind = Js | Ios_native | Android_native | Gradle | Manifest
  | Plist | Pbxproj | Strings | Static_config | Dynamic_config | Other

let classify relative =
  let name = Filename.basename relative in
  if name = "app.json" || name = "app.config.json" then Static_config
  else if List.exists (ends_with name)
      [".config.js"; ".config.ts"; ".config.mjs"; ".config.cjs";
       ".config.mts"; ".config.cts"] then Dynamic_config
  else if ends_with relative ".kts" then Gradle
  else if List.exists (ends_with relative)
      [".js"; ".jsx"; ".ts"; ".tsx"; ".mjs"; ".cjs"; ".mts"; ".cts"] then Js
  else if ends_with relative ".m" || ends_with relative ".mm" then Ios_native
  else if ends_with relative ".java" || ends_with relative ".kt" then
    Android_native
  else if ends_with relative ".gradle" then Gradle
  else if name = "AndroidManifest.xml" then Manifest
  else if name = "strings.xml" then Strings
  else if ends_with relative ".plist" then Plist
  else if ends_with relative ".pbxproj" then Pbxproj
  else Other

let relevant relative = classify relative <> Other

(* Str match state is global, so captures must be taken inside the search
   loop; each result pairs the match start with the requested groups. *)
let search_all re groups text =
  let rec loop start acc =
    match (try Some (Str.search_forward re text start)
           with Not_found | Invalid_argument _ -> None) with
    | Some index ->
        let captures = List.map (fun n ->
          try Some (Str.matched_group n text)
          with Not_found | Invalid_argument _ -> None) groups in
        loop (index + 1) ((index, captures) :: acc)
    | None -> List.rev acc in
  loop 0 []

let positions re text = List.map fst (search_all re [] text)

(* Literal and dynamic JS/TS bridge references. *)
let re_js_literal = Str.regexp
  {|\bNativeModules\.\([A-Za-z_$][A-Za-z0-9_$]*\)\|\bTurboModuleRegistry\.\(get\|getEnforcing\)([ \t]*['"]\([^'"]*\)['"]\|\brequireNativeComponent([ \t]*['"]\([^'"]*\)['"]|}

let re_js_dynamic = Str.regexp
  {|\bNativeModules[ \t]*\[\|\bTurboModuleRegistry\.\(get\|getEnforcing\)([ \t]*[^'"\ \t)]\|\brequireNativeComponent([ \t]*[^'"\ \t)]|}
(* Bounded comment stripping for the C-family and HTML comment forms used by
   JS/TS, Objective-C, Java/Kotlin, Gradle and the XML/property files we scan.
   Commented-out declarations must not become evidence; quoted contents are
   preserved, and Kotlin raw strings may conservatively under-report. *)
let strip_comments text =
  let n = String.length text in
  let out = Buffer.create n in
  let blank i =
    Buffer.add_char out (if i < n && text.[i] = '\n' then '\n' else ' ') in
  let rec loop i quote block_comment =
    if i >= n then ()
    else if block_comment then (
      let close =
        if i + 1 < n && text.[i] = '*' && text.[i + 1] = '/' then Some 2
        else if i + 2 < n && text.[i] = '-' && text.[i + 1] = '-' &&
                text.[i + 2] = '>' then Some 3
        else None in
      match close with
      | Some width ->
          for j = i to i + width - 1 do blank j done;
          loop (i + width) quote false
      | None -> blank i; loop (i + 1) quote true)
    else match quote with
      | Some q ->
          Buffer.add_char out text.[i];
          if text.[i] = '\\' && i + 1 < n then (
            Buffer.add_char out text.[i + 1]; loop (i + 2) (Some q) false)
          else loop (i + 1) (if text.[i] = q then None else Some q) false
      | None ->
          if text.[i] = '"' || text.[i] = '\'' || text.[i] = '`' then (
            Buffer.add_char out text.[i]; loop (i + 1) (Some text.[i]) false)
          else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '/' then (
            blank i; blank (i + 1); line_comment (i + 2))
          else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '*' then (
            blank i; blank (i + 1); loop (i + 2) None true)
          else if i + 3 < n && text.[i] = '<' && text.[i + 1] = '!' &&
                  text.[i + 2] = '-' && text.[i + 3] = '-' then (
            blank i; blank (i + 1); blank (i + 2); blank (i + 3);
            loop (i + 4) None true)
          else (
            Buffer.add_char out text.[i]; loop (i + 1) None false)
  and line_comment i =
    if i >= n || text.[i] = '\n' then loop i None false
    else (blank i; line_comment (i + 1)) in
  loop 0 None false;
  Buffer.contents out


let js_references text =
  let dynamic = positions re_js_dynamic text <> [] in
  let literal = search_all re_js_literal [1; 3; 4] text |>
    List.filter_map (fun (index, captures) ->
      let name = match captures with
        | [Some n; _; _] -> n | [_; Some n; _] -> n
        | [_; _; Some n] -> n | _ -> "" in
      if name = "" then None else Some (index, name)) in
  literal, dynamic

(* iOS bridge declarations: RCT_EXPORT_MODULE / RCT_EXPORT_PRECISE_MODULE /
   RCT_EXTERN_MODULE use the first argument (or bare class name) as the
   JS-visible module name; an empty argument defers to the nearest preceding
   @implementation name, and PRECISE's second argument is only the queue. *)
let re_ios_export = Str.regexp
  {|RCT_EXPORT_MODULE(\([^)]*\))\|RCT_EXPORT_PRECISE_MODULE(\([^)]*\))\|RCT_EXTERN_MODULE(\([^)]*\))|}

let re_ios_impl = Str.regexp
  {|@implementation[ \t]+\([A-Za-z_][A-Za-z0-9_]*\)|}

let quoted_argument token =
  let token = String.trim token in
  if String.length token >= 2 &&
     (token.[0] = '"' || token.[0] = '\'') &&
     token.[String.length token - 1] = token.[0] &&
     is_module_name (String.sub token 1 (String.length token - 2))
  then `Literal (String.sub token 1 (String.length token - 2))
  else `Dynamic

let module_argument token =
  let token = String.trim token in
  if token = "" then `Inferred
  else match quoted_argument token with
    | `Literal name -> `Literal name
    | `Dynamic ->
        (* The conventional RCT_EXPORT_MODULE(Name) form. *)
        if is_module_name token then `Literal token else `Dynamic

let ios_declarations text =
  let impls = search_all re_ios_impl [1] text |> List.map (function
    | index, [Some name] -> index, name
    | _ -> assert false) in
  search_all re_ios_export [1; 2; 3] text |>
  List.map (fun (index, captures) ->
    let argument = match captures with
      | [Some arg; _; _] | [_; Some arg; _] | [_; _; Some arg] -> arg
      | _ -> "" in
    let parts = List.map String.trim
      (String.split_on_char ',' argument) in
    (* The first argument carries the JS-visible module name for all three
       macros; PRECISE's second argument is only the method queue. *)
    let token = match parts with head :: _ -> head | [] -> "" in
    match module_argument token with
    | `Literal name -> index, `Literal name
    | `Dynamic -> index, `Dynamic
    | `Inferred ->
        match List.filter (fun (at, _) -> at < index) impls |> List.rev with
        | (_, name) :: _ -> index, `Inferred name
        | [] -> index, `Dynamic)

(* Android bridge declarations: @ReactModule(name = "...") with a quoted name
   only (a bare identifier is a constant reference and stays dynamic),
   ReactModuleInfo("...") and getName() returning a literal. *)
let re_reactmodule = Str.regexp
  {|@ReactModule[ \t\n]*(\([^)]*\))|}

let re_module_info = Str.regexp
  {|\bReactModuleInfo[ \t\n]*([ \t\n]*['"]\([^'"]*\)['"]|}

let re_getname_literal = Str.regexp
  {|\bgetName[ \t\n]*([ \t\n]*)[ \t\n]*\(:[ \t\n]*[A-Za-z_][A-Za-z0-9_<>?]*[ \t\n]*\)?=[ \t\n]*['"]\([^'"]*\)['"]\|\bgetName[ \t\n]*([ \t\n]*)[ \t\n]*{[^{}]*return[ \t\n]+['"]\([^'"]*\)['"]|}

let re_getname_dynamic = Str.regexp
  {|\bgetName[ \t\n]*([ \t\n]*)[ \t\n]*\(:[ \t\n]*[A-Za-z_][A-Za-z0-9_<>?]*[ \t\n]*\)?=[ \t\n]*[^'" \t\n]\|\bgetName[ \t\n]*([ \t\n]*)[ \t\n]*{[^{}]*return[ \t\n]+[^'" \t\n]|}

let android_declarations text =
  let literals =
    (search_all re_reactmodule [1] text |> List.map (function
       | index, [Some argument] ->
           let token = match String.split_on_char '=' argument with
             | [_key; token] -> String.trim token | _ -> "" in
           (match quoted_argument token with
            | `Literal name -> index, `Literal name
            | `Dynamic -> index, `Dynamic)
       | index, _ -> index, `Dynamic)) @
    (search_all re_module_info [1] text |> List.map (function
       | index, [Some name] -> index, `Literal name
       | index, _ -> index, `Dynamic)) @
    (search_all re_getname_literal [2; 3] text |> List.map (function
       | index, [Some name; _] | index, [_; Some name] ->
           index, `Literal name
       | index, _ -> index, `Dynamic)) in
  let dynamic = positions re_getname_dynamic text |> List.filter (fun index ->
    not (List.exists (fun (at, _) -> at = index) literals)) in
  List.sort (fun (a, _) (b, _) -> compare a b)
    (literals @ List.map (fun index -> index, `Dynamic) dynamic)

(* Native configuration values; Gradle substitutions stay dynamic. *)
let dynamic_value value =
  contains_sub value "$" || contains_sub value "@{"

(* A literal or parenthesised Gradle/Groovy/KTS string assignment, or the same
   assignment to any other token (dynamic). Returns the first occurrence. *)
let gradle_assignment key text =
  let quoted = Str.regexp ("\\b" ^ key ^
    {|[ \t\n]*\(=[ \t\n]*\|([ \t\n]*\)?['"]\([^'"]*\)['"]|}) in
  let bare = Str.regexp ("\\b" ^ key ^
    {|[ \t\n]*\(=[ \t\n]*\|([ \t\n]*\)[^'" \t\n}][^ \t\n})]*|}) in
  match (try Some (Str.search_forward quoted text 0)
         with Not_found | Invalid_argument _ -> None) with
  | Some index ->
      let value = Str.matched_group 2 text in
      Some (index, if dynamic_value value then `Dynamic else `Literal value)
  | None ->
      match (try Some (Str.search_forward bare text 0)
             with Not_found | Invalid_argument _ -> None) with
      | Some index -> Some (index, `Dynamic)
      | None -> None

let re_version_code_literal = Str.regexp
  {|\bversionCode[ \t\n]*\(=[ \t\n]*\|([ \t\n]*\)?\([0-9]+\)|}

let re_version_code_other = Str.regexp
  {|\bversionCode[ \t\n]*\(=[ \t\n]*\|([ \t\n]*\)[^'" \t\n}0-9][^ \t\n})]*|}

let gradle_version_code text =
  match (try Some (Str.search_forward re_version_code_literal text 0)
         with Not_found | Invalid_argument _ -> None) with
  | Some index -> Some (index, `Literal (Str.matched_group 2 text))
  | None ->
      match (try Some (Str.search_forward re_version_code_other text 0)
             with Not_found | Invalid_argument _ -> None) with
      | Some index -> Some (index, `Dynamic)
      | None -> None

let re_manifest_package = Str.regexp
  {|<manifest\b[^>]*\bpackage[ \t\n]*=[ \t\n]*"\([^"]*\)"|}

let manifest_package text =
  match (try Some (Str.search_forward re_manifest_package text 0)
         with Not_found | Invalid_argument _ -> None) with
  | Some index ->
      let value = Str.matched_group 1 text in
      Some (index, if dynamic_value value then `Dynamic else `Literal value)
  | None -> None

let pbxproj_setting key text =
  let re = Str.regexp
    ("\\b" ^ key ^ "[ \t]*=[ \t]*\\([^;\n]*\\)[ \t]*;") in
  match (try Some (Str.search_forward re text 0)
         with Not_found | Invalid_argument _ -> None) with
  | Some index ->
      let value = String.trim (Str.matched_group 1 text) in
      Some (index, if dynamic_value value then `Dynamic else `Literal value)
  | None -> None

let plist_string key text =
  let re = Str.regexp
    ("<key>" ^ Str.quote key ^ "</key>[ \t\n]*<string>\\([^<]*\\)</string>") in
  match (try Some (Str.search_forward re text 0)
         with Not_found | Invalid_argument _ -> None) with
  | Some index -> Some (index, `Literal (Str.matched_group 1 text))
  | None -> None

let re_app_name = Str.regexp
  {|<string\b[^>]*\bname[ \t\n]*=[ \t\n]*"app_name"[^>]*>\([^<]*\)</string>|}

let strings_app_name text =
  match (try Some (Str.search_forward re_app_name text 0)
         with Not_found | Invalid_argument _ -> None) with
  | Some index -> Some (index, `Literal (Str.matched_group 1 text))
  | None -> None

(* Per-key native declaration lookups over the observed host-root files.
   [read] maps a workspace-relative path to its bounded contents. *)
type finding = string * [ `Literal of string | `Dynamic ]

let find_with files expected read_setting ~read : finding list =
  List.filter_map (fun path ->
    if classify path <> expected then None
    else match read path with
      | None -> None
      | Some text ->
          Option.map (fun (_, decl) -> path, decl) (read_setting text))
    files

let ios_bundle_identifier ~read files =
  find_with files Pbxproj
    (pbxproj_setting "PRODUCT_BUNDLE_IDENTIFIER") ~read

let ios_version ~read files =
  find_with files Plist
    (plist_string "CFBundleShortVersionString") ~read @
  find_with files Pbxproj
    (pbxproj_setting "MARKETING_VERSION") ~read

let ios_display_name ~read files =
  find_with files Plist (plist_string "CFBundleDisplayName") ~read

let android_package ~read files =
  let from_gradle =
    List.filter_map (fun path ->
      match classify path with
      | Gradle ->
          (match read path with
           | Some text ->
               Option.map (fun (_, decl) -> path, decl)
                 (gradle_assignment "applicationId" text)
           | None -> None)
      | _ -> None) files in
  let from_manifest =
    List.filter_map (fun path ->
      match classify path with
      | Manifest ->
          (match read path with
           | Some text ->
               Option.map (fun (_, decl) -> path, decl)
                 (manifest_package text)
           | None -> None)
      | _ -> None) files in
  from_gradle @ from_manifest

let android_version ~read files =
  find_with files Gradle (gradle_assignment "versionName") ~read

let android_version_code ~read files =
  find_with files Gradle gradle_version_code ~read

let android_app_name ~read files =
  find_with files Strings strings_app_name ~read

let max_scanned_files = 256
let max_source_bytes = 65_536
let max_report_names = 20

(* Supported static config key sets. Expo app.json wraps values in "expo" or
   keeps them flat; a bare React Native app.json has only name/displayName,
   where name is the registered app name and displayName is the UI label, so
   only displayName may be compared with the native display name. *)
let expo_key_names platform =
  if platform = "ios" then
    ["ios.bundleIdentifier", ios_bundle_identifier;
     "version", ios_version;
     "name", ios_display_name]
  else
    ["android.package", android_package;
     "version", android_version;
     "android.versionCode", android_version_code;
     "name", android_app_name]

let react_native_key_names platform =
  if platform = "ios" then
    ["displayName", ios_display_name]
  else
    ["displayName", android_app_name]

let compare_static ~emit ~cap ~read ~platform ~config ~fields ~host_files
    ~key_names =
  let value_of key =
    let literal = function
      | `String value -> Some (`Literal value)
      | `Int n -> Some (`Literal (string_of_int n))
      | _ -> Some `Dynamic in
    match String.split_on_char '.' key with
    | [section; item] ->
        (match List.assoc_opt section fields with
         | Some (`Assoc inner) ->
             (match List.assoc_opt item inner with
              | Some v -> literal v
              | None -> None)
         | Some _ -> Some `Dynamic
         | None -> None)
    | _ ->
        (match List.assoc_opt key fields with
         | Some v -> literal v
         | None -> None) in
  let compared =
    List.filter_map (fun (key, reader) ->
      match value_of key with
      | None -> None
      | Some `Dynamic -> Some (`Unresolved key)
      | Some (`Literal value) ->
          Some (`Compare (key, value, reader ~read host_files)))
      (key_names platform) in
  if compared = [] then
    emit ("No supported static keys for platform " ^ platform ^ " in " ^
      config ^ "; nothing compared.")
  else
    List.iter (function
      | `Unresolved key ->
          emit ("Expo key " ^ key ^ " in " ^ config ^
            " is not a static string; consistency unresolved.")
      | `Compare (key, value, findings) ->
          let literals = List.filter_map (fun (file, decl) ->
            match decl with
            | `Literal v -> Some (file, v)
            | `Dynamic -> None) findings in
          let differing = List.filter (fun (_, v) -> v <> value) literals
          and matching = List.filter (fun (_, v) -> v = value) literals in
          let dynamics = List.filter (fun (_, decl) -> decl = `Dynamic)
            findings in
          List.iter (fun (file, v) ->
            emit (Printf.sprintf
              "Expo config mismatch: %s declares %s = \"%s\" but %s declares \"%s\""
              config key value file v)) differing;
          if dynamics <> [] then
            emit ("Expo key " ^ key ^ " has dynamic or unresolved " ^
              platform ^ " declarations: " ^
              cap (List.map fst dynamics) Fun.id);
          if differing = [] && dynamics = [] && matching <> [] then
            emit ("Expo key " ^ key ^
              " consistent with native declarations: " ^
              cap (List.map fst matching) Fun.id);
          if findings = [] then
            emit ("Expo key " ^ key ^ " has no observed " ^ platform ^
              " declaration; consistency unknown.")) compared
let report ~root ~subroot ~platform ~files ~directories
    ~package_dirs ~expo ~test_hint =
  if platform <> "ios" && platform <> "android" then
    fail "platform must be ios or android";
  if subroot <> "" &&
     (not (Filename.is_relative subroot) ||
      String.contains subroot '\000' ||
      List.exists (fun part -> part = "" || part = "." || part = "..")
        (String.split_on_char '/' subroot)) then
    fail "invalid package root";
  let root = Workspace_path.root_path root in
  let prefix = if subroot = "" then "" else subroot ^ "/" in
  let under path = subroot = "" || starts_with path prefix in
  let nested path =
    List.exists (fun directory ->
      directory <> "" && directory <> subroot &&
      starts_with path (directory ^ "/")) package_dirs in
  let scoped = List.filter (fun path -> under path && not (nested path))
    (List.sort_uniq String.compare files) in
  let in_host path host = starts_with path (prefix ^ host ^ "/") in
  let host_present = Hashtbl.mem directories (prefix ^ platform) in
  let scanned = ref 0 and overflow = ref false and skipped = ref [] in
  let read path =
    incr scanned;
    if !scanned > max_scanned_files then (overflow := true; None)
    else
      match (try Some (Workspace_path.checked_path root path)
             with Workspace_path.Error _ -> None) with
      | None -> None
      | Some absolute ->

          (try
             let stat = Unix.lstat absolute in
             if stat.Unix.st_kind <> Unix.S_REG then None
             else if stat.Unix.st_size > max_source_bytes then (
               if not (List.mem path !skipped) then
                 skipped := path :: !skipped;
               None)
             else
               let text = Workspace_path.read_bounded absolute
                 max_source_bytes in
               (* JSON has no comment syntax; stripping would corrupt string
                  values containing comment-looking literals. *)
               if classify path = Static_config then Some text
               else Some (strip_comments text)
           with Unix.Unix_error _ | Sys_error _ | Workspace_path.Error _ ->
             None) in
  let lines = ref [] in
  let emit line = lines := line :: !lines in
  let cap items render =
    let shown = List.filteri (fun index _ -> index < max_report_names) items in
    String.concat "; " (List.map render shown) ^
    (if List.length items > List.length shown then
       Printf.sprintf "; +%d more" (List.length items - List.length shown)
     else "") in
  (* -- literal JS/TS bridge references vs observed native declarations -- *)
  let js_refs = ref [] and js_dynamic = ref [] in
  List.iter (fun path ->
    match classify path with
    | Js when not (in_host path "ios") && not (in_host path "android") ->
        (match read path with
         | None -> ()
         | Some text ->
             let literal, dynamic = js_references text in
             List.iter (fun (_, name) ->
               if not (List.exists (fun (n, _) -> n = name) !js_refs) then
                 js_refs := !js_refs @ [(name, path)]) literal;
             if dynamic && not (List.mem path !js_dynamic) then
               js_dynamic := !js_dynamic @ [path])
    | _ -> ()) scoped;
  let declarations = ref [] in
  if host_present then
    List.iter (fun path ->
      match classify path with
      | Ios_native when platform = "ios" && in_host path "ios" ->
          (match read path with
           | Some text ->
               List.iter (fun (_, decl) ->
                 declarations := !declarations @ [(path, decl)])
                 (ios_declarations text)
           | None -> ())
      | Android_native when platform = "android" && in_host path "android" ->
          (match read path with
           | Some text ->
               List.iter (fun (_, decl) ->
                 declarations := !declarations @ [(path, decl)])
                 (android_declarations text)
           | None -> ())
      | _ -> ()) scoped;
  let literal_declarations =
    List.filter_map (fun (path, decl) ->
      match decl with
      | `Literal name | `Inferred name -> Some (name, path)
      | `Dynamic -> None) !declarations in
  let native_names =
    List.fold_left (fun acc (name, _) ->
      if List.mem name acc then acc else acc @ [name]) []
      literal_declarations in
  let literal_js = !js_refs in
  let paired = List.filter (fun (name, _) -> List.mem name native_names)
    literal_js in
  let unmatched_js = List.filter (fun (name, _) ->
    not (List.mem name native_names)) literal_js in
  let unmatched_native =
    List.fold_left (fun acc (name, native_file) ->
      if List.exists (fun (n, _) -> n = name) acc then acc
      else acc @ [(name, native_file)]) []
      (List.filter (fun (name, _) ->
        not (List.exists (fun (js, _) -> js = name) literal_js))
        literal_declarations) in
  let unresolved_native = List.filter (fun (_, decl) ->
    decl = `Dynamic) !declarations in
  if host_present then (
    emit (Printf.sprintf
      "Native bridge declarations (%s): %d literal JS reference%s, %d observed native declaration%s."
      platform (List.length literal_js)
      (if List.length literal_js = 1 then "" else "s")
      (List.length literal_declarations)
      (if List.length literal_declarations = 1 then "" else "s"));
    List.iter (fun (name, js_file) ->
      let files =
        List.fold_left (fun acc (decl_name, decl_file) ->
          if decl_name = name && not (List.mem decl_file acc) then
            acc @ [decl_file]
          else acc) [] literal_declarations in
      emit (Printf.sprintf "Paired module %s: %s <-> %s" name js_file
        (String.concat ", " files))) paired;
    List.iter (fun (name, js_file) ->
      emit (Printf.sprintf
        "Mismatch: JS native-module reference %s in %s has no %s declaration under %s%s"
        name js_file platform (prefix ^ platform)
        (if native_names = [] then ""
         else "; observed native declarations: " ^
           cap native_names Fun.id))) unmatched_js;
    List.iter (fun (name, native_file) ->
      emit (Printf.sprintf
        "Mismatch: %s declaration %s in %s is not referenced literally by JS/TS under %s"
        platform name native_file (relative_label subroot)))
      unmatched_native;
    if !js_dynamic <> [] then
      emit ("Unresolved dynamic JS/TS native-module references: " ^
        cap !js_dynamic Fun.id);
    if unresolved_native <> [] then
      emit ("Unresolved dynamic or non-literal native module declarations: " ^
        cap (List.map fst unresolved_native) Fun.id))
  else (
    emit (Printf.sprintf
      "No %s native root under %s%s." platform (relative_label subroot)
      (if expo then
         "; managed Expo without prebuilt ios/ or android/ directories - native declarations and configuration values remain unknown"
       else "; native declarations remain unknown"));
    if literal_js <> [] then
      emit ("Literal JS native-module references without a " ^ platform ^
        " host root (unknown): " ^
        cap literal_js (fun (n, p) -> n ^ " in " ^ p)));
  let host_files = List.filter (fun path -> in_host path platform) scoped in
  (* -- supported static Expo configuration vs native declarations -- *)
  if expo then (
    let package_dir = if subroot = "" then "." else subroot in
    let dynamic_config =
      List.find_opt (fun path -> classify path = Dynamic_config &&
        Filename.dirname path = package_dir) scoped in
    let static_config =
      List.find_opt (fun path -> classify path = Static_config &&
        Filename.dirname path = package_dir) scoped in
    match dynamic_config with
    | Some config ->
        emit ("Expo executable configuration " ^ config ^
          " is never evaluated; Expo configuration consistency remains unresolved.")
    | None ->
        (match static_config with
         | None ->
             emit ("No static Expo configuration (app.json/app.config.json) under " ^
               relative_label subroot ^ "; nothing compared.")
         | Some config ->
             match read config with
             | None ->
                 emit ("Static Expo configuration " ^ config ^
                   " is unreadable or oversized; consistency remains unresolved.")
             | Some text ->
                 (match (try Some (Yojson.Basic.from_string text)
                         with Yojson.Json_error _ -> None) with
                  | None ->
                      emit ("Static Expo configuration " ^ config ^
                        " is invalid JSON; consistency remains unresolved.")
                  | Some json ->
                      (* Expo app.json may wrap values in "expo"; a bare
                         React Native app.json keeps only name/displayName,
                         where name is the registered app name, not the
                         display name. *)
                      (match json with
                       | `Assoc top ->
                           (match List.assoc_opt "expo" top with
                            | Some (`Assoc inner) ->
                                compare_static ~emit ~cap ~read ~platform
                                  ~config ~fields:inner ~host_files
                                  ~key_names:expo_key_names
                            | Some _ ->
                                emit ("Expo configuration in " ^ config ^
                                  " is not an object; consistency remains unresolved.")
                            | None ->
                                if List.mem_assoc "displayName" top then
                                  compare_static ~emit ~cap ~read ~platform
                                    ~config ~fields:top ~host_files
                                    ~key_names:react_native_key_names
                                else
                                  compare_static ~emit ~cap ~read ~platform
                                    ~config ~fields:top ~host_files
                                    ~key_names:expo_key_names)
                       | _ ->
                           emit ("Expo configuration in " ^ config ^
                             " is not an object; consistency remains unresolved.")))));
  if !overflow then
    emit ("Bounded consistency scan reached its file limit (" ^
      string_of_int max_scanned_files ^
      "); additional evidence remains unknown.");
  if !skipped <> [] then
    emit ("Skipped oversized files (exceed " ^
      string_of_int max_source_bytes ^ " bytes): " ^
      cap (List.rev !skipped) Fun.id);
  emit test_hint;
  List.rev !lines
