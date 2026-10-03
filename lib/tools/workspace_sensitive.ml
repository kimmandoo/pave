(* M21a + M22a: bounded proposed-diff classifiers for iOS and Android
   sensitive workspace changes. The classifier inspects only the
   caller-provided before/after contents of one workspace-relative file:
   it never touches the filesystem, keychain or keystore material, and
   never displays secret signing values. Literal supported changes cite
   the exact file, setting and effect; executable or computed
   configuration that cannot be verified statically stays unresolved
   instead of being promoted to a trusted value. *)

exception Error of string

let fail message = raise (Error message)

type platform = Ios | Android

type change = Added | Removed | Changed

type finding = {
  platform : platform;
  category : string;
  setting : string;
  change : change;
  detail : string;
}

type report = {
  file : string;
  platform : platform;
  findings : finding list;
  unresolved : string list;
}

(* Ordinary: not a guarded mobile file, or no sensitive change detected.
   Literal: every proposed sensitive change resolved to exact values.
   Unresolved: a guarded file changed but some construct is dynamic,
   malformed or secret, so the exact change cannot be verified. *)
type verdict =
  | Ordinary
  | Literal of report
  | Unresolved of report

let max_input_bytes = 1_048_576
let max_findings = 64
let max_display = 128
let max_tag_scan = 8_192
let max_scalar_scan = 4_096
let max_raw_repr = 4_096

let string_of_platform = function
  | Ios -> "iOS"
  | Android -> "Android"

let string_of_change = function
  | Added -> "added"
  | Removed -> "removed"
  | Changed -> "changed"

let printable value =
  value <> "" && String.for_all (fun c -> Char.code c >= 32 && Char.code c < 127) value

let clip ?(limit = max_raw_repr) value =
  if String.length value > limit then String.sub value 0 limit else value

let shown ?(limit = max_display) value =
  if printable value then clip ~limit value
  else if String.length (String.trim value) = 0 then "(empty)"
  else "(non-printable value)"

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let trim value =
  let length = String.length value in
  let first = ref 0 and last = ref (length - 1) in
  while !first < length && is_space value.[!first] do incr first done;
  while !last >= !first && is_space value.[!last] do decr last done;
  if !last < !first then "" else String.sub value !first (!last - !first + 1)

let ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let ident_string value =
  value <> "" &&
  (let c = value.[0] in
   (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_') &&
  String.for_all ident_char value

let find_from haystack start needle =
  let n = String.length needle and m = String.length haystack in
  let rec scan i =
    if i + n > m then -1
    else if String.sub haystack i n = needle then i
    else scan (i + 1)
  in
  if start < 0 then -1 else scan start

let contains haystack needle = find_from haystack 0 needle >= 0

let has_interpolation value =
  contains value "$(" || contains value "${" || contains value "#{"

let starts_at haystack i needle =
  let n = String.length needle in
  i + n <= String.length haystack && String.sub haystack i n = needle


let lines_of text = String.split_on_char '\n' text

(* Extracted comparable representation of one sensitive value.
   R_dynamic keeps a bounded raw slice so unchanged dynamic constructs
   compare equal and only *changed* dynamic content stays unresolved. *)
type repr =
  | R_literal of string
  | R_dynamic of string

let repr_of text =
  if has_interpolation text then R_dynamic (clip text) else R_literal text

let is_dynamic = function R_dynamic _ -> true | R_literal _ -> false

let repr_text = function R_literal v | R_dynamic v -> v

(* ---------- path and file-kind selection ---------- *)

type kind =
  | Entitlements
  | Info_plist
  | Export_options
  | Xcode_project
  | Xcconfig
  | Podfile
  | Fastlane
  | Signing_material of platform
  | Android_manifest
  | Android_gradle
  | Android_properties of bool (* true: every key is signing-secret *)

let checked_relative path =
  if path = "" || String.contains path '\000' ||
     not (Filename.is_relative path) then
    fail "path must be a nonempty workspace-relative path";
  let parts = String.split_on_char '/' path in
  if List.exists (fun part -> part = "" || part = "..") parts then
    fail "path must not contain empty or '..' segments"

let ends_with value suffix =
  let n = String.length suffix and m = String.length value in
  m >= n && String.sub value (m - n) n = suffix

let kind_of path =
  let base = Filename.basename path in
  let lower = String.lowercase_ascii base in
  if ends_with base ".entitlements" then Some Entitlements
  else if base = "project.pbxproj" then Some Xcode_project
  else if base = "Info.plist" ||
          (ends_with base "Info.plist" && String.length base > 10 &&
           (base.[String.length base - 11] = '-' ||
            base.[String.length base - 11] = '_')) then Some Info_plist
  else if lower = "exportoptions.plist" then Some Export_options
  else if ends_with base ".xcconfig" then Some Xcconfig
  else if base = "Podfile" then Some Podfile
  else if List.mem base ["Fastfile"; "Matchfile"; "Appfile"; "Deliverfile"]
    then Some Fastlane
  else if base = "AndroidManifest.xml" then Some Android_manifest
  else if base = "build.gradle" || base = "build.gradle.kts"
    then Some Android_gradle
  else if base = "key.properties" || base = "keystore.properties"
    then Some (Android_properties true)
  else if base = "gradle.properties" || base = "local.properties"
    then Some (Android_properties false)
  else if ends_with lower ".keystore" || ends_with lower ".jks" ||
          (ends_with lower ".p12" &&
           (contains (String.lowercase_ascii path) "android" ||
            contains lower "keystore"))
    then Some (Signing_material Android)
  else if List.exists (ends_with base)
      [".mobileprovision"; ".provisionprofile"; ".p8"; ".cer"] ||
          ends_with lower ".p12"
    then Some (Signing_material Ios)
  else None

(* ---------- sensitive key vocabularies ---------- *)

let ios_assignment_category key =
  let base =
    match String.index_opt key '[' with
    | Some i -> String.sub key 0 i
    | None -> key in
  if List.mem base
      ["PRODUCT_BUNDLE_IDENTIFIER"; "PRODUCT_BUNDLE_PACKAGE_TYPE";
       "MARKETING_VERSION"; "CURRENT_PROJECT_VERSION"] then Some "bundle"
  else if ends_with base "_DEPLOYMENT_TARGET" ||
          List.mem base
            ["TARGETED_DEVICE_FAMILY"; "SUPPORTED_PLATFORMS"; "SDKROOT";
             "ARCHS"; "VALID_ARCHS"; "EXCLUDED_ARCHS"] then Some "deployment"
  else if starts_at base 0 "CODE_SIGN" || starts_at base 0 "CODE_SIGNING" ||
          starts_at base 0 "PROVISIONING_PROFILE" ||
          base = "DEVELOPMENT_TEAM" then Some "signing"
  else None

let ios_needles =
  ["PRODUCT_BUNDLE_IDENTIFIER"; "PRODUCT_BUNDLE_PACKAGE_TYPE";
   "DEPLOYMENT_TARGET"; "TARGETED_DEVICE_FAMILY"; "SUPPORTED_PLATFORMS";
   "SDKROOT"; "CODE_SIGN"; "CODE_SIGNING"; "PROVISIONING_PROFILE";
   "DEVELOPMENT_TEAM"; "MARKETING_VERSION"; "CURRENT_PROJECT_VERSION";
   "ARCHS"]

let info_plist_category = function
  | "CFBundleIdentifier" | "CFBundleVersion" |
    "CFBundleShortVersionString" -> Some "bundle"
  | "MinimumOSVersion" | "LSMinimumSystemVersion" | "UIDeviceFamily" |
    "UIRequiredDeviceCapabilities" -> Some "deployment"
  | "UIBackgroundModes" | "NSAppTransportSecurity" |
    "ITSAppUsesNonExemptEncryption" -> Some "capability"
  | _ -> None

let export_options_category = function
  | "method" | "teamID" | "signingStyle" | "signingCertificate" |
    "provisioningProfiles" | "installerSigning" |
    "distributionBundleIdentifier" | "iCloudContainerEnvironment" ->
      Some "signing"
  | "destination" | "compileBitcode" | "uploadSymbols" | "uploadBitcode" |
    "stripSwiftSymbols" | "manageAppVersionAndBuildNumber" ->
      Some "deployment"
  | _ -> None

let android_gradle_key key =
  match key with
  | "minSdk" | "minSdkVersion" | "targetSdk" | "targetSdkVersion" |
    "compileSdk" | "compileSdkVersion" | "compileSdkPreview" |
    "maxSdk" | "maxSdkVersion" -> Some ("sdk-level", false)
  | "applicationId" | "applicationIdSuffix" | "versionCode" |
    "versionName" | "testApplicationId" -> Some ("bundle", false)
  | "signingConfig" | "v1SigningEnabled" | "v2SigningEnabled" |
    "v3SigningEnabled" | "v4SigningEnabled" | "enableV1Signing" |
    "enableV2Signing" | "enableV3Signing" | "enableV4Signing" ->
      Some ("signing", false)
  | "storeFile" | "storePassword" | "keyAlias" | "keyPassword" |
    "keyStore" | "storeFilePath" -> Some ("signing", true)
  | _ -> None

let android_gradle_keys =
  ["minSdk"; "minSdkVersion"; "targetSdk"; "targetSdkVersion";
   "compileSdk"; "compileSdkVersion"; "compileSdkPreview";
   "maxSdk"; "maxSdkVersion";
   "applicationId"; "applicationIdSuffix"; "versionCode"; "versionName";
   "testApplicationId";
   "signingConfig"; "v1SigningEnabled"; "v2SigningEnabled";
   "v3SigningEnabled"; "v4SigningEnabled"; "enableV1Signing";
   "enableV2Signing"; "enableV3Signing"; "enableV4Signing";
   "storeFile"; "storePassword"; "keyAlias"; "keyPassword"; "keyStore";
   "storeFilePath"]

let properties_key ~all_secret key =
  if String.lowercase_ascii key = "sdk.dir" then Some ("sdk", false)
  else
    let norm =
      String.lowercase_ascii key |> String.to_seq |>
      Seq.filter (fun c -> c <> '_' && c <> '-' && c <> '.') |>
      String.of_seq in
    let has needle = contains norm needle in
    if all_secret || has "password" || has "passwd" || has "storefile" ||
       has "keyalias" || has "keystore" || has "signing" ||
       has "storekey" then Some ("signing", true)
    else None

(* ---------- shared per-key diff ---------- *)

(* Sorted-multiset difference between the extracted values of one key. *)
let diff_key ~platform ~category ~secret key before_reprs after_reprs =
  if before_reprs = after_reprs then ([], [])
  else
    let aggregate =
      if before_reprs = [] then Added
      else if after_reprs = [] then Removed
      else Changed in
    let dynamic = List.exists is_dynamic before_reprs ||
                  List.exists is_dynamic after_reprs in
    if secret then
      if dynamic then
        ([], [Printf.sprintf
                "%s uses a computed value; the signing change cannot be verified (value not inspected)"
                key])
      else
        ([{ platform; category; setting = key; change = aggregate;
            detail = Printf.sprintf "%s %s (value redacted)"
                key (string_of_change aggregate) }], [])
    else if dynamic then
      ([], [Printf.sprintf
              "%s uses a computed or unparseable value; the exact %s change is unresolved"
              key category])
    else
      let before = List.sort compare
          (List.map repr_text before_reprs) in
      let after = List.sort compare
          (List.map repr_text after_reprs) in
      let rec split xs ys rems adds =
        match xs, ys with
        | [], rest -> (List.rev rems, List.rev_append adds rest)
        | rest, [] -> (List.rev_append rems rest, List.rev adds)
        | x :: xs', y :: ys' ->
            if x = y then split xs' ys' rems adds
            else if x < y then split xs' ys (x :: rems) adds
            else split xs ys' rems (y :: adds) in
      let removed, added = split before after [] [] in
      let finding change detail =
        { platform; category; setting = key; change; detail } in
      if List.length removed = List.length added && removed <> [] then
        (List.map2 (fun old_value new_value ->
             finding Changed
               (Printf.sprintf "%s: %s -> %s" key
                  (shown old_value) (shown new_value))) removed added, [])
      else
        (List.map (fun value -> finding Added
             (Printf.sprintf "%s added: %s" key (shown value))) added @
         List.map (fun value -> finding Removed
             (Printf.sprintf "%s removed: %s" key (shown value))) removed, [])

let group_entries entries =
  let table = Hashtbl.create 16 in
  List.iter (fun (key, r) ->
    let old = try Hashtbl.find table key with Not_found -> [] in
    Hashtbl.replace table key (r :: old)) entries;
  let keys = Hashtbl.fold (fun key _ acc -> key :: acc) table [] in
  let keys = List.sort String.compare keys in
  List.map (fun key -> key, List.sort compare (Hashtbl.find table key)) keys

(* ---------- bounded plist dictionary scanner ---------- *)

(* Token-level plist inspection: no XML evaluation, bounded look-ahead,
   any unsupported structure marks its key dynamic rather than guessed. *)

(* index just past the '>' of a tag starting at i ('<'), quote-aware *)
let tag_end_gt s i limit =
  let stop = min (String.length s) (i + limit) in
  let rec scan j quote =
    if j >= stop then -1
    else
      match s.[j], quote with
      | '"', Some '"' | '\'', Some '\'' -> scan (j + 1) None
      | '"', None -> scan (j + 1) (Some '"')
      | '\'', None -> scan (j + 1) (Some '\'')
      | '>', None -> j
      | _, _ -> scan (j + 1) quote
  in
  scan (i + 1) None

let tag_name s i =
  (* i at '<'; returns (name, is_closing, name_end) or None *)
  if i + 1 >= String.length s || s.[i] <> '<' then None
  else
    let j = if s.[i + 1] = '/' then i + 2 else i + 1 in
    let rec scan k =
      if k >= String.length s then k
      else if ident_char s.[k] || s.[k] = '-' then scan (k + 1)
      else k in
    let e = scan j in
    if e = j then None else Some (String.sub s j (e - j), s.[i + 1] = '/', e)

let find_tag s start name =
  let needle = "<" ^ name in
  let rec scan i =
    match find_from s i needle with
    | -1 -> None
    | j ->
        let after = j + String.length needle in
        if after < String.length s &&
           (is_space s.[after] || s.[after] = '>' || s.[after] = '/') then
          Some j
        else scan (j + 1)
  in
  scan start

let plist_entries text =
  if String.contains text '\000' then
    (false, [], ["binary plist content cannot be verified"])
  else
    match find_tag text 0 "dict" with
    | None -> (false, [], ["no plist dictionary found"])
    | Some dict_lt ->
        (match tag_end_gt text dict_lt max_tag_scan with
         | -1 -> (false, [], ["unterminated plist dictionary tag"])
         | gt when gt > dict_lt + 5 && text.[gt - 1] = '/' ->
             (* <dict/> *) (true, [], [])
         | gt ->
             let entries = ref [] and notes = ref [] and ok = ref true in
             let len = String.length text in
             let rec skip_ws i =
               if i + 3 < len && starts_at text i "<!--" then
                 match find_from text (i + 4) "-->" with
                 | -1 -> i
                 | e -> skip_ws (e + 3)
               else if i < len && is_space text.[i] then skip_ws (i + 1)
               else i in
             let value_repr i depth =
               (* i at '<' of the value element *)
               match tag_name text i with
               | None -> (R_dynamic "", i + 1)
               | Some (name, closing, _) when closing || name = "key" ->
                   (* malformed: a new key or closing tag where a value
                      was expected *)
                   (R_dynamic "", i)
               | Some (("true" | "false") as name, _, _) ->
                   (match tag_end_gt text i 64 with
                    | -1 -> (R_dynamic "", i + 1)
                    | gt -> (R_literal name, gt + 1))
               | Some (("string" | "integer" | "real" | "date" | "data") as
                       name, _, _) ->
                   let gt = tag_end_gt text i max_scalar_scan in
                   let close = "</" ^ name ^ ">" in
                   if gt < 0 then (R_dynamic "", i + 1)
                   else
                     (match find_from text (gt + 1) close with
                      | -1 -> (R_dynamic (clip (String.sub text i
                                (min max_raw_repr (len - i)))), gt + 1)
                      | c ->
                          let inner = String.sub text (gt + 1) (c - gt - 1) in
                          (repr_of inner, c + String.length close))
               | Some (("array" | "dict") as name, _, _) ->
                   if depth > 4 then
                     (R_dynamic (clip (String.sub text i
                        (min max_raw_repr (len - i)))), i + 1)
                   else
                     let open_tag = "<" ^ name and close_tag = "</" ^ name in
                     let rec matching j d =
                       (* find this element's close, counting nesting *)
                       let o = find_from text j open_tag in
                       let c = find_from text j close_tag in
                       if c < 0 then -1
                       else if o >= 0 && o < c then matching (o + 2) (d + 1)
                       else if d = 1 then c
                       else matching (c + String.length close_tag) (d - 1) in
                     let gt = tag_end_gt text i max_tag_scan in
                     if gt < 0 then (R_dynamic "", i + 1)
                     else
                       let c = matching (gt + 1) 1 in
                       if c < 0 then
                         (R_dynamic (clip (String.sub text i
                            (min max_raw_repr (len - i)))), len)
                       else
                         let inner = String.sub text (gt + 1) (c - gt - 1) in
                         let next = c + String.length close_tag in
                         if name = "dict" || contains inner "<dict" ||
                            contains inner "<array" then
                           (R_dynamic (clip (String.sub text i
                              (min max_raw_repr (next - i)))), next)
                         else
                           (* flat array: only <string> items supported *)
                           let items = ref [] and valid = ref true in
                           let p = ref 0 in
                           (try
                              while find_from inner !p "<" >= 0 do
                                let t = find_from inner !p "<" in
                                (match tag_name inner t with
                                 | Some ("string", false, _) ->
                                     let g = tag_end_gt inner t 256 in
                                     let cc = find_from inner (g + 1) "</string>" in
                                     if g < 0 || cc < 0 then valid := false
                                     else items := String.sub inner (g + 1)
                                         (cc - g - 1) :: !items;
                                     if cc >= 0 then p := cc + 9
                                     else p := t + 1
                                 | Some (_, true, _) -> p := t + 1
                                 | _ -> valid := false; p := t + 1)
                              done
                            with _ -> valid := false);
                           if !valid then
                             let joined =
                               String.concat "," (List.rev !items) in
                             (repr_of joined, next)
                           else
                             (R_dynamic (clip (String.sub text i
                                (min max_raw_repr (next - i)))), next)
               | Some (_, _, _) ->
                   (match tag_end_gt text i max_tag_scan with
                    | -1 -> (R_dynamic "", i + 1)
                    | gt -> (R_dynamic (clip (String.sub text i
                              (min max_raw_repr (gt - i + 1)))), gt + 1))
             in
             let rec loop i =
               let i = skip_ws i in
               if i >= len then ok := false
               else if starts_at text i "</dict>" then ()
               else if starts_at text i "<key" &&
                       (i + 4 >= len || text.[i + 4] = '>' ||
                        is_space text.[i + 4]) then
                 (match find_from text i "</key>" with
                  | -1 -> ok := false
                  | kc ->
                      let kgt = tag_end_gt text i 64 in
                      if kgt < 0 || kgt > kc then ok := false
                      else
                        let key = String.sub text (kgt + 1) (kc - kgt - 1) in
                        let vstart = skip_ws (kc + 6) in
                        if vstart >= len || text.[vstart] <> '<' then
                          ok := false
                        else
                          let r, next = value_repr vstart 1 in
                          entries := (key, r) :: !entries;
                          if next <= i then ok := false else loop next)
               else if starts_at text i "</" then
                 (* unexpected closing tag inside dict *)
                 (notes := "unexpected closing element inside plist dictionary"
                    :: !notes;
                  match tag_end_gt text i 256 with
                  | -1 -> ok := false
                  | gt -> loop (gt + 1))
               else if text.[i] = '<' then
                 (notes := "unsupported element inside plist dictionary"
                    :: !notes;
                  match tag_end_gt text i max_tag_scan with
                  | -1 -> ok := false
                  | gt -> loop (gt + 1))
               else
                 (* stray character data between entries *)
                 (match find_from text (i + 1) "<" with
                  | -1 -> ok := false
                  | j -> loop j)
             in
             loop (gt + 1);
             (!ok, List.rev !entries,
              List.sort_uniq String.compare (List.rev !notes)))

let plist_diff platform category_of before after =
  let ok_b, entries_b, notes_b = plist_entries before in
  let ok_a, entries_a, notes_a = plist_entries after in
  let grouped_b = group_entries entries_b in
  let grouped_a = group_entries entries_a in
  let table_b = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace table_b k v) grouped_b;
  let table_a = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace table_a k v) grouped_a;
  let keys =
    List.sort_uniq String.compare
      (List.map fst grouped_b @ List.map fst grouped_a) in
  let effects = ref [] and unresolved = ref [] in
  List.iter (fun key ->
    match category_of key with
    | None -> ()
    | Some category ->
        let b = try Hashtbl.find table_b key with Not_found -> [] in
        let a = try Hashtbl.find table_a key with Not_found -> [] in
        let e, u = diff_key ~platform ~category ~secret:false key b a in
        effects := e @ !effects;
        unresolved := u @ !unresolved) keys;
  let unresolved =
    if ok_b && ok_a then !unresolved
    else List.sort_uniq String.compare
        (notes_b @ notes_a @
         ["plist content is malformed or truncated; effects cannot be \
           verified"] @ !unresolved) in
  (platform, List.rev !effects, unresolved)

(* ---------- pbxproj / xcconfig / properties line scanners ---------- *)

(* token positions of each needle inside line, expanded to ident bounds *)
let token_occurrences line needles =
  let found = ref [] in
  List.iter (fun needle ->
    let rec scan i =
      match find_from line i needle with
      | -1 -> ()
      | j ->
          (* expand token boundaries over ident chars *)
          let left = ref j in
          while !left > 0 && ident_char line.[!left - 1] do decr left done;
          let right = ref (j + String.length needle) in
          while !right < String.length line &&
                ident_char line.[!right] do incr right done;
          let token = String.sub line !left (!right - !left) in
          if not (List.exists (fun (t, _, _) -> t = token) !found) then
            found := (token, !left, !right) :: !found;
          scan (j + 1)
    in
    scan 0) needles;
  List.sort (fun (_, a, _) (_, b, _) -> compare a b) !found

(* assignment `KEY = value` on one line; value ends at `;` if required *)
let assignment_on_line ~require_semicolon line =
  match String.index_opt line '=' with
  | None -> None
  | Some eq ->
      (* key token: ident chars immediately before '=' after ws *)
      let kend = ref eq in
      while !kend > 0 && is_space line.[!kend - 1] do decr kend done;
      let kstart = ref !kend in
      while !kstart > 0 && ident_char line.[!kstart - 1] do decr kstart done;
      let key = String.sub line !kstart (!kend - !kstart) in
      if not (ident_string key) then None
      else
        let vstart = ref (eq + 1) in
        while !vstart < String.length line && is_space line.[!vstart] do
          incr vstart done;
        let value =
          if require_semicolon then
            match String.index_opt line ';' with
            | Some sc when sc >= !vstart ->
                Some (String.sub line !vstart (sc - !vstart))
            | _ -> None
          else Some (String.sub line !vstart
              (String.length line - !vstart)) in
        (match value with
         | None -> Some (key, !kstart, R_dynamic (clip line))
         | Some v ->
             let v = String.trim v in
             if contains v "=" || (v <> "" && v.[String.length v - 1] = '\\')
               then Some (key, !kstart, R_dynamic (clip line))
             else Some (key, !kstart, repr_of v))

let xcode_entries text =
  let entries = ref [] in
  lines_of text |> List.iter (fun line ->
    let assignment = assignment_on_line ~require_semicolon:true line in
    (match assignment with
     | Some (key, kstart, r) ->
         if ios_assignment_category key <> None then
           entries := (key, r) :: !entries;
         let covered = kstart in
         token_occurrences line ios_needles |> List.iter
           (fun (token, start, _) ->
             if ios_assignment_category token <> None && start <> covered
               then entries := (token, R_dynamic (clip line)) :: !entries)
     | None ->
         token_occurrences line ios_needles |> List.iter
           (fun (token, _, _) ->
             if ios_assignment_category token <> None then
               entries := (token, R_dynamic (clip line)) :: !entries)));
  !entries

let ios_assignment_diff platform before after =
  let grouped_b = group_entries (xcode_entries before) in
  let grouped_a = group_entries (xcode_entries after) in
  let tb = Hashtbl.create 16 and ta = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace tb k v) grouped_b;
  List.iter (fun (k, v) -> Hashtbl.replace ta k v) grouped_a;
  let keys = List.sort_uniq String.compare
      (List.map fst grouped_b @ List.map fst grouped_a) in
  let effects = ref [] and unresolved = ref [] in
  List.iter (fun key ->
    match ios_assignment_category key with
    | None -> ()
    | Some category ->
        let b = try Hashtbl.find tb key with Not_found -> [] in
        let a = try Hashtbl.find ta key with Not_found -> [] in
        let e, u = diff_key ~platform ~category ~secret:false key b a in
        effects := e @ !effects;
        unresolved := u @ !unresolved) keys;
  (platform, List.rev !effects, List.rev !unresolved)

let xcconfig_diff before after =
  let entries = ref [] and includes = ref [] in
  List.iter (fun line ->
    let trimmed = trim line in
    if trimmed = "" || starts_at trimmed 0 "//" then ()
    else if starts_at trimmed 0 "#include" ||
            starts_at trimmed 0 "#include?" then
      includes := repr_of trimmed :: !includes
    else
      match assignment_on_line ~require_semicolon:false line with
      | Some (key, kstart, r) ->
          if ios_assignment_category key <> None then
            entries := (key, r) :: !entries;
          token_occurrences line ios_needles |> List.iter
            (fun (token, start, _) ->
              if ios_assignment_category token <> None && start <> kstart
                then entries := (token, R_dynamic (clip line)) :: !entries)
      | None ->
          token_occurrences line ios_needles |> List.iter
            (fun (token, _, _) ->
              if ios_assignment_category token <> None then
                entries := (token, R_dynamic (clip line)) :: !entries))
    (lines_of before);
  let before_entries = !entries and before_includes = !includes in
  entries := []; includes := [];
  List.iter (fun line ->
    let trimmed = trim line in
    if trimmed = "" || starts_at trimmed 0 "//" then ()
    else if starts_at trimmed 0 "#include" ||
            starts_at trimmed 0 "#include?" then
      includes := repr_of trimmed :: !includes
    else
      match assignment_on_line ~require_semicolon:false line with
      | Some (key, kstart, r) ->
          if ios_assignment_category key <> None then
            entries := (key, r) :: !entries;
          token_occurrences line ios_needles |> List.iter
            (fun (token, start, _) ->
              if ios_assignment_category token <> None && start <> kstart
                then entries := (token, R_dynamic (clip line)) :: !entries)
      | None ->
          token_occurrences line ios_needles |> List.iter
            (fun (token, _, _) ->
              if ios_assignment_category token <> None then
                entries := (token, R_dynamic (clip line)) :: !entries))
    (lines_of after);
  let grouped_b = group_entries before_entries in
  let grouped_a = group_entries !entries in
  let tb = Hashtbl.create 16 and ta = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace tb k v) grouped_b;
  List.iter (fun (k, v) -> Hashtbl.replace ta k v) grouped_a;
  let keys = List.sort_uniq String.compare
      (List.map fst grouped_b @ List.map fst grouped_a) in
  let effects = ref [] and unresolved = ref [] in
  List.iter (fun key ->
    match ios_assignment_category key with
    | None -> ()
    | Some category ->
        let b = try Hashtbl.find tb key with Not_found -> [] in
        let a = try Hashtbl.find ta key with Not_found -> [] in
        let e, u = diff_key ~platform:Ios ~category ~secret:false key b a in
        effects := e @ !effects;
        unresolved := u @ !unresolved) keys;
  if List.sort compare before_includes <> List.sort compare !includes then
    unresolved := "xcconfig #include directives changed; included files \
                   are not resolved statically" :: !unresolved;
  (Ios, List.rev !effects, List.rev !unresolved)

let properties_diff ~all_secret before after =
  let collect text =
    lines_of text |> List.concat_map (fun line ->
      let trimmed = trim line in
      if trimmed = "" || trimmed.[0] = '#' || trimmed.[0] = '!' then []
      else
        let sep =
          let rec first i =
            if i >= String.length trimmed then -1
            else match trimmed.[i] with
              | '=' | ':' -> i
              | ' ' | '\t' -> i
              | _ -> first (i + 1) in
          first 0 in
        if sep <= 0 then []
        else
          let key = String.sub trimmed 0 sep in
          let value =
            let j = ref sep in
            (if trimmed.[!j] = ' ' || trimmed.[!j] = '\t' then
               (while !j < String.length trimmed &&
                      (trimmed.[!j] = ' ' || trimmed.[!j] = '\t') do
                  incr j done;
               if !j < String.length trimmed &&
                  (trimmed.[!j] = '=' || trimmed.[!j] = ':') then incr j)
             else incr j);
            while !j < String.length trimmed && is_space trimmed.[!j] do
              incr j done;
            String.sub trimmed !j (String.length trimmed - !j) in
          [(key, repr_of value)])
  in
  let grouped_b = group_entries (collect before) in
  let grouped_a = group_entries (collect after) in
  let tb = Hashtbl.create 16 and ta = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace tb k v) grouped_b;
  List.iter (fun (k, v) -> Hashtbl.replace ta k v) grouped_a;
  let keys = List.sort_uniq String.compare
      (List.map fst grouped_b @ List.map fst grouped_a) in
  let effects = ref [] and unresolved = ref [] in
  List.iter (fun key ->
    match properties_key ~all_secret key with
    | None -> ()
    | Some (category, secret) ->
        let b = try Hashtbl.find tb key with Not_found -> [] in
        let a = try Hashtbl.find ta key with Not_found -> [] in
        let e, u = diff_key ~platform:Android ~category ~secret key b a in
        effects := e @ !effects;
        unresolved := u @ !unresolved) keys;
  (Android, List.rev !effects, List.rev !unresolved)

(* ---------- Gradle build script scanner ---------- *)

(* strip // line comments and /* */ blocks (tracked across lines) *)
let strip_groovy_comments lines =
  let in_block = ref false in
  List.map (fun line ->
    let buf = Buffer.create (String.length line) in
    let len = String.length line in
    let rec scan i =
      if i >= len then ()
      else if !in_block then
        if i + 1 < len && line.[i] = '*' && line.[i + 1] = '/' then
          (in_block := false; scan (i + 2))
        else scan (i + 1)
      else if i + 1 < len && line.[i] = '/' && line.[i + 1] = '/' then ()
      else if i + 1 < len && line.[i] = '/' && line.[i + 1] = '*' then
        (in_block := true; scan (i + 2))
      else (Buffer.add_char buf line.[i]; scan (i + 1))
    in
    scan 0;
    Buffer.contents buf) lines

(* parse the value token following a key occurrence inside a line *)
let gradle_value line from =
  let len = String.length line in
  let i = ref from in
  while !i < len && is_space line.[!i] do incr i done;
  if !i < len && line.[!i] = '=' then
    (incr i; while !i < len && is_space line.[!i] do incr i done);
  if !i >= len then R_dynamic (clip line)
  else
    let c = line.[!i] in
    if c = '"' || c = '\'' then
      match
        (let rec find j = if j >= len then -1
           else if line.[j] = c then j else find (j + 1) in
         find (!i + 1)) with
      | -1 -> R_dynamic (clip line)
      | e -> repr_of (String.sub line (!i + 1) (e - !i - 1))
    else if c = '(' then
      (* method-call form minSdk(24) / file("x") *)
      let rec find j depth =
        if j >= len then -1
        else match line.[j] with
          | '(' -> find (j + 1) (depth + 1)
          | ')' -> if depth = 1 then j else find (j + 1) (depth - 1)
          | _ -> find (j + 1) depth in
      (match find !i 0 with
       | -1 -> R_dynamic (clip line)
       | e ->
           let inner = String.sub line (!i + 1) (e - !i - 1) |> String.trim in
           if (String.length inner >= 2 &&
               (inner.[0] = '"' || inner.[0] = '\'') &&
               inner.[String.length inner - 1] = inner.[0]) then
             repr_of (String.sub inner 1 (String.length inner - 2))
           else if inner <> "" &&
                   String.for_all (fun ch -> ch >= '0' && ch <= '9') inner
             then R_literal inner
           else R_dynamic (clip line))
    else if !i + 5 < len && starts_at line !i "file(" &&
            (line.[!i + 5] = '"' || line.[!i + 5] = '\'') then
      (* storeFile file("path") — literal path, treated as secret *)
      let q = line.[!i + 5] in
      (match
         (let rec find j = if j >= len then -1
            else if line.[j] = q then j else find (j + 1) in
          find (!i + 6)) with
       | e when e > !i + 6 ->
           repr_of (String.sub line (!i + 6) (e - !i - 6))
       | _ -> R_dynamic (clip line))
    else
      (* bare token until ws or delimiter *)
      let j = ref !i in
      while !j < len &&
            (not (is_space line.[!j])) && line.[!j] <> ')' &&
            line.[!j] <> ',' && line.[!j] <> '}' && line.[!j] <> ';'
      do incr j done;
      let tok = String.sub line !i (!j - !i) in
      if tok = "" then R_dynamic (clip line)
      else if String.for_all (fun ch -> ch >= '0' && ch <= '9') tok ||
              tok = "true" || tok = "false" then R_literal tok
      else if String.length tok > 15 &&
              starts_at tok 0 "signingConfigs." &&
              String.for_all ident_char
                (String.sub tok 15 (String.length tok - 15)) then
        (* a literal reference to a named signing config block *)
        R_literal tok
      else if String.length tok > 6 && starts_at tok 0 "file(\"" &&
              ends_with tok "\")" then
        R_literal (String.sub tok 6 (String.length tok - 8))
      else R_dynamic (clip line)

let gradle_diff before after =
  let collect text =
    strip_groovy_comments (lines_of text) |> List.concat_map (fun line ->
      token_occurrences line android_gradle_keys |>
      List.filter_map (fun (token, start, right) ->
        match android_gradle_key token with
        | None -> None
        | Some _ -> Some (token, start, gradle_value line right)))
  in
  let b = collect before and a = collect after in
  let gb = group_entries (List.map (fun (k, _, r) -> (k, r)) b) in
  let ga = group_entries (List.map (fun (k, _, r) -> (k, r)) a) in
  let tb = Hashtbl.create 16 and ta = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace tb k v) gb;
  List.iter (fun (k, v) -> Hashtbl.replace ta k v) ga;
  let keys = List.sort_uniq String.compare
      (List.map fst gb @ List.map fst ga) in
  let effects = ref [] and unresolved = ref [] in
  List.iter (fun key ->
    match android_gradle_key key with
    | None -> ()
    | Some (category, secret) ->
        let bv = try Hashtbl.find tb key with Not_found -> [] in
        let av = try Hashtbl.find ta key with Not_found -> [] in
        let e, u = diff_key ~platform:Android ~category ~secret key bv av in
        effects := e @ !effects;
        unresolved := u @ !unresolved) keys;
  (Android, List.rev !effects, List.rev !unresolved)

(* ---------- Android manifest element scanner ---------- *)

let manifest_tags =
  ["uses-permission-sdk-23"; "uses-permission"; "uses-feature";
   "uses-sdk"; "permission-group"; "permission-tree"; "permission";
   "manifest"]

let manifest_tag_category = function
  | "uses-permission-sdk-23" | "uses-permission" | "permission-group" |
    "permission-tree" | "permission" -> "permission"
  | "uses-feature" -> "feature"
  | "uses-sdk" -> "sdk-level"
  | _ -> "manifest"

let manifest_sensitive_attrs =
  ["package"; "android:sharedUserId"; "android:sharedUserLabel";
   "android:versionCode"; "android:versionName"; "android:installLocation"]

type elt = {
  tag : string;
  name : string;
  attrs : (string * string) list;
  malformed : bool;
  raw : string;
}

let parse_attrs s ~start ~stop =
  (* attribute region [start,stop); strict ident="v" pairs *)
  let attrs = ref [] and ok = ref true in
  let i = ref start in
  (try
     while !i < stop do
       let c = s.[!i] in
       if is_space c || c = '/' then incr i
       else begin
         let j = ref !i in
         while !j < stop &&
               (ident_char s.[!j] || s.[!j] = ':' || s.[!j] = '.' ||
                s.[!j] = '-') do incr j done;
         let name = String.sub s !i (!j - !i) in
         if name = "" then raise Exit;
         while !j < stop && is_space s.[!j] do incr j done;
         if !j >= stop || s.[!j] <> '=' then raise Exit;
         incr j;
         while !j < stop && is_space s.[!j] do incr j done;
         if !j >= stop || (s.[!j] <> '"' && s.[!j] <> '\'') then
           raise Exit;
         let q = s.[!j] in
         incr j;
         let vstart = !j in
         while !j < stop && s.[!j] <> q do incr j done;
         if !j >= stop then raise Exit;
         let v = String.sub s vstart (!j - vstart) in
         incr j;
         attrs := (name, v) :: !attrs;
         i := !j
       end
     done
   with Exit -> ok := false);
  (!ok, List.rev !attrs)

let manifest_elements text =
  let found = ref [] and malformed_doc = ref false in
  if String.contains text '\000' then malformed_doc := true
  else
    List.iter (fun tag ->
      let needle = "<" ^ tag in
      let rec scan i =
        match find_from text i needle with
        | -1 -> ()
        | j ->
            let after = j + String.length needle in
            if after < String.length text &&
               (is_space text.[after] || text.[after] = '>' ||
                text.[after] = '/') then
              (match tag_end_gt text j max_tag_scan with
               | -1 ->
                   found := { tag; name = ""; attrs = [];
                              malformed = true;
                              raw = clip (String.sub text j
                                (min 256 (String.length text - j))) }
                     :: !found;
                   scan (j + 1)
               | gt ->
                   let ok, attrs =
                     parse_attrs text ~start:after ~stop:gt in
                   let name =
                     match List.assoc_opt "android:name" attrs with
                     | Some v -> v
                     | None ->
                         (match List.assoc_opt "name" attrs with
                          | Some v -> v | None -> "") in
                   let needs_name =
                     tag <> "manifest" && tag <> "uses-sdk" in
                   let placeholder =
                     List.exists (fun (_, v) -> contains v "${") attrs in
                   let malformed = (not ok) || placeholder ||
                                   (needs_name && name = "") in
                   found := { tag; name; attrs; malformed;
                              raw = clip (String.sub text j
                                (min 512 (gt - j + 1))) } :: !found;
                   scan (gt + 1))
            else scan (j + 1)
      in
      scan 0) manifest_tags;
  (!malformed_doc, List.rev !found)


let canonical_elt e =
  let attrs = List.sort compare e.attrs in
  String.concat "\x02" (List.map (fun (n, v) -> n ^ "=" ^ v) attrs)

let attr_list_text attrs =
  attrs |> List.map (fun (n, v) -> n ^ "=" ^ shown v) |>
  String.concat ", " |> clip ~limit:256

let manifest_diff before after =
  let bad_b, eb = manifest_elements before in
  let bad_a, ea = manifest_elements after in
  let key e = e.tag ^ "\x01" ^ e.name in
  let group elts =
    let t = Hashtbl.create 16 in
    List.iter (fun e ->
      let old = try Hashtbl.find t (key e) with Not_found -> [] in
      Hashtbl.replace t (key e) (e :: old)) elts;
    t in
  let tb = group eb and ta = group ea in
  let keys =
    let acc = Hashtbl.fold (fun k _ acc -> k :: acc) tb [] in
    Hashtbl.fold (fun k _ acc ->
      if List.mem k acc then acc else k :: acc) ta acc |>
    List.sort String.compare in
  let effects = ref [] and unresolved = ref [] in
  let push e = effects := e :: !effects in
  let unresolved_push s =
    if not (List.mem s !unresolved) then unresolved := s :: !unresolved in
  List.iter (fun k ->
    let bs = try Hashtbl.find tb k with Not_found -> [] in
    let a_s = try Hashtbl.find ta k with Not_found -> [] in
    match bs, a_s with
    | [], [] -> ()
    | _ ->
        let tag = match bs, a_s with
          | e :: _, _ -> e.tag | _, e :: _ -> e.tag | _ -> "" in
        let category = manifest_tag_category tag in
        if tag = "manifest" then
          (* only the sensitive manifest attributes are compared *)
          let attrs_of e = List.filter (fun (n, _) ->
            List.mem n manifest_sensitive_attrs) e.attrs in
          let bv = match bs with
            | e :: _ -> attrs_of e | [] -> [] in
          let av = match a_s with
            | e :: _ -> attrs_of e | [] -> [] in
          let names = List.sort_uniq String.compare
              (List.map fst bv @ List.map fst av) in
          List.iter (fun n ->
            let b = List.assoc_opt n bv and a = List.assoc_opt n av in
            if b = a then ()
            else
              let dyn = (match b with Some v -> contains v "${"
                                    | None -> false) ||
                        (match a with Some v -> contains v "${"
                                    | None -> false) in
              if dyn then
                unresolved_push
                  ("manifest attribute " ^ n ^
                   " uses a placeholder; the exact change is unresolved")
              else
                let change, detail =
                  match b, a with
                  | None, Some v -> Added,
                      "manifest " ^ n ^ " added: " ^ shown v
                  | Some v, None -> Removed,
                      "manifest " ^ n ^ " removed: " ^ shown v
                  | Some o, Some n' -> Changed,
                      "manifest " ^ n ^ ": " ^ shown o ^ " -> " ^ shown n'
                  | None, None -> Changed, "" in
                push { platform = Android; category;
                       setting = n; change; detail }) names
        else
          let cs_b = List.sort compare (List.map canonical_elt bs) in
          let cs_a = List.sort compare (List.map canonical_elt a_s) in
          if cs_b = cs_a then ()
          else if List.exists (fun e -> e.malformed) (bs @ a_s) then
            unresolved_push
              ("a malformed <" ^ tag ^ "> element changed; " ^
               "the exact manifest effect is unresolved")
          else if List.length bs = 1 && List.length a_s = 1 then
            List.iter2 (fun old_e new_e ->
              let frag = ref [] in
              let names = List.sort_uniq String.compare
                  (List.map fst old_e.attrs @ List.map fst new_e.attrs) in
              List.iter (fun n ->
                let o = List.assoc_opt n old_e.attrs in
                let nw = List.assoc_opt n new_e.attrs in
                if o = nw then ()
                else
                  match o, nw with
                  | None, Some v -> frag :=
                      ("added " ^ n ^ "=" ^ shown v) :: !frag
                  | Some v, None -> frag :=
                      ("removed " ^ n ^ "=" ^ shown v) :: !frag
                  | Some ov, Some nv -> frag :=
                      (n ^ ": " ^ shown ov ^ " -> " ^ shown nv) :: !frag
                  | None, None -> ()) names;
              let frags = String.concat "; " (List.rev !frag) in
              let label =
                if new_e.name = "" then "" else shown new_e.name ^ ": " in
              push { platform = Android; category;
                     setting = (if new_e.name = "" then tag else new_e.name);
                     change = Changed;
                     detail = "<" ^ tag ^ "> " ^ label ^ frags })
              (List.sort compare bs) (List.sort compare a_s)
          else begin
            List.iter (fun e ->
              if not (List.exists (fun o -> canonical_elt o =
                        canonical_elt e) bs) then
                push { platform = Android; category;
                       setting = (if e.name = "" then tag else e.name);
                       change = Added;
                       detail = "<" ^ tag ^ "> added: " ^
                                (if e.name = "" then tag else shown e.name) ^
                                " (" ^ attr_list_text e.attrs ^ ")" })
              a_s;
            List.iter (fun e ->
              if not (List.exists (fun o -> canonical_elt o =
                        canonical_elt e) a_s) then
                push { platform = Android; category;
                       setting = (if e.name = "" then tag else e.name);
                       change = Removed;
                       detail = "<" ^ tag ^ "> removed: " ^
                                (if e.name = "" then tag else shown e.name) ^
                                " (" ^ attr_list_text e.attrs ^ ")" })
              bs
          end) keys;
  let unresolved =
    if bad_b || bad_a then
      "manifest content is binary or unparseable; changes cannot be verified"
      :: !unresolved
    else !unresolved in
  (Android, List.rev !effects, List.rev unresolved)

(* ---------- Podfile / Fastlane (executable configuration) ---------- *)

let podfile_diff before after =
  let platforms text =
    lines_of text |> List.filter_map (fun line ->
      let trimmed = trim line in
      if starts_at trimmed 0 "platform" then
        match String.index_opt trimmed ',' with
        | Some comma ->
            let os = String.sub trimmed 0 comma |> String.trim in
            let ver = String.sub trimmed (comma + 1)
                (String.length trimmed - comma - 1) |> String.trim in
            Some (repr_of (os ^ " " ^ ver))
        | None -> Some (R_dynamic (clip trimmed))
      else None) in
  let effects, unresolved =
    diff_key ~platform:Ios ~category:"deployment" ~secret:false
      "platform" (platforms before) (platforms after) in
  (Ios, effects,
   "Podfile is executable configuration; only the literal platform line \
    is verified" :: unresolved)

let fastlane_diff () =
  (Ios, [],
   ["Fastlane configuration is executable; signing and deployment \
     effects cannot be verified statically"])

(* ---------- top-level ---------- *)

let finalize path platform findings unresolved =
  let findings = List.sort (fun a b ->
    match String.compare a.category b.category with
    | 0 -> String.compare a.detail b.detail
    | c -> c) findings in
  let unresolved = List.sort_uniq String.compare unresolved in
  if findings = [] && unresolved = [] then Ordinary
  else if List.length findings > max_findings then
    Unresolved { file = path; platform;
                 findings = List.filteri (fun i _ -> i < max_findings)
                     findings;
                 unresolved = "more than 64 sensitive changes; the finding \
                               list is not exact" :: unresolved }
  else if unresolved = [] then
    Literal { file = path; platform; findings; unresolved = [] }
  else
    Unresolved { file = path; platform; findings; unresolved }
let classify ~path ~before ~after =
  checked_relative path;
  if String.length before > max_input_bytes ||
     String.length after > max_input_bytes then
    fail "proposed content exceeds the 1 MiB classifier bound";
  match kind_of path with
  | None -> Ordinary
  | Some _ when before = after -> Ordinary
  | Some (Signing_material platform) ->
      let base = Filename.basename path in
      let change =
        if before = "" then Added
        else if after = "" then Removed
        else Changed in
      Literal { file = path; platform;
                findings = [{ platform; category = "signing-material";
                              setting = base; change;
                              detail = base ^ " " ^ string_of_change change ^
                                       "; signing material contents are \
                                        never inspected" }];
                unresolved = [] }
  | Some kind ->
      let platform, effects, unresolved =
        match kind with
        | Entitlements ->
            plist_diff Ios (fun _ -> Some "entitlement") before after
        | Info_plist -> plist_diff Ios info_plist_category before after
        | Export_options ->
            plist_diff Ios export_options_category before after
        | Xcode_project -> ios_assignment_diff Ios before after
        | Xcconfig -> xcconfig_diff before after
        | Podfile -> podfile_diff before after
        | Fastlane -> fastlane_diff ()
        | Android_manifest -> manifest_diff before after
        | Android_gradle -> gradle_diff before after
        | Android_properties all_secret ->
            properties_diff ~all_secret before after
        | Signing_material _ -> assert false in
      finalize path platform effects unresolved

let describe verdict =
  match verdict with
  | Ordinary -> "ordinary workspace change (no sensitive mobile changes)"
  | Literal report ->
      let lines =
        List.map (fun e -> "  - " ^ e.detail) report.findings in
      "sensitive " ^ string_of_platform report.platform ^
      " change in " ^ report.file ^ ":\n" ^ String.concat "\n" lines
  | Unresolved report ->
      let elines =
        List.map (fun e -> "  - " ^ e.detail) report.findings in
      let ulines = List.map (fun u -> "  ! " ^ u) report.unresolved in
      "unresolved sensitive " ^ string_of_platform report.platform ^
      " change in " ^ report.file ^ ":\n" ^
      String.concat "\n" (elines @ ulines)

(* Consumer-facing entry point: `before = None` means the original file
   content is not confirmed. For a guarded path that is *unresolved* —
   never treat an unconfirmed original as a brand-new file, which would
   mislabel every existing sensitive value as an addition. *)
let classify_proposal ~path ~before ~after =
  match before with
  | Some content -> classify ~path ~before:content ~after
  | None ->
      checked_relative path;
      if String.length after > max_input_bytes then
        fail "proposed content exceeds the 1 MiB classifier bound";
      (match kind_of path with
       | None -> Ordinary
       | Some k ->
           let platform = match k with
             | Android_manifest | Android_gradle | Android_properties _
             | Signing_material Android -> Android
             | _ -> Ios in
           Unresolved { file = path; platform; findings = [];
                        unresolved = ["original content of " ^ path ^
                                      " is unconfirmed; the exact change \
                                       cannot be verified"] })
