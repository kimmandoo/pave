type values = {
  default_provider : string option;
  default_model : string option;
  default_api : string option;
  disable_shell : bool;
  max_turns : int option;
  approval_mode : Approval.mode option;
  tool_approval : (string * Approval.policy) list;
  command_patterns : Approval.command_rule list;
}

type loaded = { values : values; diagnostics : string list }

let empty = {
  default_provider = None; default_model = None; default_api = None;
  disable_shell = false; max_turns = None; approval_mode = None;
  tool_approval = []; command_patterns = [];
}

let member name fields = List.assoc_opt name fields
let first_some first second = match first with Some _ -> first | None -> second

let string_field name fields = match member name fields with
  | None -> None
  | Some (`String value) when String.trim value <> "" &&
      String.length value <= 256 &&
      not (String.exists (fun character -> Char.code character < 32) value) ->
      Some value
  | Some _ -> invalid_arg (name ^ " must be a nonempty string (at most 256 bytes)")

let positive_field name fields = match member name fields with
  | None -> None
  | Some (`Int count) when count > 0 && count <= 100 -> Some count
  | Some _ -> invalid_arg (name ^ " must be an integer between 1 and 100")
let check_unique_fields label fields =
  let names = List.map fst fields in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    invalid_arg ("duplicate " ^ label)

let valid_policy_name text =
  text <> "" && String.length text <= 64 &&
  not (String.exists (fun character ->
    Char.code character <= 32 || Char.code character = 127) text)

let parse_policy = function
  | `String value ->
      (match Approval.policy_of_string value with
       | Some policy -> policy
       | None -> invalid_arg "approval policy must be allow, deny, or prompt")
  | _ -> invalid_arg "approval policy must be a string"

let parse_approval_settings = function
  | `Null -> None, [], []
  | `Assoc fields ->
      check_unique_fields "tool setting" fields;
      let allowed = ["approvalMode"; "approval"; "commandPatterns"] in
      List.iter (fun (name, _) -> if not (List.mem name allowed) then
        invalid_arg ("unknown tool setting " ^ name)) fields;
      let approval_mode = match member "approvalMode" fields with
        | None -> None
        | Some (`String value) ->
            (match Approval.mode_of_string (String.lowercase_ascii
              (String.trim value)) with
             | Some mode -> Some mode
             | None -> invalid_arg
                 "tools.approvalMode must be always-ask, write, or yolo")
        | Some _ -> invalid_arg "tools.approvalMode must be a string" in
      let tool_approval = match member "approval" fields with
        | None -> []
        | Some (`Assoc policies) ->
            check_unique_fields "tool approval policy" policies;
            List.map (fun (name, value) ->
              if not (valid_policy_name name) then
                invalid_arg "tool approval name must be a bounded identifier";
              name, parse_policy value) policies
        | Some _ -> invalid_arg "tools.approval must be an object" in
      let command_patterns = match member "commandPatterns" fields with
        | None -> []
        | Some (`List patterns) when List.length patterns <= 64 ->
            List.map (function
              | `Assoc rule ->
                  check_unique_fields "command pattern field" rule;
                  List.iter (fun (name, _) ->
                    if not (List.mem name ["match"; "approval"]) then
                      invalid_arg ("unknown command pattern field " ^ name)) rule;
                  let match_text = match member "match" rule with
                    | Some (`String text) when String.trim text <> "" &&
                        String.length text <= 256 &&
                        not (String.exists (fun character ->
                          Char.code character < 32 || Char.code character = 127) text) ->
                        String.trim text
                    | _ -> invalid_arg
                        "command pattern match must be a nonempty string (at most 256 bytes)" in
                  let policy = match member "approval" rule with
                    | None -> invalid_arg "command pattern approval is required"
                    | Some value -> parse_policy value in
                  { Approval.match_text = match_text; policy }
              | _ -> invalid_arg "command pattern must be an object") patterns
        | Some (`List _) -> invalid_arg "tools.commandPatterns allows at most 64 rules"
        | Some _ -> invalid_arg "tools.commandPatterns must be an array" in
      approval_mode, tool_approval, command_patterns
  | _ -> invalid_arg "tools must be an object"

let parse text =
  let fields = match Yojson.Basic.from_string text with
    | `Assoc fields -> fields
    | _ -> invalid_arg "expected a JSON object" in
  check_unique_fields "setting" fields;
  let allowed = ["default_provider"; "default_model"; "default_api";
    "disable_shell"; "max_turns"; "tools"] in
  List.iter (fun (name, _) -> if not (List.mem name allowed) then
    invalid_arg ("unknown setting " ^ name)) fields;
  let disable_shell = match member "disable_shell" fields with
    | None -> false
    | Some (`Bool value) -> value
    | Some _ -> invalid_arg "disable_shell must be boolean" in
  let default_provider = string_field "default_provider" fields in
  let default_model = string_field "default_model" fields in
  let default_api = string_field "default_api" fields in
  (match default_model, default_api, default_provider with
   | _, Some _, None ->
       invalid_arg "default_api requires default_provider"
   | Some _, _, None -> invalid_arg "default_model requires default_provider"
   | _ -> ());
  let approval_mode, tool_approval, command_patterns =
    match List.assoc_opt "tools" fields with
    | None -> None, [], []
    | Some value -> parse_approval_settings value in
  { default_provider; default_model; default_api; disable_shell;
    max_turns = positive_field "max_turns" fields;
    approval_mode; tool_approval; command_patterns }

let same_file a b =
  a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino

let read path =
  let before = Unix.lstat path in
  if before.Unix.st_kind <> Unix.S_REG then
    invalid_arg "settings file must be regular, not a symlink";
  let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 in
  let input = Unix.in_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let stats = Unix.fstat fd and after = Unix.lstat path in
    if stats.Unix.st_kind <> Unix.S_REG || stats.Unix.st_size > 65_536 ||
       not (same_file before stats && same_file stats after) then
      invalid_arg "settings file changed or exceeds 64 KiB";
    parse (really_input_string input stats.Unix.st_size))

let config_home () =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some path when Filename.is_relative path ->
      (Filename.concat (Sys.getenv "HOME") ".config",
       ["Ignoring relative XDG_CONFIG_HOME"])
  | Some path when path <> "" -> path, []
  | _ -> Filename.concat (Sys.getenv "HOME") ".config", []
let merge_tool_approval user project =
  let names = List.sort_uniq String.compare
    (List.map fst user @ List.map fst project) in
  List.filter_map (fun name ->
    let user_policy = List.assoc_opt name user
    and project_policy = List.assoc_opt name project in
    let policy = match user_policy, project_policy with
      | Some Approval.Deny, _ | _, Some Approval.Deny ->
          Some Approval.Deny
      | _, Some policy -> Some policy
      | Some policy, None -> Some policy
      | None, None -> None in
    Option.map (fun policy -> name, policy) policy) names

let load ~root =
  let user_home, diagnostics = config_home () in
  let diagnostics = ref diagnostics in
  let user_file = Filename.concat (Filename.concat user_home "pave") "settings.json" in
  let project_file = Filename.concat (Filename.concat root ".pave") "settings.json" in
  let attempt path =
    try Some (read path) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | (Unix.Unix_error _ | Sys_error _ | Invalid_argument _ | Yojson.Json_error _) as exn ->
        diagnostics := (path ^ ": " ^ Printexc.to_string exn) :: !diagnostics;
        None in
  let user = attempt user_file in
  let project = try
    let directory = Unix.lstat (Filename.dirname project_file) in
    if directory.Unix.st_kind <> Unix.S_DIR then
      invalid_arg "project .pave must be a real directory";
    attempt project_file
  with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | (Unix.Unix_error _ | Sys_error _ | Invalid_argument _) as exn ->
        diagnostics := (project_file ^ ": " ^ Printexc.to_string exn) :: !diagnostics;
        None in
  let user = Option.value ~default:empty user
  and project = Option.value ~default:empty project in
  { values = {
      default_provider = first_some project.default_provider user.default_provider;
      default_model = (match project.default_provider with
        | Some _ -> project.default_model
        | None -> first_some project.default_model user.default_model);
      default_api = (match project.default_provider with
        | Some _ -> project.default_api
        | None -> first_some project.default_api user.default_api);
      disable_shell = user.disable_shell || project.disable_shell;
      max_turns = first_some project.max_turns user.max_turns;
      approval_mode = first_some project.approval_mode user.approval_mode;
      tool_approval = merge_tool_approval user.tool_approval project.tool_approval;
      command_patterns = project.command_patterns @ user.command_patterns;
    };
    diagnostics = List.rev !diagnostics }

(* Project edits are explicit. Both scopes use the same locked, atomic
   replacement; a pre-existing settings symlink is never followed. *)
let update_file ?(require_owner = false) ~directory change =
  (try Unix.mkdir directory 0o700 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let stat = Unix.lstat directory in
  if stat.Unix.st_kind <> Unix.S_DIR ||
     (require_owner && stat.Unix.st_uid <> Unix.geteuid ()) then
    invalid_arg "settings directory must be an owned real directory";
  let lock_path = Filename.concat directory "settings.lock" in
  let previous = try Some (Unix.lstat lock_path) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None in
  (match previous with
   | Some stat when stat.Unix.st_kind <> Unix.S_REG ->
       invalid_arg "settings lock must not be a symlink"
   | _ -> ());
  let lock = Unix.openfile lock_path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_NONBLOCK]
    0o600 in
  Fun.protect ~finally:(fun () -> Unix.close lock) (fun () ->
    let stat = Unix.fstat lock and current = Unix.lstat lock_path in
    if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_nlink <> 1 ||
       stat.Unix.st_uid <> Unix.geteuid () ||
       not (same_file stat current) ||
       (match previous with Some prior -> not (same_file prior stat)
        | None -> false) then
      invalid_arg "settings lock must be an owned regular file";
    Unix.lockf lock Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf lock Unix.F_ULOCK 0) (fun () ->
      let path = Filename.concat directory "settings.json" in
      let current = try read path with
        | Unix.Unix_error (Unix.ENOENT, _, _) -> empty in
      let updated = change current in
      let option name = function
        | None -> []
        | Some value -> [name, `String value] in
      let tools_fields =
        (match updated.approval_mode with
         | None -> []
         | Some mode -> ["approvalMode", `String (Approval.string_of_mode mode)]) @
        (if updated.tool_approval = [] then [] else
          ["approval", `Assoc (List.map (fun (name, policy) ->
            name, `String (Approval.string_of_policy policy))
            updated.tool_approval)]) @
        (if updated.command_patterns = [] then [] else
          ["commandPatterns", `List (List.map (fun (rule : Approval.command_rule) ->
            `Assoc ["match", `String rule.match_text;
              "approval", `String (Approval.string_of_policy rule.policy)])
            updated.command_patterns)]) in
      let fields =
        option "default_provider" updated.default_provider
        @ option "default_model" updated.default_model
        @ option "default_api" updated.default_api
        @ ["disable_shell", `Bool updated.disable_shell]
        @ (match updated.max_turns with None -> []
          | Some count -> ["max_turns", `Int count])
        @ (if tools_fields = [] then [] else
          ["tools", `Assoc tools_fields]) in
      let text = Yojson.Basic.to_string (`Assoc fields) ^ "\n" in
      ignore (parse text);
      let temp, output = Filename.open_temp_file ~mode:[Open_binary]
        ~temp_dir:directory "settings-" ".tmp" in
      Fun.protect ~finally:(fun () ->
        close_out_noerr output;
        try Sys.remove temp with Sys_error _ -> ()) (fun () ->
        Unix.chmod temp 0o600;
        output_string output text;
        flush output;
        Unix.fsync (Unix.descr_of_out_channel output);
        close_out output;
        Unix.rename temp path;
        let directory_fd = Unix.openfile directory [Unix.O_RDONLY] 0 in
        Fun.protect ~finally:(fun () -> Unix.close directory_fd) (fun () ->
          Unix.fsync directory_fd);
        updated)))

let update_project ~root change =
  let directory = Filename.concat root ".pave" in
  update_file ~directory change

let user_directory () =
  let home, diagnostics = config_home () in
  if diagnostics <> [] || Filename.is_relative home then
    invalid_arg "XDG_CONFIG_HOME must be absolute to save user settings";
  let rec ensure_directory path =
    if path <> Filename.dirname path then (
      (try
         let stat = Unix.lstat path in
         if stat.Unix.st_kind <> Unix.S_DIR then
           invalid_arg "user config path must be a real directory"
       with Unix.Unix_error (Unix.ENOENT, _, _) ->
         ensure_directory (Filename.dirname path);
         Unix.mkdir path 0o700)) in
  ensure_directory home;
  let directory = Filename.concat home "pave" in
  (try Unix.mkdir directory 0o700 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let stat = Unix.lstat directory in
  if stat.Unix.st_kind <> Unix.S_DIR ||
     stat.Unix.st_uid <> Unix.geteuid () then
    invalid_arg "user settings directory must be an owned real directory";
  directory

let update_user change =
  update_file ~require_owner:true ~directory:(user_directory ()) change
