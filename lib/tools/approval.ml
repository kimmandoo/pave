type tier = Read | Write | Exec

(* Where a permission demand originates, mirroring the reference
   PermissionInteractionSpec.TriggerSource vocabulary. *)
type trigger = Tool_call | Dangerous_command | File_access | Network

type mode = Ask_writes | Ask_exec | Auto_all

type policy = Allow | Deny | Prompt

type decision = {
  tier : tier;
  policy : policy option;
  override : bool;
  reason : string option;
}

type command_rule = { match_text : string; policy : policy }

type resolution = Allowed | Denied of string | Requires_prompt of string option

type request = {
  tool_name : string;
  tier : tier;
  trigger : trigger;
  impact : string;
  details : string list;
  reason : string option;
}

let mode_of_string = function
  | "always-ask" -> Some Ask_writes
  | "write" -> Some Ask_exec
  | "yolo" -> Some Auto_all
  | _ -> None

let string_of_mode = function
  | Ask_writes -> "always-ask"
  | Ask_exec -> "write"
  | Auto_all -> "yolo"

let policy_of_string value = match String.lowercase_ascii (String.trim value) with
  | "allow" -> Some Allow
  | "deny" -> Some Deny
  | "prompt" -> Some Prompt
  | _ -> None

let string_of_policy = function
  | Allow -> "allow"
  | Deny -> "deny"
  | Prompt -> "prompt"

let normalize text =
  let output = Buffer.create (String.length text) in
  let pending_space = ref false in
  String.iter (fun character ->
    if character = ' ' || character = '\t' || character = '\n' || character = '\r' then
      pending_space := Buffer.length output > 0
    else (
      if !pending_space then Buffer.add_char output ' ';
      pending_space := false;
      Buffer.add_char output character)) text;
  Buffer.contents output

let glob_matches pattern text =
  let pattern_length = String.length pattern
  and text_length = String.length text in
  let pattern_index = ref 0 and text_index = ref 0 in
  let star_index = ref (-1) and star_text_index = ref 0 in
  while !text_index < text_length do
    if !pattern_index < pattern_length &&
       (pattern.[!pattern_index] = text.[!text_index]) then (
      incr pattern_index;
      incr text_index)
    else if !pattern_index < pattern_length && pattern.[!pattern_index] = '*' then (
      star_index := !pattern_index;
      incr pattern_index;
      star_text_index := !text_index)
    else if !star_index >= 0 then (
      pattern_index := !star_index + 1;
      incr star_text_index;
      text_index := !star_text_index)
    else text_index := text_length + 1
  done;
  if !text_index > text_length then false
  else (
    while !pattern_index < pattern_length && pattern.[!pattern_index] = '*' do
      incr pattern_index
    done;
    !pattern_index = pattern_length)

let contains_glob pattern text =
  glob_matches ("*" ^ pattern ^ "*") (normalize text)

let shell_segments command =
  let segments = ref [] and current = Buffer.create 64 in
  let quote = ref None and escaped = ref false in
  let push () =
    let segment = normalize (Buffer.contents current) in
    if segment <> "" then segments := segment :: !segments;
    Buffer.clear current in
  let rec scan index =
    if index < String.length command then (
      let character = command.[index] in
      if !escaped then (
        Buffer.add_char current character;
        escaped := false;
        scan (index + 1))
      else match !quote with
      | Some '\'' ->
          if character = '\'' then quote := None
          else Buffer.add_char current character;
          scan (index + 1)
      | Some '"' ->
          if character = '"' then quote := None
          else if character = '\\' && index + 1 < String.length command then
            escaped := true
          else Buffer.add_char current character;
          scan (index + 1)
      | _ ->
          (match character with
           | '\'' | '"' -> quote := Some character
           | '\\' -> escaped := true
           | ';' | '&' | '|' | '\n' | '(' | ')' ->
               push ();
               if index + 1 < String.length command &&
                  ((character = '&' && command.[index + 1] = '&') ||
                   (character = '|' && command.[index + 1] = '|'))
               then scan (index + 2) else scan (index + 1)
           | _ -> Buffer.add_char current character; scan (index + 1))) in
  scan 0;
  if !quote <> None || !escaped then None
  else (push (); Some (List.rev !segments))

let command_rule_matches command segments rule =
  let pattern = normalize rule.match_text in
  match rule.policy with
  | Allow ->
      (match segments with
       | [segment] -> glob_matches pattern segment
       | _ -> false)
  | Prompt ->
      glob_matches pattern (normalize command) ||
      List.exists (glob_matches pattern) segments
  | Deny ->
      glob_matches pattern (normalize command) ||
      List.exists (fun segment -> contains_glob pattern segment) segments ||
      contains_glob pattern command

let command_decision rules command =
  let segments = Option.value ~default:[] (shell_segments command) in
  let matching policy = List.find_opt (fun rule ->
    rule.policy = policy && command_rule_matches command segments rule) rules in
  match matching Deny with
  | Some rule -> { tier = Exec; policy = Some Deny; override = true;
      reason = Some ("Blocked by command policy: " ^ rule.match_text) }
  | None ->
      (match matching Prompt with
       | Some rule -> { tier = Exec; policy = Some Prompt; override = true;
           reason = Some ("Prompt required by command policy: " ^ rule.match_text) }
       | None ->
           (match matching Allow with
            | Some _ -> { tier = Write; policy = Some Allow;
                override = false; reason = None }
            | None -> { tier = Exec; policy = None; override = false;
                reason = None }))

let mode_approves mode tier = match mode, tier with
  | Ask_writes, Read | Ask_exec, (Read | Write) | Auto_all, _ -> true
  | _ -> false

let resolve ~mode ~(decision : decision) ~user_policy =
  if decision.policy = Some Deny then
    Denied (Option.value ~default:"Blocked by tool policy" decision.reason)
  else if user_policy = Some Deny then
    Denied "Blocked by user policy (tools.approval)"
  else
    let policy =
      if mode = Auto_all then
        match decision.policy, user_policy with
        | Some policy, _ -> policy
        | None, Some policy -> policy
        | None, None -> Allow
      else if decision.override then
        if decision.policy = Some Allow then Allow else Prompt
      else match decision.policy with
        | Some policy -> policy
        | None -> (match user_policy with
            | Some policy -> policy
            | None -> if mode_approves mode decision.tier then Allow else Prompt)
    in
    match policy with
    | Allow -> Allowed
    | Deny -> Denied (Option.value ~default:"Blocked by policy" decision.reason)
    | Prompt -> Requires_prompt decision.reason

let tier_name = function Read -> "read" | Write -> "write" | Exec -> "exec"

(* Line prompts accept y/yes in any case, plus ㅛ, the y key under a Korean
   two-set input method; anything else, including end of input, denies. *)
let confirmed_answer = function
  | None -> false
  | Some line ->
      match String.lowercase_ascii (String.trim line) with
      | "y" | "yes" | "\xe3\x85\x9b" -> true
      | _ -> false

type answer = Deny_once | Allow_once | Allow_for_session | Allow_always

(* Only reviewable, low-blast-radius tools may be allowed for the rest of the
   run; shell, process, device, remote, external and child-agent actions keep
   their mandatory per-call approval. *)
let session_grantable tool_name =
  List.mem tool_name [
    "web_search"; "web_fetch"; "write_file"; "edit_file"; "apply_edits";
    "ast_edit"; "image_ocr" ]

(* Only an exact, fully reviewed shell command may be remembered across runs;
   the persisted rule matches the literal normalized command text. *)
let always_grantable tool_name =
  List.mem tool_name ["run_command"; "start_shell"]

let trigger_name = function
  | Tool_call -> "tool call"
  | Dangerous_command -> "dangerous command"
  | File_access -> "file access"
  | Network -> "network"

(* Line prompts add `a`/`all` (or ㅁ, the a key under a Korean two-set input
   method) for a session grant where one is offered. *)
let answer_of_line ~session ?(always = false) = function
  | None -> Deny_once
  | Some line ->
      match String.lowercase_ascii (String.trim line) with
      | "y" | "yes" | "\xe3\x85\x9b" -> Allow_once
      | ("a" | "all" | "\xe3\x85\x81") when session -> Allow_for_session
      | ("w" | "always") when always -> Allow_always
      | _ -> Deny_once
