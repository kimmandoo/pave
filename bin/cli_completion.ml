type option_spec = { name : string; takes_value : bool; choices : string list }

let filter_candidates ~prefix candidates =
  let prefix_length = String.length prefix in
  List.filter (fun candidate ->
    String.length candidate >= prefix_length &&
    String.sub candidate 0 prefix_length = prefix) candidates

let shell_quote value =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' value) ^ "'"

let fish_quote value =
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '\'';
  String.iter (fun char ->
    if char = '\\' || char = '\'' then Buffer.add_char buffer '\\';
    Buffer.add_char buffer char) value;
  Buffer.add_char buffer '\'';
  Buffer.contents buffer

let words values = String.concat " " (List.map shell_quote values)

let enum_cases options ~shell =
  List.concat_map (fun option ->
    if option.choices = [] then []
    else
      let choices = String.concat " " option.choices in
      let case = match shell with
        | "bash" ->
            "    " ^ shell_quote option.name ^ ") COMPREPLY=( $(compgen -W " ^
            shell_quote choices ^ " -- \"$cur\") ); return ;;\n"
        | "zsh" ->
            "    " ^ shell_quote option.name ^ ") compadd -- " ^
            words option.choices ^ "; return ;;\n"
        | _ -> assert false in
      [case]) options
  |> String.concat ""

let value_cases options =
  options
  |> List.filter (fun option -> option.takes_value &&
    option.choices = [] && option.name <> "--model" &&
    option.name <> "--session")
  |> List.map (fun option -> "    " ^ shell_quote option.name ^
    ") return ;;\n")
  |> String.concat ""

let generate_bash ~executable ~options ~task_operations =
  let flags = List.map (fun option -> option.name) options in
  let static_flags = words flags in
  let top_commands = words ["task"; "update"; "completions"] in
  let dynamic kind =
    "    --" ^ kind ^ ") while IFS= read -r candidate; do " ^
    "COMPREPLY+=(\"$candidate\"); done < <(" ^ shell_quote executable ^
    " __complete " ^ kind ^ " \"$cur\"); return ;;\n" in
  "_pave_completion() {\n" ^
  "  local cur prev candidate\n" ^
  "  cur=\"${COMP_WORDS[COMP_CWORD]}\"\n" ^
  "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n" ^
  "  if [[ ${COMP_WORDS[1]} == task ]]; then\n" ^
  "    if [[ $COMP_CWORD -eq 2 ]]; then\n" ^
  "      COMPREPLY=( $(compgen -W " ^ words task_operations ^
  " -- \"$cur\") )\n      return\n    fi\n    return\n  fi\n" ^
  "  case \"$prev\" in\n" ^ dynamic "model" ^ dynamic "session" ^
  "    completions) COMPREPLY=( $(compgen -W 'bash zsh fish' -- \"$cur\") ); return ;;\n" ^
  enum_cases options ~shell:"bash" ^ value_cases options ^ "  esac\n" ^
  "  if [[ \"$cur\" == -* ]]; then\n" ^
  "    COMPREPLY=( $(compgen -W " ^ static_flags ^ " -- \"$cur\") )\n" ^
  "  else\n    COMPREPLY=( $(compgen -W " ^ top_commands ^ " -- \"$cur\") )\n  fi\n" ^
  "}\ncomplete -F _pave_completion " ^ shell_quote executable ^ "\n"

let generate_zsh ~executable ~options ~task_operations =
  let flags = List.map (fun option -> option.name) options in
  let static_flags = words flags in
  let dynamic kind =
    "    --" ^ kind ^ ") candidates=(${(f)\"$(" ^ shell_quote executable ^
    " __complete " ^ kind ^ " \"$cur\")\"}); compadd -Q -- \"${candidates[@]}\"; return ;;\n" in
  "_pave_completion() {\n" ^
  "  local cur=${words[CURRENT]} prev=${words[CURRENT-1]} candidates\n" ^
  "  if [[ ${words[2]} == task ]]; then\n" ^
  "    if [[ $CURRENT -eq 3 ]]; then compadd -- " ^
  words task_operations ^ "; fi\n    return\n  fi\n" ^
  "  case \"$prev\" in\n" ^ dynamic "model" ^ dynamic "session" ^
  "    completions) compadd -- bash zsh fish; return ;;\n" ^
  enum_cases options ~shell:"zsh" ^ value_cases options ^ "  esac\n" ^
  "  if [[ \"$cur\" == -* ]]; then compadd -- " ^ static_flags ^
  "\n  else compadd -- task update completions\n  fi\n}\n" ^
  "compdef _pave_completion " ^ shell_quote executable ^ "\n"

let fish_option name =
  if String.starts_with ~prefix:"--" name then
    " -l " ^ fish_quote (String.sub name 2 (String.length name - 2))
  else " -o " ^ fish_quote (String.sub name 1 (String.length name - 1))

let generate_fish ~executable ~options ~task_operations =
  let base = "complete -c " ^ fish_quote executable ^ " -f" in
  let top = "__fish_use_subcommand" in
  let static = List.concat_map (fun option ->
    let flag = base ^ fish_option option.name in
    let valued = flag ^ (if option.takes_value then " -r" else "") ^
      " -n " ^ fish_quote top in
    let choices = List.map (fun choice ->
      flag ^ " -a " ^ fish_quote choice ^ " -n " ^
      fish_quote (top ^ "; __fish_seen_argument " ^ option.name))
      option.choices in
    let dynamic = match option.name with
      | "--model" | "--session" ->
          let kind = String.sub option.name 2 (String.length option.name - 2) in
          [flag ^ " -r -a (" ^ fish_quote executable ^
            " __complete " ^ fish_quote kind ^
            " (commandline -ct)) -n " ^ fish_quote top]
      | _ -> [] in
    valued :: (choices @ dynamic)) options in
  let subcommands = [
    base ^ " -a " ^ fish_quote "task update completions" ^ " -n " ^
      fish_quote top;
    base ^ " -a " ^ fish_quote (String.concat " " task_operations) ^
      " -n " ^ fish_quote "__fish_seen_subcommand_from task";
    base ^ " -a " ^ fish_quote "bash zsh fish" ^ " -n " ^
      fish_quote "__fish_seen_subcommand_from completions";
    base ^ " -a " ^ fish_quote "--check" ^ " -n " ^
      fish_quote "__fish_seen_subcommand_from update"
  ] in
  String.concat "\n" (static @ subcommands) ^ "\n"

let generate ~shell ~executable ~options ~task_operations =
  match shell with
  | "bash" -> generate_bash ~executable ~options ~task_operations
  | "zsh" -> generate_zsh ~executable ~options ~task_operations
  | "fish" -> generate_fish ~executable ~options ~task_operations
  | _ -> invalid_arg ("unsupported completion shell: " ^ shell)
