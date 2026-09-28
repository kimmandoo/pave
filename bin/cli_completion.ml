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

let task_cases ~task_operations ~task_options ~shell =
  List.map (fun operation ->
    let flags = task_options operation in
    let candidates = match shell with
      | "bash" -> "COMPREPLY=( $(compgen -W " ^
          shell_quote (String.concat " " flags) ^ " -- \"$cur\") )"
      | "zsh" -> "compadd -- " ^ words flags
      | _ -> assert false in
    let values = List.map (fun flag ->
      let choices = Task_cli.option_choices operation flag in
      let completion = match shell, choices with
        | "bash", _ :: _ -> "COMPREPLY=( $(compgen -W " ^
            shell_quote (String.concat " " choices) ^ " -- \"$cur\") ); "
        | "zsh", _ :: _ -> "compadd -- " ^ words choices ^ "; "
        | _, [] -> ""
        | _ -> assert false in
      "          " ^ shell_quote flag ^ ") " ^ completion ^ "return ;;\n")
      flags |> String.concat "" in
    "      " ^ shell_quote operation ^ ")\n" ^
    "        case \"$prev\" in\n" ^ values ^ "        esac\n" ^
    "        if [[ \"$cur\" == -* ]]; then " ^ candidates ^ "; fi\n" ^
    "        return ;;\n")
    task_operations
  |> String.concat ""

let generate_bash ~executable ~options ~task_operations ~task_options =
  let flags = List.map (fun option -> option.name) options in
  let static_flags = shell_quote (String.concat " " flags) in
  let top_commands = shell_quote "task update completions" in
  let dynamic kind =
    "    --" ^ kind ^ ") while IFS= read -r candidate; do " ^
    "COMPREPLY+=(\"$candidate\"); done < <(" ^ shell_quote executable ^
    " __complete " ^ kind ^ " \"$cur\" \"$root\"); " ^
    (if kind = "model" then
      "if [[ $COMP_WORDBREAKS == *'@'* && $cur == *@* ]]; then " ^
      "local word_prefix=${cur%${cur##*@}}; " ^
      "for ((i=0; i<${#COMPREPLY[@]}; i++)); do " ^
      "COMPREPLY[i]=${COMPREPLY[i]#\"$word_prefix\"}; done; fi; "
     else "") ^
    "compopt -o filenames; return ;;\n" in
  "_pave_completion() {\n" ^
  "  local cur prev candidate root=. i\n" ^
  "  cur=\"${COMP_WORDS[COMP_CWORD]}\"\n" ^
  "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n" ^
  "  for ((i=1; i+1<COMP_CWORD; i++)); do\n" ^
  "    if [[ ${COMP_WORDS[i]} == --root ]]; then root=${COMP_WORDS[i+1]}; fi\n" ^
  "  done\n" ^
  "  if [[ ${COMP_WORDS[1]} == task ]]; then\n" ^
  "    if [[ $COMP_CWORD -eq 2 ]]; then\n" ^
  "      COMPREPLY=( $(compgen -W " ^
  shell_quote (String.concat " " task_operations) ^
  " -- \"$cur\") )\n      return\n    fi\n" ^
  "    case \"${COMP_WORDS[2]}\" in\n" ^
  task_cases ~task_operations ~task_options ~shell:"bash" ^
  "    esac\n    return\n  fi\n" ^
  "  if [[ ${COMP_WORDS[1]} == update ]]; then\n" ^
  "    if [[ $COMP_CWORD -eq 2 ]]; then\n" ^
  "      COMPREPLY=( $(compgen -W '--check' -- \"$cur\") )\n    fi\n" ^
  "    return\n  fi\n" ^
  "  if [[ ${COMP_WORDS[1]} == completions ]]; then\n" ^
  "    if [[ $COMP_CWORD -eq 2 ]]; then\n" ^
  "      COMPREPLY=( $(compgen -W 'bash zsh fish' -- \"$cur\") )\n    fi\n" ^
  "    return\n  fi\n" ^
  "  case \"$prev\" in\n" ^ dynamic "model" ^ dynamic "session" ^
  enum_cases options ~shell:"bash" ^ value_cases options ^ "  esac\n" ^
  "  if [[ \"$cur\" == -* ]]; then\n" ^
  "    COMPREPLY=( $(compgen -W " ^ static_flags ^ " -- \"$cur\") )\n" ^
  "  else\n    COMPREPLY=( $(compgen -W " ^ top_commands ^ " -- \"$cur\") )\n  fi\n" ^
  "}\ncomplete -F _pave_completion " ^ shell_quote executable ^ "\n"

let generate_zsh ~executable ~options ~task_operations ~task_options =
  let flags = List.map (fun option -> option.name) options in
  let static_flags = words flags in
  let dynamic kind =
    "    --" ^ kind ^ ") candidates=(${(f)\"$(" ^ shell_quote executable ^
    " __complete " ^ kind ^ " \"$cur\" \"$root\")\"}); " ^
    "compadd -- \"${candidates[@]}\"; return ;;\n" in
  "_pave_completion() {\n" ^
  "  local cur=${words[CURRENT]} prev=${words[CURRENT-1]} candidates root=. i\n" ^
  "  for ((i=2; i+1<CURRENT; i++)); do\n" ^
  "    if [[ ${words[i]} == --root ]]; then root=${words[i+1]}; fi\n" ^
  "  done\n" ^
  "  if [[ ${words[2]} == task ]]; then\n" ^
  "    if [[ $CURRENT -eq 3 ]]; then compadd -- " ^
  words task_operations ^ "; return; fi\n" ^
  "    case \"${words[3]}\" in\n" ^
  task_cases ~task_operations ~task_options ~shell:"zsh" ^
  "    esac\n    return\n  fi\n" ^
  "  if [[ ${words[2]} == update ]]; then\n" ^
  "    if [[ $CURRENT -eq 3 ]]; then compadd -- --check; fi\n" ^
  "    return\n  fi\n" ^
  "  if [[ ${words[2]} == completions ]]; then\n" ^
  "    if [[ $CURRENT -eq 3 ]]; then compadd -- bash zsh fish; fi\n" ^
  "    return\n  fi\n" ^
  "  case \"$prev\" in\n" ^ dynamic "model" ^ dynamic "session" ^
  enum_cases options ~shell:"zsh" ^ value_cases options ^ "  esac\n" ^
  "  if [[ \"$cur\" == -* ]]; then compadd -- " ^ static_flags ^
  "\n  else compadd -- task update completions\n  fi\n}\n" ^
  "compdef _pave_completion " ^ shell_quote executable ^ "\n"

let fish_option name =
  if String.starts_with ~prefix:"--" name then
    " -l " ^ fish_quote (String.sub name 2 (String.length name - 2))
  else " -o " ^ fish_quote (String.sub name 1 (String.length name - 1))

let generate_fish ~executable ~options ~task_operations ~task_options =
  let base = "complete -c " ^ fish_quote executable ^ " -f" in
  let top = "__pave_top" and global = "__pave_global" in
  let valued_options =
    List.filter_map (fun option ->
      if option.takes_value then Some option.name else None) options @
    List.concat_map task_options task_operations
    |> List.sort_uniq String.compare in
  let flag_condition predicate =
    fish_quote (predicate ^ "; and not __pave_value_pending") in
  let static = List.map (fun option ->
    let flag = base ^ fish_option option.name in
    let choices = if option.choices = [] then "" else
      " -a " ^ fish_quote (String.concat " " option.choices) in
    let dynamic = match option.name with
      | "--model" | "--session" ->
          let kind = String.sub option.name 2 (String.length option.name - 2) in
          " -a " ^ fish_quote ("(" ^ executable ^
            " __complete " ^ kind ^ " (commandline -ct) (__pave_root))")
      | _ -> "" in
    flag ^ (if option.takes_value then " -r" else "") ^
    choices ^ dynamic ^ " -n " ^ flag_condition global) options in
  let subcommands = [
    base ^ " -a " ^ fish_quote "task update completions" ^ " -n " ^
      fish_quote top;
    base ^ " -a " ^ fish_quote (String.concat " " task_operations) ^
      " -n " ^ fish_quote "__pave_task_operations";
    base ^ " -a " ^ fish_quote "bash zsh fish" ^ " -n " ^
      fish_quote "__pave_completions";
    base ^ " -a " ^ fish_quote "--check" ^ " -n " ^
      fish_quote "__pave_update"
  ] in
  let task_flags = List.concat_map (fun operation ->
    List.map (fun flag ->
      let choices = Task_cli.option_choices operation flag in
      base ^ fish_option flag ^ " -r" ^
      (if choices = [] then "" else
        " -a " ^ fish_quote (String.concat " " choices)) ^
      " -n " ^ flag_condition ("__pave_task_option " ^ operation))
      (task_options operation)) task_operations in
  let predicates =
    "function __pave_value_pending\n" ^
    "  string match -q -- '-*' (commandline -ct); or return 1\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  switch $tokens[-1]\n" ^
    "    case " ^ String.concat " " (List.map fish_quote valued_options) ^ "\n" ^
    "      return 0\n" ^
    "  end\n" ^
    "  return 1\n" ^
    "end\n" ^
    "function __pave_top\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  test (count $tokens) -eq 1\n" ^
    "end\n" ^
    "function __pave_global\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  test (count $tokens) -lt 2; or not contains -- $tokens[2] task update completions\n" ^
    "end\n" ^
    "function __pave_task_operations\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  test (count $tokens) -eq 2; and test \"$tokens[2]\" = task\n" ^
    "end\n" ^
    "function __pave_task_option\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  test (count $tokens) -ge 3; and test \"$tokens[2]\" = task; and test \"$tokens[3]\" = \"$argv[1]\"\n" ^
    "end\n" ^
    "function __pave_completions\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  test (count $tokens) -eq 2; and test \"$tokens[2]\" = completions\n" ^
    "end\n" ^
    "function __pave_update\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  test (count $tokens) -eq 2; and test \"$tokens[2]\" = update\n" ^
    "end\n" ^
    "function __pave_root\n" ^
    "  set -l tokens (commandline -xpc)\n" ^
    "  set -l root .\n" ^
    "  set -l index 2\n" ^
    "  while test (math $index + 1) -le (count $tokens)\n" ^
    "    if test \"$tokens[$index]\" = --root\n" ^
    "      set root $tokens[(math $index + 1)]\n" ^
    "    end\n" ^
    "    set index (math $index + 1)\n" ^
    "  end\n" ^
    "  echo $root\n" ^
    "end\n" in
  predicates ^ String.concat "\n" (static @ task_flags @ subcommands) ^ "\n"

let generate ~shell ~executable ~options ~task_operations ~task_options =
  match shell with
  | "bash" -> generate_bash ~executable ~options ~task_operations ~task_options
  | "zsh" -> generate_zsh ~executable ~options ~task_operations ~task_options
  | "fish" -> generate_fish ~executable ~options ~task_operations ~task_options
  | _ -> invalid_arg ("unsupported completion shell: " ^ shell)
