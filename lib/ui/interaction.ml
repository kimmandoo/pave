type command = Login of string option | Model of string option | Other

let invalid_argument message = invalid_arg ("interaction: " ^ message)

let is_whitespace_or_control char =
  let code = Char.code char in
  code <= 32 || code = 127

let require_single_argument command argument =
  let argument = String.trim argument in
  if argument = "" then invalid_argument (command ^ " requires a nonempty argument");
  if String.exists is_whitespace_or_control argument then
    invalid_argument (command ^ " accepts only one argument");
  Some argument

let parse line =
  let line = String.trim line in
  let parse_command name constructor =
    let length = String.length name in
    if line = name then Some (constructor None)
    else if String.starts_with ~prefix:name line
            && String.length line > length
            && is_whitespace_or_control line.[length] then
      if line.[length] <> ' ' then
        invalid_argument (name ^ " requires a space before its argument")
      else Some (constructor (require_single_argument name
        (String.sub line (length + 1) (String.length line - length - 1))))
    else None in
  match parse_command "/login" (fun argument -> Login argument) with
  | Some command -> command
  | None -> (match parse_command "/model" (fun argument -> Model argument) with
    | Some command -> command
    | None -> Other)

let selectable_providers () = Provider_catalog.all ()

let resolve_model ~current_provider ~input =
  let input = String.trim input in
  if input = "" || String.exists is_whitespace_or_control input then
    invalid_argument "model selector must be a nonempty single argument";
  let provider_id, model = match String.index_opt input '/' with
    | Some slash ->
      String.sub input 0 slash,
      String.sub input (slash + 1) (String.length input - slash - 1)
    | None -> current_provider, input in
  if model = "" then invalid_argument "model ID must not be empty";
  let descriptor = match Provider_catalog.find provider_id with
    | Some descriptor -> descriptor
    | None -> invalid_argument ("unknown provider: " ^ provider_id) in
  let route = match Provider_catalog.route descriptor ~model "" with
    | Some route -> route
    | None -> invalid_argument ("no route for provider: " ^ provider_id) in
  descriptor, model, route

let history_for_model ~wire ~model messages =
  let retain (message : Protocol.message) =
    match message.provider_state with
    | None -> true
    | Some state ->
        wire = Provider.Codex_responses &&
        Protocol.member "provider" state = `String "openai-codex" &&
        Protocol.member "model" state = `String model in
  if List.for_all retain messages then messages
  else List.map (fun (message : Protocol.message) ->
    if retain message then message else { message with provider_state = None })
    messages
