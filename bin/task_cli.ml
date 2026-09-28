let operations = ["embed"; "image"; "speak"; "transcribe"; "rerank"]

let operation_options operation =
  let specific = match operation with
    | "embed" -> ["--input"]
    | "image" -> ["--prompt"; "--output"]
    | "speak" -> ["--input"; "--voice"; "--output"]
    | "transcribe" -> ["--file"]
    | "rerank" -> ["--query"; "--document"; "--top-n"]
    | _ -> [] in
  if List.mem operation operations then
    ["--model"; "--root"] @ specific else []


let general_help =
  "Usage: pave task OPERATION --model EXACT_ID [options]\n\n" ^
  "Non-chat API tasks (not provider chat routes):\n" ^
  "  embed         Create one OpenAI embedding; JSON vector to stdout\n" ^
  "  image         Generate one OpenAI PNG; write a new workspace file\n" ^
  "  speak         Create OpenAI WAV speech; write a new workspace file\n" ^
  "  transcribe    Transcribe an OpenAI-supported audio file; text to stdout\n" ^
  "  rerank        Rerank Cohere documents; JSON results to stdout\n\n" ^
  "Use pave task OPERATION --help for required task-specific options.\n" ^
  "Model access and entitlement are controlled by the provider account.\n" ^
  "OpenAI video generation is unavailable: the Videos API/Sora was shut down,\n" ^
  "and OpenAI documents no replacement endpoint."

let help operation =
  match operation with
  | "embed" ->
      "Usage: pave task embed --model ID --input TEXT [--root DIR]\n" ^
      "Requires OPENAI_API_KEY. Prints one validated embedding response as JSON."
  | "image" ->
      "Usage: pave task image --model ID --prompt TEXT --output PATH [--root DIR]\n" ^
      "Requires OPENAI_API_KEY. Writes a single 1024x1024 PNG to a new workspace-relative .png file."
  | "speak" ->
      "Usage: pave task speak --model ID --input TEXT --voice VOICE --output PATH [--root DIR]\n" ^
      "Requires OPENAI_API_KEY. VOICE is alloy, ash, ballad, coral, echo, fable, onyx, nova, sage, shimmer, verse, marin, or cedar; writes a new workspace-relative WAV file."
  | "transcribe" ->
      "Usage: pave task transcribe --model ID --file PATH [--root DIR]\n" ^
      "Requires OPENAI_API_KEY. Reads a workspace-relative audio file (up to 25 MB) and prints transcript text."
  | "rerank" ->
      "Usage: pave task rerank --model ID --query TEXT --document TEXT... [--top-n N] [--root DIR]\n" ^
      "Requires COHERE_API_KEY. Repeat --document for each input; prints validated ranked JSON results."
  | _ -> general_help

type options = {
  mutable model : string option;
  mutable root : string;
  mutable root_supplied : bool;
  mutable input : string option;
  mutable prompt : string option;
  mutable output : string option;
  mutable voice : string option;
  mutable file : string option;
  mutable query : string option;
  mutable documents : string list;
  mutable top_n : int option;
}

let new_options () = {
  model = None; root = "."; root_supplied = false; input = None; prompt = None;
  output = None; voice = None; file = None; query = None; documents = []; top_n = None;
}

let take_value args index option =
  if index + 1 >= Array.length args then failwith (option ^ " requires a value");
  let value = args.(index + 1) in
  value

let parse args options =
  let rec loop index =
    if index >= Array.length args then ()
    else
      let option = args.(index) in
      let value = take_value args index option in
      (match option with
       | "--model" ->
           if Option.is_some options.model then failwith "--model may be supplied only once";
           options.model <- Some value
       | "--root" ->
           if options.root_supplied then failwith "--root may be supplied only once";
           options.root <- value;
           options.root_supplied <- true
       | "--input" ->
           if Option.is_some options.input then failwith "--input may be supplied only once";
           options.input <- Some value
       | "--prompt" ->
           if Option.is_some options.prompt then failwith "--prompt may be supplied only once";
           options.prompt <- Some value
       | "--output" ->
           if Option.is_some options.output then failwith "--output may be supplied only once";
           options.output <- Some value
       | "--voice" ->
           if Option.is_some options.voice then failwith "--voice may be supplied only once";
           options.voice <- Some value
       | "--file" ->
           if Option.is_some options.file then failwith "--file may be supplied only once";
           options.file <- Some value
       | "--query" ->
           if Option.is_some options.query then failwith "--query may be supplied only once";
           options.query <- Some value
       | "--document" ->
           if List.length options.documents = 1000 then failwith "at most 1000 --document values are supported";
           options.documents <- options.documents @ [value]
       | "--top-n" ->
           if Option.is_some options.top_n then failwith "--top-n may be supplied only once";
           (match int_of_string_opt value with
            | Some n -> options.top_n <- Some n
            | None -> failwith "--top-n must be a positive integer")
       | _ -> failwith ("unknown task option: " ^ option));
      loop (index + 2)
  in
  loop 1

let required name = function
  | Some value when value <> "" -> value
  | _ -> failwith (name ^ " is required")

let ensure_only used options =
  let allowed name = List.mem name used in
  let check name present = if present && not (allowed name) then failwith (name ^ " is not used by this task") in
  check "--input" (Option.is_some options.input);
  check "--prompt" (Option.is_some options.prompt);
  check "--output" (Option.is_some options.output);
  check "--voice" (Option.is_some options.voice);
  check "--file" (Option.is_some options.file);
  check "--query" (Option.is_some options.query);
  check "--document" (options.documents <> []);
  check "--top-n" (Option.is_some options.top_n)

let execute operation options cancelled =
  let model = required "--model" options.model in
  ensure_only (operation_options operation) options;
  let cancel () = !cancelled in
  let result = match operation with
    | "embed" ->
        Pave.Non_chat.embed ~cancel ~key:(Option.value ~default:"" (Sys.getenv_opt "OPENAI_API_KEY"))
          ~model ~input:(required "--input" options.input) ()
    | "image" ->
        Pave.Non_chat.generate_image ~cancel
          ~key:(Option.value ~default:"" (Sys.getenv_opt "OPENAI_API_KEY")) ~model
          ~root:options.root ~prompt:(required "--prompt" options.prompt)
          ~output:(required "--output" options.output) ()
    | "speak" ->
        Pave.Non_chat.speak ~cancel
          ~key:(Option.value ~default:"" (Sys.getenv_opt "OPENAI_API_KEY")) ~model
          ~root:options.root ~input:(required "--input" options.input)
          ~voice:(required "--voice" options.voice)
          ~output:(required "--output" options.output) ()
    | "transcribe" ->
        Pave.Non_chat.transcribe ~cancel
          ~key:(Option.value ~default:"" (Sys.getenv_opt "OPENAI_API_KEY")) ~model
          ~root:options.root ~input:(required "--file" options.file) ()
    | "rerank" ->
        Pave.Non_chat.rerank ~cancel
          ~key:(Option.value ~default:"" (Sys.getenv_opt "COHERE_API_KEY")) ~model
          ~query:(required "--query" options.query) ~documents:options.documents
          ~top_n:options.top_n ()
    | _ -> failwith ("unknown task operation " ^ operation ^ "\n\n" ^ general_help) in
  match result with
  | Ok output -> print_string output
  | Error error -> failwith (Pave.Non_chat.message error)

let run args =
  if Array.length args = 0 || args.(0) = "--help" || args.(0) = "-h" then
    print_endline general_help
  else
    let operation = args.(0) in
    if Array.length args > 1 && (args.(1) = "--help" || args.(1) = "-h") then
      print_endline (help operation)
    else if not (List.mem operation operations) then
      failwith ("unknown task operation " ^ operation ^ "\n\n" ^ general_help)
    else
      let options = new_options () in
      parse args options;
      let cancelled = ref false in
      let previous = Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> cancelled := true)) in
      Fun.protect ~finally:(fun () -> ignore (Sys.signal Sys.sigint previous))
        (fun () -> execute operation options cancelled)
