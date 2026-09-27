(* Local Microsoft Entra token retrieval is pinned to Azure CLI. No shell is
   involved and endpoint-derived input is used only to select one of the two
   documented, fixed OAuth scopes.

   Sources:
   https://learn.microsoft.com/en-us/azure/foundry/foundry-models/how-to/configure-entra-id
   https://learn.microsoft.com/en-us/rest/api/microsoftfoundry/azureopenai/responses
   https://learn.microsoft.com/en-us/cli/azure/account?view=azure-cli-latest *)
exception Authentication_error of string
exception Cancelled

let fail message = raise (Authentication_error message)
let max_output_bytes = 16 * 1024
let max_command_output_bytes = 4 * 1024 * 1024
let default_timeout = 10.

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let check_cancel = function
  | Some cancel when cancel () -> raise Cancelled
  | _ -> ()

let check_deadline ~cancel ~deadline =
  check_cancel cancel;
  if Unix.gettimeofday () >= deadline then
    fail "Azure CLI command exceeded its deadline"

let run_az ?timeout ?cancel arguments () =
  let timeout = Option.value ~default:default_timeout timeout in
  if not (timeout > 0. && timeout <= 30.) then
    fail "invalid Azure CLI command deadline";
  if arguments = [] ||
     List.exists (fun argument -> String.contains argument '\000' ||
       String.length argument > 8192) arguments then
    fail "invalid Azure CLI command arguments";
  let deadline = Unix.gettimeofday () +. timeout in
  check_deadline ~cancel ~deadline;
  let input = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let errors = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
  let output_read, output_write = Unix.pipe () in
  let pid =
    try
      Unix.set_close_on_exec output_read;
      let argv = Array.of_list ("az" :: arguments) in
      Unix.create_process "az" argv input output_write errors
    with exn ->
      List.iter close_fd [input; errors; output_read; output_write];
      (match exn with
       | Unix.Unix_error _ ->
           fail "Azure CLI is required for local cloud identity"
       | _ -> raise exn) in
  List.iter close_fd [input; errors; output_write];
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
    close_fd output_read;
    if not !reaped then (
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())))
    (fun () ->
      let output = Buffer.create 256 in
      let chunk = Bytes.create 4096 in
      let rec read_output () =
        check_deadline ~cancel ~deadline;
        let remaining = deadline -. Unix.gettimeofday () in
        let ready, _, _ =
          Unix.select [output_read] [] [] (min 0.05 (max 0. remaining)) in
        if ready = [] then read_output ()
        else
          let count = Unix.read output_read chunk 0 (Bytes.length chunk) in
          if count = 0 then ()
          else if Buffer.length output + count > max_command_output_bytes then
            fail "Azure CLI output exceeded the size limit"
          else (Buffer.add_subbytes output chunk 0 count; read_output ()) in
      read_output ();
      let rec await () =
        check_deadline ~cancel ~deadline;
        match Unix.waitpid [Unix.WNOHANG] pid with
        | 0, _ -> ignore (Unix.select [] [] [] 0.05); await ()
        | _, status -> reaped := true; status in
      match await () with
      | Unix.WEXITED 0 -> Buffer.contents output
      | _ -> fail "Azure CLI command failed")

let trim_cli_newline output =
  let length = String.length output in
  if length >= 2 && String.sub output (length - 2) 2 = "\r\n" then
    String.sub output 0 (length - 2)
  else if length > 0 && output.[length - 1] = '\n' then
    String.sub output 0 (length - 1)
  else output

let valid_token token =
  token <> "" && String.length token <= max_output_bytes &&
  String.for_all (fun c -> Char.code c >= 33 && Char.code c <= 126) token

let access_token ~endpoint ?timeout ?cancel () =
  let cloud =
    try Azure_wire.endpoint_kind ~endpoint
    with Invalid_argument _ ->
      fail "Azure endpoint is not trusted for Microsoft Entra authentication" in
  let scope = match cloud with
    | Azure_wire.Azure_openai_cloud -> "https://cognitiveservices.azure.com/.default"
    | Azure_wire.Foundry_cloud -> "https://ai.azure.com/.default" in
  let output = run_az ?timeout ?cancel
    ["account"; "get-access-token"; "--scope"; scope;
     "--query"; "accessToken"; "--output"; "tsv"] () in
  let token = trim_cli_newline output in
  if not (valid_token token) then
    fail "Azure CLI returned an invalid Microsoft Entra access token";
  token
