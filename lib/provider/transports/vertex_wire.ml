open Protocol

let invalid detail = raise (Invalid_response ("invalid Vertex response: " ^ detail))

let valid_segment ~min_length ~max_length ~first ~rest value =
  let length = String.length value in
  length >= min_length && length <= max_length && first value.[0] &&
  String.for_all rest value

let letter = function 'a'..'z' -> true | _ -> false
let digit = function '0'..'9' -> true | _ -> false
let project_id value =
  valid_segment ~min_length:6 ~max_length:30
    ~first:(fun c -> letter c || digit c)
    ~rest:(fun c -> letter c || digit c || c = '-') value &&
  (let c = value.[String.length value - 1] in letter c || digit c)

let location_id value =
  if List.mem value ["global"; "eu"; "us"] then true
  else
    let pieces = String.split_on_char '-' value in
    match List.rev pieces with
    | last :: region :: rest when region <> "" ->
        let n = String.length last in
        n >= 2 && n <= 16 && digit last.[n - 1] &&
        String.for_all (fun c -> letter c || digit c) last &&
        List.for_all (fun part -> part <> "" && String.for_all letter part)
          (region :: rest)
    | _ -> false

let model_id model =
  let model = if String.starts_with ~prefix:"models/" model then
    String.sub model 7 (String.length model - 7) else model in
  if not (valid_segment ~min_length:1 ~max_length:256
    ~first:(fun c -> letter c || digit c)
    ~rest:(fun c -> letter c || digit c ||
      (match c with 'A'..'Z' | '-' | '_' | '.' -> true | _ -> false)) model)
  then invalid_arg "invalid Vertex model ID";
  model

let first_environment keys =
  let rec find = function
    | [] -> None
    | key :: rest ->
        (match Sys.getenv_opt key with
         | Some value when value <> "" -> Some value
         | _ -> find rest) in
  find keys

let resolve_environment () =
  let require label keys = match first_environment keys with
    | Some value -> value
    | None -> invalid_arg ("Vertex requires " ^ label) in
  let project = require "GOOGLE_CLOUD_PROJECT" [
    "GOOGLE_CLOUD_PROJECT"; "GCP_PROJECT"; "GCLOUD_PROJECT" ] in
  let location = require "GOOGLE_VERTEX_LOCATION" [
    "GOOGLE_VERTEX_LOCATION"; "GOOGLE_CLOUD_LOCATION"; "VERTEX_LOCATION" ] in
  if not (project_id project) then invalid_arg "invalid Vertex project ID";
  if not (location_id location) then invalid_arg "invalid Vertex location";
  project, location

let endpoint ~project ~location ~model =
  if not (project_id project) then invalid_arg "invalid Vertex project ID";
  if not (location_id location) then invalid_arg "invalid Vertex location";
  let model = model_id model in
  let host = match location with
    | "global" -> "aiplatform.googleapis.com"
    | "eu" | "us" -> "aiplatform." ^ location ^ ".rep.googleapis.com"
    | _ -> location ^ "-aiplatform.googleapis.com" in
  Printf.sprintf
    "https://%s/v1/projects/%s/locations/%s/publishers/google/models/%s:streamGenerateContent?alt=sse"
    host project location model

let strip_function_id = function
  | `Assoc fields as part ->
      (match List.assoc_opt "functionCall" fields,
             List.assoc_opt "functionResponse" fields with
       | Some (`Assoc fn), _ ->
           `Assoc (List.map (fun (key, value) ->
             key, if key = "functionCall" then
               `Assoc (List.remove_assoc "id" fn) else value) fields)
       | _, Some (`Assoc fn) ->
           `Assoc (List.map (fun (key, value) ->
             key, if key = "functionResponse" then
               `Assoc (List.remove_assoc "id" fn) else value) fields)
       | _ -> part)
  | part -> part

let map_contents body = match body with
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      key, if key <> "contents" then value else
        match value with
        | `List messages -> `List (List.map (function
            | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
                key, if key <> "parts" then value else
                  match value with
                  | `List parts -> `List (List.map strip_function_id parts)
                  | _ -> value) fields)
            | msg -> msg) messages)
        | _ -> value) fields)
  | _ -> invalid "malformed Gemini request"

let tag_state ~model (msg : message) =
  match msg.provider_state with
  | None -> msg
  | Some (`Assoc fields) ->
      if List.assoc_opt "provider" fields <> Some (`String "google") ||
         List.assoc_opt "model" fields <> Some (`String model)
      then invalid "foreign native state";
      let fields = List.map (fun (key, value) ->
        key, if key = "provider" then `String "google-vertex"
        else if key = "parts" then
          match value with
          | `List parts -> `List (List.map strip_function_id parts)
          | _ -> invalid "malformed native parts"
        else value) fields in
      let ids = `List (List.map (fun (call : tool_call) -> `String call.id)
        msg.tool_calls) in
      { msg with provider_state = Some (`Assoc (fields @ ["ids", ids])) }
  | _ -> invalid "malformed native state"

let untag_state ~model (msg : message) =
  match msg.provider_state with
  | None -> msg
  | Some (`Assoc fields) ->
      let ids = `List (List.map (fun (call : tool_call) -> `String call.id)
        msg.tool_calls) in
      if List.assoc_opt "provider" fields <> Some (`String "google-vertex") ||
         List.assoc_opt "model" fields <> Some (`String model) ||
         List.assoc_opt "ids" fields <> Some ids ||
         List.length fields <> 4 then invalid "foreign or modified native state";
      { msg with provider_state = Some (`Assoc (List.filter_map
          (fun (key, value) -> if key = "ids" then None else
            Some (key, if key = "provider" then `String "google" else value))
          fields)) }
  | _ -> invalid "malformed native state"

let request ~model messages tools =
  ignore (model_id model);
  map_contents (Gemini_wire.request ~model (List.map (untag_state ~model) messages) tools)

let parse_completion ~model json =
  ignore (model_id model);
  Gemini_wire.parse_completion ~model json |> tag_state ~model

let finish_stream ~model stream =
  Gemini_stream.finish stream |> tag_state ~model

let usage = Gemini_wire.usage
