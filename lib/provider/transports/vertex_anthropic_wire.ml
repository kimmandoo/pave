open Protocol

let invalid detail =
  raise (Invalid_response ("invalid Vertex Claude response: " ^ detail))

let endpoint ~project ~location ~model ~streaming =
  if not (Vertex_wire.project_id project) then invalid_arg "invalid Vertex project ID";
  if not (Vertex_wire.location_id location) then invalid_arg "invalid Vertex location";
  let model = Vertex_wire.model_id model in
  let host = match location with
    | "global" -> "aiplatform.googleapis.com"
    | "eu" | "us" -> "aiplatform." ^ location ^ ".rep.googleapis.com"
    | _ -> location ^ "-aiplatform.googleapis.com" in
  if not (String.starts_with ~prefix:"claude-" model) then
    invalid_arg "Vertex Claude requires a Claude model ID";
  Printf.sprintf
    "https://%s/v1/projects/%s/locations/%s/publishers/anthropic/models/%s:%s"
    host project location model
    (if streaming then "streamRawPredict" else "rawPredict")
let request ~model ~max_tokens ~streaming ?thinking messages tools =
  let upstream_model = Vertex_wire.model_id model in
  if not (String.starts_with ~prefix:"claude-" upstream_model) then
    invalid_arg "Vertex Claude requires a Claude model ID";
  if max_tokens <= 0 then invalid_arg "invalid Claude max_tokens";
  let replay_assistant_content =
    Anthropic_wire.replay_native_content ~provider:"google-vertex" ~model in
  let body = Anthropic_wire.request ~model:upstream_model ~max_tokens ?thinking
    ~replay_assistant_content messages tools in
  match body with
  | `Assoc fields -> `Assoc (List.filter (fun (key, _) -> key <> "model") fields @ [
      "anthropic_version", `String "vertex-2023-10-16";
      "stream", `Bool streaming ])
  | _ -> assert false

let validate_completion (reply : Protocol.message) =
  if reply.content = None && reply.tool_calls = [] &&
     reply.provider_state = None then
    invalid "empty completion";
  reply


let parse_completion ~model json =
  if not (String.starts_with ~prefix:"claude-" model) then
    invalid_arg "Vertex Claude requires a Claude model ID";
  (match Protocol.member "type" json with
   | `String "error" -> invalid "upstream returned an error"
   | _ -> ());
  validate_completion
    (Anthropic_wire.parse_native_completion ~provider:"google-vertex" ~model json)

let create_stream ?on_tool_arguments ?(model = "") ~on_text () =
  Anthropic_stream.create ?on_tool_arguments ~provider:"google-vertex"
    ~model ~on_text ()
let feed_stream = Anthropic_stream.feed
let stream_is_finished = Anthropic_stream.is_finished
let stream_usage = Anthropic_stream.usage
let finish_stream ~model stream =
  if not (String.starts_with ~prefix:"claude-" model) then
    invalid_arg "Vertex Claude requires a Claude model ID";
  validate_completion (Anthropic_stream.finish stream)
