type tool_call = { id : string; name : string; arguments : Yojson.Basic.t }

type usage = { input_tokens : int; output_tokens : int }

type message = {
  role : string;
  content : string option;
  tool_calls : tool_call list;
  tool_call_id : string option;
  provider_state : Yojson.Basic.t option;
}

exception Invalid_response of string
let add_usage left right =
  if right.input_tokens > max_int - left.input_tokens ||
    right.output_tokens > max_int - left.output_tokens then
    raise (Invalid_response "provider token totals exceed host integer");
  { input_tokens = left.input_tokens + right.input_tokens;
    output_tokens = left.output_tokens + right.output_tokens }

let user content =
  { role = "user"; content = Some content; tool_calls = [];
    tool_call_id = None; provider_state = None }
let tool_result id content =
  { role = "tool"; content = Some content; tool_calls = [];
    tool_call_id = Some id; provider_state = None }

let member key = function
  | `Assoc fields -> (match List.assoc_opt key fields with Some v -> v | None -> `Null)
  | _ -> `Null

let string = function `String s -> s | _ -> raise (Invalid_response "expected string")

let call_to_json call =
  `Assoc [ "id", `String call.id; "type", `String "function";
    "function", `Assoc [ "name", `String call.name;
                            "arguments", `String (Yojson.Basic.to_string call.arguments) ] ]

let message_to_json ?(stored = false) msg =
  let fields = [ "role", `String msg.role ] in
  let fields = match msg.content with None -> fields | Some s -> fields @ [ "content", `String s ] in
  let fields = if msg.tool_calls = [] then fields
    else fields @ [ "tool_calls", `List (List.map call_to_json msg.tool_calls) ] in
  let fields = match msg.tool_call_id with None -> fields
    | Some id -> fields @ [ "tool_call_id", `String id ] in
  let fields = match stored, msg.provider_state with
    | true, Some state -> fields @ [ "provider_state", state ]
    | _ -> fields in
  `Assoc fields

let parse_call json =
  let id = member "id" json |> string in
  let fn = member "function" json in
  let name = member "name" fn |> string in
  let args = member "arguments" fn |> string in
  let arguments = try Yojson.Basic.from_string args
    with Yojson.Json_error _ -> raise (Invalid_response "invalid function arguments JSON") in
  if id = "" || name = "" then raise (Invalid_response "empty tool call id or name");
  { id; name; arguments }

let parse_calls = function
  | `Null -> []
  | `List calls ->
      let calls = List.map parse_call calls in
      let ids = List.map (fun call -> call.id) calls in
      if List.length (List.sort_uniq String.compare ids) <> List.length ids then
        raise (Invalid_response "duplicate tool call id");
      calls
  | _ -> raise (Invalid_response "invalid tool_calls")

let parse_message json =
  let content = match member "content" json with
    | `Null -> None | `String s -> Some s | _ -> raise (Invalid_response "invalid assistant content") in
  let tool_calls = parse_calls (member "tool_calls" json) in
  { role = "assistant"; content; tool_calls; tool_call_id = None;
    provider_state = None }

let message_from_json json =
  let role = member "role" json |> string in
  let content = match member "content" json with
    | `Null -> None | `String s -> Some s | _ -> raise (Invalid_response "invalid content") in
  let tool_calls = parse_calls (member "tool_calls" json) in
  let tool_call_id = match member "tool_call_id" json with
    | `Null -> None | `String id -> Some id | _ -> raise (Invalid_response "invalid tool_call_id") in
  let provider_state = match member "provider_state" json with
    | `Null -> None
    | (`Assoc _ as state) -> Some state
    | _ -> raise (Invalid_response "invalid provider state") in
  (match role, content, tool_calls, tool_call_id, provider_state with
  | "user", Some _, [], None, None
  | "assistant", _, _, None, _
  | "tool", Some _, [], Some _, None -> ()
  | _ -> raise (Invalid_response "invalid stored message"));
  { role; content; tool_calls; tool_call_id; provider_state }

let parse_completion json =
  match member "choices" json with
  | `List (choice :: _) ->
      let msg = member "message" choice |> parse_message in
      let finish = member "finish_reason" choice in
      (match finish with
      | `String "stop" when msg.tool_calls = [] -> msg
      | `String "tool_calls" when msg.tool_calls <> [] -> msg
      | `String reason -> raise (Invalid_response ("unexpected finish_reason: " ^ reason))
      | _ -> raise (Invalid_response "missing finish_reason"))
  | _ -> raise (Invalid_response "missing choices")
