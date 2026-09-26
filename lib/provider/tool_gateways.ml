(* Official fixed HTTPS origins only. Model IDs come exclusively from live listings;
   no seed, family-name heuristic or caller-supplied endpoint is involved. *)
type spec = {
  id : string;
  display_name : string;
  api_key_env : string;
  chat_url : string;
  models_url : string;
  max_response_bytes : int;
}

let all = [
  { id = "aimlapi"; display_name = "AIML API";
    api_key_env = "AIMLAPI_API_KEY";
    chat_url = "https://api.aimlapi.com/v1/chat/completions";
    models_url = "https://api.aimlapi.com/v1/models?type=openai%2Fchat-completions&include=capabilities";
    max_response_bytes = 4 * 1_048_576 };
  { id = "aiand"; display_name = "ai&";
    api_key_env = "AIAND_API_KEY";
    chat_url = "https://api.aiand.com/v1/chat/completions";
    models_url = "https://api.aiand.com/v1/models";
    max_response_bytes = 1_048_576 };
]

let find id = List.find_opt (fun spec -> spec.id = id) all

let field name = function
  | `Assoc entries -> List.assoc_opt name entries
  | _ -> None

let valid_id id =
  id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let capability name row =
  match field "capabilities" row with
  | Some (`List values) when List.for_all
      (function `String _ -> true | _ -> false) values ->
      Ok (List.mem (`String name) values)
  | _ -> Error "missing or malformed model capabilities"

let parse_models ~provider body =
  match find provider with
  | None -> Error "unsupported gateway"
  | Some spec when String.length body > spec.max_response_bytes ->
      Error "listing exceeds size limit"
  | Some _ ->
      let json = try Ok (Yojson.Basic.from_string body)
        with Yojson.Json_error _ -> Error "malformed model listing JSON" in
      match json with
      | Error _ as error -> error
      | Ok json ->
          (match field "object" json, field "data" json with
          | Some (`String "list"), Some (`List rows) ->
              let seen = Hashtbl.create (List.length rows) in
              let rec collect result count = function
                | [] -> Ok (List.rev result)
                | _ when count >= 4096 -> Error "too many model rows"
                | row :: rest ->
                    (match field "id" row with
                    | Some (`String id) when valid_id id ->
                        if Hashtbl.mem seen id then Error "duplicate model ID"
                        else (
                          Hashtbl.add seen id ();
                          let inclusion = match provider with
                            | "aimlapi" ->
                                (match field "type" row with
                                | Some (`String "openai/chat-completions") ->
                                    capability "tools" row
                                | Some (`String _) ->
                                    (match capability "tools" row with
                                    | Ok _ -> Ok false
                                    | Error _ as error -> error)
                                | _ -> Error "missing or malformed model type")
                            | "aiand" -> capability "tool_calling" row
                            | _ -> Error "unsupported gateway" in
                          (match inclusion with
                          | Error _ as error -> error
                          | Ok include_row ->
                              collect (if include_row then id :: result else result)
                                (count + 1) rest))
                    | _ -> Error "missing or invalid model ID") in
              collect [] 0 rows
          | _ -> Error "missing or malformed model data array")
