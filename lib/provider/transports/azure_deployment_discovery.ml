(* Azure deployment IDs live on the management plane. The data-plane API key
   cannot list deployments; Azure CLI supplies the signed-in public-cloud
   management identity. Every command is fixed, bounded and shell-free.

   Sources:
   https://learn.microsoft.com/en-us/rest/api/microsoftfoundry/accountmanagement/deployments/list?view=rest-microsoftfoundry-accountmanagement-2025-06-01
   https://learn.microsoft.com/en-us/cli/azure/resource?view=azure-cli-latest *)

type deployment = { id : string; display_name : string option }
type result = { account_id : string; endpoint : string; deployments : deployment list }
type error = Credential_error of string | Invalid_response of string

let endpoint_base = "https://management.azure.com"
let api_version = "2025-06-01"
let max_pages = 16
let max_deployments = 4096
let max_total_seconds = 30.
let page_timeout_seconds = 10.

let invalid detail = Error (Invalid_response detail)
let starts_with ~prefix value = String.starts_with ~prefix value
let trim = Azure_auth.trim_cli_newline

let unique_fields fields =
  let names = List.map fst fields in
  List.length names = List.length (List.sort_uniq String.compare names)

let field name = function
  | `Assoc fields when unique_fields fields -> List.assoc_opt name fields
  | _ -> None

let valid_subscription value =
  if String.length value <> 36 then false
  else
    let rec valid index =
      if index = 36 then true
      else
        let character = value.[index] in
        let accepted =
          if index = 8 || index = 13 || index = 18 || index = 23 then
            character = '-'
          else match character with
            | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
            | _ -> false in
        accepted && valid (index + 1) in
    valid 0

let valid_resource_group value =
  value <> "" && String.length value <= 90 &&
  not (String.exists (fun character ->
    let code = Char.code character in
    code <= 32 || code = 127 || character = '/' || character = '\\' ||
    character = '?' || character = '#') value)

let encode_segment value =
  let output = Buffer.create (String.length value) in
  String.iter (fun character ->
    match character with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '_' | '~' ->
        Buffer.add_char output character
    | _ -> Buffer.add_string output (Printf.sprintf "%%%02X" (Char.code character)))
    value;
  Buffer.contents output

let deployment_url ~subscription ~resource_group ~account =
  Printf.sprintf
    "%s/subscriptions/%s/resourceGroups/%s/providers/Microsoft.CognitiveServices/accounts/%s/deployments?api-version=%s"
    endpoint_base (encode_segment subscription) (encode_segment resource_group)
    (encode_segment account) api_version

let parse_resources ~account body =
  let json = try Ok (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> invalid "Azure CLI returned malformed resource JSON" in
  match json with
  | Error _ as error -> error
  | Ok (`List rows) when List.length rows <= 4096 ->
      let matches = List.filter_map (fun row ->
        match field "name" row, field "resourceGroup" row, field "type" row with
        | Some (`String name), Some (`String resource_group),
          Some (`String "Microsoft.CognitiveServices/accounts")
          when String.lowercase_ascii name = String.lowercase_ascii account ->
            Some resource_group
        | Some (`String _), Some (`String _), Some (`String _) -> None
        | _ -> raise (Invalid_argument "malformed Azure account resource row")) rows in
      (match matches with
       | [resource_group] when valid_resource_group resource_group ->
           Ok resource_group
       | [] -> Error (Credential_error
           "No Azure account matched the configured endpoint in the current subscription")
       | [_] -> invalid "Azure CLI returned an invalid resource-group name"
       | _ -> invalid "Azure CLI returned ambiguous Azure account resources")
  | Ok (`List _) -> invalid "Azure CLI returned too many Azure account resources"
  | Ok _ -> invalid "Azure CLI account resource result must be an array"
  | exception Invalid_argument _ ->
      invalid "Azure CLI returned a malformed Azure account resource row"

let valid_deployment_id value =
  value <> "" && String.length value <= 256 &&
  String.for_all (fun character ->
    let code = Char.code character in code > 32 && code < 127) value

let parse_page body =
  let json = try Ok (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> invalid "Azure returned malformed deployment JSON" in
  match json with
  | Error _ as error -> error
  | Ok (`Assoc _ as object_) ->
      (match field "value" object_ with
       | Some (`List rows) when List.length rows <= max_deployments ->
           let rec collect deployments = function
             | [] ->
                 let next = match field "nextLink" object_ with
                   | None | Some `Null -> Ok None
                   | Some (`String value) when value <> "" -> Ok (Some value)
                   | Some (`String "") -> invalid "Azure returned an empty deployment next link"
                   | _ -> invalid "Azure returned an invalid deployment next link" in
                 Result.map (fun next -> List.rev deployments, next) next
             | row :: rest ->
                 (match field "name" row, field "properties" row with
                  | Some (`String id), Some (`Assoc _ as properties)
                    when valid_deployment_id id ->
                      (match field "provisioningState" properties,
                             field "model" properties with
                       | Some (`String state), Some (`Assoc _ as model) ->
                           (match field "name" model with
                            | Some (`String model_name)
                              when valid_deployment_id model_name ->
                                let deployments =
                                  if state = "Succeeded" then
                                    { id; display_name = Some (id ^ " · " ^ model_name) }
                                      :: deployments
                                  else deployments in
                                collect deployments rest
                            | _ -> invalid "Azure returned an invalid deployment model name")
                       | _ -> invalid "Azure returned incomplete deployment properties")
                  | _ -> invalid "Azure returned an invalid deployment row") in
           collect [] rows
       | Some (`List _) -> invalid "Azure returned too many deployments in one page"
       | _ -> invalid "Azure deployment response is missing its value array")
  | Ok _ -> invalid "Azure deployment response must be an object"

let valid_next_link ~base value =
  let prefix = base ^ "?" in
  if String.length value > 4096 || not (starts_with ~prefix value) ||
     String.exists (fun character ->
       let code = Char.code character in
       code <= 32 || code = 127 || character = '#' || character = '\\') value then
    false
  else
    let query = String.sub value (String.length prefix)
      (String.length value - String.length prefix) in
    let hex = function
      | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
      | _ -> false in
    let rec valid_percent index =
      if index >= String.length query then true
      else if query.[index] <> '%' then valid_percent (index + 1)
      else if index + 2 >= String.length query then false
      else hex query.[index + 1] && hex query.[index + 2] &&
        valid_percent (index + 3) in
    let pairs = String.split_on_char '&' query in
    let valid_pair pair =
      match String.index_opt pair '=' with
      | None -> false
      | Some index ->
          let key = String.sub pair 0 index in
          let value = String.sub pair (index + 1)
            (String.length pair - index - 1) in
          if value = "" then false
          else match String.lowercase_ascii key with
            | "api-version" -> value = api_version
            | "$skiptoken" -> true
            | _ -> false in
    let key_of_pair pair =
      match String.index_opt pair '=' with
      | Some index -> String.lowercase_ascii (String.sub pair 0 index)
      | None -> "" in
    let count_key key =
      List.length (List.filter (fun pair -> key_of_pair pair = key) pairs) in
    String.length query > 0 && valid_percent 0 &&
    List.length pairs = 2 && List.for_all valid_pair pairs &&
    count_key "api-version" = 1 && count_key "$skiptoken" = 1

let without_query url =
  match String.index_opt url '?' with
  | Some index -> String.sub url 0 index
  | None -> url

let check_cancel cancel =
  match cancel with
  | Some cancel when cancel () -> raise Azure_auth.Cancelled
  | _ -> ()

let run_command ~cancel ~deadline arguments =
  check_cancel cancel;
  let remaining = deadline -. Unix.gettimeofday () in
  if remaining <= 0. then
    raise (Azure_auth.Authentication_error "Azure deployment listing exceeded its deadline");
  let timeout = min page_timeout_seconds remaining in
  Azure_auth.run_az ~timeout ?cancel arguments ()

let list_deployments ?cancel ~endpoint () =
  let started = Unix.gettimeofday () in
  let deadline = started +. max_total_seconds in
  let account = try Azure_wire.resource_name ~endpoint
    with Invalid_argument _ ->
      raise (Azure_auth.Authentication_error "Azure endpoint is not trusted for deployment listing") in
  try
    let subscription = run_command ~cancel ~deadline
      ["account"; "show"; "--query"; "id"; "--output"; "tsv";
       "--only-show-errors"] |> trim in
    if not (valid_subscription subscription) then
      invalid "Azure CLI returned an invalid subscription ID"
    else
      let resources = run_command ~cancel ~deadline
        ["resource"; "list"; "--name"; account; "--resource-type";
         "Microsoft.CognitiveServices/accounts"; "--subscription"; subscription;
         "--query"; "[].{name:name,resourceGroup:resourceGroup,type:type}";
         "--output"; "json"; "--only-show-errors"] in
      (match parse_resources ~account resources with
       | Error _ as error -> error
       | Ok resource_group ->
           let first_url = deployment_url ~subscription ~resource_group ~account in
           let rec pages page_number url seen deployments =
             if page_number > max_pages then
               invalid "Azure deployment listing exceeded its page limit"
             else
               let body = run_command ~cancel ~deadline
                 ["rest"; "--method"; "get"; "--url"; url;
                  "--output"; "json"; "--only-show-errors"] in
               match parse_page body with
               | Error _ as error -> error
               | Ok (page, next) ->
                   let combined = List.rev_append page deployments in
                   if List.length combined > max_deployments then
                     invalid "Azure deployment listing exceeded its model limit"
                   else
                     let duplicate = List.exists (fun deployment ->
                       if Hashtbl.mem seen deployment.id then true
                       else (Hashtbl.add seen deployment.id (); false)) page in
                     if duplicate then invalid "Azure returned duplicate deployment IDs"
                     else
                       match next with
                       | None -> Ok (List.rev combined)
                       | Some next_link when valid_next_link
                           ~base:(without_query first_url) next_link ->
                           pages (page_number + 1) next_link seen combined
                       | Some _ -> invalid "Azure returned a deployment next link outside its pinned account endpoint" in
           let seen = Hashtbl.create 64 in
           (match pages 1 first_url seen [] with
            | Error _ as error -> error
            | Ok deployments ->
                Ok { account_id = account; endpoint = first_url; deployments }))
  with
  | Azure_auth.Cancelled -> raise Azure_auth.Cancelled
  | Azure_auth.Authentication_error _ ->
      Error (Credential_error
        "Azure deployment listing requires Azure CLI sign-in and current-subscription management read access")
  | Unix.Unix_error _ | Sys_error _ ->
      Error (Credential_error
        "Azure deployment listing requires Azure CLI sign-in and current-subscription management read access")
