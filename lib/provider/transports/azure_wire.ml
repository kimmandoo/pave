(* Microsoft documents Azure OpenAI v1 at `openai.azure.com` and Foundry
   Models v1 at `services.ai.azure.com`. The deployment alias is the JSON
   `model`, never a URL component. Sovereign and project-scoped endpoints are
   deliberately outside this integration.

   Sources:
   https://learn.microsoft.com/en-us/rest/api/microsoftfoundry/azureopenai/responses
   https://learn.microsoft.com/en-us/azure/foundry/foundry-models/concepts/endpoints
   https://learn.microsoft.com/en-us/azure/azure-government/compare-azure-government-global-azure *)
type route = Responses | Chat_completions
type authentication = Api_key of string | Entra_token of string
type cloud = Azure_openai_cloud | Foundry_cloud

let fail detail = invalid_arg ("Azure OpenAI " ^ detail)

let valid_resource resource =
  let resource = String.lowercase_ascii resource in
  let length = String.length resource in
  length >= 2 && length <= 64 &&
  (match resource.[0], resource.[length - 1] with
   | ('a'..'z' | '0'..'9'), ('a'..'z' | '0'..'9') -> true
   | _ -> false) &&
  String.for_all (function 'a'..'z' | '0'..'9' | '-' -> true | _ -> false) resource

let cloud_of_authority authority =
  match String.split_on_char '.' (String.lowercase_ascii authority) with
  | [resource; "openai"; "azure"; "com"] when valid_resource resource ->
      Azure_openai_cloud
  | [resource; "services"; "ai"; "azure"; "com"] when valid_resource resource ->
      Foundry_cloud
  | _ -> fail "endpoint must use a documented Azure OpenAI or Foundry Models resource hostname"

let parse_endpoint endpoint =
  let length = String.length endpoint in
  if length > 4096 || String.exists (fun c ->
      Char.code c <= 32 || Char.code c = 127 || c = '\\' || c = '#') endpoint then
    fail "endpoint contains invalid URL characters";
  let prefix = "https://" in
  if not (String.starts_with ~prefix endpoint) then
    fail "endpoint must use HTTPS";
  let authority_start = String.length prefix in
  let slash = String.index_from_opt endpoint authority_start '/' in
  let authority_end = Option.value ~default:length slash in
  let authority = String.sub endpoint authority_start (authority_end - authority_start) in
  if authority = "" then fail "endpoint is missing its resource hostname";
  let cloud = cloud_of_authority authority in
  let base = String.sub endpoint 0 authority_end in
  let path = String.sub endpoint authority_end (length - authority_end) in
  cloud, base, path

let route_path = function
  | Responses -> "/openai/v1/responses"
  | Chat_completions -> "/openai/v1/chat/completions"

let version_suffix = function
  | "" -> ""
  | "v1" -> "?api-version=v1"
  | "preview" -> "?api-version=preview"
  | _ -> fail "API version must be v1 or preview for the v1 API"

let valid_path route path =
  let endpoint = route_path route in
  path = endpoint ||
  path = endpoint ^ "?api-version=v1" ||
  path = endpoint ^ "?api-version=preview"

let endpoint_kind ~endpoint =
  let cloud, _, path = parse_endpoint endpoint in
  if path <> "" &&
     not (valid_path Responses path || valid_path Chat_completions path) then
    fail "endpoint must use the documented OpenAI v1 Responses or Chat Completions path";
  cloud

let resource_name ~endpoint =
  ignore (endpoint_kind ~endpoint);
  let _, base, _ = parse_endpoint endpoint in
  let host = String.sub base 8 (String.length base - 8) in
  match String.split_on_char '.' host with
  | resource :: _ -> resource
  | [] -> fail "endpoint is missing its resource name"

let endpoint ?(route=Responses) () =
  let base = match Sys.getenv_opt "AZURE_OPENAI_ENDPOINT" with
    | Some value when value <> "" -> value
    | _ -> fail "requires AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com or https://<resource>.services.ai.azure.com" in
  let base = if String.ends_with ~suffix:"/" base then
      String.sub base 0 (String.length base - 1)
    else base in
  let _, normalized_base, path = parse_endpoint base in
  if path <> "" then fail "AZURE_OPENAI_ENDPOINT must be a resource base URL";
  let version = Option.value ~default:"" (Sys.getenv_opt "AZURE_OPENAI_API_VERSION") in
  normalized_base ^ route_path route ^ version_suffix version

let valid_deployment deployment =
  deployment <> "" && String.length deployment <= 256 &&
  not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) deployment)

let deployment_model ~deployment =
  if not (valid_deployment deployment) then
    fail "requires an explicit valid deployment ID";
  `Assoc ["model", `String deployment]

let valid_credential ~max_length value =
  value <> "" && String.length value <= max_length &&
  String.for_all (fun c -> Char.code c >= 33 && Char.code c <= 126) value

let resolve ~route ~endpoint ~deployment ~authentication =
  let _, _, path = parse_endpoint endpoint in
  if not (valid_path route path) then
    fail "endpoint path does not match the selected v1 API route";
  ignore (deployment_model ~deployment);
  let headers = match authentication with
    | Api_key key ->
        if not (valid_credential ~max_length:8192 key) then
          fail "requires a valid Azure API key";
        ["api-key: " ^ key]
    | Entra_token token ->
        if not (valid_credential ~max_length:16384 token) then
          fail "requires a valid Microsoft Entra access token";
        ["Authorization: Bearer " ^ token] in
  endpoint, headers
