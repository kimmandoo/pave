(* Azure OpenAI v1 Responses: the deployment is the request body model, not
   part of the URL. Only public-cloud Azure OpenAI resource hosts are trusted
   with the resource's API key. *)
let fail detail = invalid_arg ("Azure OpenAI " ^ detail)

let valid_resource resource =
  let length = String.length resource in
  length > 0 && length <= 64 &&
  (match resource.[0], resource.[length - 1] with
   | ('a'..'z' | '0'..'9'), ('a'..'z' | '0'..'9') -> true
   | _ -> false) &&
  String.for_all (function 'a'..'z' | '0'..'9' | '-' -> true | _ -> false) resource

let host base =
  let prefix = "https://" in
  if not (String.starts_with ~prefix base) then
    fail "endpoint must use HTTPS and an Azure OpenAI resource hostname";
  let authority = String.sub base (String.length prefix)
    (String.length base - String.length prefix) in
  match String.split_on_char '.' authority with
  | [resource; "openai"; "azure"; "com"] when valid_resource resource -> ()
  | _ -> fail "endpoint must use a public Azure OpenAI resource hostname"

let version_suffix = function
  | "" -> ""
  | "v1" -> "?api-version=v1"
  | "preview" -> "?api-version=preview"
  | _ -> fail "API version must be v1 or preview for the v1 Responses API"

let endpoint () =
  let base = match Sys.getenv_opt "AZURE_OPENAI_ENDPOINT" with
    | Some value when value <> "" -> value
    | _ -> fail "requires AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com" in
  let base = if String.ends_with ~suffix:"/" base then
    String.sub base 0 (String.length base - 1) else base in
  host base;
  let version = Option.value ~default:"" (Sys.getenv_opt "AZURE_OPENAI_API_VERSION") in
  base ^ "/openai/v1/responses" ^ version_suffix version

let resolve ~endpoint ~deployment ~api_key =
  let prefix = "https://" in
  if not (String.starts_with ~prefix endpoint) then
    fail "endpoint must use HTTPS and an Azure OpenAI resource hostname";
  let slash = match String.index_from_opt endpoint (String.length prefix) '/' with
    | Some index -> index | None -> fail "endpoint must be /openai/v1/responses" in
  let base = String.sub endpoint 0 slash in
  host base;
  let path = String.sub endpoint slash (String.length endpoint - slash) in
  if path <> "/openai/v1/responses" &&
     path <> "/openai/v1/responses?api-version=v1" &&
     path <> "/openai/v1/responses?api-version=preview" then
    fail "endpoint must be /openai/v1/responses with optional v1 or preview version";
  if deployment = "" || String.exists (fun c -> Char.code c < 32 || Char.code c = 127) deployment then
    fail "requires an explicit valid deployment ID";
  if api_key = "" || String.length api_key > 8192 ||
     String.exists (fun c -> Char.code c < 33 || Char.code c = 127) api_key then
    fail "requires a valid AZURE_OPENAI_API_KEY";
  endpoint, ["api-key: " ^ api_key]
