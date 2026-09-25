(* Mantle's OpenAI-compatible Responses endpoint is not Bedrock Converse. The
   OpenAI model cards use /openai/v1 for inference, while the documented
   Mantle Models API uses /v1/models on the same regional host. *)
type target = { url : string; host : string; path : string }

let supported_regions = [
  "us-east-1"; "us-east-2"; "us-west-2";
  "ap-southeast-3"; "ap-south-1"; "ap-southeast-2"; "ap-northeast-1";
  "eu-central-1"; "eu-west-1"; "eu-west-2"; "eu-south-1"; "eu-north-1";
  "sa-east-1"; "us-gov-west-1" ]

let validate_region region =
  if not (List.mem region supported_regions) then
    invalid_arg "unsupported Bedrock Mantle AWS region"

let region ?(getenv = Sys.getenv_opt) () =
  let first_nonempty name = match getenv name with
    | Some value when value <> "" -> Some value
    | _ -> None in
  let value = match first_nonempty "AWS_REGION" with
    | Some value -> value
    | None -> (match first_nonempty "AWS_DEFAULT_REGION" with
      | Some value -> value
      | None -> invalid_arg "AWS_REGION or AWS_DEFAULT_REGION is required for Bedrock Mantle") in
  validate_region value;
  value

let target ~region path =
  validate_region region;
  let host = "bedrock-mantle." ^ region ^ ".api.aws" in
  { host; path; url = "https://" ^ host ^ path }

let endpoint ~region () = target ~region "/openai/v1/responses"
let discovery_endpoint ~region () = target ~region "/v1/models"

let valid_token token =
  token <> "" && String.length token <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) token

(* A selected provider key wins over the AWS environment token. Neither
   SigV4 credentials nor OpenAI credentials are bearer tokens for Mantle. *)
let bearer_token ?(getenv = Sys.getenv_opt) ~api_key () =
  let token = if api_key <> "" then api_key else
    match getenv "AWS_BEARER_TOKEN_BEDROCK" with
    | Some token -> token
    | None -> invalid_arg "AWS_BEARER_TOKEN_BEDROCK or a Bedrock API key is required" in
  if not (valid_token token) then invalid_arg "invalid Bedrock Mantle bearer token";
  token

let resolve ?(getenv = Sys.getenv_opt) ~endpoint:url ~api_key () =
  let expected = (endpoint ~region:(region ~getenv ()) ()).url in
  if url <> expected then invalid_arg "invalid Bedrock Mantle Responses endpoint";
  let token = bearer_token ~getenv ~api_key () in
  url, ["Authorization: Bearer " ^ token]

let parse_models json =
  let invalid reason = raise (Protocol.Invalid_response ("invalid Bedrock Mantle model listing: " ^ reason)) in
  let rows = match Protocol.member "data" json with
    | `List rows -> rows
    | _ -> invalid "missing data array" in
  let seen = Hashtbl.create 32 in
  let rec collect result = function
    | [] -> List.rev result
    | row :: rest ->
        let id = match Protocol.member "id" row with
          | `String id when id <> "" && String.length id <= 256 &&
              String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id -> id
          | _ -> invalid "invalid model ID" in
        let available = match Protocol.member "status" row with
          | `Null | `String "available" -> true
          | `String "unavailable" -> false
          | _ -> invalid "invalid model status" in
        if available && not (Hashtbl.mem seen id) then (
          Hashtbl.add seen id ();
          collect (id :: result) rest)
        else collect result rest in
  collect [] rows
