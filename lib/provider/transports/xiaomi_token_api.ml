(* MiMo Token Plan OpenAI-compatible Chat endpoints and subscription keys.
   https://mimo.mi.com/docs/en-US/tokenplan/Token%20Plan/quick-access
   The plan's regional Base URL is shown in the customer's console; never send
   a plan credential to the pay-as-you-go API or to a different region. The
   published GET /v1/models endpoint is on the pay-as-you-go host, not here:
   https://mimo.mi.com/docs/en-US/api/model/list-models
   No Token Plan listing endpoint is documented; callers supply a known model
   ID and must not infer eligibility or region from its spelling. *)
type region = Ams | Cn | Sgp

let chat_url = function
  | Ams -> "https://token-plan-ams.xiaomimimo.com/v1/chat/completions"
  | Cn -> "https://token-plan-cn.xiaomimimo.com/v1/chat/completions"
  | Sgp -> "https://token-plan-sgp.xiaomimimo.com/v1/chat/completions"

let env_name = function
  | Ams -> "XIAOMI_TOKEN_PLAN_AMS_API_KEY"
  | Cn -> "XIAOMI_TOKEN_PLAN_CN_API_KEY"
  | Sgp -> "XIAOMI_TOKEN_PLAN_SGP_API_KEY"

let valid_key key =
  let length = String.length key in
  length > 3 && length <= 8192 &&
  (String.starts_with ~prefix:"tp-" key && length > 3 ||
   String.starts_with ~prefix:"ttp-" key && length > 4) &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key region = match Sys.getenv_opt (env_name region) with
  | Some key when valid_key key -> Some key
  | _ -> None

(* An opaque tp-/ttp- string does not encode its cluster. When regional
   credentials are supplied through scoped environment variables, reject a
   key registered for a different cluster instead of silently relabelling it.
   An explicit key absent from those variables still needs the caller's
   selected region, because its cluster cannot be derived from its bytes. *)
let registered_as region api_key =
  match env_api_key region with
  | Some key -> key = api_key
  | None -> false

let key_registered_elsewhere region api_key = match region with
  | Ams -> registered_as Cn api_key || registered_as Sgp api_key
  | Cn -> registered_as Ams api_key || registered_as Sgp api_key
  | Sgp -> registered_as Ams api_key || registered_as Cn api_key

let chat_headers ~region ~endpoint ~api_key =
  if endpoint <> chat_url region then
    invalid_arg "Xiaomi Token Plan key requires its pinned regional Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Xiaomi Token Plan API key";
  if key_registered_elsewhere region api_key then
    invalid_arg "Xiaomi Token Plan API key belongs to another configured region";
  ["api-key: " ^ api_key]

(* Xiaomi's native OpenAI-compatible multi-turn function calls require replay
   of the assistant's authentic reasoning_content along with tool_calls. *)
let request = Xiaomi_api.request
let parse_completion = Xiaomi_api.parse_completion
