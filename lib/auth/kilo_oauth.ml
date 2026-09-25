(* Kilo's approval endpoint is a device-code exchange, not RFC 8628 OAuth.
   Keep the code and its returned gateway token bound to api.kilo.ai. *)
let base = "https://api.kilo.ai/api/device-auth"
let fail message = raise (Oauth_device.OAuth_error message)

let field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with
    | Some value -> value | None -> `Null)
  | _ -> `Null

let text value = value <> "" && String.length value <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) value

let code value = value <> "" && String.length value <= 256 &&
  String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true
    | _ -> false) value

let verify_url value =
  let uri = try Oauth_flow.validate_url value
    with Oauth_flow.OAuth_error _ -> fail "Invalid Kilo verification URL" in
  if uri.scheme <> "https" || not (List.mem uri.authority ["kilo.ai"; "app.kilo.ai"])
  then fail "Kilo verification URL must use Kilo's website"

type authorization = { code : string; verification_url : string; expires_at : float }

let start ?post ?(now = Unix.gettimeofday ()) () =
  let json = Oauth_flow.post ?http:post ~url:(base ^ "/codes")
    ~headers:[] ~format:Oauth_flow.Json [] in
  let code = match field "code" json with
    | `String value when code value -> value
    | _ -> fail "Kilo device authorization returned an invalid code" in
  let verification_url = match field "verificationUrl" json with
    | `String value -> verify_url value; value
    | _ -> fail "Kilo device authorization returned no verification URL" in
  let seconds = match field "expiresIn" json with
    | `Int number when number > 0 && number <= 3600 -> float_of_int number
    | _ -> fail "Kilo device authorization returned an invalid expiry" in
  if not (Float.is_finite now) || not (Float.is_finite (now +. seconds))
  then fail "Invalid Kilo device clock";
  { code; verification_url; expires_at = now +. seconds }

let poll ?get ?(now = Unix.gettimeofday) ?(sleep = fun seconds ->
    ignore (Unix.select [] [] [] seconds)) auth =
  if not (code auth.code) then fail "Invalid Kilo device code";
  let get = Option.value ~default:(fun ~url ~headers ->
    Model_discovery.default_http ~url ~headers ()) get in
  let rec await remaining =
    let current = now () in
    if not (Float.is_finite current) || current >= auth.expires_at || remaining = 0
    then fail "Kilo device authorization expired";
    let url = base ^ "/codes/" ^ Oauth_flow.url_encode auth.code in
    let status, body = match get ~url ~headers:["Accept", "application/json"] with
      | Ok response -> response
      | Error _ -> fail "Kilo device approval request failed" in
    if status = 202 then (
      sleep (min 5. (auth.expires_at -. current)); await (remaining - 1))
    else if status = 403 then fail "Kilo device authorization denied"
    else if status = 410 then fail "Kilo device authorization expired"
    else if status <> 200 || String.length body > 65536 then
      fail "Kilo device approval failed"
    else
      let json = try Yojson.Basic.from_string body
        with Yojson.Json_error _ -> fail "Invalid Kilo approval response" in
      match field "status" json with
      | `String "approved" ->
          (match field "token" json with
           | `String token when text token ->
               ({ access = token; refresh = None; expires_at = None;
                  account_id = None; metadata = [] } : Oauth_store.credential)
           | _ -> fail "Kilo approval returned an invalid gateway token")
      | `String "denied" -> fail "Kilo device authorization denied"
      | `String "expired" -> fail "Kilo device authorization expired"
      | _ -> fail "Invalid Kilo approval status" in
  await 720

let login ?post ?get ?now ?sleep ~on_authorization () =
  let auth = start ?post ?now:(Option.map (fun clock -> clock ()) now) () in
  on_authorization auth;
  poll ?get ?now ?sleep auth
