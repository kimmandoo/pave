type t = {
  provider : string;
  account_id : string option;
  route : string;
  upstream_id : string;
}

let valid_component value =
  value <> "" && not (String.exists (fun character ->
    let code = Char.code character in code <= 32 || code = 127) value)

let make ~provider ?account_id ~route ~upstream_id () =
  if not (valid_component provider && valid_component route &&
      valid_component upstream_id &&
      Option.fold ~none:true ~some:valid_component account_id) then
    invalid_arg "invalid model identity";
  { provider; account_id; route; upstream_id }

let equal left right =
  left.provider = right.provider && left.account_id = right.account_id &&
  left.route = right.route && left.upstream_id = right.upstream_id

let encode_into output value =
  let hex = "0123456789ABCDEF" in
  String.iter (fun character ->
    match character with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char output character
    | _ ->
        let code = Char.code character in
        Buffer.add_char output '%';
        Buffer.add_char output hex.[code lsr 4];
        Buffer.add_char output hex.[code land 15])
    value

let encode_component value =
  let output = Buffer.create (String.length value) in
  encode_into output value;
  Buffer.contents output

let decode_component value =
  let length = String.length value in
  let output = Buffer.create length in
  let hex character = match character with
    | '0'..'9' -> Char.code character - Char.code '0'
    | 'a'..'f' -> Char.code character - Char.code 'a' + 10
    | 'A'..'F' -> Char.code character - Char.code 'A' + 10
    | _ -> -1 in
  let rec loop index =
    if index < length then
      if value.[index] = '%' then (
        if index + 2 >= length then invalid_arg "invalid escaped account ID";
        let high = hex value.[index + 1] and low = hex value.[index + 2] in
        if high < 0 || low < 0 then invalid_arg "invalid escaped account ID";
        Buffer.add_char output (Char.chr (high * 16 + low));
        loop (index + 3))
      else (
        Buffer.add_char output value.[index];
        loop (index + 1)) in
  loop 0;
  let decoded = Buffer.contents output in
  if not (valid_component decoded) then invalid_arg "invalid account ID";
  decoded

let selector identity =
  let account_size = Option.fold ~none:0
    ~some:(fun account -> 1 + (3 * String.length account))
    identity.account_id in
  let output = Buffer.create (String.length identity.provider + 1 +
    String.length identity.route + account_size + 1 +
    String.length identity.upstream_id) in
  Buffer.add_string output identity.provider;
  Buffer.add_char output '@';
  Buffer.add_string output identity.route;
  Option.iter (fun account ->
    Buffer.add_char output '#';
    encode_into output account) identity.account_id;
  Buffer.add_char output '/';
  Buffer.add_string output identity.upstream_id;
  Buffer.contents output

let parse_selector_prefix prefix =
  let provider_route, account = match String.index_opt prefix '#' with
    | None -> prefix, None
    | Some marker ->
        if String.index_from_opt prefix (marker + 1) '#' <> None then
          invalid_arg "model selector has multiple account separators";
        let encoded = String.sub prefix (marker + 1)
          (String.length prefix - marker - 1) in
        if encoded = "" then invalid_arg "model selector account ID is empty";
        String.sub prefix 0 marker, Some (decode_component encoded) in
  let provider, route = match String.index_opt provider_route '@' with
    | None -> provider_route, None
    | Some marker ->
        if String.index_from_opt provider_route (marker + 1) '@' <> None then
          invalid_arg "model selector has multiple route separators";
        let selected_route = String.sub provider_route (marker + 1)
          (String.length provider_route - marker - 1) in
        if selected_route = "" then invalid_arg "model selector route is empty";
        String.sub provider_route 0 marker, Some selected_route in
  if not (valid_component provider) then invalid_arg "model selector provider is empty";
  provider, route, account
