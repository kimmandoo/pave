let user text = Pave.Protocol.user text

let () =
  let open Pave.Context_budget in
  let tools = [`Assoc ["type", `String "function";
    "function", `Assoc ["name", `String "read_file"]]] in
  let short = request ~system:"system" ~messages:[user (String.make 1024 'a')]
    ~tools in
  assert (short.estimated_bytes > 1024);
  assert (short.unmeasured_images = 0);
  assert (status ~window_tokens:(short.estimated_bytes + 2048)
    ~reserve_tokens:2048 short = Within_budget);
  assert (status ~window_tokens:(short.estimated_bytes + 2047)
    ~reserve_tokens:2048 short = Over_budget);
  let signed = { (user "recent") with provider_state = Some (`Assoc [
    "provider", `String "gemini"; "thoughtSignature", `String (String.make 512 's')]) } in
  let signed_size = request ~system:"system" ~messages:[signed] ~tools:[] in
  let plain_size = request ~system:"system" ~messages:[user "recent"] ~tools:[] in
  assert (signed_size.estimated_bytes >= plain_size.estimated_bytes + 512);
  let image = { (user "look") with attachments = [{
    Pave.Protocol.name = "fixture.png"; mime_type = "image/png";
    data = "aGVsbG8=" }] } in
  let with_image = request ~system:"system" ~messages:[image] ~tools:[] in
  assert (with_image.unmeasured_images = 1);
  assert (status ~window_tokens:100_000 ~reserve_tokens:4096 with_image =
    Images_unmeasured);
  let small_image = { image with attachments = [{
    Pave.Protocol.name = "fixture.png"; mime_type = "image/png";
    data = "aGVsbG8=" }] } in
  let large_image = { small_image with attachments = [{
    Pave.Protocol.name = "fixture.png"; mime_type = "image/png";
    data = "a" ^ String.make 4096 'b' }] } in
  let small_bytes =
    (request ~system:"system" ~messages:[small_image] ~tools:[]).estimated_bytes in
  let large_bytes =
    (request ~system:"system" ~messages:[large_image] ~tools:[]).estimated_bytes in
  assert (large_bytes - small_bytes =
    String.length (List.hd large_image.attachments).data -
    String.length (List.hd small_image.attachments).data);
  assert ((request ~system:"system" ~messages:[large_image] ~tools:[])
    .unmeasured_images = 1);
  assert (status ~window_tokens:8192 ~reserve_tokens:4096
    { with_image with estimated_bytes = 4097 } = Over_budget);
  let long_text = String.make 1500 'x' in
  let tool_result = Pave.Protocol.tool_result_blocks "call-long"
    [Pave.Protocol.Text long_text] in
  let tool_result_size = request ~system:"" ~messages:[tool_result] ~tools:[]
    |> fun estimate -> estimate.estimated_bytes in
  let empty_result = { tool_result with content = None; tool_result_content = None } in
  let empty_result_size = request ~system:"" ~messages:[empty_result] ~tools:[]
    |> fun estimate -> estimate.estimated_bytes in
  assert (tool_result_size - empty_result_size = String.length long_text);
  let call : Pave.Protocol.tool_call = {
    id = "call-large"; name = "read_file"; arguments = `Assoc [] } in
  let assistant = { (user "request") with role = "assistant";
    tool_calls = [call] } in
  let result = Pave.Protocol.tool_result_blocks call.id [
    Pave.Protocol.Text ("aaaa🙂" ^ String.make 64 'x' ^ String.make 64 'b')] in
  let trimmed, count = trim_tool_results
    ~max_bytes:(String.length truncation_note + 10) [assistant; result] in
  assert (count = 1);
  (match trimmed with
   | [call_message; result_message] ->
       assert (call_message.tool_calls = [call]);
       assert (result_message.tool_call_id = Some call.id);
       assert (result_message.content = Some ("aaaa" ^ truncation_note ^ "bbbbb"));
       assert (result_message.tool_result_content =
         Some [Pave.Protocol.Text ("aaaa" ^ truncation_note ^ "bbbbb")])
   | _ -> assert false);
  let first = user "first" and second = user "second" in
  let final : Pave.Protocol.message = {
    role = "assistant"; content = Some "answer"; tool_calls = [];
    tool_call_id = None; tool_result_content = None;
    provider_state = None; attachments = [] } in
  assert (List.map List.length (turn_groups [
    first; assistant; result; second; final
  ]) = [3; 2]);
  let attached = { second with attachments = [{
    Pave.Protocol.name = "keep.png"; mime_type = "image/png"; data = "aGVsbG8=" }] } in
  let untouched, count = trim_tool_results ~max_bytes:100 [attached] in
  assert (count = 0 && untouched = [attached]);
  let projected = Pave.Context_compaction.summary_projection attached in
  assert (attached.attachments <> [] && projected.attachments = []);
  assert (projected.content = Some
    "second\n[attached image omitted from summary input: keep.png (image/png)]");
  assert ((Pave.Context_compaction.summary_projection signed).provider_state = None);
  assert ((request_text_bytes ~system:"x" ~text_bytes:100).estimated_bytes =
    (request ~system:"x" ~messages:[user (String.make 100 'a')] ~tools:[])
      .estimated_bytes);
  print_endline "context budget: ok"
