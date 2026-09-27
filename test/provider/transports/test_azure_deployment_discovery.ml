open Pave

let subscription = "00000000-1111-2222-3333-444444444444"
let account = "project-7"
let resource_group = "rg-(east)"
let endpoint = "https://project-7.openai.azure.com/openai/v1/responses"

let deployment id model state = `Assoc [
  "name", `String id;
  "properties", `Assoc [
    "model", `Assoc ["name", `String model];
    "provisioningState", `String state]]

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect ~finally:(fun () ->
    match previous with
    | Some value -> Unix.putenv name value
    | None -> Unix.putenv name "") f

let write_file path contents =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output contents)

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let contains text pattern =
  let text_length = String.length text and pattern_length = String.length pattern in
  let rec scan index =
    if index + pattern_length > text_length then false
    else if String.sub text index pattern_length = pattern then true
    else scan (index + 1) in
  scan 0

let with_fake_az ~page1 ~page2 f =
  let directory = Filename.temp_file "pave-azure-discovery-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let executable = Filename.concat directory "az" in
  let log = Filename.concat directory "arguments" in
  write_file executable {|#!/bin/sh
printf '%s\n' "$*" >> "$AZURE_TEST_LOG"
case "$1:$2" in
  account:show) printf '%s\n' "$AZURE_TEST_SUBSCRIPTION" ;;
  resource:list) printf '%s\n' "$AZURE_TEST_RESOURCES" ;;
  rest:--method)
    case "$5" in
      *skiptoken=page2*) printf '%s\n' "$AZURE_TEST_PAGE2" ;;
      *) printf '%s\n' "$AZURE_TEST_PAGE1" ;;
    esac ;;
  *) exit 17 ;;
esac
|};
  Unix.chmod executable 0o700;
  let old_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ())
      [executable; log];
    try Unix.rmdir directory with Unix.Unix_error _ -> ())
    (fun () ->
      with_env "PATH" (directory ^ ":" ^ old_path) (fun () ->
        with_env "AZURE_TEST_LOG" log (fun () ->
          with_env "AZURE_TEST_SUBSCRIPTION" subscription (fun () ->
            with_env "AZURE_TEST_RESOURCES"
              (Yojson.Basic.to_string (`List [`Assoc [
                "name", `String account;
                "resourceGroup", `String resource_group;
                "type", `String "Microsoft.CognitiveServices/accounts"]]))
              (fun () ->
                with_env "AZURE_TEST_PAGE1" page1 (fun () ->
                  with_env "AZURE_TEST_PAGE2" page2 f))))))

let () =
  assert (Provider_catalog.unclassified_models "azure");
  let first_url = Azure_deployment_discovery.deployment_url
    ~subscription ~resource_group ~account in
  assert (contains first_url "resourceGroups/rg-%28east%29/");
  let next_link = Azure_deployment_discovery.without_query first_url ^
    "?api-version=2025-06-01&$skiptoken=page2" in
  let page1 = Yojson.Basic.to_string (`Assoc [
    "value", `List [
      deployment "prod-chat" "gpt-4.1" "Succeeded";
      deployment "warming" "gpt-4.1" "Creating"];
    "nextLink", `String next_link]) in
  let page2 = Yojson.Basic.to_string (`Assoc [
    "value", `List [deployment "region-claude" "claude-sonnet" "Succeeded"]]) in
  with_fake_az ~page1 ~page2 (fun () ->
    Unix.putenv "AZURE_OPENAI_ENDPOINT" "https://project-7.openai.azure.com";
    Unix.putenv "AZURE_OPENAI_API_KEY" "private-data-plane-key";
    let direct = Azure_deployment_discovery.list_deployments ~endpoint () in
    (match direct with
     | Error _ -> failwith "Azure management deployment listing failed"
     | Ok listing ->
         assert (listing.account_id = account);
         assert (listing.endpoint = first_url);
         assert (List.map (fun model -> model.Azure_deployment_discovery.id)
           listing.deployments = ["prod-chat"; "region-claude"]));
    let discover route = Model_discovery.discover ~provider:"azure" ~route_name:route
      ~credential:(Model_discovery.Api_key "private-data-plane-key") () in
    (match discover "responses" with
     | Error _ -> failwith "Azure Responses route discovery failed"
     | Ok listing ->
         assert (List.length listing.models = 2);
         let model = List.hd listing.models in
         assert (model.Model_catalog.identity.account_id = Some account);
         assert (model.identity.route = "responses");
         assert (model.identity.upstream_id = "prod-chat");
         assert (model.display_name = Some "prod-chat · gpt-4.1");
         assert (model.provenance.endpoint = Some first_url);
         assert (model.capabilities.supported_endpoints = None));
    (match discover "chat" with
     | Error _ -> failwith "Azure Chat route discovery failed"
     | Ok listing ->
         assert (List.for_all (fun model -> model.Model_catalog.identity.route = "chat")
           listing.models));
    let log = read_file (Option.get (Sys.getenv_opt "AZURE_TEST_LOG")) in
    assert (contains log ("--subscription " ^ subscription));
    assert (contains log "--resource-type Microsoft.CognitiveServices/accounts");
    assert (contains log ("--url " ^ first_url));
    assert (contains log "skiptoken=page2");
    assert (not (contains log "private-data-plane-key"));
    assert (not (Azure_deployment_discovery.valid_next_link
      ~base:(Azure_deployment_discovery.without_query first_url)
      "https://attacker.example/deployments?api-version=2025-06-01&$skiptoken=x")));
  print_endline "Azure management deployment discovery and route scoping: ok"
