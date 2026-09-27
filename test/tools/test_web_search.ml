module Web_search = Pave.Web_search

let fail label = failwith ("web search: " ^ label)
let expect label condition = if not condition then fail label
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec seek index =
    index + m <= n &&
    (String.sub text index m = fragment || seek (index + 1)) in
  seek 0

let expect_error label fragment fn =
  match fn () with
  | _ -> fail (label ^ " was accepted")
  | exception Web_search.Error message ->
      expect (label ^ " error details") (contains message fragment)
  | exception exn -> fail (label ^ " raised the wrong exception: " ^ Printexc.to_string exn)

let environment values name = List.assoc_opt name values
let brave_response =
  "{\"web\":{\"results\":[{\"title\":\"Example\",\"url\":\"https://docs.example.org/article\",\"description\":\"A result snippet\"}]}}"
let tavily_response =
  "{\"results\":[{\"title\":\"Tavily example\",\"url\":\"https://docs.example.org/tavily\",\"content\":\"Tavily snippet\"}]}"

let () =
  let captured = ref [] in
  let http request =
    captured := request :: !captured;
    Ok (200, tavily_response) in
  let response = Web_search.search ~http ~env:(environment [
      "PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "tavily,brave";
      "BRAVE_SEARCH_API_KEY", "brave-secret";
      "TAVILY_API_KEY", "tavily-secret"])
      ~query:"C++ web search" ~count:1 () in
  expect "priority selects the first configured provider" (response.provider = "tavily");
  expect "provider provenance on normalized result" ((List.hd response.results).provider = "tavily");
  expect "normalized title and snippet" ((List.hd response.results).title = "Tavily example" &&
    (List.hd response.results).snippet = "Tavily snippet");
  expect "exact source URL" ((List.hd response.results).url = "https://docs.example.org/tavily");
  expect "citation preserves the source URL" (response.citations = ["[1] https://docs.example.org/tavily"]);
  (match !captured with
   | [request] ->
       expect "Tavily fixed endpoint" ((request : Web_search.request).url = "https://api.tavily.com/search");
       expect "Tavily bearer credential binding"
         (List.assoc_opt "Authorization" request.headers = Some "Bearer tavily-secret");
       expect "Tavily receives the requested bounded count" (contains request.body "\"max_results\":1");
       expect "Tavily answer and raw content disabled"
         (contains request.body "\"include_answer\":false" && contains request.body "\"include_raw_content\":false")
   | _ -> fail "priority issued exactly one request");

  let brave_request = ref None in
  let brave_http request =
    brave_request := Some request;
    Ok (200, brave_response) in
  let brave = Web_search.search ~http:brave_http ~env:(environment [
      "PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave,tavily";
      "BRAVE_SEARCH_API_KEY", "brave-secret";
      "TAVILY_API_KEY", "tavily-secret"])
      ~query:"C++ web search" ~page:2 ~count:3 () in
  expect "configured Brave priority" (brave.provider = "brave");
  (match !brave_request with
   | Some request ->
       expect "Brave fixed HTTPS host and API path"
         (contains request.url "https://api.search.brave.com/res/v1/web/search?");
       expect "Brave query safely encoded" (contains request.url "q=C%2B%2B%20web%20search");
       expect "Brave bounded count and page"
         (contains request.url "&count=3&offset=2");
       expect "Brave token sent only in its provider header"
         (List.assoc_opt "X-Subscription-Token" request.headers = Some "brave-secret" &&
          List.assoc_opt "Authorization" request.headers = None)
   | None -> fail "Brave request was sent");

  let called = ref false in
  expect_error "all configured credentials missing" "BRAVE_SEARCH_API_KEY"
    (fun () -> Web_search.search ~http:(fun _ -> called := true; Ok (200, brave_response))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave"])
       ~query:"query" ());
  expect "missing credential sends no request" (not !called);

  let only_tavily = Web_search.search ~http:(fun _ -> Ok (200, tavily_response))
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave,tavily";
                        "TAVILY_API_KEY", "tavily-secret"])
      ~query:"query" () in
  expect "missing higher-priority key advances only within explicit priority"
    (only_tavily.provider = "tavily");
  let cancelled_request = ref false in
  expect_error "pre-request cancellation is honored" "cancelled"
    (fun () -> Web_search.search ~cancel:(fun () -> true)
       ~http:(fun _ -> cancelled_request := true; Ok (200, brave_response))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());
  expect "cancelled search does not issue HTTP" (not !cancelled_request);
  expect_error "priority is mandatory" "PAVE_WEB_SEARCH_PROVIDER_PRIORITY"
    (fun () -> Web_search.search ~http:(fun _ -> fail "request must not be sent")
       ~env:(environment ["BRAVE_SEARCH_API_KEY", "brave-secret"]) ~query:"query" ());
  expect_error "unsupported provider is rejected" "unsupported search provider"
    (fun () -> Web_search.search ~http:(fun _ -> fail "request must not be sent")
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "google";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"]) ~query:"query" ());
  expect_error "page bounds" "page must be between"
    (fun () -> Web_search.search ~http:(fun _ -> fail "request must not be sent")
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ~page:10 ());
  expect_error "Tavily page is rejected rather than ignored" "does not support paged"
    (fun () -> Web_search.search ~http:(fun _ -> fail "request must not be sent")
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "tavily";
                         "TAVILY_API_KEY", "tavily-secret"])
       ~query:"query" ~page:1 ());

  let attempts = ref [] in
  let no_substitution (request : Web_search.request) =
    attempts := request.url :: !attempts;
    if contains request.url "search.brave.com" then Ok (503, "provider unavailable")
    else Ok (200, tavily_response) in
  expect_error "provider failure is not silently substituted" "HTTP 503"
    (fun () -> Web_search.search ~http:no_substitution ~env:(environment [
      "PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave,tavily";
      "BRAVE_SEARCH_API_KEY", "brave-secret";
      "TAVILY_API_KEY", "tavily-secret"])
      ~query:"query" ());
  expect "provider errors do not trigger a secondary request" (List.length !attempts = 1);

  expect_error "malformed JSON response" "malformed search response JSON"
    (fun () -> Web_search.search ~http:(fun _ -> Ok (200, "{"))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());
  expect_error "wrong provider response structure" "web must be an object"
    (fun () -> Web_search.search ~http:(fun _ -> Ok (200, "{\"web\":[]}"))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());
  expect_error "duplicate result URLs, including fragment variants" "duplicate result URL"
    (fun () -> Web_search.search ~http:(fun _ -> Ok (200,
      "{\"web\":{\"results\":[" ^
      "{\"title\":\"One\",\"url\":\"https://EXAMPLE.org/a\",\"description\":\"one\"}," ^
      "{\"title\":\"Two\",\"url\":\"https://example.org/a#section\",\"description\":\"two\"}]}}"))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ~count:2 ());
  expect_error "private result URL rejected" "private, local"
    (fun () -> Web_search.search ~http:(fun _ -> Ok (200,
      "{\"web\":{\"results\":[{\"title\":\"Private\",\"url\":\"https://127.0.0.1/private\",\"description\":\"x\"}]}}"))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());
  expect_error "oversized search results rejected" "response exceeds the size limit"
    (fun () -> Web_search.search ~http:(fun _ -> Ok (200, String.make (Web_search.max_response_bytes + 1) 'x'))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());

  let fetch_calls = ref 0 in
  let fetch_http (request : Web_search.request) =
    incr fetch_calls;
    expect "fetch is a GET request" (request.method_ = "GET");
    expect "fetch carries no search credentials"
      (List.assoc_opt "Authorization" request.headers = None &&
       List.assoc_opt "X-Subscription-Token" request.headers = None);
    Ok (200, "<html><body><h1>Heading</h1><p>Fish &amp; chips</p>" ^
      "<script>never show this secret</script><style>never show style</style>" ^
      "<!-- never show comment --><ul><li>Item</li></ul></body></html>") in
  let fetched = Web_search.fetch_url ~http:fetch_http
      "https://93.184.216.34/article" () in
  expect "fetch returns original source URL" (fetched.source_url = "https://93.184.216.34/article");
  expect "HTML converts heading, paragraphs, entities and lists"
    (contains fetched.markdown "# Heading" && contains fetched.markdown "Fish & chips" &&
     contains fetched.markdown "- Item");
  expect "script, style and comment content is excluded"
    (not (contains fetched.markdown "never show this secret") &&
     not (contains fetched.markdown "never show style") &&
     not (contains fetched.markdown "never show comment"));
  expect "fetch is independent of search calls" (!fetch_calls = 1);
  let links = Web_search.convert_html_to_markdown
      ("<a title=\"quoted > character\" href='https://safe.example/path?a=1&amp;b=2'>" ^
       "Safe &amp; [linked]</a> " ^
       "<a href='javascript:alert(1)'>JavaScript label</a> " ^
       "<a href='http://safe.example/'>HTTP label</a> " ^
       "<a href='https://user:password@safe.example/'>Credential label</a>") in
  expect "quoted greater-than attributes and entity-decoded safe HTTPS links"
    (contains links "[Safe & \\[linked\\]](<https://safe.example/path?a=1&b=2>)");
  expect "unsafe href destinations are dropped while their text remains"
    (contains links "JavaScript label" && contains links "HTTP label" &&
     contains links "Credential label" && not (contains links "javascript:") &&
     not (contains links "http://safe.example/") &&
     not (contains links "user:password"));
  let table = Web_search.convert_html_to_markdown
      ("<table><tr><th>Name</th><th>Age</th></tr>" ^
       "<tr><td>Ada</td><td>37</td></tr><tr><td>Lin</td><td>41</td></tr></table>") in
  expect "tables render header separators and every row"
    (table = "| Name | Age |\n| --- | --- |\n| Ada | 37 |\n| Lin | 41 |");
  let long_link_html = "<a href='https://e.com'>" ^ String.make 20 ']' ^ "</a>" in
  expect_error "expanded Markdown is rejected instead of truncated" "converted page exceeds"
    (fun () -> Web_search.convert_html_to_markdown
       ~max_bytes:(String.length long_link_html) long_link_html);
  expect_error "redirect response rejected" "HTTP 302"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> Ok (302, "redirect body"))
       "https://93.184.216.34/redirect" ());
  expect_error "private loopback fetch host rejected" "private or local IP"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> fail "private URL must not be requested")
       "https://127.0.0.1/private" ());
  expect_error "private IPv4 fetch host rejected" "private or local IP"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> fail "private URL must not be requested")
       "https://10.0.0.7/private" ());
  expect_error "private IPv6 fetch host rejected" "private or local IP"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> fail "private URL must not be requested")
       "https://[::1]/private" ());
  expect_error "oversized fetched HTML rejected" "exceeds the size limit"
    (fun () -> Web_search.fetch_url ~max_bytes:8 ~http:(fun _ -> Ok (200, "<p>too much</p>"))
       "https://93.184.216.34/large" ());
  expect_error "nonstandard fetch port rejected" "port 443"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> fail "non-443 URL must not be requested")
       "https://pages.example.org:8443/article" ());
  expect_error "credentialed fetch URL rejected" "credential-free public HTTPS"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> fail "credentialed URL must not be requested")
       "https://user:password@pages.example.org/article" ());
  expect_error "HTML input limit enforced" "HTML page exceeds"
    (fun () -> Web_search.convert_html_to_markdown ~max_bytes:4 "<p>hello</p>");
  print_endline "web search pinned providers, provenance, and safe fetch: ok"
