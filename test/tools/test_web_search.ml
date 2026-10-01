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

(* Unit tests fake HTTP but still ran the real browser-rendered fallback on
   hosts with Chrome installed (CI runners), launching a real browser and
   network call whenever the chain reached Ecosia. Tests now pin the browser
   probe off by default; tests exercising the rendered path pass their own
   find_browser/render injections. *)
let search ?http ?cancel ?env ?(find_browser = fun () -> None) ?render
    ?page ?count ~query () =
  Web_search.search ?http ?cancel ?env ~find_browser ?render ?page ?count
    ~query ()
let brave_response =
  "{\"web\":{\"results\":[{\"title\":\"Example\",\"url\":\"https://docs.example.org/article\",\"description\":\"A result snippet\"}]}}"
let tavily_response =
  "{\"results\":[{\"title\":\"Tavily example\",\"url\":\"https://docs.example.org/tavily\",\"content\":\"Tavily snippet\"}]}"

let () =
  let captured = ref [] in
  let http request =
    captured := request :: !captured;
    Ok (200, tavily_response) in
  let response = search ~http ~env:(environment [
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
  let brave = search ~http:brave_http ~env:(environment [
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
    (fun () -> search ~http:(fun _ -> called := true; Ok (200, brave_response))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave"])
       ~query:"query" ());
  expect "missing credential sends no request" (not !called);

  let only_tavily = search ~http:(fun _ -> Ok (200, tavily_response))
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave,tavily";
                        "TAVILY_API_KEY", "tavily-secret"])
      ~query:"query" () in
  expect "missing higher-priority key advances only within explicit priority"
    (only_tavily.provider = "tavily");
  let cancelled_request = ref false in
  expect_error "pre-request cancellation is honored" "cancelled"
    (fun () -> search ~cancel:(fun () -> true)
       ~http:(fun _ -> cancelled_request := true; Ok (200, brave_response))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());
  expect "cancelled search does not issue HTTP" (not !cancelled_request);
  let automatic = ref [] in
  let auto_brave = search
      ~http:(fun request -> automatic := request :: !automatic; Ok (200, brave_response))
      ~env:(environment ["BRAVE_SEARCH_API_KEY", "brave-secret"]) ~query:"query" () in
  expect "automatic order uses the first engine with a credential"
    (auto_brave.provider = "brave" && List.length !automatic = 1);
  expect_error "unsupported provider is rejected" "unsupported search provider"
    (fun () -> search ~http:(fun _ -> fail "request must not be sent")
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "google";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"]) ~query:"query" ());
  expect_error "page bounds" "page must be between"
    (fun () -> search ~http:(fun _ -> fail "request must not be sent")
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ~page:10 ());
  expect_error "Tavily page is rejected rather than ignored" "does not support paged"
    (fun () -> search ~http:(fun _ -> fail "request must not be sent")
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "tavily";
                         "TAVILY_API_KEY", "tavily-secret"])
       ~query:"query" ~page:1 ());

  let attempts = ref [] in
  let no_substitution (request : Web_search.request) =
    attempts := request.url :: !attempts;
    if contains request.url "search.brave.com" then Ok (503, "provider unavailable")
    else Ok (200, tavily_response) in
  let substituted = search ~http:no_substitution ~env:(environment [
      "PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave,tavily";
      "BRAVE_SEARCH_API_KEY", "brave-secret";
      "TAVILY_API_KEY", "tavily-secret"])
      ~query:"query" () in
  expect "a failed provider falls back to the next and is reported"
    (substituted.provider = "tavily" && List.length !attempts = 2 &&
     (match substituted.failed with
      | ["brave", reason] -> contains reason "HTTP 503"
      | _ -> false));
  expect_error "every failure is listed when all providers fail" "all web search providers failed"
    (fun () -> search ~http:(fun _ -> Ok (503, "down")) ~env:(environment [
      "PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave,tavily";
      "BRAVE_SEARCH_API_KEY", "brave-secret";
      "TAVILY_API_KEY", "tavily-secret"]) ~query:"query" ());
  let after_cancel = ref 0 in
  let cancelled = ref false in
  expect_error "cancellation stops the fallback chain" "cancelled"
    (fun () -> search ~cancel:(fun () -> !cancelled)
      ~http:(fun _ -> incr after_cancel; cancelled := true; Ok (503, "down"))
      ~env:(environment [
        "PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave,tavily";
        "BRAVE_SEARCH_API_KEY", "brave-secret";
        "TAVILY_API_KEY", "tavily-secret"]) ~query:"query" ());
  expect "no request after cancellation" (!after_cancel = 1);

  (* Credential-free DuckDuckGo is the automatic fallback. *)
  let ddg_html =
    "<div class=\"result results_links\"><a rel=\"nofollow\" class=\"result__a\" " ^
    "href=\"//duckduckgo.com/l/?uddg=https%3A%2F%2Fgithub.com%2Focaml%2Fyojson&amp;rut=x\">" ^
    "<b>Yojson</b> &amp; friends</a>" ^
    "<a class=\"result__snippet\" href=\"#\">Fast JSON\nfor <b>OCaml</b></a></div>" ^
    "<div class=\"result\"><a class=\"result__a\" href=\"https://duckduckgo.com/y.js?ad=1\">Ad</a></div>" ^
    "<div class=\"result\"><a class=\"result__a\" href=\"http://insecure.example/\">Plain HTTP</a></div>" ^
    "<div class=\"result\"><a class=\"result__a\" href=\"https://github.com/ocaml/yojson#readme\">Duplicate</a></div>" ^
    "<div class=\"result\"><a class=\"result__a\" href=\"https://10.0.0.1/x\">Private</a></div>" ^
    "<div class=\"result\"><a class=\"result__a\" href=\"https://ocaml.org/p/yojson\">ocaml.org</a>" ^
    "<div class=\"result__snippet\">Package page</div></div>" in
  let ddg_request = ref None in
  let ddg = search
      ~http:(fun request -> ddg_request := Some request; Ok (200, ddg_html))
      ~env:(environment []) ~query:"yojson & json" ~count:5 () in
  expect "no credentials selects DuckDuckGo" (ddg.provider = "duckduckgo");
  (match !ddg_request with
   | Some request ->
       expect "DuckDuckGo HTML form POST without credentials"
         (request.method_ = "POST" && request.url = "https://html.duckduckgo.com/html/" &&
          request.search = Some Web_search.Duckduckgo &&
          contains request.body "q=yojson%20%26%20json" &&
          List.assoc_opt "Authorization" request.headers = None)
   | None -> fail "DuckDuckGo request was sent");
  expect "DuckDuckGo results unwrap redirects and drop ads, HTTP, private and duplicate links"
    (List.map (fun (result : Web_search.result) -> result.url, result.title, result.snippet)
       ddg.results =
     ["https://github.com/ocaml/yojson", "Yojson & friends", "Fast JSON for OCaml";
      "https://ocaml.org/p/yojson", "ocaml.org", "Package page"]);
  expect_error "DuckDuckGo bot challenge is explained" "bot-detection"
    (fun () -> search ~http:(fun _ -> Ok (200, "<div class=\"anomaly-modal\">"))
      ~env:(environment []) ~query:"query" ());
  let challenge_calls = ref [] in
  let challenge_fallback = search
      ~http:(fun request ->
        challenge_calls := request.search :: !challenge_calls;
        match request.search with
        | Some Web_search.Duckduckgo ->
            Ok (202, "<form class=\"anomaly-modal\">private-challenge-token</form>")
        | Some Web_search.Brave -> Ok (200, brave_response)
        | _ -> fail "unexpected challenge fallback")
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "duckduckgo,brave";
                        "BRAVE_SEARCH_API_KEY", "brave-secret"])
      ~query:"query" () in
  expect "HTTP 202 challenge falls back without publishing challenge data"
    (challenge_fallback.provider = "brave" &&
     List.rev !challenge_calls = [Some Web_search.Duckduckgo; Some Web_search.Brave] &&
     match challenge_fallback.failed with
     | ["duckduckgo", reason] -> not (contains reason "private-challenge-token")
     | _ -> false);
  List.iter (fun html ->
    let empty = search ~http:(fun _ -> Ok (200, html))
        ~env:(environment []) ~query:"unindexed topic" () in
    expect "live no-result quote and class variants return no invented citations"
      (empty.results = [] && empty.citations = [] && empty.failed = []))
    ["<span class='no-results'><h1>No results found</h1></span>";
     "<div class=\"result results_links results_links_deep web-result result--no-result\">No results found</div>"];
  let varied_rows = Web_search.parse_duckduckgo 5
      "<a CLASS='extra result__a selected' href='https://example.org/docs?a=1&amp;b=2'>Actual &amp; Result</a>" in
  expect "result links accept class membership and single-quoted attributes"
    (varied_rows = ["Actual & Result", "https://example.org/docs?a=1&b=2", ""]);
  let empty_json = search
      ~http:(fun _ -> Ok (200, {|{"results":[]}|}))
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "tavily";
                        "TAVILY_API_KEY", "tavily-secret"])
      ~query:"unindexed topic" () in
  expect "valid JSON empty results remain empty"
    (empty_json.results = [] && empty_json.citations = [] && empty_json.failed = []);
  let malformed_html = "<a class=\"result__a\" href=\"https://example.com/\">Unclosed" in
  let recovered = search
      ~http:(fun request -> match request.search with
        | Some Web_search.Duckduckgo -> Ok (200, malformed_html)
        | Some Web_search.Brave -> Ok (200, brave_response)
        | _ -> fail "unexpected provider after malformed HTML")
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "duckduckgo,brave";
                        "BRAVE_SEARCH_API_KEY", "brave-secret"])
      ~query:"query" () in
  expect "malformed HTML falls back rather than escaping as a parser exception"
    (recovered.provider = "brave" && List.map fst recovered.failed = ["duckduckgo"]);
  let recovered_rows = Web_search.parse_duckduckgo 5
      (malformed_html ^ "<a class=\"result__a\" href=\"https://example.org/\">Valid</a>") in
  expect "an unclosed result cannot steal the following result's title"
    (recovered_rows = ["Valid", "https://example.org/", ""]);

  (* Credentialed engines normalize their distinct response envelopes. *)
  let exa_request = ref None in
  let exa = search
      ~http:(fun request -> exa_request := Some request; Ok (200,
        "{\"results\":[{\"title\":null,\"url\":\"https://exa.example.org/a\"," ^
        "\"highlights\":[\"First line\\nsecond line\"]}]}"))
      ~env:(environment ["EXA_API_KEY", "exa-secret"; "BRAVE_SEARCH_API_KEY", "brave-secret"])
      ~query:"query" ~count:3 () in
  expect "Exa leads the automatic order" (exa.provider = "exa");
  expect "Exa result uses URL for a missing title and a single-line highlight"
    (match exa.results with
     | [result] -> result.title = "https://exa.example.org/a" &&
         result.snippet = "First line second line"
     | _ -> false);
  (match !exa_request with
   | Some request ->
       expect "Exa key only in x-api-key on its pinned endpoint"
         (request.url = "https://api.exa.ai/search" &&
          List.assoc_opt "x-api-key" request.headers = Some "exa-secret" &&
          List.assoc_opt "Authorization" request.headers = None &&
          contains request.body "\"numResults\":3")
   | None -> fail "Exa request was sent");
  let engine name variable body =
    search ~http:(fun request ->
        expect (name ^ " bearer credential")
          (List.assoc_opt "Authorization" request.headers = Some ("Bearer " ^ name ^ "-secret"));
        Ok (200, body))
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", name; variable, name ^ "-secret"])
      ~query:"query" () in
  let first (response : Web_search.response) = match response.results with
    | result :: _ -> result.url, result.snippet
    | [] -> "", "" in
  expect "Kagi data.search results"
    (first (engine "kagi" "KAGI_API_KEY"
      "{\"data\":{\"search\":[{\"title\":\"K\",\"url\":\"https://kagi.example.org/\",\"snippet\":\"ks\"}]}}")
     = ("https://kagi.example.org/", "ks"));
  expect "Jina data array results"
    (first (engine "jina" "JINA_API_KEY"
      "{\"code\":200,\"data\":[{\"title\":\"J\",\"url\":\"https://jina.example.org/\",\"description\":\"js\"}]}")
     = ("https://jina.example.org/", "js"));
  expect "Firecrawl data.web results"
    (first (engine "firecrawl" "FIRECRAWL_API_KEY"
      "{\"success\":true,\"data\":{\"web\":[{\"title\":\"F\",\"url\":\"https://fc.example.org/\",\"description\":\"fs\"}]}}")
     = ("https://fc.example.org/", "fs"));
  let long_title = String.concat "" (List.init 400 (fun _ -> "界")) in
  let bounded = search
      ~http:(fun _ -> Ok (200, Yojson.Basic.to_string (`Assoc [
        "results", `List [`Assoc [
          "title", `String long_title; "url", `String "https://example.org/";
          "highlights", `List [`String "snippet"]]]])))
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "exa";
                        "EXA_API_KEY", "exa-secret"])
      ~query:"query" () in
  expect "long UTF-8 titles remain usable and bounded including the ellipsis"
    (match bounded.results with
     | [result] -> result.title =
         String.concat "" (List.init 340 (fun _ -> "界")) ^ "…"
     | _ -> false);
  let failed_payloads = [
    "kagi", "KAGI_API_KEY", "{\"error\":[{\"message\":\"secret-echo\\u001b[31m\"}]," ^
      "\"data\":{\"search\":[{\"title\":\"Partial\",\"url\":\"https://example.org/\",\"snippet\":\"partial\"}]}}";
    "firecrawl", "FIRECRAWL_API_KEY",
      "{\"success\":false,\"error\":\"secret-echo\\u001b[31m\"}"
  ] in
  List.iter (fun (name, variable, payload) ->
    let result = search
        ~http:(fun request -> match request.search with
          | Some Web_search.Brave -> Ok (200, brave_response)
          | _ -> Ok (200, payload))
        ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", name ^ ",brave";
          variable, "secret"; "BRAVE_SEARCH_API_KEY", "brave-secret"])
        ~query:"query" () in
    expect (name ^ " error envelope does not publish partial data or reflected secrets")
      (result.provider = "brave" && match result.failed with
       | [provider, reason] -> provider = name &&
           not (contains reason "secret-echo") && not (String.contains reason '\027')
       | _ -> false)) failed_payloads;

  expect_error "malformed JSON response" "malformed search response JSON"
    (fun () -> search ~http:(fun _ -> Ok (200, "{"))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());
  expect_error "wrong provider response structure" "web must be an object"
    (fun () -> search ~http:(fun _ -> Ok (200, "{\"web\":[]}"))
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                         "BRAVE_SEARCH_API_KEY", "brave-secret"])
       ~query:"query" ());
  let brave_only = environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "brave";
                                 "BRAVE_SEARCH_API_KEY", "brave-secret"] in
  let mixed = search ~env:brave_only ~query:"query" ~count:2
      ~http:(fun _ -> Ok (200,
        "{\"web\":{\"results\":[" ^
        "{\"title\":\"One\",\"url\":\"https://EXAMPLE.org/a\",\"description\":\"one\"}," ^
        "{\"title\":\"Dup\",\"url\":\"https://example.org/a#section\",\"description\":\"dup\"}," ^
        "{\"title\":\"Private\",\"url\":\"https://127.0.0.1/private\",\"description\":\"x\"}," ^
        "{\"title\":\"Plain\",\"url\":\"http://insecure.example/\",\"description\":\"x\"}," ^
        "{\"title\":\"Two\",\"url\":\"https://example.org/b\",\"description\":\"two\"}," ^
        "{\"title\":\"Three\",\"url\":\"https://example.org/c\",\"description\":\"three\"}]}}")) () in
  expect "duplicate, private and non-HTTPS rows are dropped and extra rows cut to count"
    (List.map (fun (result : Web_search.result) -> result.title) mixed.results = ["One"; "Two"]);
  expect_error "a structurally invalid response still fails" "web must be an object"
    (fun () -> search ~env:brave_only ~query:"query"
       ~http:(fun _ -> Ok (200, "{\"web\":[]}")) ());

  (* Empty answers move to the next provider; all-empty is an empty answer. *)
  let tried = ref [] in
  let empty_then_found = search ~query:"query"
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "tavily,brave";
                        "TAVILY_API_KEY", "tavily-secret"; "BRAVE_SEARCH_API_KEY", "brave-secret"])
      ~http:(fun request ->
        tried := request.search :: !tried;
        if request.search = Some Web_search.Tavily then Ok (200, {|{"results":[]}|})
        else Ok (200, brave_response)) () in
  expect "an empty provider falls through to the next"
    (empty_then_found.provider = "brave" && List.length !tried = 2 &&
     empty_then_found.failed = []);

  (* Browser-rendered Ecosia, with an injected renderer. *)
  let ecosia_html =
    "<article data-test-id=\"organic-result\" aria-label=\"Notty &amp; OCaml\">" ^
    "<a data-test-id=\"result-link\" href=\"https://www.ecosia.org/settings\">x</a>" ^
    "<a data-test-id=\"result-link\" href=\"https://github.com/pqwy/notty\">Notty</a>" ^
    "<p data-test-id=\"web-result-description\">Declarative\n<b>terminal</b> graphics</p></article>" ^
    "<article data-test-id=\"organic-result\" aria-label=\"Insecure\">" ^
    "<a data-test-id=\"result-link\" href=\"http://plain.example/\">p</a></article>" in
  let rendered = ref [] in
  let ecosia = search ~query:"notty"
      ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "ecosia"])
      ~find_browser:(fun () -> Some "/fixture/chrome")
      ~http:(fun _ -> fail "Ecosia must not use curl")
      ~render:(fun ~program url -> rendered := (program, url) :: !rendered; ecosia_html) () in
  expect "Ecosia renders its fixed URL in the detected browser"
    (!rendered = ["/fixture/chrome", "https://www.ecosia.org/search?q=notty"]);
  expect "Ecosia rows skip Ecosia-internal and plain-HTTP links"
    (List.map (fun (result : Web_search.result) -> result.url, result.title, result.snippet)
       ecosia.results =
     ["https://github.com/pqwy/notty", "Notty & OCaml", "Declarative terminal graphics"]);
  expect_error "Ecosia without a browser explains how to enable it" "PAVE_BROWSER"
    (fun () -> search ~query:"q"
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "ecosia"])
       ~find_browser:(fun () -> None) ());
  let automatic_without_browser = Web_search.plan_summary ~env:(environment [])
      ~find_browser:(fun () -> None) () in
  let automatic_with_browser = Web_search.plan_summary ~env:(environment [])
      ~find_browser:(fun () -> Some "/fixture/chrome") () in
  expect "automatic plan adds browser-rendered Ecosia only when a browser exists"
    (automatic_without_browser = (false, ["duckduckgo", false, false]) &&
     automatic_with_browser = (false, ["duckduckgo", false, false; "ecosia", false, true]));
  expect_error "Ecosia challenge page is explained" "bot-detection"
    (fun () -> search ~query:"q"
       ~env:(environment ["PAVE_WEB_SEARCH_PROVIDER_PRIORITY", "ecosia"])
       ~find_browser:(fun () -> Some "/fixture/chrome")
       ~render:(fun ~program:_ _ -> "<script>window._cf_chl_opt={}</script>") ());
  expect_error "relative PAVE_BROWSER is rejected" "absolute path"
    (fun () -> Web_search.detect_browser ~env:(environment ["PAVE_BROWSER", "chrome"]) ());
  expect "PAVE_BROWSER=none disables browser engines"
    (Web_search.detect_browser ~env:(environment ["PAVE_BROWSER", "none"]) () = None);
  expect_error "oversized search results rejected" "response exceeds the size limit"
    (fun () -> search ~http:(fun _ -> Ok (200, String.make (Web_search.max_response_bytes + 1) 'x'))
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
  let fetch_cancelled = ref false in
  expect_error "transport failure preserves fetch cancellation" "cancelled"
    (fun () -> Web_search.fetch_url
      ~cancel:(fun () -> !fetch_cancelled)
      ~http:(fun _ -> fetch_cancelled := true; Error "request interrupted")
      "https://93.184.216.34/article" ());
  let links, links_truncated = Web_search.convert_html_to_markdown
      ("<a title=\"quoted > character\" href='https://safe.example/path?a=1&amp;b=2'>" ^
       "Safe &amp; [linked]</a> " ^
       "<a href='javascript:alert(1)'>JavaScript label</a> " ^
       "<a href='http://safe.example/'>HTTP label</a> " ^
       "<a href='https://user:password@safe.example/'>Credential label</a>") in
  expect "quoted greater-than attributes and entity-decoded safe HTTPS links"
    (not links_truncated && contains links "[Safe & \\[linked\\]](<https://safe.example/path?a=1&b=2>)");
  expect "unsafe href destinations are dropped while their text remains"
    (contains links "JavaScript label" && contains links "HTTP label" &&
     contains links "Credential label" && not (contains links "javascript:") &&
     not (contains links "http://safe.example/") &&
     not (contains links "user:password"));
  let table, table_truncated = Web_search.convert_html_to_markdown
      ("<table><tr><th>Name</th><th>Age</th></tr>" ^
       "<tr><td>Ada</td><td>37</td></tr><tr><td>Lin</td><td>41</td></tr></table>") in
  expect "tables render header separators and every row"
    (not table_truncated && table = "| Name | Age |\n| --- | --- |\n| Ada | 37 |\n| Lin | 41 |");
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
  let large_html = Web_search.fetch_url
      ~http:(fun _ -> Ok (200, "<script>" ^ String.make 300_000 'x' ^
        "</script><p>Visible document</p>"))
      "https://93.184.216.34/large" () in
  expect "download budget is independent of output and ignored HTML"
    (large_html.markdown = "Visible document" && not large_html.truncated);
  let raw_json = {|{"content":"<p>Preserve &amp; markup</p>","value":42}|} in
  let json_page = Web_search.fetch_url ~http:(fun _ -> Ok (200, raw_json))
      "https://93.184.216.34/data.json" () in
  expect "JSON source survives without HTML stripping or entity decoding"
    (json_page.markdown = raw_json && not json_page.truncated);
  let prefix = "{\"value\":\"" in
  let partial_json = Web_search.fetch_url ~max_bytes:(String.length prefix + 4)
      ~http:(fun _ -> Ok (200, prefix ^ "한글\"}"))
      "https://93.184.216.34/data.json" () in
  expect "JSON preview marks truncation without splitting UTF-8"
    (partial_json.truncated && partial_json.markdown = prefix ^ "한");
  let partial_html = Web_search.fetch_url ~max_bytes:4
      ~http:(fun _ -> Ok (200, "<p>한글</p>"))
      "https://93.184.216.34/article" () in
  expect "HTML preview marks truncation without splitting UTF-8"
    (partial_html.truncated && partial_html.markdown = "한");
  let table_preview, table_cut = Web_search.convert_html_to_markdown ~max_bytes:8
      "<table><tr><td>abcdefghijklmnop</td></tr></table>" in
  expect "large first table cell retains a bounded visible preview"
    (table_cut && table_preview = "| abcdef");
  let oversized = Web_search.fetch_url
      ~http:(fun _ -> Ok (200, "<p>Lead paragraph</p>" ^
        String.make (Web_search.max_fetch_response_bytes + 1) 'x'))
      "https://93.184.216.34/too-large" () in
  expect "oversized downloads keep a leading preview marked truncated"
    (oversized.truncated && String.starts_with ~prefix:"Lead paragraph" oversized.markdown);
  expect_error "nonstandard fetch port rejected" "port 443"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> fail "non-443 URL must not be requested")
       "https://pages.example.org:8443/article" ());
  expect_error "credentialed fetch URL rejected" "credential-free public HTTPS"
    (fun () -> Web_search.fetch_url ~http:(fun _ -> fail "credentialed URL must not be requested")
       "https://user:password@pages.example.org/article" ());
  print_endline "web search pinned providers, provenance, and safe fetch: ok"
