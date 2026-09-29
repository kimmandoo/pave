(* Opt-in runtime acceptance: CI provisions real toolchains, then these checks
   execute disposable projects through the same approved tool API as a session. *)
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub text i m = fragment || loop (i + 1)) in
  loop 0

let create path text =
  let channel = open_out path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let mkdir path = Unix.mkdir path 0o700

let expect label expected output =
  if not (contains output expected) then
    failwith (Printf.sprintf "%s: expected %S in output:\n%s" label expected output)

let () =
  match Sys.getenv_opt "PAVE_REAL_MOBILE_STACK" with
  | None -> ()
  | Some stack ->
      let root = Filename.temp_file "pave-real-mobile-" "" in
      Sys.remove root;
      mkdir root;
      let root = Unix.realpath root in
      let context = Pave.Tools.create_session_context ~owner:"mobile-real"
        ~root ~process_manager:(Pave.Workspace_process.create_manager ())
        ~read_artifact:(fun _ -> None)
        ~record_file_change:(fun ~path:_ ~before:_ ~after:_ -> ()) () in
      let call ?(approved = true) name fields =
        Pave.Tools.execute ~root ~context ~approved ~name
          ~args:(`Assoc fields) () in
      let mobile ?(more = []) action =
        call "mobile_check" (["stack", `String stack;
          "subroot", `String "."; "action", `String action;
          "timeout_seconds", `Int 300] @ more) in
      let run code =
        let before = Unix.getcwd () in
        Fun.protect ~finally:(fun () -> Unix.chdir before) (fun () ->
          Unix.chdir root;
          if Sys.command code <> 0 then failwith ("fixture setup failed: " ^ code)) in
      Fun.protect ~finally:(fun () -> Pave.Tools.close_session_context context)
        (fun () -> match stack with
        | "swiftpm" ->
            create (Filename.concat root "Package.swift")
              "// swift-tools-version: 5.9\nimport PackageDescription\nlet package = Package(name: \"MobileFixture\", products: [], targets: [.testTarget(name: \"MobileFixtureTests\")])\n";
            mkdir (Filename.concat root "Tests");
            mkdir (Filename.concat root "Tests/MobileFixtureTests");
            create (Filename.concat root "Tests/MobileFixtureTests/ExampleTests.swift")
              "import XCTest\nfinal class ExampleTests: XCTestCase { func testFocused() { XCTAssertEqual(2 + 2, 4) } }\n";
            let discovery = mobile "discover" in
            expect "SwiftPM discovery" "exit 0" discovery;
            let selected = String.split_on_char '\n' discovery
              |> List.find_opt (fun line -> contains line "testFocused")
              |> Option.value ~default:"" in
            if selected = "" then failwith ("SwiftPM test missing: " ^ discovery);
            let result = mobile ~more:["target", `String selected] "run" in
            expect "SwiftPM selected test" "exit 0" result;
            expect "SwiftPM selected test" "testFocused" result;
            print_endline "real SwiftPM focused test: exit 0"
        | "gradle" ->
            create (Filename.concat root "settings.gradle.kts")
              "rootProject.name = \"MobileFixture\"\ninclude(\":app\")\n";
            mkdir (Filename.concat root "app");
            create (Filename.concat root "app/build.gradle.kts")
              "tasks.register(\"assembleDebug\") { doLast { println(\"selected-debug-ok\") } }\ntasks.register(\"assembleRelease\") { doLast { println(\"release-not-selected\") } }\n";
            let discovery = mobile "tasks" in
            expect "Gradle task discovery" "exit 0" discovery;
            expect "Gradle task discovery" ":app:assembleDebug" discovery;
            let result = mobile ~more:["target", `String ":app:assembleDebug"]
              "run" in
            expect "Gradle selected variant" "exit 0" result;
            expect "Gradle selected variant" "selected-debug-ok" result;
            if contains result "release-not-selected" then
              failwith "Gradle ran an unselected variant";
            print_endline "real Gradle selected variant: exit 0"
        | "flutter" ->
            run "flutter create --project-name mobile_fixture --platforms=android .";
            let result = mobile "analyze" in
            expect "Flutter analysis" "exit 0" result;
            print_endline "real Flutter analysis: exit 0"
        | "xcode" ->
            mkdir (Filename.concat root "Sources");
            create (Filename.concat root "Sources/App.swift")
              "import UIKit\n@main final class AppDelegate: UIResponder, UIApplicationDelegate { var window: UIWindow?; func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool { true } }\n";
            create (Filename.concat root "project.yml")
              "name: MobileFixture\noptions:\n  bundleIdPrefix: dev.pave\ntargets:\n  MobileFixture:\n    type: application\n    platform: iOS\n    sources: [Sources]\n    settings:\n      base:\n        CODE_SIGNING_ALLOWED: NO\n        IPHONEOS_DEPLOYMENT_TARGET: '15.0'\nschemes:\n  MobileFixture:\n    build:\n      targets:\n        MobileFixture: all\n";
            run "xcodegen generate";
            let xcode action more = call "xcode_preflight"
              (["subroot", `String "MobileFixture.xcodeproj";
                "action", `String action; "timeout_seconds", `Int 300] @ more) in
            let schemes = xcode "schemes" [] in
            expect "Xcode schemes" "MobileFixture" schemes;
            let destinations = xcode "destinations"
              ["scheme", `String "MobileFixture"] in
            let marker = "Available iOS Simulator IDs: " in
            let id = if contains destinations marker then
                let rec find i =
                  if i + String.length marker > String.length destinations then
                    failwith ("missing simulator IDs: " ^ destinations)
                  else if String.sub destinations i (String.length marker) = marker
                  then i + String.length marker else find (i + 1) in
                let start = find 0 in
                let rest = String.sub destinations start (String.length destinations - start)
                  |> String.split_on_char '\n' |> List.hd in
                String.trim (List.hd (String.split_on_char ',' rest))
              else failwith ("no simulator destination: " ^ destinations) in
            if id = "none; select another scheme or make a compatible simulator runtime available"
            then failwith ("no available iOS simulator: " ^ destinations);
            let result = xcode "build"
              ["scheme", `String "MobileFixture"; "destination", `String id] in
            expect "Xcode selected non-signing build" "Xcode build: exit 0" result;
            print_endline "real Xcode simulator build: exit 0"
        | _ -> failwith ("unknown mobile acceptance stack: " ^ stack))
