(* Opt-in runtime acceptance exercises disposable projects through the approved
   tool API. CI provisions non-Xcode toolchains; Xcode is manual-only. *)
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
        match Pave.Tools.execute ~root ~context ~approved ~name
          ~args:(`Assoc fields) () with
        | Ok blocks -> Pave.Protocol.display_content_blocks blocks
        | Error message -> message in
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
        | "android_test" ->
            create (Filename.concat root "settings.gradle.kts")
              {|pluginManagement { repositories { google(); mavenCentral(); gradlePluginPortal() } }
dependencyResolutionManagement { repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS); repositories { google(); mavenCentral() } }
rootProject.name = "MobileInstrumentationFixture"
include(":app")
|};
            mkdir (Filename.concat root "app");
            create (Filename.concat root "app/build.gradle.kts")
              {|plugins { id("com.android.application") version "9.1.1" }
android {
  namespace = "dev.pave.mobilefixture"
  compileSdk = 35
  defaultConfig {
    applicationId = "dev.pave.mobilefixture"
    minSdk = 23
    targetSdk = 28
    versionCode = 1
    versionName = "1"
    testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
  }
  testOptions { animationsDisabled = true }
}
configurations.configureEach {
  resolutionStrategy.force(
    "androidx.lifecycle:lifecycle-common:2.6.2",
    "androidx.annotation:annotation-jvm:1.9.1")
}
dependencies {
  androidTestImplementation("androidx.test:runner:1.7.0")
}
|};
            mkdir (Filename.concat root "app/src");
            mkdir (Filename.concat root "app/src/main");
            mkdir (Filename.concat root "app/src/main/java");
            mkdir (Filename.concat root "app/src/main/java/dev");
            mkdir (Filename.concat root "app/src/main/java/dev/pave");
            mkdir (Filename.concat root "app/src/main/java/dev/pave/mobilefixture");
            create (Filename.concat root "app/src/main/java/dev/pave/mobilefixture/SmokeActivity.java")
              {|package dev.pave.mobilefixture;
public final class SmokeActivity extends android.app.Activity {
  private int count = 0;
  private android.widget.TextView counter;
  @Override public void onCreate(android.os.Bundle state) {
    super.onCreate(state);
    android.widget.LinearLayout layout = new android.widget.LinearLayout(this);
    layout.setOrientation(android.widget.LinearLayout.VERTICAL);
    counter = new android.widget.TextView(this);
    counter.setContentDescription("count:0");
    layout.addView(counter);
    android.widget.Button button = new android.widget.Button(this);
    button.setText("Increment");
    button.setContentDescription("Increment");
    button.setOnClickListener(view -> {
      count++;
      counter.setText("Counter " + count);
      counter.setContentDescription("count:" + count);
    });
    layout.addView(button);
    setContentView(layout);
  }
}
|};
            mkdir (Filename.concat root "app/src/androidTest");
            create (Filename.concat root "app/src/main/AndroidManifest.xml")
              {|<manifest xmlns:android="http://schemas.android.com/apk/res/android"><application android:label="Pave Fixture"><activity android:name=".SmokeActivity" android:exported="true"><intent-filter><action android:name="android.intent.action.MAIN"/><category android:name="android.intent.category.LAUNCHER"/></intent-filter></activity></application></manifest>|};
            mkdir (Filename.concat root "app/src/androidTest/java");
            mkdir (Filename.concat root "app/src/androidTest/java/dev");
            mkdir (Filename.concat root "app/src/androidTest/java/dev/pave");
            mkdir (Filename.concat root "app/src/androidTest/java/dev/pave/mobilefixture");
            create (Filename.concat root "app/src/androidTest/java/dev/pave/mobilefixture/SmokeTest.java")
              {|package dev.pave.mobilefixture;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
public final class SmokeTest {
  @Test public void disposableInstrumentationRunsOnSelectedEmulator() {
    assertEquals(4, 2 + 2);
  }
}
|};
            let approve name args =
              let preview = Pave.Tools.approval_request ~context ~root ~name ~args
                (Pave.Tools.approval_decision ~command_patterns:[] ~name ~args) in
              if not (Unix.isatty (Unix.descr_of_in_channel stdin)) then
                failwith "manual Android test acceptance requires an interactive terminal";
              Printf.printf "Disposable project: %s\n%s\n" root preview.impact;
              List.iter print_endline preview.details;
              print_string "Approve this command? [y/N] ";
              flush stdout;
              if (try read_line () with End_of_file -> "") <> "y" then
                failwith "manual Android command denied" in
            let run_mobile fields =
              let args = `Assoc fields in
              approve "mobile_check" args;
              call "mobile_check" fields in
            let discovery = run_mobile [
              "stack", `String "gradle"; "action", `String "tasks";
              "subroot", `String "."; "timeout_seconds", `Int 300] in
            expect "Gradle instrumentation task discovery" ":app:assembleDebugAndroidTest" discovery;
            expect "Gradle app task discovery" ":app:assembleDebug" discovery;
            let devices_args = `Assoc [
              "action", `String "devices"; "subroot", `String "."] in
            approve "android_devices" devices_args;
            let inventory = call "android_devices"
              ["action", `String "devices"; "subroot", `String "."] in
            expect "Android emulator inventory" "Android ADB inventory: exit 0"
              inventory;
            let ready = String.split_on_char '\n' inventory
              |> List.find_opt (fun line ->
                   contains line "Ready emulators:" &&
                   not (contains line "none")) in
            let serial = match ready with
              | None -> failwith ("no attached ready Android emulator: " ^ inventory)
              | Some line ->
                  let marker = "emulator-" in
                  let rec find index =
                    if index + String.length marker > String.length line then
                      failwith ("ready inventory had no selectable emulator: " ^ line)
                    else if String.sub line index (String.length marker) = marker
                    then index + String.length marker
                    else find (index + 1) in
                  let start = find 0 in
                  let stop = try String.index_from line start ' '
                    with Not_found -> String.length line in
                  String.sub line (start - String.length marker)
                    (stop - start + String.length marker) in
            Printf.printf "Selected ready emulator: %s\n" serial;
            print_string "Use this emulator for the disposable instrumentation test? [y/N] ";
            flush stdout;
            if (try read_line () with End_of_file -> "") <> "y" then
              failwith "Android emulator selection denied";
            let session_fields = [
              "platform", `String "android";
              "subroot", `String ".";
              "device", `String serial;
              "app_id", `String "dev.pave.mobilefixture";
              "app_path", `String "app/build/outputs/apk/debug/app-debug.apk";
              "variant", `String "debug";
              "activity", `String "dev.pave.mobilefixture/.SmokeActivity"] in
            let selected = call "mobile_session"
              (("action", `String "select") :: session_fields) in
            expect "Android app session selection" "Selected mobile app session:" selected;
            let run_session action extra =
              let fields = ["action", `String action;
                "session_id", `String "mobile-1";
                "timeout_seconds", `Int 300] @ extra in
              approve "mobile_session" (`Assoc fields);
              call "mobile_session" fields in
            let observe action =
              let fields = ["action", `String action;
                "session_id", `String "mobile-1";
                "timeout_seconds", `Int 60] in
              approve "mobile_observe" (`Assoc fields);
              match Pave.Tools.execute ~root ~context ~approved:true
                ~name:"mobile_observe" ~args:(`Assoc fields) () with
              | Ok blocks -> blocks
              | Error message -> failwith ("Android " ^ action ^ ": " ^ message) in
            let control action extra =
              let fields = ["action", `String action;
                "session_id", `String "mobile-1";
                "timeout_seconds", `Int 60] @ extra in
              approve "mobile_control" (`Assoc fields);
              call "mobile_control" fields in
            let accessibility_nodes tree =
              let json = Yojson.Basic.from_string tree in
              Yojson.Basic.Util.(json |> member "nodes" |> to_list) in
            let node_string node field =
              Yojson.Basic.Util.(node |> member field |> to_string) in
            expect "Android session build" "Mobile build completed"
              (run_session "build" ["task", `String ":app:assembleDebug"]);
            let build_test = run_mobile [
              "stack", `String "gradle"; "action", `String "run";
              "subroot", `String "."; "target", `String ":app:assembleDebugAndroidTest";
              "timeout_seconds", `Int 300] in
            expect "Android instrumentation APK build" "exit 0" build_test;
            let run_shell command =
              let args = `Assoc [
                "command", `String command; "timeout_seconds", `Int 60] in
              approve "run_command" args;
              call "run_command" [
                "command", `String command; "timeout_seconds", `Int 60] in
            let test_apk = Filename.concat root "app/build/outputs/apk/androidTest/debug/app-debug-androidTest.apk" in
            expect "Android session install" "Mobile install completed"
              (run_session "install" []);
            let package_state = run_shell ("adb -s " ^ Filename.quote serial ^
              " shell dumpsys package dev.pave.mobilefixture") in
            let relevant = String.split_on_char '\n' package_state
              |> List.filter (fun line -> contains line "SmokeActivity" ||
                   contains line "MAIN" || contains line "LAUNCHER") in
            print_endline ("Android fixture package diagnostic:\n" ^
              String.concat "\n" relevant);
            expect "Android session launch" "Mobile launch completed"
              (run_session "launch" []);
            (match observe "screenshot" with
             | [Pave.Protocol.Text summary;
                Pave.Protocol.Image { mime_type = "image/png"; data }] ->
                 expect "Android screenshot metadata" "\"status\":\"available\"" summary;
                 expect "Android screenshot payload" "iVBOR" data
             | _ -> failwith "Android screenshot did not return an image block");
            let before_tree = match observe "accessibility" with
              | [Pave.Protocol.Text tree] ->
                  expect "Android accessibility tree" "\"status\":\"available\"" tree;
                  expect "Android accessibility nodes" "\"node_count\":" tree;
                  tree
              | _ -> failwith "Android accessibility capture returned unexpected blocks" in
            let nodes = accessibility_nodes before_tree in
            if not (List.exists (fun node ->
                node_string node "description" = "count:0") nodes) then
              failwith ("Android fixture did not expose its initial state: " ^ before_tree);
            let button = match List.find_opt (fun node ->
                node_string node "text" = "Increment") nodes with
              | Some node -> node
              | None -> failwith ("Android fixture button missing from accessibility tree: " ^
                  before_tree) in
            let x1, y1, x2, y2 =
              try Scanf.sscanf (node_string button "bounds") "[%d,%d][%d,%d]"
                (fun x1 y1 x2 y2 -> x1, y1, x2, y2)
              with _ -> failwith "Android fixture button bounds were malformed" in
            if x2 <= x1 || y2 <= y1 then failwith "Android fixture button has empty bounds";
            let tap_x = x1 + (x2 - x1) / 2 and tap_y = y1 + (y2 - y1) / 2 in
            expect "Android tap control" "Mobile tap completed"
              (control "tap" ["x", `Int tap_x; "y", `Int tap_y]);
            let after_tree = match observe "accessibility" with
              | [Pave.Protocol.Text tree] -> tree
              | _ -> failwith "Android post-tap accessibility capture returned unexpected blocks" in
            if not (List.exists (fun node ->
                node_string node "description" = "count:1")
                (accessibility_nodes after_tree)) then
              failwith ("Android tap did not change the accessible fixture state: " ^ after_tree);
            print_endline ("real Android UI control on " ^ serial ^
              ": tapped the accessible Increment button and verified count:1");
            expect "Android session stop" "Mobile stop completed"
              (run_session "stop" []);
            print_endline ("real Android app session on " ^ serial ^
              ": built, installed, launched and stopped");
            if not (Sys.file_exists test_apk) then
              failwith ("expected test APK missing: " ^ test_apk);
            let installed_test = run_shell ("adb -s " ^ Filename.quote serial ^
              " install -r " ^ Filename.quote test_apk) in
            expect "selected emulator test APK install" "Success" installed_test;
            let test = run_shell ("adb -s " ^ Filename.quote serial ^
              " shell am instrument -w " ^
              "dev.pave.mobilefixture.test/androidx.test.runner.AndroidJUnitRunner") in
            expect "selected Android emulator instrumentation" "OK (1 test)" test;
            print_endline ("real Android instrumentation on " ^ serial ^
              ": 1 test passed")
        | "flutter" ->
            run "flutter create --project-name mobile_fixture --platforms=android .";
            let result = mobile "analyze" in
            expect "Flutter analysis" "exit 0" result;
            print_endline "real Flutter analysis: exit 0"
        | "android_devices" ->
            create (Filename.concat root "settings.gradle.kts")
              "rootProject.name = \"DeviceFixture\"\ninclude(\":app\")\n";
            let inventory action =
              let args = `Assoc ["subroot", `String ".";
                "action", `String action] in
              let preview = Pave.Tools.approval_request ~root
                ~name:"android_devices" ~args
                (Pave.Tools.approval_decision ~command_patterns:[]
                  ~name:"android_devices" ~args) in
              if not (Unix.isatty (Unix.descr_of_in_channel stdin)) then
                failwith "manual Android acceptance requires an interactive terminal";
              Printf.printf "Disposable project: %s\n%s\n" root preview.impact;
              List.iter print_endline preview.details;
              print_string "Approve this command? [y/N] ";
              flush stdout;
              if (try read_line () with End_of_file -> "") <> "y" then
                failwith "manual Android command denied";
              call "android_devices" ["subroot", `String ".";
                "action", `String action] in
            let avds = inventory "avds" in
            expect "Android AVD inventory" "Android AVD inventory: exit 0" avds;
            if contains avds "SDK image readiness unknown): none" then
              failwith "no real configured Android AVD available";
            print_endline avds;
            let devices = inventory "devices" in
            expect "Android ADB inventory" "Android ADB inventory: exit 0" devices;
            print_endline devices
        | "xcode" | "simulators" | "xcode_test" ->
            mkdir (Filename.concat root "Sources");
            create (Filename.concat root "Sources/App.swift")
              "import UIKit\n@main final class AppDelegate: UIResponder, UIApplicationDelegate { var window: UIWindow?; func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool { true } }\n";
            if stack = "xcode_test" then (
              mkdir (Filename.concat root "Tests");
              create (Filename.concat root "Tests/MobileFixtureTests.swift")
                "import XCTest\nfinal class MobileFixtureTests: XCTestCase { func testDisposableFocus() { XCTAssertEqual(2 + 2, 4) } }\n");
            create (Filename.concat root "project.yml")
              ("name: MobileFixture\noptions:\n  bundleIdPrefix: dev.pave\ntargets:\n  MobileFixture:\n    type: application\n    platform: iOS\n    sources: [Sources]\n    settings:\n      base:\n        CODE_SIGNING_ALLOWED: NO\n        GENERATE_INFOPLIST_FILE: YES\n        IPHONEOS_DEPLOYMENT_TARGET: '15.0'\n" ^
               (if stack = "xcode_test" then
                 "  MobileFixtureTests:\n    type: bundle.unit-test\n    platform: iOS\n    sources: [Tests]\n    dependencies:\n      - target: MobileFixture\n    settings:\n      base:\n        CODE_SIGNING_ALLOWED: NO\n        GENERATE_INFOPLIST_FILE: YES\n        IPHONEOS_DEPLOYMENT_TARGET: '15.0'\n"
               else "") ^
               "schemes:\n  MobileFixture:\n    build:\n      targets:\n        MobileFixture: all\n" ^
               (if stack = "xcode_test" then
                 "    test:\n      targets:\n        - MobileFixtureTests\n"
               else ""));
            run "xcodegen generate";
            let xcode action more =
              let fields = ["subroot", `String "MobileFixture.xcodeproj";
                "action", `String action; "timeout_seconds", `Int 300] @ more in
              let args = `Assoc fields in
              let preview = Pave.Tools.approval_request ~root
                ~name:"xcode_preflight" ~args
                (Pave.Tools.approval_decision ~command_patterns:[]
                  ~name:"xcode_preflight" ~args) in
              if not (Unix.isatty (Unix.descr_of_in_channel stdin)) then
                failwith "manual Xcode acceptance requires an interactive terminal";
              Printf.printf "Disposable project: %s\n%s\n" root preview.impact;
              List.iter print_endline preview.details;
              print_string "Approve this command? [y/N] ";
              flush stdout;
              if (try read_line () with End_of_file -> "") <> "y" then
                failwith "manual Xcode command denied";
              call "xcode_preflight" fields in
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
            if stack = "simulators" then (
              let inventory = xcode "simulators"
                ["scheme", `String "MobileFixture"] in
              expect "Apple simulator inventory" "Apple simulator inventory: exit 0"
                inventory;
              expect "Selected simulator" id inventory;
              print_endline ("real compatible Apple simulator: " ^ id))
            else if stack = "xcode_test" then (
              let inventory = xcode "simulators"
                ["scheme", `String "MobileFixture"] in
              expect "Apple simulator inventory" "Apple simulator inventory: exit 0"
                inventory;
              expect "Selected simulator" id inventory;
              let result = xcode "test"
                ["scheme", `String "MobileFixture"; "destination", `String id] in
              expect "Xcode selected simulator test" "Xcode test: exit 0" result;
              print_endline ("real Xcode simulator test on " ^ id ^ ": exit 0"))
            else (
              let result = xcode "build"
                ["scheme", `String "MobileFixture"; "destination", `String id] in
              expect "Xcode selected non-signing build" "Xcode build: exit 0" result;
              print_endline "real Xcode simulator build: exit 0")
        | _ -> failwith ("unknown mobile acceptance stack: " ^ stack))
