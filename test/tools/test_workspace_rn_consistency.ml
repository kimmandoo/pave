module C = Pave.Workspace_rn_consistency

let expect label condition = if not condition then failwith label

let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec search i =
    i + m <= n && (String.sub text i m = fragment || search (i + 1)) in
  search 0

let write path text =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output text)

let rec remove_tree path =
  try match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.iter (fun name ->
          remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(* Pure declaration extraction. *)
let () =
  let literal, dynamic = C.js_references
    "import {NativeModules} from 'react-native';\n\
     const a = NativeModules.LocalStorage;\n\
     const b = NativeModules[key];\n\
     TurboModuleRegistry.get('');\n\
     TurboModuleRegistry.getEnforcing(\"Cald\");\n\
     requireNativeComponent('BarView');\n\
     requireNativeComponent(viewName);\n" in
  expect "js literal names"
    (List.map snd literal = ["LocalStorage"; "Cald"; "BarView"]);
  expect "js dynamic flagged" dynamic;
  let literal, _ = C.js_references "myNativeModules.Foo; NativeModules?.Bar" in
  expect "no spurious literals" (literal = []);
  let ios = C.ios_declarations
    "@implementation LocalStorage\nRCT_EXPORT_MODULE();\n@end\n\
     @implementation Other\nRCT_EXPORT_MODULE(CustomName);\n@end\n\
     RCT_EXTERN_MODULE(SwiftThing);\n\
     RCT_EXPORT_PRECISE_MODULE(JsName, dispatch_get_main_queue());\n\
     RCT_EXPORT_MODULE(factory());\n" in
  let ios_names = List.map snd ios in
  expect "ios inferred + literal"
    (ios_names = [`Inferred "LocalStorage"; `Literal "CustomName";
                  `Literal "SwiftThing"; `Literal "JsName"; `Dynamic]);
  let android = C.android_declarations
    "class A { public String getName() { return \"StorageModule\"; } }\n\
     class B { public String getName() { return name; } }\n\
     @ReactModule(name = \"CalModule\")\n\
     @ReactModule(name = Constants.C)\n\
     new ReactModuleInfo(\"InfoModule\")\n" in
  expect "android literal + dynamic"
    (List.map snd android =
       [`Literal "StorageModule"; `Dynamic;
        `Literal "CalModule"; `Dynamic; `Literal "InfoModule"]);
  expect "kts not js" (C.classify "app/android/build.gradle.kts" = C.Gradle);
  expect "app.config.ts not js" (C.classify "app/app.config.ts" = C.Dynamic_config);
  expect "app.json static" (C.classify "app/app.json" = C.Static_config);
  expect "objc mm" (C.classify "app/ios/X.mm" = C.Ios_native)

let () =
  let root = Filename.temp_file "pave-rn-consistency-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let directories = Hashtbl.create 64 in
    Hashtbl.add directories "." ();
    let files = ref [] in
    let mkdir relative =
      let absolute = Filename.concat root relative in
      Unix.mkdir absolute 0o700;
      Hashtbl.replace directories relative () in
    let create relative text =
      let absolute = Filename.concat root relative in
      write absolute text;
      files := relative :: !files in
    (* ---------- fixture: Expo app with ios+android hosts ---------- *)
    mkdir "app";
    mkdir "app/ios";
    mkdir "app/ios/App.xcodeproj";
    mkdir "app/android";
    mkdir "app/android/app";
    mkdir "app/android/app/src";
    mkdir "app/android/app/src/main";
    mkdir "app/android/app/src/main/res";
    mkdir "app/android/app/src/main/res/values";
    mkdir "app/android/app/src/main/java";
    mkdir "app/android/app/src/main/java/com";
    mkdir "app/android/app/src/main/java/com/example";
    mkdir "app/android/app/src/main/java/com/example/app";
    create "app/package.json"
      {|{"dependencies":{"expo":"52","react-native":"0.76"},"scripts":{"test":"jest"}}|};
    create "app/package-lock.json" "{}";
    create "app/app.json"
      {|{"expo":{"name":"My App","version":"1.2.3","ios":{"bundleIdentifier":"com.example.app","buildNumber":"7"},"android":{"package":"com.example.app","versionCode":7}}}|};
    create "app/storage.js"
      {|import {NativeModules, TurboModuleRegistry} from 'react-native';
const s = NativeModules.LocalStorage;
const t = NativeModules.StorageModule;
const c = TurboModuleRegistry.getEnforcing('Calendar');
const d = NativeModules[dynamicName()];
export const view = requireNativeComponent('BarView');
|};
    create "app/ios/LocalStorage.m"
      {|@implementation LocalStorage
RCT_EXPORT_MODULE();
@end
|};
    create "app/ios/Extra.m"
      {|@implementation Extra
RCT_EXPORT_MODULE(ExtraModule);
@end
|};
    create "app/ios/App.xcodeproj/project.pbxproj"
      {|PRODUCT_BUNDLE_IDENTIFIER = com.other.app;
MARKETING_VERSION = $(MARKETING_VERSION);
PRODUCT_NAME = "$(TARGET_NAME)";
|};
    create "app/ios/Info.plist"
      {|<?xml version="1.0"?>
<dict>
<key>CFBundleDisplayName</key><string>My App</string>
<key>CFBundleShortVersionString</key><string>1.2.3</string>
</dict>
|};
    create "app/android/app/build.gradle"
      {|android {
  defaultConfig {
    applicationId "com.example.app"
    versionName "1.2.3"
    versionCode 7
  }
}
|};
    create "app/android/app/src/main/AndroidManifest.xml"
      {|<manifest xmlns:android="http://schemas.android.com/apk/res/android" package="com.example.app">
</manifest>
|};
    create "app/android/app/src/main/res/values/strings.xml"
      {|<resources><string name="app_name">My App</string></resources>
|};
    create "app/android/app/src/main/java/com/example/app/StorageModule.kt"
      {|class StorageModule : ReactContextBaseJavaModule() {
  override fun getName() = "StorageModule"
}
|};
    create "app/android/app/src/main/java/com/example/app/CalModule.java"
      {|@ReactModule(name = "CalendarModule")
public class CalModule {
  public String getName() { return "CalendarModule"; }
}
|};
    (* ---------- managed Expo package without native roots ---------- *)
    mkdir "managed";
    create "managed/package.json"
      {|{"dependencies":{"expo":"52"},"scripts":{"test":"jest"}}|};
    create "managed/package-lock.json" "{}";
    create "managed/app.json" {|{"expo":{"name":"Solo","version":"2.0.0"}}|};
    create "managed/index.js" "const x = NativeModules.SoloMod;\n";
    (* ---------- Expo package with executable config ---------- *)
    mkdir "dynamiccfg";
    create "dynamiccfg/package.json"
      {|{"dependencies":{"expo":"52","react-native":"0.76"},"scripts":{"lint":"eslint ."}}|};
    create "dynamiccfg/package-lock.json" "{}";
    create "dynamiccfg/app.json" {|{"expo":{"version":"9.9.9"}}|};
    create "dynamiccfg/app.config.js" "module.exports = () => ({version: '1'});\n";
    mkdir "dynamiccfg/android";
    (* ---------- package with no declared test script ---------- *)
    mkdir "noscript";
    create "noscript/package.json"
      {|{"dependencies":{"react-native":"0.76"},"scripts":{"lint":"eslint ."}}|};
    create "noscript/package-lock.json" "{}";
    mkdir "noscript/ios";
    create "noscript/index.js" "const m = NativeModules.Missing;\n";
    (* ---------- sibling package + out-of-root native files ---------- *)
    mkdir "other";
    create "other/package.json"
      {|{"dependencies":{"react-native":"0.76"}}|};
    create "other/package-lock.json" "{}";
    mkdir "other/ios";
    create "other/ios/Other.m"
      "@implementation Other\nRCT_EXPORT_MODULE(OtherMod);\n@end\n";
    mkdir "ios";
    create "ios/Loose.m"
      "@implementation Loose\nRCT_EXPORT_MODULE(LooseMod);\n@end\n";
    create "ios/Match.m"
      "@implementation Match\nRCT_EXPORT_MODULE(LocalStorage);\n@end\n";
    create "loose.js" "const y = NativeModules.LooseMod;\n";
    (* ---------- report calls ---------- *)
    let package_dirs = ["app"; "managed"; "dynamiccfg"; "noscript"; "other"] in
    let report ~subroot ~platform ~expo ~test_hint =
      String.concat "\n"
        (C.report ~root ~subroot ~platform ~files:!files ~directories
           ~package_dirs ~expo ~test_hint) in
    (* ios platform *)
    let ios = report ~subroot:"app" ~platform:"ios" ~expo:true
      ~test_hint:"Observed declared test script: cd 'app' && npm run 'test' (preview only; run via approved mobile_check)" in
    expect "ios pair" (contains ios
      "Paired module LocalStorage: app/storage.js <-> app/ios/LocalStorage.m");
    expect "ios js mismatch cites files" (contains ios
      "Mismatch: JS native-module reference Calendar in app/storage.js has no ios declaration under app/ios");
    expect "ios js view mismatch" (contains ios
      "Mismatch: JS native-module reference BarView");
    expect "ios native mismatch cites files" (contains ios
      "Mismatch: ios declaration ExtraModule in app/ios/Extra.m is not referenced literally");
    expect "ios dynamic js" (contains ios
      "Unresolved dynamic JS/TS native-module references: app/storage.js");
    expect "ios config bundle mismatch" (contains ios
      "Expo config mismatch: app/app.json declares ios.bundleIdentifier = \"com.example.app\" but app/ios/App.xcodeproj/project.pbxproj declares \"com.other.app\"");
    expect "ios version not consistent (dynamic MARKETING_VERSION present)"
      (not (contains ios "Expo key version consistent"));
    expect "ios marketing version dynamic" (contains ios
      "Expo key version has dynamic or unresolved ios declarations: app/ios/App.xcodeproj/project.pbxproj");
    expect "ios name consistent" (contains ios
      "Expo key name consistent with native declarations: app/ios/Info.plist");
    expect "ios test hint" (contains ios "npm run 'test'");
    (* out-of-root declarations must not be used *)
    expect "loose ios file ignored" (not (contains ios "ios/Loose.m"));
    expect "loose match not paired" (not (contains ios "ios/Match.m"));
    expect "other package ignored" (not (contains ios "other/ios/Other.m"));
    (* android platform *)
    let android = report ~subroot:"app" ~platform:"android" ~expo:true
      ~test_hint:"Observed declared test script: cd 'app' && npm run 'test' (preview only; run via approved mobile_check)" in
    expect "android pair kotlin" (contains android
      "Paired module StorageModule: app/storage.js <-> app/android/app/src/main/java/com/example/app/StorageModule.kt");
    expect "android unmatched native" (contains android
      "Mismatch: android declaration CalendarModule in app/android/app/src/main/java/com/example/app/CalModule.java is not referenced literally");
    expect "android js unmatched" (contains android
      "Mismatch: JS native-module reference LocalStorage in app/storage.js has no android declaration under app/android");
    expect "android package consistent" (contains android
      "Expo key android.package consistent with native declarations");
    expect "android build gradle cited" (contains android "app/android/app/build.gradle");
    expect "android manifest cited" (contains android
      "app/android/app/src/main/AndroidManifest.xml");
    expect "android versionName consistent" (contains android
      "Expo key version consistent with native declarations: app/android/app/build.gradle");
    expect "android versionCode consistent" (contains android
      "Expo key android.versionCode consistent");
    expect "android app_name consistent" (contains android
      "Expo key name consistent with native declarations: app/android/app/src/main/res/values/strings.xml");
    (* managed Expo without native roots *)
    let managed = report ~subroot:"managed" ~platform:"ios" ~expo:true
      ~test_hint:"Observed declared test script: cd 'managed' && npm run 'test' (preview only; run via approved mobile_check)" in
    expect "managed reports no root" (contains managed
      "No ios native root under managed");
    expect "managed honest" (contains managed
      "managed Expo without prebuilt ios/ or android/ directories");
    expect "managed refs unknown" (contains managed
      "Literal JS native-module references without a ios host root (unknown): SoloMod in managed/index.js");
    expect "managed key compared-with-nothing" (contains managed
      "Expo key version has no observed ios declaration; consistency unknown.");
    (* executable config stays unresolved *)
    let dynamic = report ~subroot:"dynamiccfg" ~platform:"android" ~expo:true
      ~test_hint:"No declared 'test' script in dynamiccfg/package.json; no test suggested." in
    expect "dynamic config unresolved" (contains dynamic
      "Expo executable configuration dynamiccfg/app.config.js is never evaluated");
    expect "no test script honest" (contains dynamic
      "No declared 'test' script in dynamiccfg/package.json; no test suggested.");
    expect "dynamic android host" (contains dynamic "Native bridge declarations (android)");
    (* absent script + absent declarations *)
    let noscript = report ~subroot:"noscript" ~platform:"ios" ~expo:false
      ~test_hint:"No declared 'test' script in noscript/package.json; no test suggested." in
    expect "noscript hint" (contains noscript
      "No declared 'test' script in noscript/package.json; no test suggested.");
    expect "noscript mismatch" (contains noscript
      "Mismatch: JS native-module reference Missing in noscript/index.js has no ios declaration under noscript/ios");
    (* out-of-root: package root files under ios/ at workspace root stay outside *)
    let other = report ~subroot:"other" ~platform:"ios" ~expo:false
      ~test_hint:"No declared 'test' script in other/package.json; no test suggested." in
    expect "other sees own decl" (contains other
      "other/ios/Other.m");
    expect "out-of-root decls stay unused"
      (not (contains other "ios/Loose.m") &&
       not (contains other "LooseMod") &&
       not (contains other "ios/Match.m") &&
       not (contains other "loose.js"));
    (* consumer proof through the real tool surface *)
    let tool name fields =
      match Pave.Tools.execute ~root ~name
        ~args:(`Assoc (List.map (fun (k, v) -> k, `String v) fields)) () with
      | Ok blocks -> Pave.Protocol.display_content_blocks blocks
      | Error message -> message in
    let selected_ios = tool "mobile_project"
      ["subroot", "app"; "platform", "ios"] in
    expect "consumer shows pairing" (contains selected_ios
      "Paired module LocalStorage: app/storage.js <-> app/ios/LocalStorage.m");
    expect "consumer cites mismatch files" (contains selected_ios
      "Mismatch: JS native-module reference Calendar in app/storage.js has no ios declaration under app/ios");
    expect "consumer cites config files" (contains selected_ios
      "Expo config mismatch: app/app.json declares ios.bundleIdentifier = \"com.example.app\" but app/ios/App.xcodeproj/project.pbxproj declares \"com.other.app\"");
    expect "consumer test preview" (contains selected_ios
      "Observed declared test script: cd 'app' && npm run 'test' (preview only; run via approved mobile_check)");
    let managed_out = tool "mobile_project"
      ["subroot", "managed"; "platform", "ios"] in
    expect "consumer managed honest" (contains managed_out
      "managed Expo without prebuilt ios/ or android/ directories");
    let dyn_out = tool "mobile_project"
      ["subroot", "dynamiccfg"; "platform", "android"] in
    expect "consumer dynamic config" (contains dyn_out
      "Expo executable configuration dynamiccfg/app.config.js is never evaluated");
    expect "consumer absent script" (contains dyn_out
      "No declared 'test' script in dynamiccfg/package.json; no test suggested.");
    let noscript_out = tool "mobile_project"
      ["subroot", "noscript"; "platform", "ios"] in
    expect "consumer ios mismatch" (contains noscript_out
      "Mismatch: JS native-module reference Missing in noscript/index.js has no ios declaration under noscript/ios");
    (* unselected inventory stays evidence-only *)
    let inventory = tool "mobile_project" [] in
    expect "unselected hides consistency"
      (not (contains inventory "Native bridge declarations") &&
       not (contains inventory "Expo config mismatch"));
    (* previews never execute the declared script *)
    let marker = Filename.concat root "should-not-exist" in
    write (Filename.concat root "app/package.json")
      {|{"dependencies":{"expo":"52","react-native":"0.76"},"scripts":{"test":"touch ../should-not-exist"}}|};
    files := List.filter (( <> ) "app/package.json") !files;
    files := "app/package.json" :: !files;
    ignore (tool "mobile_project" ["subroot", "app"; "platform", "ios"]);
    expect "preview never ran script" (not (Sys.file_exists marker));
    (* restore for stable assertions below *)
    write (Filename.concat root "app/package.json")
      {|{"dependencies":{"expo":"52","react-native":"0.76"},"scripts":{"test":"jest"}}|};
    print_endline "workspace rn consistency: ok")
