module S = Pave.Workspace_sensitive

let expect label value = if not value then failwith ("sensitive: " ^ label)
let rejects label operation =
  try ignore (operation ()); failwith ("sensitive accepted " ^ label)
  with S.Error _ -> ()

let ordinary label path before after =
  match S.classify ~path ~before ~after with
  | S.Ordinary -> ()
  | S.Literal r -> failwith ("sensitive: " ^ label ^ " unexpectedly literal (" ^
                             String.concat "; "
                               (List.map (fun f -> f.S.detail) r.S.findings) ^ ")")
  | S.Unresolved r -> failwith ("sensitive: " ^ label ^ " unexpectedly unresolved (" ^
                                String.concat "; " r.S.unresolved ^ ")")

let literal label path before after =
  match S.classify ~path ~before ~after with
  | S.Literal r -> r
  | S.Ordinary -> failwith ("sensitive: " ^ label ^ " came back ordinary")
  | S.Unresolved r -> failwith ("sensitive: " ^ label ^ " unexpectedly unresolved (" ^
                                String.concat "; " r.S.unresolved ^ ")")

let unresolved label path before after =
  match S.classify ~path ~before ~after with
  | S.Unresolved r -> r
  | S.Ordinary -> failwith ("sensitive: " ^ label ^ " came back ordinary")
  | S.Literal r -> failwith ("sensitive: " ^ label ^ " unexpectedly literal (" ^
                             String.concat "; "
                               (List.map (fun f -> f.S.detail) r.S.findings) ^ ")")

let contains haystack needle =
  let n = String.length needle in
  let rec find i =
    i + n <= String.length haystack &&
    (String.sub haystack i n = needle || find (i + 1)) in
  find 0

let has_detail report needle =
  List.exists (fun f -> contains f.S.detail needle) report.S.findings

let no_secret label verdict needles =
  let text = S.describe verdict ^
    (match verdict with
     | S.Ordinary -> ""
     | S.Literal r | S.Unresolved r ->
         String.concat " " (List.map (fun f -> f.S.detail) r.S.findings) ^
         " " ^ String.concat " " r.S.unresolved) in
  List.iter (fun n ->
    let rec find i =
      if i + String.length n <= String.length text &&
         String.sub text i (String.length n) = n then
        failwith ("sensitive: " ^ label ^ " leaked secret text")
      else if i + String.length n <= String.length text then find (i + 1) in
    find 0) needles

let plist body =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<plist version=\"1.0\">\n" ^
  "<dict>\n" ^ body ^ "</dict>\n</plist>\n"

let () =
  (* ---- iOS literal changes ---- *)
  let r = literal "entitlement added" "App/App.entitlements"
      (plist "") (plist "<key>aps-environment</key>\n<string>development</string>\n") in
  expect "entitlement effect names key and value"
    (has_detail r "aps-environment" && has_detail r "development" &&
     r.S.platform = S.Ios && r.S.file = "App/App.entitlements");
  let r = literal "entitlement removed" "App/App.entitlements"
      (plist "<key>com.apple.security.app-sandbox</key>\n<true/>\n")
      (plist "") in
  expect "entitlement removal cited" (has_detail r "app-sandbox" &&
    List.exists (fun f -> f.S.change = S.Removed) r.S.findings);
  let r = literal "entitlement boolean change" "App/App.entitlements"
      (plist "<key>com.apple.security.network.client</key>\n<false/>\n")
      (plist "<key>com.apple.security.network.client</key>\n<true/>\n") in
  expect "boolean entitlement change cited"
    (has_detail r "false -> true" || has_detail r "true");
  let pbx_b = "X1 /* conf */ = { isa = XCBuildConfiguration;\n" ^
              "  buildSettings = {\n" ^
              "    PRODUCT_BUNDLE_IDENTIFIER = com.example.old;\n" ^
              "    IPHONEOS_DEPLOYMENT_TARGET = 15.0;\n" ^
              "    DEVELOPMENT_TEAM = TEAM123;\n" ^
              "    CODE_SIGN_STYLE = Automatic;\n  };\n};\n" in
  let pbx_a = "X1 /* conf */ = { isa = XCBuildConfiguration;\n" ^
              "  buildSettings = {\n" ^
              "    PRODUCT_BUNDLE_IDENTIFIER = com.example.new;\n" ^
              "    IPHONEOS_DEPLOYMENT_TARGET = 15.0;\n" ^
              "    DEVELOPMENT_TEAM = TEAM456;\n" ^
              "    CODE_SIGN_STYLE = Manual;\n  };\n};\n" in
  let r = literal "pbxproj bundle and signing" "App.xcodeproj/project.pbxproj"
      pbx_b pbx_a in
  expect "bundle id old->new cited"
    (has_detail r "com.example.old -> com.example.new");
  expect "team and sign style cited"
    (has_detail r "DEVELOPMENT_TEAM" && has_detail r "CODE_SIGN_STYLE");
  let r = literal "xcconfig literal signing" "Config/Release.xcconfig"
      "DEVELOPMENT_TEAM = ABC\n// comment\n"
      "DEVELOPMENT_TEAM = XYZ\n// comment\n" in
  expect "xcconfig team cited" (has_detail r "ABC -> XYZ");
  let info_b = plist ("<key>CFBundleIdentifier</key>\n<string>com.a</string>\n" ^
                      "<key>CFBundleVersion</key>\n<string>1</string>\n") in
  let info_a = plist ("<key>CFBundleIdentifier</key>\n<string>com.a</string>\n" ^
                      "<key>CFBundleVersion</key>\n<string>2</string>\n") in
  let r = literal "Info.plist version" "App/Info.plist" info_b info_a in
  expect "CFBundleVersion change cited" (has_detail r "CFBundleVersion");
  (* ---- Android literal changes ---- *)
  let man_b = "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\">\n" ^
              "  <uses-sdk android:minSdkVersion=\"24\" android:targetSdkVersion=\"34\"/>\n" ^
              "  <application android:label=\"app\"/>\n</manifest>\n" in
  let man_a = "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\">\n" ^
              "  <uses-sdk android:minSdkVersion=\"26\" android:targetSdkVersion=\"34\"/>\n" ^
              "  <uses-permission android:name=\"android.permission.CAMERA\"/>\n" ^
              "  <application android:label=\"app\"/>\n</manifest>\n" in
  let r = literal "manifest permission and sdk" "app/src/main/AndroidManifest.xml"
      man_b man_a in
  expect "permission addition cited"
    (has_detail r "android.permission.CAMERA" && r.S.platform = S.Android);
  expect "minSdk literal change cited"
    (has_detail r "minSdkVersion" && has_detail r "24 -> 26");
  let gradle_b = "android {\n  defaultConfig {\n    minSdk 24\n" ^
                 "    targetSdk = 34\n  }\n}\n" in
  let gradle_a = "android {\n  defaultConfig {\n    minSdk 26\n" ^
                 "    targetSdk = 35\n  }\n}\n" in
  let r = literal "gradle sdk levels" "app/build.gradle.kts" gradle_b gradle_a in
  expect "minSdk and targetSdk cited"
    (has_detail r "minSdk" && has_detail r "24 -> 26" &&
     has_detail r "35");
  let r = literal "gradle signing settings" "app/build.gradle"
      ("buildTypes {\n  release {\n    signingConfig signingConfigs.debug\n" ^
       "    v1SigningEnabled true\n  }\n}\n")
      ("buildTypes {\n  release {\n    signingConfig signingConfigs.release\n" ^
       "    v1SigningEnabled false\n  }\n}\n") in
  expect "signing config name and v1 flag cited"
    (has_detail r "signingConfig" && has_detail r "v1SigningEnabled");
  (* ---- ordinary files stay ordinary ---- *)
  ordinary "swift source" "App/main.swift" "let a = 1\n" "let a = 2\n";
  ordinary "kotlin source" "app/Main.kt"
      "fun f() = 1\n" "fun f() = 2\n";
  ordinary "unrelated plist key" "App/Info.plist"
      (plist "<key>CFBundleDisplayName</key>\n<string>Old</string>\n")
      (plist "<key>CFBundleDisplayName</key>\n<string>New</string>\n");
  ordinary "unrelated gradle line" "app/build.gradle"
      "defaultConfig {\n  applicationId \"com.a\"\n  shrinkResources false\n}\n"
      "defaultConfig {\n  applicationId \"com.a\"\n  shrinkResources true\n}\n";
  ordinary "manifest application label" "app/src/main/AndroidManifest.xml"
      "<manifest><application android:label=\"old\"/></manifest>\n"
      "<manifest><application android:label=\"new\"/></manifest>\n";
  ordinary "unchanged sensitive file" "App/App.entitlements"
      (plist "<key>a</key>\n<true/>\n")
      (plist "<key>a</key>\n<true/>\n");
  ordinary "readme" "README.md" "a\n" "b\n";
  (* ---- dynamic / malformed stays unresolved ---- *)
  let r = unresolved "computed bundle id" "App.xcodeproj/project.pbxproj"
      "PRODUCT_BUNDLE_IDENTIFIER = com.example.old;\n"
      "PRODUCT_BUNDLE_IDENTIFIER = $(BUNDLE_ID);\n" in
  expect "dynamic value explained" (r.S.unresolved <> []);
  let _ = unresolved "entitlements malformed" "App/App.entitlements"
      (plist "") "<plist><dict>\n<key>x</key>\n" in
  let _ = unresolved "xcconfig include change" "Config/Base.xcconfig"
      "#include \"Base-A.xcconfig\"\nDEVELOPMENT_TEAM = A\n"
      "#include \"Base-B.xcconfig\"\nDEVELOPMENT_TEAM = A\n" in
  let _ = unresolved "gradle computed sdk" "app/build.gradle"
      "minSdkVersion 24\n" "minSdkVersion rootProject.ext.min\n" in
  let _ = unresolved "manifest placeholder" "app/src/main/AndroidManifest.xml"
      "<manifest package=\"com.a\"/>\n"
      "<manifest package=\"${applicationId}\"/>\n" in
  let _ = unresolved "manifest truncated element" "app/src/main/AndroidManifest.xml"
      "<manifest/>\n" "<manifest>\n<uses-permission android:name=\"x\"\n" in
  let _ = unresolved "podfile executable" "ios/Podfile"
      "platform :ios, '15.0'\ntarget 'A'\n"
      "platform :ios, '16.0'\ntarget 'A'\n" in
  let _ = unresolved "fastfile executable" "ios/fastlane/Fastfile"
      "lane :a do\nend\n" "lane :b do\nend\n" in
  (match S.classify_proposal ~path:"App/App.entitlements" ~before:None
         ~after:(plist "<key>aps-environment</key>\n<true/>\n") with
   | S.Unresolved _ -> ()
   | _ -> failwith "sensitive: unconfirmed original became a verified change");
  (* ---- bounds and path escapes ---- *)
  rejects "parent traversal" (fun () ->
    S.classify ~path:"../Info.plist" ~before:"" ~after:"<dict/>");
  rejects "embedded traversal" (fun () ->
    S.classify ~path:"app/../Info.plist" ~before:"" ~after:"<dict/>");
  rejects "absolute path" (fun () ->
    S.classify ~path:"/tmp/Info.plist" ~before:"" ~after:"x");
  rejects "empty path" (fun () ->
    S.classify ~path:"" ~before:"" ~after:"x");
  rejects "NUL path" (fun () ->
    S.classify ~path:"a\000b.plist" ~before:"" ~after:"x");
  let big = String.make (1_048_576 + 1) 'x' in
  rejects "oversized after" (fun () ->
    S.classify ~path:"App/Info.plist" ~before:"" ~after:big);
  rejects "oversized before" (fun () ->
    S.classify ~path:"App/Info.plist" ~before:big ~after:"");
  (* ---- no secret disclosure ---- *)
  let v = S.classify ~path:"app/build.gradle"
      ~before:("signingConfigs {\n  release {\n    storePassword \"hunter2\"\n" ^
               "    keyPassword \"oldk3y\"\n    storeFile file(\"release.jks\")\n  }\n}\n")
      ~after:("signingConfigs {\n  release {\n    storePassword \"n3wpass\"\n" ^
              "    keyPassword \"n3wk3y\"\n    storeFile file(\"other.jks\")\n  }\n}\n") in
  (match v with
   | S.Literal r ->
       expect "signing findings redacted" (r.S.findings <> []);
       no_secret "gradle signing" v
         ["hunter2"; "n3wpass"; "oldk3y"; "n3wk3y"; "release.jks"; "other.jks"]
   | _ -> failwith "sensitive: literal signing change not classified");
  let v = S.classify ~path:"app/key.properties" ~before:
      "storePassword=hunter2\nkeyAlias=old\n"
      ~after:"storePassword=n3wpass\nkeyAlias=new\n" in
  (match v with
   | S.Literal r -> expect "key.properties findings" (r.S.findings <> [])
   | _ -> failwith "sensitive: key.properties not classified");
  no_secret "key.properties" v ["hunter2"; "n3wpass"];
  let v = S.classify ~path:"app/release.keystore"
      ~before:"RAWKEYSTOREBYTES\x00\x01" ~after:"OTHERBYTES\x00\x02" in
  no_secret "keystore bytes" v ["RAWKEYSTOREBYTES"; "OTHERBYTES"];
  let v = S.classify ~path:"ios/AuthKey_ABC123.p8"
      ~before:"PRIVATE KEY MATERIAL" ~after:"OTHER KEY MATERIAL" in
  no_secret "p8 signing key" v ["PRIVATE KEY MATERIAL"; "OTHER KEY MATERIAL"];
  let v = S.classify ~path:"ios/App.mobileprovision"
      ~before:"PROFILE-A-BYTES" ~after:"PROFILE-B-BYTES" in
  no_secret "provisioning profile" v ["PROFILE-A-BYTES"; "PROFILE-B-BYTES"];
  (* describe stays bounded and printable *)
  let v = S.classify ~path:"App/App.entitlements"
      ~before:(plist "") ~after:(plist "<key>aps-environment</key>\n<string>development</string>\n") in
  let text = S.describe v in
  expect "describe cites file" (text <> "" && contains text "App.entitlements");
  print_endline "workspace sensitive change classifier: ok"
