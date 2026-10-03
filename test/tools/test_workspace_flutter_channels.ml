module Channels = Pave.Workspace_flutter_channels


let any_contains lines fragment =
  List.exists (fun line ->
    let n = String.length line and m = String.length fragment in
    let rec search i =
      i + m <= n &&
      (String.sub line i m = fragment || search (i + 1)) in
    search 0) lines

let expect_contains lines fragment =
  if not (any_contains lines fragment) then
    failwith (Printf.sprintf
      "Flutter channel pairing: missing line containing %S in:\n%s"
      fragment (String.concat "\n" lines))

let expect_absent lines fragment =
  if any_contains lines fragment then
    failwith (Printf.sprintf
      "Flutter channel pairing: unexpected line containing %S in:\n%s"
      fragment (String.concat "\n" lines))

let write path contents =
  Pave.Workspace_path.with_fd path
    [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
    (fun fd -> Pave.Workspace_path.write_all fd contents)

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let () =
  let root = Filename.temp_file "pave-flutter-channels-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let at path = Filename.concat root path in
    let rec mkdir_p path =
      let absolute = at path in
      if not (Sys.file_exists absolute) then (
        mkdir_p (Filename.dirname path) ;
        if Filename.dirname path <> path then Unix.mkdir absolute 0o700) in
    let create path contents =
      mkdir_p (Filename.dirname path);
      write (at path) contents in

    (* A disposable plugin-style package exercising every pairing outcome. *)
    create "pkg/lib/channels.dart"
      {|import 'package:flutter/services.dart';

const MethodChannel _battery = MethodChannel('com.example/battery');
final EventChannel _events = EventChannel('com.example/events');
final MethodChannel _computed = MethodChannel('com.example/' + suffix);
final MethodChannel _lonely = MethodChannel('com.example/lonely');
final MethodChannel _scalar = MethodChannel('com.example/scalar');

Future<int> level() => _battery.invokeMethod<int>('getLevel');
Future<void> bogus() => _battery.invokeMethod('bogus');
Future<int> inline() =>
    MethodChannel('com.example/inline').invokeMethod<int>('fetch');
Future<void> opaque() => _battery.invokeMethod('getLevel', settings);
Future<void> odd() => _scalar.invokeMethod('ping', 'hello');
Future<void> missed() => other.invokeMethod('whatever');
|};
    create "pkg/ios/Classes/BatteryPlugin.swift"
      {|import Flutter

public class BatteryPlugin: NSObject, FlutterPlugin {
  public static func register(with registrar: FlutterPluginRegistrar) {
    let channel = FlutterMethodChannel(
      name: "com.example/battery",
      binaryMessenger: registrar.messenger())
    let events = FlutterEventChannel(
      name: "com.example/events",
      binaryMessenger: registrar.messenger())
    channel.setMethodCallHandler { (call, result) in
      switch call.method {
      case "getLevel":
        let args = call.arguments as? [String: Any]
        result(args?["level"] as? Int)
      default:
        result(FlutterMethodNotImplemented)
      }
    }
    events.setStreamHandler(StreamHandler())
  }
  // MethodChannel('com.example/commented') is a comment, not a declaration.
}
|};
    create "pkg/android/src/main/kotlin/ExamplePlugin.kt"
      {|package com.example

import io.flutter.plugin.common.MethodChannel
import io.flutter.plugin.common.MethodCall

class ExamplePlugin {
  fun register(messenger: BinaryMessenger) {
    val scalar = MethodChannel(messenger, "com.example/scalar")
    scalar.setMethodCallHandler { call, result ->
      when (call.method) {
        "ping" -> {
          val key = call.argument<String>("key")
          result.success(key)
        }
        else -> result.notImplemented()
      }
    }
    // MethodChannel(messenger, computed()) stays unresolved.
  }
}
|};
    create "pkg/test/channels_test.dart"
      {|import 'package:flutter_test/flutter_test.dart';

void main() {
  const channel = MethodChannel('com.example/battery');
  test('battery channel returns a level', () {
    channel.invokeMethod('getLevel');
  });
}
|};
    (* Native declarations outside the package's ios/android roots are not
       verified even when names match. *)
    create "neighbour/ios/Classes/Other.swift"
      {|import Flutter

let inline = FlutterMethodChannel(
  name: "com.example/inline",
  binaryMessenger: messenger)
|};

    let lines = Channels.report_lines ~root ~subroot:"pkg" in

    (* Positive pair: Dart and native literal names agree on both hosts. *)
    expect_contains lines
      "Paired method channel 'com.example/battery': Dart declaration pkg/lib/channels.dart:3; native pkg/ios/Classes/BatteryPlugin.swift:";
    expect_contains lines "handler registered";
    expect_contains lines "Paired event channel 'com.example/events'";

    (* Supported literal shapes: consistent call and focused test evidence. *)
    expect_contains lines
      "call 'getLevel' (no arguments) at pkg/lib/channels.dart:9: consistent with native handler (expects map arguments).";
    expect_contains lines
      "Existing focused test: pkg/test/channels_test.dart";

    (* Literal method not handled natively: MISMATCH cites the Dart call site. *)
    expect_contains lines
      "MISMATCH at pkg/lib/channels.dart:10: Dart calls method 'bogus' but no native literal case handles it";

    (* Scalar-vs-map shape mismatch on the Kotlin handler. *)
    expect_contains lines
      "Paired method channel 'com.example/scalar': Dart declaration pkg/lib/channels.dart:7; native pkg/android/src/main/kotlin/ExamplePlugin.kt:";
    expect_contains lines
      "MISMATCH at pkg/lib/channels.dart:14: Dart sends a scalar but the native handler expects a collection";
    expect_contains lines
      "Dart scalar arguments; native expects map arguments";

    (* Unsupported Dart argument expression stays unresolved. *)
    expect_contains lines
      "call at pkg/lib/channels.dart:13 unresolved: Dart argument shape is not a literal.";

    (* Computed channel name never becomes a declaration pair. *)
    expect_contains lines
      "unresolved: computed or unsupported MethodChannel name at pkg/lib/channels.dart:5";
    expect_absent lines "'com.example/' ";

    (* Unknown receivers stay unresolved. *)
    expect_contains lines
      "unresolved: invokeMethod receiver at pkg/lib/channels.dart:15 is not a literal channel";

    (* In-root handlers missing: no verified native handler. *)
    expect_contains lines
      "Unresolved: Dart method channel 'com.example/lonely' at pkg/lib/channels.dart:6";

    (* Out-of-root matches are reported but stay unresolved, not paired. *)
    expect_contains lines
      "outside this package's ios/android roots and is not verified";
    expect_absent lines "Paired method channel 'com.example/lonely'";
    expect_absent lines "Paired method channel 'com.example/inline'";

    (* Commented-out Dart code declares nothing. *)
    expect_absent lines "com.example/commented";

    (* Channel pairing is read-only. *)
    expect_contains lines "Channel pairing is read-only: no command was run";

    (* Name mismatch: nearest native name differs; both files cited. *)
    create "typo/lib/main.dart"
      {|import 'package:flutter/services.dart';
final ch = MethodChannel('com.example/battery');
|};
    create "typo/ios/Classes/Typo.swift"
      {|import Flutter
let ch = FlutterMethodChannel(name: "com.example/batery", binaryMessenger: m)
|};
    let typo = Channels.report_lines ~root ~subroot:"typo" in
    expect_contains typo
      "MISMATCH: Dart method channel 'com.example/battery' at typo/lib/main.dart:2 has no native handler; the nearest native method channel name 'com.example/batery' at typo/ios/Classes/Typo.swift:2 differs.";

    (* Kind mismatch: same name, event vs method. *)
    create "kinds/lib/main.dart"
      {|import 'package:flutter/services.dart';
final ch = EventChannel('com.example/stream');
|};
    create "kinds/android/src/main/kotlin/K.kt"
      {|import io.flutter.plugin.common.MethodChannel
val ch = MethodChannel(messenger, "com.example/stream")
|};
    let kinds = Channels.report_lines ~root ~subroot:"kinds" in
    expect_contains kinds
      "MISMATCH: Dart event channel 'com.example/stream' at kinds/lib/main.dart:2 is declared as a native method channel at kinds/android/src/main/kotlin/K.kt:2.";

    (* Dart-only package: channels never gain an imaginary handler. *)
    create "pure/lib/main.dart"
      {|import 'package:flutter/services.dart';
final ch = MethodChannel('com.example/pure');
|};
    let pure = Channels.report_lines ~root ~subroot:"pure" in
    expect_contains pure
      "No ios/ or android/ host root in this package; native handlers stay unresolved.";
    expect_contains pure
      "Unresolved: Dart method channel 'com.example/pure' at pure/lib/main.dart:2 has no verified native handler in this package.";
    expect_absent pure "Paired method channel";

    (* Absent tests stay unknown for a paired channel. *)
    create "quiet/lib/main.dart"
      {|import 'package:flutter/services.dart';
final ch = MethodChannel('com.example/quiet');
void f() { ch.invokeMethod('go'); }
|};
    create "quiet/ios/Classes/Q.swift"
      {|import Flutter
let ch = FlutterMethodChannel(name: "com.example/quiet", binaryMessenger: m)
ch.setMethodCallHandler { (call, result) in
  if call.method == "go" { result(nil) }
}
|};
    let quiet = Channels.report_lines ~root ~subroot:"quiet" in
    expect_contains quiet
      "Paired method channel 'com.example/quiet'";
    expect_contains quiet
      "Focused test: none observed under the package test/ directory referencing 'com.example/quiet'; unknown.";
    expect_absent quiet "Existing focused test";

    (* A native channel no Dart code references stays unresolved. *)
    create "extra/lib/main.dart"
      {|import 'package:flutter/services.dart';
final ch = MethodChannel('com.example/used');
|};
    create "extra/android/src/main/java/E.java"
      {|import io.flutter.plugin.common.MethodChannel;
class E {
  void register(Object messenger) {
    MethodChannel used = new MethodChannel(messenger, "com.example/used");
    MethodChannel stray = new MethodChannel(messenger, "com.example/stray");
    used.setMethodCallHandler((call, result) -> {
      if (call.method.equals("go")) { result.success(null); }
    });
  }
}
|};
    let extra = Channels.report_lines ~root ~subroot:"extra" in
    expect_contains extra "Paired method channel 'com.example/used'";
    expect_contains extra
      "Unresolved: native method channel 'com.example/stray' at extra/android/src/main/java/E.java:5 is not referenced by any literal Dart channel.";

    (* Invalid subroots fail cleanly. *)
    (try
       ignore (Channels.report_lines ~root ~subroot:"../escape");
       failwith "Flutter channel pairing: accepted traversal"
     with Channels.Error _ -> ());
    (try
       ignore (Channels.report_lines ~root ~subroot:"missing");
       failwith "Flutter channel pairing: accepted missing subroot"
     with Channels.Error _ -> ());
    print_endline "Flutter channel pairing: ok")
