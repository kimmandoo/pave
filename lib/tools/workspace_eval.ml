module Internal : sig
  exception Error of string
  type language = Python | JavaScript
  type result = { output : string; error : string option; truncated : bool }
  type tool_bridge = name:string -> arguments:Yojson.Basic.t -> string
  type transport = {
    start : unit -> unit;
    exchange :
      request:string -> timeout_seconds:int -> cancel:(unit -> bool) ->
      tool_bridge:tool_bridge option -> string;
    close : unit -> unit;
  }
  type launcher = language -> transport
  type t
  val max_source_bytes : int
  val max_output_bytes : int
  val max_timeout_seconds : int
  val create : ?launcher:launcher -> owner:string -> language -> t
  val evaluate :
    ?timeout_seconds:int -> ?cancel:(unit -> bool) -> ?tool_bridge:tool_bridge ->
    t -> string -> result
  val reset : t -> unit
  val close : t -> unit
end = struct
exception Error of string

type language = Python | JavaScript

type result = {
  output : string;
  error : string option;
  truncated : bool;
}

(* This process-local evaluator is unsandboxed: code may access host resources,
   including files or network APIs. Callers must gate kernel start/reset as
   process effects and obtain explicit approval before every [evaluate]. *)
(* Trusted, bounded synchronous parent callback. The parent must independently
   allowlist read-only local tools; this bridge only transfers bounded JSON/text. *)
type tool_bridge = name:string -> arguments:Yojson.Basic.t -> string

type transport = {
  start : unit -> unit;
  exchange :
    request:string -> timeout_seconds:int -> cancel:(unit -> bool) ->
    tool_bridge:tool_bridge option -> string;
  close : unit -> unit;
}

type launcher = language -> transport

type t = {
  owner : string;
  language : language;
  launcher : launcher;
  lock : Mutex.t;
  mutable transport : transport option;
  mutable closed : bool;
}

let max_source_bytes = 65_536
let max_output_bytes = 65_536
let default_timeout_seconds = 10
let max_timeout_seconds = 120
let max_kernels = 16
let max_protocol_frame_bytes = (8 * max_output_bytes) + 16_384
let max_tool_argument_bytes = 16_384
let max_tool_result_bytes = 32_768
let max_tool_calls_per_evaluation = 16
let registry_lock = Mutex.create ()
let registry : ((string * language), t) Hashtbl.t = Hashtbl.create 16

let fail message = raise (Error message)

let with_lock lock fn =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) fn

let language_name = function Python -> "python3" | JavaScript -> "node"

let check_owner owner =
  if owner = "" || String.length owner > 512 || String.contains owner '\000' then
    fail "workspace evaluation requires a valid private session owner"

let check_timeout = function
  | None -> default_timeout_seconds
  | Some seconds when seconds >= 1 && seconds <= max_timeout_seconds -> seconds
  | Some _ -> fail (Printf.sprintf "evaluation timeout must be between 1 and %d seconds" max_timeout_seconds)
let check_evaluation_live ~deadline ~cancel =
  if cancel () then fail "workspace evaluation cancelled";
  if Unix.gettimeofday () >= deadline then fail "workspace evaluation timed out"

let validate_tool_name name =
  let alpha = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false in
  let rest = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
    | _ -> false in
  if String.length name = 0 || String.length name > 64 ||
     not (alpha name.[0]) || not (String.for_all rest name) then
    fail "tool name is invalid"

let validate_tool_arguments arguments =
  (match arguments with
   | `Assoc _ -> ()
   | _ -> fail "tool arguments must be a JSON object");
  let nodes = ref 0 in
  let rec visit depth = function
    | _ when depth > 16 -> fail "tool arguments exceed the JSON nesting limit"
    | `Null | `Bool _ | `Int _ -> incr nodes
    | `Float number ->
        incr nodes;
        if classify_float number = FP_nan || classify_float number = FP_infinite then
          fail "tool arguments contain a non-finite number"
    | `String text ->
        incr nodes;
        if String.length text > 8192 then fail "tool arguments exceed the JSON size limit"
    | `List values ->
        incr nodes;
        if List.length values > 512 then fail "tool arguments contain too many values";
        List.iter (visit (depth + 1)) values
    | `Assoc fields ->
        incr nodes;
        if List.length fields > 128 then fail "tool arguments contain too many fields";
        let keys = Hashtbl.create (List.length fields) in
        List.iter (fun (key, value) ->
          if String.length key > 256 || Hashtbl.mem keys key then
            fail "tool arguments contain an invalid or duplicate field";
          Hashtbl.add keys key ();
          visit (depth + 1) value) fields
    | _ -> fail "tool arguments contain an unsupported JSON value"
  in
  visit 0 arguments;
  if !nodes > 1024 then fail "tool arguments contain too many values";
  let encoded =
    try Yojson.Basic.to_string arguments
    with _ -> fail "tool arguments are not valid JSON" in
  if String.length encoded > max_tool_argument_bytes then
    fail "tool arguments exceed the JSON size limit"

let parse_tool_call = function
  | `Assoc fields ->
      let seen = Hashtbl.create 4 in
      List.iter (fun (name, _) ->
        if Hashtbl.mem seen name then fail "workspace kernel returned a duplicate protocol field";
        Hashtbl.add seen name ()) fields;
      if List.length fields <> 4 then fail "workspace kernel returned an invalid tool-call frame";
      let field name = List.assoc_opt name fields in
      (match field "type", field "id", field "name", field "arguments" with
       | Some (`String "tool_call"), Some (`Int id), Some (`String name), Some arguments
         when id > 0 -> id, name, arguments
       | _ -> fail "workspace kernel returned an invalid tool-call frame")
  | _ -> fail "workspace kernel returned an invalid tool-call frame"

let tool_result_frame id result =
  `Assoc ["type", `String "tool_result"; "id", `Int id; "result", `String result]

let tool_error_frame id error =
  `Assoc ["type", `String "tool_result"; "id", `Int id; "error", `String error]

let call_tool_bridge ~tool_bridge ~deadline ~cancel ~id ~name ~arguments =
  check_evaluation_live ~deadline ~cancel;
  let valid_arguments =
    try validate_tool_name name; validate_tool_arguments arguments; true
    with Error _ -> false in
  if not valid_arguments then tool_error_frame id "tool name or arguments are invalid"
  else match tool_bridge with
    | None -> tool_error_frame id "tool bridge is not enabled"
    | Some bridge ->
        let result =
          try Result.Ok (bridge ~name ~arguments)
          with _ -> Result.Error "tool bridge callback failed" in
        check_evaluation_live ~deadline ~cancel;
        (match result with
         | Result.Error message -> tool_error_frame id message
         | Result.Ok output when String.length output > max_tool_result_bytes ->
             tool_error_frame id "tool bridge result exceeds its size limit"
         | Result.Ok output ->
             (try
                ignore (Yojson.Basic.to_string (`String output));
                tool_result_frame id output
              with _ -> tool_error_frame id "tool bridge result is not valid text"))

(* Package policy: Python may import only these computational standard-library
   roots: collections, datetime, decimal, fractions, functools, itertools,
   json, math, operator, re, statistics, string, and typing. JavaScript has
   no package allowlist entries. pip/npm installation is not provided. This
   import policy is not a sandbox. *)
let python_wrapper = {|import builtins as _builtins
import json as _json
import sys as _sys

_PROTOCOL_OUT = _sys.stdout
_PROTOCOL_IN = _sys.stdin
_ALLOWED = frozenset(("collections", "datetime", "decimal", "fractions", "functools", "itertools", "json", "math", "operator", "re", "statistics", "string", "typing"))
_GLOBALS = {"__name__": "__main__"}
_LIMIT = 65536

class _CappedText:
    def __init__(self, limit=_LIMIT):
        self.limit = limit
        self.parts = []
        self.size = 0
        self.truncated = False
    def write(self, text):
        if not isinstance(text, str):
            text = str(text)
        remaining = self.limit - self.size
        if remaining <= 0:
            if text:
                self.truncated = True
            return len(text)
        used = 0
        end = 0
        for char in text:
            point = ord(char)
            width = 1 if point <= 0x7f else (2 if point <= 0x7ff else (4 if point > 0xffff else 3))
            if used + width > remaining:
                self.truncated = True
                break
            used += width
            end += 1
        if end < len(text):
            self.truncated = True
        if end:
            self.parts.append(text[:end])
            self.size += used
        return len(text)
    def flush(self):
        pass
    def isatty(self):
        return False
    def getvalue(self):
        return "".join(self.parts)

def _limited_error(exc):
    text = type(exc).__name__ + ": " + str(exc)
    output = _CappedText(4096)
    output.write(text)
    return output.getvalue()

def _respond(value):
    _PROTOCOL_OUT.write(_json.dumps(value, ensure_ascii=True, separators=(",", ":"), allow_nan=False) + "\n")
    _PROTOCOL_OUT.flush()

def _check_tool_arguments(value, depth=0, budget=None):
    if budget is None:
        budget = [0]
    if depth > 16:
        raise ValueError("pave.tool argument nesting limit exceeded")
    budget[0] += 1
    if budget[0] > 1024:
        raise ValueError("pave.tool argument value limit exceeded")
    if isinstance(value, dict):
        if len(value) > 128:
            raise ValueError("pave.tool argument field limit exceeded")
        for key, item in value.items():
            if not isinstance(key, str) or len(key) > 256:
                raise ValueError("pave.tool argument field is invalid")
            _check_tool_arguments(item, depth + 1, budget)
    elif isinstance(value, list):
        if len(value) > 512:
            raise ValueError("pave.tool argument list limit exceeded")
        for item in value:
            _check_tool_arguments(item, depth + 1, budget)
    elif isinstance(value, str):
        if len(value.encode("utf-8", "surrogatepass")) > 8192:
            raise ValueError("pave.tool argument string limit exceeded")
    elif value is None or isinstance(value, (bool, int, float)):
        pass
    else:
        raise TypeError("pave.tool arguments must contain only JSON values")

class _Pave:
    def __init__(self):
        self.sequence = 0
    def tool(self, name, arguments):
        if not isinstance(name, str) or not isinstance(arguments, dict):
            raise TypeError("pave.tool requires a string name and an object of JSON arguments")
        _check_tool_arguments(arguments)
        encoder = _json.JSONEncoder(ensure_ascii=True, separators=(",", ":"), allow_nan=False)
        size = 0
        for piece in encoder.iterencode(arguments):
            size += len(piece)
            if size > 16384:
                raise ValueError("pave.tool arguments exceed the JSON size limit")
        self.sequence += 1
        call_id = self.sequence
        _respond({"type": "tool_call", "id": call_id, "name": name, "arguments": arguments})
        line = _PROTOCOL_IN.readline()
        if not line:
            raise RuntimeError("workspace tool bridge closed")
        reply = _json.loads(line)
        if not isinstance(reply, dict) or reply.get("type") != "tool_result" or reply.get("id") != call_id:
            raise RuntimeError("workspace tool bridge returned an invalid response")
        if isinstance(reply.get("error"), str):
            raise RuntimeError(reply["error"])
        result = reply.get("result")
        if not isinstance(result, str):
            raise RuntimeError("workspace tool bridge returned an invalid result")
        return result

_GLOBALS["pave"] = _Pave()
_respond({"type": "ready"})
for _line in _PROTOCOL_IN:
    _capture = _CappedText()
    _error = None
    try:
        _request = _json.loads(_line)
        if not isinstance(_request, dict) or not isinstance(_request.get("code"), str):
            raise ValueError("code must be a string")
        def _policy_import(name, globals=None, locals=None, fromlist=(), level=0):
            root = name.split(".", 1)[0]
            if level != 0 or root not in _ALLOWED:
                raise ImportError("package is not permitted by the workspace evaluation allowlist: " + root)
            return _original_import(name, globals, locals, fromlist, level)
        _safe_builtins = _builtins.__dict__.copy()
        _original_import = _builtins.__import__
        _safe_builtins["__import__"] = _policy_import
        _GLOBALS["__builtins__"] = _safe_builtins
        _old_out, _old_err = _sys.stdout, _sys.stderr
        _sys.stdout = _capture
        _sys.stderr = _capture
        try:
            exec(compile(_request["code"], "<workspace-eval>", "exec"), _GLOBALS, _GLOBALS)
        except BaseException as _exc:
            _error = _limited_error(_exc)
        finally:
            _sys.stdout, _sys.stderr = _old_out, _old_err
        _respond({"type": "result", "output": _capture.getvalue(), "error": _error, "truncated": _capture.truncated})
    except BaseException as _exc:
        _sys.stdout, _sys.stderr = _PROTOCOL_OUT, _PROTOCOL_OUT
        _respond({"type": "result", "output": _capture.getvalue(), "error": _limited_error(_exc), "truncated": _capture.truncated})
|}
let javascript_wrapper = {|const fs = require("node:fs");
const vm = require("node:vm");
let pending = Buffer.alloc(0);
const readBuffer = Buffer.allocUnsafe(65536);
const maxFrameBytes = 8 * 65536 + 16384;
const readFrame = () => {
  let newline = pending.indexOf(10);
  if (newline < 0) {
    const chunks = [pending];
    let length = pending.length;
    while (newline < 0) {
      if (length > maxFrameBytes) throw new Error("workspace protocol frame exceeded limit");
      const count = fs.readSync(0, readBuffer, 0, readBuffer.length, null);
      if (count === 0) return null;
      const chunk = Buffer.from(readBuffer.subarray(0, count));
      const found = chunk.indexOf(10);
      if (found >= 0) newline = length + found;
      chunks.push(chunk);
      length += count;
    }
    pending = Buffer.concat(chunks, length);
  }
  if (newline > maxFrameBytes) throw new Error("workspace protocol frame exceeded limit");
  const line = pending.subarray(0, newline).toString("utf8");
  pending = pending.subarray(newline + 1);
  return line;
};
const writeFrame = value => {
  const bytes = Buffer.from(JSON.stringify(value) + "\n");
  let offset = 0;
  while (offset < bytes.length)
    offset += fs.writeSync(1, bytes, offset, bytes.length - offset);
};
const normalizeToolJson = (value, depth = 0, state = { nodes: 0 }) => {
  if (depth > 16) throw new TypeError("pave.tool argument nesting limit exceeded");
  state.nodes += 1;
  if (state.nodes > 1024) throw new TypeError("pave.tool argument value limit exceeded");
  if (value === null || typeof value === "boolean") return value;
  if (typeof value === "string") {
    if (Buffer.byteLength(value, "utf8") > 8192)
      throw new TypeError("pave.tool argument string limit exceeded");
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value)) throw new TypeError("pave.tool arguments must be finite JSON numbers");
    return value;
  }
  if (Array.isArray(value)) {
    if (value.length > 512) throw new TypeError("pave.tool argument list limit exceeded");
    return value.map(item => normalizeToolJson(item, depth + 1, state));
  }
  if (typeof value === "object") {
    const keys = Object.keys(value);
    if (keys.length > 128) throw new TypeError("pave.tool argument field limit exceeded");
    const normalized = Object.create(null);
    for (const key of keys) {
      if (Buffer.byteLength(key, "utf8") > 256)
        throw new TypeError("pave.tool argument field is invalid");
      normalized[key] = normalizeToolJson(value[key], depth + 1, state);
    }
    return normalized;
  }
  throw new TypeError("pave.tool arguments must contain only JSON values");
};
let sequence = 0;
const bridgeTool = (name, argumentsValue) => {
  if (typeof name !== "string" || argumentsValue === null ||
      typeof argumentsValue !== "object" || Array.isArray(argumentsValue))
    throw new TypeError("pave.tool requires a string name and an object of JSON arguments");
  const normalized = normalizeToolJson(argumentsValue);
  const encodedArguments = JSON.stringify(normalized);
  if (Buffer.byteLength(encodedArguments, "utf8") > 16384)
    throw new TypeError("pave.tool arguments exceed the JSON limit");
  sequence += 1;
  const id = sequence;
  writeFrame({ type: "tool_call", id, name, arguments: normalized });
  const line = readFrame();
  if (line === null) throw new Error("workspace tool bridge closed");
  const reply = JSON.parse(line);
  if (!reply || reply.type !== "tool_result" || reply.id !== id)
    throw new Error("workspace tool bridge returned an invalid response");
  if (typeof reply.error === "string") throw new Error(reply.error);
  if (typeof reply.result !== "string")
    throw new Error("workspace tool bridge returned an invalid result");
  return reply.result;
};
const context = vm.createContext({
  pave: Object.freeze({ tool: bridgeTool }),
}, { microtaskMode: "afterEvaluate" });
const bootstrap = `
(() => {
  const byteWidth = code => code <= 0x7f ? 1 : code <= 0x7ff ? 2 : code <= 0xffff ? 3 : 4;
  let text = "";
  let bytes = 0;
  let wasTruncated = false;
  const format = value => {
    if (typeof value === "string") return value;
    try { return JSON.stringify(value); } catch (_) { return String(value); }
  };
  const emit = (...values) => {
    let line = values.map(format).join(" ") + "\\n";
    for (let i = 0; i < line.length;) {
      const point = line.codePointAt(i);
      const width = byteWidth(point);
      if (bytes + width > 65536) { wasTruncated = true; break; }
      const chars = point > 0xffff ? 2 : 1;
      text += line.slice(i, i + chars);
      bytes += width;
      i += chars;
    }
  };
  globalThis.console = Object.freeze({ log: emit, info: emit, warn: emit, error: emit });
  globalThis.require = name => { throw new Error("package imports are disabled by the workspace evaluation allowlist: " + String(name)); };
  globalThis.__paveResetOutput = () => { text = ""; bytes = 0; wasTruncated = false; };
  globalThis.__paveReadOutput = () => ({ output: text, truncated: wasTruncated });
})();`;
vm.runInContext(bootstrap, context);
const resetOutput = context.__paveResetOutput;
const readOutput = context.__paveReadOutput;
delete context.__paveResetOutput;
delete context.__paveReadOutput;
const limitError = error => {
  let text;
  try { text = String(error && error.name ? error.name : "Error") + ": " + String(error && error.message !== undefined ? error.message : error); }
  catch (_) { text = "JavaScript evaluation failed"; }
  let bytes = 0;
  let end = 0;
  for (const character of text) {
    const width = Buffer.byteLength(character, "utf8");
    if (bytes + width > 4096) break;
    bytes += width;
    end += character.length;
  }
  return end < text.length ? text.slice(0, end) : text;
};
writeFrame({ type: "ready" });
let requestLine;
while ((requestLine = readFrame()) !== null) {
  let response;
  try {
    const request = JSON.parse(requestLine);
    if (!request || typeof request.code !== "string") throw new Error("code must be a string");
    resetOutput();
    let error = null;
    try { vm.runInContext(request.code, context, { timeout: request.timeout_seconds * 1000 + 2000, displayErrors: false }); }
    catch (exception) { error = limitError(exception); }
    const captured = readOutput();
    response = { type: "result", output: captured.output, error, truncated: captured.truncated };
  } catch (exception) {
    response = { type: "result", output: "", error: limitError(exception), truncated: false };
  }
  writeFrame(response);
}
|}


let fixed_candidates = function
  | Python -> [
      "/usr/bin/python3"; "/usr/local/bin/python3";
      "/opt/homebrew/bin/python3"; "/opt/local/bin/python3"]
  | JavaScript ->
      ["/usr/bin/node"; "/usr/local/bin/node";
       "/opt/homebrew/bin/node"; "/opt/local/bin/node"] @
      (match Sys.getenv_opt "NVM_BIN" with
       | Some directory when not (Filename.is_relative directory) &&
                             not (String.contains directory '\000') ->
           [Filename.concat directory "node"]
       | _ -> [])

let fixed_runtime language =
  let executable path =
    Sys.file_exists path &&
    try Unix.access path [Unix.X_OK]; true with Unix.Unix_error _ -> false in
  match List.find_opt executable (fixed_candidates language) with
  | Some path -> path
  | None -> fail (Printf.sprintf "%s runtime is unavailable (fixed launcher not found)" (language_name language))

let minimal_environment = [|
  "PATH=/usr/bin:/bin:/usr/local/bin:/opt/homebrew/bin:/opt/local/bin";
  "LANG=C.UTF-8";
  "PYTHONDONTWRITEBYTECODE=1"
|]

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let rec write_all fd text offset deadline cancel =
  if offset < String.length text then (
    if cancel () then fail "workspace evaluation cancelled";
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0. then fail "workspace evaluation timed out";
    let _, writable, _ =
      try Unix.select [] [fd] [] (min 0.05 remaining)
      with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
    if writable = [] then write_all fd text offset deadline cancel
    else
      try
        let count = Unix.write_substring fd text offset (String.length text - offset) in
        if count = 0 then fail "workspace kernel closed its input";
        write_all fd text (offset + count) deadline cancel
      with
      | Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          write_all fd text offset deadline cancel
      | Unix.Unix_error (Unix.EPIPE, _, _) -> fail "workspace kernel closed its input")

let signal_group pid signal =
  try Unix.kill (-pid) signal
  with Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> ()

let reap_nonblocking pid =
  try
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ -> false
    | _, _ -> true
  with Unix.Unix_error (Unix.ECHILD, _, _) -> true

let reap_blocking pid =
  let rec loop () =
    try ignore (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
       | Unix.Unix_error (Unix.ECHILD, _, _) -> ()
  in
  loop ()

let terminate_process pid =
  signal_group pid Sys.sigterm;
  let deadline = Unix.gettimeofday () +. 0.15 in
  let reaped = ref false in
  let rec wait_briefly () =
    if not !reaped then (
      reaped := reap_nonblocking pid;
      if not !reaped then
        if Unix.gettimeofday () >= deadline then ()
        else (Thread.delay 0.01; wait_briefly ()))
  in
  wait_briefly ();
  (* Reap the leader and kill descendants too, including descendants that
     ignored TERM after the direct child exited. *)
  signal_group pid Sys.sigkill;
  if not !reaped then (
    (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
    reap_blocking pid)

type child = {
  pid : int;
  input_fd : Unix.file_descr;
  output_fd : Unix.file_descr;
  pending : Buffer.t;
  mutable scanned : int;
  mutable stopped : bool;
}

let spawn language =
  let executable = fixed_runtime language in
  let arguments = match language with
    | Python -> [|executable; "-I"; "-u"; "-c"; python_wrapper|]
    | JavaScript -> [|executable; "--no-warnings"; "-e"; javascript_wrapper|] in
  let input_read, input_write = Unix.pipe ~cloexec:true () in
  let output_read, output_write =
    try Unix.pipe ~cloexec:true ()
    with exn -> close_fd input_read; close_fd input_write; raise exn in
  let null_fd =
    try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0
    with exn ->
      close_fd input_read; close_fd input_write;
      close_fd output_read; close_fd output_write;
      raise exn in
  let child_pid = ref None in
  try
    match Unix.fork () with
    | 0 ->
        (try
           close_fd input_write;
           close_fd output_read;
           ignore (Unix.setsid ());
           Unix.dup2 input_read Unix.stdin;
           Unix.dup2 output_write Unix.stdout;
           Unix.dup2 null_fd Unix.stderr;
           close_fd input_read;
           close_fd output_write;
           close_fd null_fd;
           Unix.execve executable arguments minimal_environment
         with _ -> Unix._exit 127)
    | pid ->
        child_pid := Some pid;
        close_fd input_read;
        close_fd output_write;
        close_fd null_fd;
        Unix.set_nonblock input_write;
        Unix.set_nonblock output_read;
        { pid; input_fd = input_write; output_fd = output_read;
          pending = Buffer.create 4096; scanned = 0; stopped = false }
  with exn ->
    close_fd input_read; close_fd input_write;
    close_fd output_read; close_fd output_write; close_fd null_fd;
    (match !child_pid with
     | None -> ()
     | Some pid -> terminate_process pid);
    raise exn

let stop_child child =
  if not child.stopped then (
    child.stopped <- true;
    terminate_process child.pid;
    close_fd child.input_fd;
    close_fd child.output_fd;
    Buffer.reset child.pending;
    child.scanned <- 0)

(* Only bytes appended since the last scan are searched, so a large response
   arriving in many reads costs linear rather than quadratic time. *)
let rec find_newline buffer at =
  if at >= Buffer.length buffer then None
  else if Buffer.nth buffer at = '\n' then Some at
  else find_newline buffer (at + 1)

let read_line child deadline cancel =
  let rec extract () =
    match find_newline child.pending child.scanned with
    | Some index ->
        if index > max_protocol_frame_bytes then fail "workspace kernel response exceeded the protocol limit";
        let line = Buffer.sub child.pending 0 index in
        let rest = Buffer.sub child.pending (index + 1) (Buffer.length child.pending - index - 1) in
        Buffer.reset child.pending;
        Buffer.add_string child.pending rest;
        child.scanned <- 0;
        if String.length line > 0 && line.[String.length line - 1] = '\r' then
          String.sub line 0 (String.length line - 1)
        else line
    | None ->
        child.scanned <- Buffer.length child.pending;
        if Buffer.length child.pending > max_protocol_frame_bytes then
          fail "workspace kernel response exceeded the protocol limit";
        if cancel () then fail "workspace evaluation cancelled";
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then fail "workspace evaluation timed out";
        let readable, _, _ =
          try Unix.select [child.output_fd] [] [] (min 0.05 remaining)
          with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
        if readable = [] then extract ()
        else
          let bytes = Bytes.create 65_536 in
          (try
             let count = Unix.read child.output_fd bytes 0 (Bytes.length bytes) in
             if count = 0 then fail "workspace kernel exited before returning a response";
             Buffer.add_subbytes child.pending bytes 0 count;
             extract ()
           with
           | Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> extract ())
  in
  extract ()

let default_launcher language =
  let child = ref None in
  let ensure_child () = match !child with
    | Some current when not current.stopped -> current
    | _ -> fail "workspace kernel is not running" in
  let close () =
    Option.iter stop_child !child;
    child := None
  in
  {
    start = (fun () ->
      if !child <> None then fail "workspace kernel was already started";
      let process = spawn language in
      child := Some process;
      try
        let deadline = Unix.gettimeofday () +. 5.0 in
        let response = read_line process deadline (fun () -> false) in
        (match Yojson.Basic.from_string response with
         | `Assoc fields when List.assoc_opt "type" fields = Some (`String "ready") -> ()
         | _ -> fail "workspace kernel returned an invalid startup frame")
      with exn -> close (); raise exn);
    exchange = (fun ~request ~timeout_seconds ~cancel ~tool_bridge ->
      let process = ensure_child () in
      let deadline = Unix.gettimeofday () +. float_of_int timeout_seconds in
      let framed = request ^ "\n" in
      let calls = ref 0 in
      (try
         write_all process.input_fd framed 0 deadline cancel;
         let rec await_result () =
           let line = read_line process deadline cancel in
           let frame =
             try Yojson.Basic.from_string line
             with Yojson.Json_error _ -> fail "workspace kernel returned malformed protocol data" in
           match frame with
           | `Assoc fields when List.assoc_opt "type" fields = Some (`String "tool_call") ->
               let id, name, arguments = parse_tool_call frame in
               incr calls;
               if !calls > max_tool_calls_per_evaluation then
                 fail "workspace evaluation tool-call limit exceeded";
               let reply =
                 call_tool_bridge ~tool_bridge ~deadline ~cancel ~id ~name ~arguments in
               write_all process.input_fd (Yojson.Basic.to_string reply ^ "\n")
                 0 deadline cancel;
               await_result ()
           | `Assoc fields when List.assoc_opt "type" fields = Some (`String "result") -> line
           | _ -> fail "workspace kernel returned an invalid protocol frame"
         in
         await_result ()
       with exn -> close (); raise exn));
    close;
  }



let start_transport kernel =
  let current =
    try kernel.launcher kernel.language with
    | Error _ as error -> raise error
    | _ -> fail (Printf.sprintf "%s workspace kernel could not start" (language_name kernel.language)) in
  try
    current.start ();
    kernel.transport <- Some current
  with exn ->
    (try current.close () with _ -> ());
    match exn with
    | Error _ -> raise exn
    | _ -> fail (Printf.sprintf "%s workspace kernel could not start" (language_name kernel.language))

let create ?(launcher = default_launcher) ~owner language =
  check_owner owner;
  with_lock registry_lock (fun () ->
    match Hashtbl.find_opt registry (owner, language) with
    | Some kernel when not kernel.closed -> kernel
    | _ ->
        if Hashtbl.length registry >= max_kernels then
          fail (Printf.sprintf "workspace evaluation is limited to %d persistent kernels" max_kernels);
        let kernel = {
          owner; language; launcher; lock = Mutex.create ();
          transport = None; closed = false;
        } in
        start_transport kernel;
        Hashtbl.replace registry (owner, language) kernel;
        kernel)

let discard_transport kernel =
  Option.iter (fun current -> try current.close () with _ -> ()) kernel.transport;
  kernel.transport <- None

let ensure_transport kernel =
  if kernel.closed then fail "workspace evaluation kernel is closed";
  match kernel.transport with
  | Some _ -> ()
  | None -> start_transport kernel

let parse_response json =
  (match json with
   | `Assoc fields ->
       let seen = Hashtbl.create 4 in
       List.iter (fun (name, _) ->
         if not (List.mem name ["type"; "output"; "error"; "truncated"]) ||
            Hashtbl.mem seen name then
           fail "workspace kernel returned an invalid or duplicate result field";
         Hashtbl.add seen name ()) fields
   | _ -> fail "workspace kernel returned an invalid result frame");
  let field name = match json with
    | `Assoc fields -> List.assoc_opt name fields
    | _ -> None in
  (match field "type" with
   | Some (`String "result") -> ()
   | _ -> fail "workspace kernel returned an invalid result frame");
  let output = match field "output" with
    | Some (`String text) when String.length text <= max_output_bytes -> text
    | _ -> fail "workspace kernel returned an invalid output frame" in
  let error = match field "error" with
    | Some `Null -> None
    | Some (`String text) when String.length text <= 4096 -> Some text
    | _ -> fail "workspace kernel returned an invalid error frame" in
  let truncated = match field "truncated" with
    | Some (`Bool value) -> value
    | _ -> fail "workspace kernel returned an invalid truncation frame" in
  { output; error; truncated }
let evaluate ?timeout_seconds ?(cancel = fun () -> false) ?tool_bridge kernel source =
  let timeout_seconds = check_timeout timeout_seconds in
  if String.length source > max_source_bytes then
    fail (Printf.sprintf "evaluation source exceeds the %d-byte limit" max_source_bytes);
  let deadline = Unix.gettimeofday () +. float_of_int timeout_seconds in
  let rec acquire () =
    check_evaluation_live ~deadline ~cancel;
    if not (Mutex.try_lock kernel.lock) then (Thread.delay 0.01; acquire ()) in
  acquire ();
  Fun.protect ~finally:(fun () -> Mutex.unlock kernel.lock) (fun () ->
    check_evaluation_live ~deadline ~cancel;
    ensure_transport kernel;
    let current = Option.get kernel.transport in
    let request = Yojson.Basic.to_string (`Assoc [
      "code", `String source;
      "timeout_seconds", `Int timeout_seconds;
    ]) in
    let succeeded = ref false in
    Fun.protect ~finally:(fun () -> if not !succeeded then discard_transport kernel) (fun () ->
      check_evaluation_live ~deadline ~cancel;
      let remaining = max 1 (int_of_float (ceil (deadline -. Unix.gettimeofday ()))) in
      let cancelled () = cancel () || Unix.gettimeofday () >= deadline in
      let response =
        try current.exchange ~request ~timeout_seconds:remaining ~cancel:cancelled ~tool_bridge
        with Error _ when not (cancel ()) && Unix.gettimeofday () >= deadline ->
          fail "workspace evaluation timed out" in
      check_evaluation_live ~deadline ~cancel;
      let result =
        try parse_response (Yojson.Basic.from_string response)
        with Yojson.Json_error _ -> fail "workspace kernel returned malformed protocol data" in
      succeeded := true;
      result))

let reset kernel =
  with_lock kernel.lock (fun () ->
    if kernel.closed then fail "workspace evaluation kernel is closed";
    discard_transport kernel;
    start_transport kernel)

let close kernel =
  with_lock registry_lock (fun () ->
    with_lock kernel.lock (fun () ->
      if not kernel.closed then (
        kernel.closed <- true;
        discard_transport kernel);
      match Hashtbl.find_opt registry (kernel.owner, kernel.language) with
      | Some current when current == kernel -> Hashtbl.remove registry (kernel.owner, kernel.language)
      | _ -> ()))

let () =
  at_exit (fun () ->
    with_lock registry_lock (fun () ->
      Hashtbl.iter (fun _ kernel ->
        with_lock kernel.lock (fun () ->
          kernel.closed <- true;
          discard_transport kernel)) registry;
      Hashtbl.clear registry))
end

exception Error = Internal.Error
type language = Internal.language = Python | JavaScript
type result = Internal.result = {
  output : string;
  error : string option;
  truncated : bool;
}
type tool_bridge = Internal.tool_bridge
type transport = Internal.transport = {
  start : unit -> unit;
  exchange :
    request:string -> timeout_seconds:int -> cancel:(unit -> bool) ->
    tool_bridge:tool_bridge option -> string;
  close : unit -> unit;
}
type launcher = Internal.launcher
type t = Internal.t
let max_source_bytes = Internal.max_source_bytes
let max_output_bytes = Internal.max_output_bytes
let max_timeout_seconds = Internal.max_timeout_seconds
let create = Internal.create
let evaluate = Internal.evaluate
let reset = Internal.reset
let close = Internal.close
