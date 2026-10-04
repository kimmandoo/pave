#!/usr/bin/env python3
"""Run a native install/update transaction against a disposable local HTTPS release."""
import hashlib
import http.server
import io
import os
import pathlib
import ssl
import subprocess
import sys
import tarfile
import tempfile
import threading
import time

BASE = "https://127.0.0.1:18443"
TAG = "v0.0.2"
ASSET = "pave-" + ("darwin-arm64" if sys.platform == "darwin" and os.uname().machine == "arm64" else
                   "darwin-x86_64" if sys.platform == "darwin" else
                   "linux-aarch64" if os.uname().machine in ("aarch64", "arm64") else "linux-x86_64") + ".tar.gz"

class ReleaseHandler(http.server.BaseHTTPRequestHandler):
    mode = "good"
    archive = b""
    def log_message(self, *_args):
        pass
    def write_body(self, body):
        try:
            self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError):
            pass

    def do_GET(self):
        if self.path == "/releases/latest":
            self.send_response(302)
            self.send_header("Location", BASE + "/releases/tag/" + TAG)
            self.end_headers()
            return
        prefix = "/releases/download/" + TAG + "/"
        if not self.path.startswith(prefix):
            self.send_error(404)
            return
        name = self.path[len(prefix):]
        if name == "SHA256SUMS":
            digest = hashlib.sha256(self.archive).hexdigest()
            if self.mode == "corrupt-checksum":
                digest = "0" * 64
            names = ("pave-darwin-arm64.tar.gz", "pave-darwin-x86_64.tar.gz",
                     "pave-linux-aarch64.tar.gz", "pave-linux-x86_64.tar.gz")
            body = "".join(
                (digest if name == ASSET else "0" * 64) + "  " + name + "\n"
                for name in names).encode()
            if self.mode == "oversized-manifest":
                body += b"x" * (1_048_577 - len(body))
            self.send_response(200)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.write_body(body)
        elif name == ASSET:
            if self.mode == "oversized-archive-header":
                self.send_response(200)
                self.send_header("Content-Length", str(536_870_913))
                self.end_headers()
                return
            if self.mode == "stalled-archive":
                self.send_response(200)
                self.send_header("Content-Length", str(len(self.archive)))
                self.end_headers()
                self.write_body(b"x")
                self.wfile.flush()
                time.sleep(8)
                return
            body = self.archive
            self.send_response(200)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.write_body(body)
        else:
            self.send_error(404)


def make_archive(binary, mode, license_bytes=b"controlled license\n"):
    output = io.BytesIO()
    with tarfile.open(fileobj=output, mode="w:gz") as archive:
        def add(name, data, kind=tarfile.REGTYPE):
            item = tarfile.TarInfo(name)
            item.type = kind
            if kind == tarfile.REGTYPE:
                item.size = len(data)
                archive.addfile(item, io.BytesIO(data))
            else:
                item.linkname = "pave"
                archive.addfile(item)
        add("pave", binary, tarfile.SYMTYPE if mode == "link" else tarfile.REGTYPE)
        add("LICENSE", license_bytes)
        add("THIRD_PARTY_NOTICES", b"controlled notices\n")
        if mode == "unexpected-member":
            add("extra", b"not allowed")
    return output.getvalue()


def run(command, env, expect=0, timeout=None):
    result = subprocess.run(command, env=env, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, timeout=timeout)
    if (result.returncode == 0) != (expect == 0):
        raise RuntimeError("unexpected status for " + " ".join(map(str, command)) +
                           " (" + str(result.returncode) + "):\n" + result.stdout)
    return result.stdout


def digest(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def check_staging(prefix, license_dir, temporary, label):
    leftovers = [path for directory in (prefix, license_dir) for path in directory.iterdir()
                 if path.name.startswith(".") and path.name != ".native-install"]
    leftovers.extend(temporary.iterdir())
    if leftovers:
        raise RuntimeError(label + " left staging files: " + ", ".join(map(str, leftovers)))

def main():
    if len(sys.argv) != 3:
        raise SystemExit("usage: installed_update.py PAVE_BINARY INSTALL_SCRIPT")
    binary = pathlib.Path(sys.argv[1]).resolve().read_bytes()
    install_source = pathlib.Path(sys.argv[2]).read_text()
    with tempfile.TemporaryDirectory(prefix="pave-update-fixture-") as temporary:
        root = pathlib.Path(temporary)
        cert = root / "fixture.pem"
        key = root / "fixture.key"
        subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-keyout", str(key),
                        "-out", str(cert), "-days", "1", "-subj", "/CN=127.0.0.1",
                        "-addext", "subjectAltName=IP:127.0.0.1"], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        ReleaseHandler.mode = "good"
        ReleaseHandler.archive = make_archive(binary, "good")
        server = http.server.ThreadingHTTPServer(("127.0.0.1", 18443), ReleaseHandler)
        tls = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        tls.load_cert_chain(certfile=cert, keyfile=key)
        server.socket = tls.wrap_socket(server.socket, server_side=True)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        try:
            home = root / "home"
            prefix = home / "tools" / "bin"
            prefix.mkdir(parents=True)
            temporary_staging = root / "tmp"
            temporary_staging.mkdir()
            env = os.environ.copy()
            env.update({"HOME": str(home), "PAVE_INSTALL_DIR": str(prefix), "PAVE_VERSION": TAG,
                        "CURL_CA_BUNDLE": str(cert), "TMPDIR": str(temporary_staging),
                        "HTTPS_PROXY": "", "https_proxy": "", "ALL_PROXY": "", "all_proxy": "",
                        "NO_PROXY": "*", "no_proxy": "*", "PAVE_UPDATE_OUTPUT": ""})
            installer = root / "install.sh"
            installer.write_text(install_source.replace(
                "https://github.com/kimmandoo/pave/releases", BASE + "/releases"))
            run(["/bin/sh", str(installer)], env)
            stalled_installer = root / "stalled-install.sh"
            installer_source = installer.read_text()
            if installer_source.count("--max-time 180") != 1:
                raise RuntimeError("installer fixture has no unique transfer timeout")
            stalled_installer.write_text(installer_source.replace(
                "--max-time 180", "--max-time 1", 1))
            pave = prefix / "pave"
            license_dir = prefix.parent / "share" / "licenses" / "pave"
            unrelated = prefix / "keep-me"
            unrelated.write_text("unrelated user file\n")
            user_state = home / ".config" / "pave" / "oauth.json"
            user_state.parent.mkdir(parents=True)
            user_state.write_text("user state\n")
            prior_install = {str(path): digest(path) for path in
                             (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES",
                              license_dir / ".native-install")}
            ReleaseHandler.mode = "oversized-manifest"
            run(["/bin/sh", str(installer)], env, expect=1)
            after_rejected_install = {str(path): digest(path) for path in
                                      (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES",
                                       license_dir / ".native-install")}
            if prior_install != after_rejected_install:
                raise RuntimeError("oversized installer manifest changed the prior installation")
            check_staging(prefix, license_dir, temporary_staging, "oversized installer manifest")
            ReleaseHandler.mode = "stalled-archive"
            started = time.monotonic()
            run(["/bin/sh", str(stalled_installer)], env, expect=1, timeout=15)
            if time.monotonic() - started > 5:
                raise RuntimeError("stalled installer transfer exceeded the fixture deadline")
            after_stalled_install = {str(path): digest(path) for path in
                                     (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES",
                                      license_dir / ".native-install")}
            if prior_install != after_stalled_install:
                raise RuntimeError("stalled installer transfer changed the prior installation")
            check_staging(prefix, license_dir, temporary_staging, "stalled installer archive")
            ReleaseHandler.mode = "good"
            checked_before = {str(path): digest(path) for path in
                              (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES",
                               license_dir / ".native-install")}
            run([str(pave), "update", "--check"], env)
            checked_after = {str(path): digest(path) for path in
                             (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES",
                              license_dir / ".native-install")}
            if checked_before != checked_after:
                raise RuntimeError("update --check changed installation files")
            # An update must publish into the installer-owned destination and
            # preserve unrelated files and per-user state.
            ReleaseHandler.archive = make_archive(binary, "good", b"updated license\n")
            hostile = env | {"PAVE_INSTALL_DIR": str(root / "redirect"), "PAVE_VERSION": "v9.9.9"}
            run([str(pave), "update"], hostile)
            if not pave.exists() or (root / "redirect").exists():
                raise RuntimeError("updater honored an inherited destination override")
            updated_license = (license_dir / "LICENSE").read_bytes()
            retained_user_file = unrelated.read_text()
            retained_user_state = user_state.read_text()
            if (updated_license != b"updated license\n" or
                    retained_user_file != "unrelated user file\n" or
                    retained_user_state != "user state\n"):
                raise RuntimeError(
                    "successful update state mismatch: license=" + repr(updated_license) +
                    ", unrelated=" + repr(retained_user_file) +
                    ", user_state=" + repr(retained_user_state))
            check_staging(prefix, license_dir, temporary_staging, "successful update")
            for scenario in ("corrupt-checksum", "link", "unexpected-member",
                             "oversized-manifest", "oversized-archive-header", "stalled-archive"):
                ReleaseHandler.mode = scenario
                ReleaseHandler.archive = make_archive(binary, scenario)
                before = {str(path): digest(path) for path in
                          (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES", license_dir / ".native-install")}
                scenario_env = env
                scenario_timeout = None
                if scenario == "stalled-archive":
                    scenario_env = env | {"PAVE_TEST_RELEASE_TIMEOUT_SECONDS": "1"}
                    scenario_timeout = 15
                started = time.monotonic()
                run([str(pave), "update"], scenario_env, expect=1,
                    timeout=scenario_timeout)
                if scenario == "stalled-archive" and time.monotonic() - started > 5:
                    raise RuntimeError("stalled updater transfer exceeded the fixture deadline")
                after = {str(path): digest(path) for path in
                         (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES", license_dir / ".native-install")}
                if before != after:
                    raise RuntimeError(scenario + " modified installed binary or metadata")
                check_staging(prefix, license_dir, temporary_staging, scenario)
            ReleaseHandler.mode = "good"
            ReleaseHandler.archive = make_archive(binary, "good", b"new license\n")
            wrapper_dir = root / "wrapper"
            wrapper_dir.mkdir()
            wrapper = wrapper_dir / "mv"
            fail_once = wrapper_dir / "failed-once"
            wrapper.write_text("#!/bin/sh\nfor last do :; done\nif [ \"$last\" = \"$PAVE_TEST_FAIL_PUBLISH\" ] && [ ! -e \"$PAVE_TEST_FAIL_ONCE\" ]; then : > \"$PAVE_TEST_FAIL_ONCE\"; exit 76; fi\nexec /bin/mv \"$@\"\n")
            wrapper.chmod(0o755)
            before = {str(path): digest(path) for path in
                      (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES", license_dir / ".native-install")}
            failed = env | {"PATH": str(wrapper_dir) + os.pathsep + env.get("PATH", ""),
                            "PAVE_TEST_FAIL_PUBLISH": str(pave), "PAVE_TEST_FAIL_ONCE": str(fail_once)}
            run([str(pave), "update"], failed, expect=1)
            after = {str(path): digest(path) for path in
                     (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES", license_dir / ".native-install")}
            if before != after:
                raise RuntimeError("failed executable publication changed prior executable or metadata")
            check_staging(prefix, license_dir, temporary_staging, "failed publication")
            marker = license_dir / ".native-install"
            marker.write_text("invalid marker\n")
            before_invalid_marker = {str(path): digest(path) for path in
                                     (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES", marker)}
            run([str(pave), "update"], env, expect=1)
            after_invalid_marker = {str(path): digest(path) for path in
                                    (pave, license_dir / "LICENSE", license_dir / "THIRD_PARTY_NOTICES", marker)}
            if before_invalid_marker != after_invalid_marker:
                raise RuntimeError("invalid marker changed installed executable or metadata")
            marker.write_text("pave-native-v1\n")
            run([str(pave), "uninstall"], env)
            if (pave.exists() or (license_dir / "LICENSE").exists() or
                    (license_dir / "THIRD_PARTY_NOTICES").exists() or marker.exists()):
                raise RuntimeError("uninstall left native installation files")
            if unrelated.read_text() != "unrelated user file\n" or user_state.read_text() != "user state\n":
                raise RuntimeError("uninstall removed unrelated files or user state")
        finally:
            server.shutdown()
            server.server_close()
            thread.join(timeout=5)
    print("Installed native updater transaction fixtures passed")

if __name__ == "__main__":
    main()
