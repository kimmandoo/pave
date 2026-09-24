#!/bin/sh
# Install a verified, prebuilt Pave release without opam or elevated privileges.
set -eu

fail() {
    printf 'pave installer: %s\n' "$*" >&2
    exit 1
}

for prerequisite in uname curl tar mktemp awk mkdir cp chmod mv rm; do
    command -v "$prerequisite" >/dev/null 2>&1 || fail "missing prerequisite: $prerequisite"
done

case "$(uname -s)" in
    Darwin) os=darwin ;;
    Linux) os=linux ;;
    *) fail "unsupported operating system; releases are available for macOS and Linux" ;;
esac

machine=$(uname -m)
case "$os/$machine" in
    darwin/arm64 | darwin/aarch64) arch=arm64 ;;
    darwin/x86_64) arch=x86_64 ;;
    linux/aarch64 | linux/arm64) arch=aarch64 ;;
    linux/x86_64) arch=x86_64 ;;
    *) fail "unsupported architecture $machine on $os; supported release targets are darwin-arm64, darwin-x86_64, linux-aarch64, and linux-x86_64" ;;
esac

if command -v sha256sum >/dev/null 2>&1; then
    hash_tool=sha256sum
elif command -v shasum >/dev/null 2>&1; then
    hash_tool=shasum
else
    fail 'missing SHA-256 prerequisite: install sha256sum (Linux) or shasum (macOS)'
fi

if [ "${PAVE_VERSION+x}" = x ]; then
    case "$PAVE_VERSION" in
        '' | [!a-zA-Z0-9]* | *[!a-zA-Z0-9._-]*)
            fail 'PAVE_VERSION must be a nonempty release tag starting with a letter or digit and containing only letters, digits, dots, underscores, or hyphens' ;;
    esac
    release_url="https://github.com/kimmandoo/pave/releases/download/$PAVE_VERSION"
else
    release_url='https://github.com/kimmandoo/pave/releases/latest/download'
fi

if [ -n "${PAVE_INSTALL_DIR:-}" ]; then
    install_dir=$PAVE_INSTALL_DIR
else
    [ -n "${HOME:-}" ] || fail 'HOME is unset; set PAVE_INSTALL_DIR to an installation directory'
    install_dir=$HOME/.local/bin
fi
case "$install_dir" in
    /*) ;;
    *) fail 'PAVE_INSTALL_DIR must be an absolute directory path' ;;
esac
while [ "$install_dir" != / ] && [ "${install_dir%/}" != "$install_dir" ]; do
    install_dir=${install_dir%/}
done

case "${TMPDIR:-/tmp}" in
    /*) ;;
    *) fail 'TMPDIR must be an absolute directory path' ;;
esac

umask 077
temp_dir=$(mktemp -d "${TMPDIR:-/tmp}/pave-install.XXXXXXXX") || fail 'cannot create a private temporary directory'
staged_binary=
staged_license=
staged_notices=
cleanup() {
    [ -z "$staged_binary" ] || rm -f "$staged_binary"
    [ -z "$staged_license" ] || rm -f "$staged_license"
    [ -z "$staged_notices" ] || rm -f "$staged_notices"
    rm -rf "$temp_dir"
}
trap cleanup 0
trap 'exit 1' 1 2 3 15

asset="pave-$os-$arch.tar.gz"
fetch() {
    curl --fail --location --silent --show-error --proto '=https' --proto-redir '=https' \
        --output "$2" "$release_url/$1" ||
        fail "cannot download $1; check that the release exists and is public at $release_url"
}
fetch SHA256SUMS "$temp_dir/SHA256SUMS"
fetch "$asset" "$temp_dir/$asset"

# Require one well-formed checksum for each of the four published archives.
expected=$(awk -v wanted="$asset" '
    {
        hash = substr($0, 1, 64)
        separator = substr($0, 65, 2)
        filename = substr($0, 67)
        if (length(hash) != 64 || hash !~ /^[0-9a-fA-F]+$/ || separator != "  " ||
            filename !~ /^pave-(darwin-(arm64|x86_64)|linux-(aarch64|x86_64))\.tar\.gz$/ ||
            seen[filename]++) {
            invalid = 1
            exit
        }
        count++
        if (filename == wanted) expected_hash = hash
    }
    END {
        if (invalid || count != 4 || expected_hash == "") exit 1
        print expected_hash
    }
' "$temp_dir/SHA256SUMS") || fail "malformed SHA256SUMS or missing checksum for $asset"

case "$hash_tool" in
    sha256sum) sha256sum "$temp_dir/$asset" > "$temp_dir/computed" || fail 'SHA-256 computation failed' ;;
    shasum) shasum -a 256 "$temp_dir/$asset" > "$temp_dir/computed" || fail 'SHA-256 computation failed' ;;
esac
awk -v expected="$expected" 'NR == 1 && tolower($1) == tolower(expected) { valid = 1 } END { exit !valid }' \
    "$temp_dir/computed" || fail "SHA-256 mismatch for $asset; refusing to install"

# Refuse extra paths, duplicates, links, or directories before extracting any entries.
tar -tzf "$temp_dir/$asset" > "$temp_dir/entries" || fail "cannot read archive $asset"
awk '
    ($0 == "pave" || $0 == "LICENSE" || $0 == "THIRD_PARTY_NOTICES") && !seen[$0]++ { count++; next }
    { invalid = 1 }
    END { if (invalid || count != 3) exit 1 }
' "$temp_dir/entries" || fail "archive $asset does not contain exactly pave, LICENSE, and THIRD_PARTY_NOTICES"
tar -tvzf "$temp_dir/$asset" > "$temp_dir/details" || fail "cannot inspect archive $asset"
awk 'substr($0, 1, 1) != "-" { invalid = 1 } END { if (invalid || NR != 3) exit 1 }' \
    "$temp_dir/details" || fail "archive $asset contains a link or nonregular file"
mkdir "$temp_dir/extracted" || fail 'cannot prepare extraction directory'
tar -xzf "$temp_dir/$asset" -C "$temp_dir/extracted" pave LICENSE THIRD_PARTY_NOTICES ||
    fail "cannot extract expected files from $asset"
for file in pave LICENSE THIRD_PARTY_NOTICES; do
    [ -f "$temp_dir/extracted/$file" ] && [ ! -L "$temp_dir/extracted/$file" ] ||
        fail "archive $asset has an invalid $file entry"
done

# Stage all files first; publish the executable last, by a rename in its own directory.
license_dir=${install_dir%/*}/share/licenses/pave
mkdir -p "$install_dir" "$license_dir" || fail 'cannot create install or license directories; set PAVE_INSTALL_DIR to a writable location'
staged_binary=$(mktemp "$install_dir/.pave.XXXXXXXX") || fail "cannot stage executable in $install_dir"
staged_license=$(mktemp "$license_dir/.LICENSE.XXXXXXXX") || fail "cannot stage LICENSE in $license_dir"
staged_notices=$(mktemp "$license_dir/.THIRD_PARTY_NOTICES.XXXXXXXX") || fail "cannot stage THIRD_PARTY_NOTICES in $license_dir"
cp "$temp_dir/extracted/pave" "$staged_binary" || fail 'cannot copy executable'
cp "$temp_dir/extracted/LICENSE" "$staged_license" || fail 'cannot copy LICENSE'
cp "$temp_dir/extracted/THIRD_PARTY_NOTICES" "$staged_notices" || fail 'cannot copy THIRD_PARTY_NOTICES'
chmod 755 "$staged_binary" || fail 'cannot make executable runnable'
chmod 644 "$staged_license" "$staged_notices" || fail 'cannot set license file permissions'
for target in "$install_dir/pave" "$license_dir/LICENSE" "$license_dir/THIRD_PARTY_NOTICES"; do
    [ ! -d "$target" ] || fail "cannot replace directory $target"
done
mv -f "$staged_license" "$license_dir/LICENSE" || fail 'cannot install LICENSE'
staged_license=
mv -f "$staged_notices" "$license_dir/THIRD_PARTY_NOTICES" || fail 'cannot install THIRD_PARTY_NOTICES'
staged_notices=
mv -f "$staged_binary" "$install_dir/pave" || fail 'cannot install pave executable'
staged_binary=
printf 'Installed pave to %s/pave\n' "$install_dir"
case ":${PATH:-}:" in
    *":$install_dir:"*) ;;
    *) printf 'Add %s to your PATH to run pave.\n' "$install_dir" ;;
esac
