#!/bin/sh
# Install a verified, prebuilt Pave release without opam or elevated privileges.
set -eu

animator=
temp_dir=
fetch_pid=

# Direct terminal installs animate a cat paving the road; `pave update` draws
# its own animation from the phase lines, and pipes and dumb terminals get none.
fancy=0
if [ "${PAVE_UPDATE_OUTPUT:-}" != 1 ] && [ -t 1 ]; then
    case "${TERM:-dumb}" in
        dumb | '') ;;
        *) fancy=1 ;;
    esac
fi
case "${LC_ALL:-${LC_CTYPE:-${LANG:-}}}" in
    *[Uu][Tt][Ff]-8* | *[Uu][Tt][Ff]8*) paved='▰' unpaved='▱' ;;
    *) paved='=' unpaved='-' ;;
esac
if [ -n "${NO_COLOR:-}" ]; then
    road_color= face_color= dim_color= reset_color=
else
    road_color=$(printf '\033[36m') face_color=$(printf '\033[1;33m')
    dim_color=$(printf '\033[2m') reset_color=$(printf '\033[0m')
fi

animate() {
    parent=$1 tick=0 width=16
    while kill -0 "$parent" 2>/dev/null; do
        step=$((tick % (width + 4)))
        position=$step
        [ "$position" -le "$width" ] || position=$width
        if [ "$step" -gt "$width" ]; then face='(=^w^=)'
        elif [ $((tick % 16)) -eq 15 ]; then face='(=-.-=)'
        elif [ $((tick % 2)) -eq 0 ]; then face='(=^.^=)'
        else face='(=^o^=)'
        fi
        road= rest= index=0
        while [ "$index" -lt "$width" ]; do
            if [ "$index" -lt "$position" ]; then road="$road$paved"; else rest="$rest$unpaved"; fi
            index=$((index + 1))
        done
        case $((tick / 3 % 3)) in 0) dots=. ;; 1) dots=.. ;; *) dots=... ;; esac
        label=$(cat "$temp_dir/phase" 2>/dev/null) || label='Paving'
        printf '\r\033[K  %s%s%s%s%s%s%s%s  %s%s' "$road_color" "$road" "$reset_color" \
            "$face_color" "$face" "$reset_color" "$dim_color" "$rest$reset_color" "$label" "$dots"
        tick=$((tick + 1))
        sleep 0.12 2>/dev/null || sleep 1
    done
}

stop_animation() {
    if [ -n "$animator" ]; then
        kill "$animator" 2>/dev/null || :
        wait "$animator" 2>/dev/null || :
        animator=
        printf '\r\033[K\033[?25h'
    fi
}

fail() {
    stop_animation
    printf 'pave installer: %s\n' "$*" >&2
    exit 1
}

# `pave update` shows these phase names beside its progress animation.
phase() {
    if [ "${PAVE_UPDATE_OUTPUT:-}" = 1 ]; then
        printf 'pave-phase: %s\n' "$*"
    elif [ "$fancy" = 1 ]; then
        printf '%s' "$*" > "$temp_dir/phase.next" && mv -f "$temp_dir/phase.next" "$temp_dir/phase"
        if [ -z "$animator" ]; then
            printf '\033[?25l'
            animate $$ &
            animator=$!
        fi
    fi
}

for prerequisite in uname curl tar mktemp awk mkdir cp chmod mv rm mkfifo head wc; do
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
staged_marker=
backup_license=
backup_notices=
backup_marker=
backup_binary=
had_binary=0
had_license=0
had_notices=0
had_marker=0
installed_license=0
installed_notices=0
installed_marker=0
installed_binary=0
publish_started=0
published=0
cleanup() {
    if [ -n "$fetch_pid" ]; then
        kill "$fetch_pid" 2>/dev/null || :
        wait "$fetch_pid" 2>/dev/null || :
        fetch_pid=
    fi
    stop_animation
    if [ "$publish_started" -eq 1 ] && [ "$published" -eq 0 ]; then
        if [ "$had_license" -eq 1 ]; then
            if [ -e "$backup_license" ] || [ -L "$backup_license" ]; then mv -f "$backup_license" "$license_dir/LICENSE"; fi
        elif [ "$installed_license" -eq 1 ]; then
            rm -f "$license_dir/LICENSE"
        fi
        if [ "$had_notices" -eq 1 ]; then
            if [ -e "$backup_notices" ] || [ -L "$backup_notices" ]; then mv -f "$backup_notices" "$license_dir/THIRD_PARTY_NOTICES"; fi
        elif [ "$installed_notices" -eq 1 ]; then
            rm -f "$license_dir/THIRD_PARTY_NOTICES"
        fi
        if [ "$had_marker" -eq 1 ]; then
            if [ -e "$backup_marker" ] || [ -L "$backup_marker" ]; then mv -f "$backup_marker" "$license_dir/.native-install"; fi
        elif [ "$installed_marker" -eq 1 ]; then
            rm -f "$license_dir/.native-install"
        fi
        if [ "$installed_binary" -eq 1 ] && [ "$had_binary" -eq 1 ]; then
            [ ! -e "$backup_binary" ] || mv -f "$backup_binary" "$install_dir/pave"
        elif [ "$installed_binary" -eq 1 ]; then
            rm -f "$install_dir/pave"
        fi
    fi
    [ -z "$staged_binary" ] || rm -f "$staged_binary"
    [ -z "$staged_license" ] || rm -f "$staged_license"
    [ -z "$staged_notices" ] || rm -f "$staged_notices"
    [ -z "$staged_marker" ] || rm -f "$staged_marker"
    [ -z "$backup_license" ] || rm -f "$backup_license"
    [ -z "$backup_notices" ] || rm -f "$backup_notices"
    [ -z "$backup_marker" ] || rm -f "$backup_marker"
    [ -z "$backup_binary" ] || rm -f "$backup_binary"
    rm -rf "$temp_dir"
}
trap cleanup 0
trap 'exit 1' 1 2 3 15

asset="pave-$os-$arch.tar.gz"
# Each transfer uses a 10-second connect timeout, 180-second total timeout,
# and a per-response byte cap. SHA256SUMS is at most 1 MiB; archives at most
# 512 MiB. Download caps do not constrain expanded archive size.
fetch() {
    case "$1" in
        SHA256SUMS) max_bytes=1048576 ;;
        "$asset") max_bytes=536870912 ;;
        *) fail "refusing unexpected release asset $1" ;;
    esac
    fifo="$temp_dir/download.fifo"
    rm -f "$fifo"
    mkfifo "$fifo" || fail 'cannot create a private release-transfer pipe'
    curl --fail --location --silent --show-error --proto '=https' --proto-redir '=https' \
        --connect-timeout 10 --max-time 180 --max-filesize "$max_bytes" \
        --output - "$release_url/$1" > "$fifo" 2> "$temp_dir/curl-error" &
    fetch_pid=$!
    if head -c "$((max_bytes + 1))" < "$fifo" > "$2"; then
        reader_status=0
    else
        reader_status=$?
    fi
    if wait "$fetch_pid"; then curl_status=0; else curl_status=$?; fi
    fetch_pid=
    rm -f "$fifo"
    size=$(wc -c < "$2")
    if [ "$size" -gt "$max_bytes" ]; then
        fail "$1 exceeds its $max_bytes-byte transfer limit"
    fi
    if [ "$reader_status" -ne 0 ] || [ "$curl_status" -ne 0 ]; then
        stop_animation
        cat "$temp_dir/curl-error" >&2
        fail "cannot download $1 within its transfer budget; check that the release exists and is public at $release_url"
    fi
}
phase 'Fetching checksums'
fetch SHA256SUMS "$temp_dir/SHA256SUMS"
phase "Downloading $asset"
fetch "$asset" "$temp_dir/$asset"
phase 'Verifying checksum'

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

phase 'Inspecting archive'
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

phase 'Installing'
# Stage all files first; publish the executable last, by a rename in its own directory.
# A marker distinguishes native installs from opam-managed binaries for `pave update`.
license_dir=${install_dir%/*}/share/licenses/pave
mkdir -p "$install_dir" "$license_dir" || fail 'cannot create install or license directories; set PAVE_INSTALL_DIR to a writable location'
staged_binary=$(mktemp "$install_dir/.pave.XXXXXXXX") || fail "cannot stage executable in $install_dir"
staged_license=$(mktemp "$license_dir/.LICENSE.XXXXXXXX") || fail "cannot stage LICENSE in $license_dir"
staged_notices=$(mktemp "$license_dir/.THIRD_PARTY_NOTICES.XXXXXXXX") || fail "cannot stage THIRD_PARTY_NOTICES in $license_dir"
staged_marker=$(mktemp "$license_dir/.native-install.XXXXXXXX") || fail 'cannot stage native-install marker'
printf 'pave-native-v1\n' > "$staged_marker" || fail 'cannot write native-install marker'
cp "$temp_dir/extracted/pave" "$staged_binary" || fail 'cannot copy executable'
cp "$temp_dir/extracted/LICENSE" "$staged_license" || fail 'cannot copy LICENSE'
cp "$temp_dir/extracted/THIRD_PARTY_NOTICES" "$staged_notices" || fail 'cannot copy THIRD_PARTY_NOTICES'
chmod 755 "$staged_binary" || fail 'cannot make executable runnable'
chmod 644 "$staged_license" "$staged_notices" || fail 'cannot set license file permissions'
for target in "$install_dir/pave" "$license_dir/LICENSE" "$license_dir/THIRD_PARTY_NOTICES" "$license_dir/.native-install"; do
    [ ! -d "$target" ] || fail "cannot replace directory $target"
done
# Preserve old metadata and binary until the executable has been renamed into place.
backup_license=$(mktemp "$license_dir/.backup.XXXXXXXX") || fail 'cannot stage rollback for LICENSE'
backup_notices=$(mktemp "$license_dir/.backup.XXXXXXXX") || fail 'cannot stage rollback for THIRD_PARTY_NOTICES'
backup_marker=$(mktemp "$license_dir/.backup.XXXXXXXX") || fail 'cannot stage rollback for native-install marker'
backup_binary=$(mktemp "$install_dir/.backup.XXXXXXXX") || fail 'cannot stage rollback for executable'
rm -f "$backup_license" "$backup_notices" "$backup_marker" "$backup_binary" ||
    fail 'cannot prepare rollback files'
if [ -e "$install_dir/pave" ] || [ -L "$install_dir/pave" ]; then
    cp -p "$install_dir/pave" "$backup_binary" || fail 'cannot preserve existing executable'
    had_binary=1
else
    backup_binary=
fi
publish_started=1
if [ -e "$license_dir/LICENSE" ] || [ -L "$license_dir/LICENSE" ]; then
    had_license=1
    mv "$license_dir/LICENSE" "$backup_license" || fail 'cannot preserve existing LICENSE'
else
    backup_license=
fi
if [ -e "$license_dir/THIRD_PARTY_NOTICES" ] || [ -L "$license_dir/THIRD_PARTY_NOTICES" ]; then
    had_notices=1
    mv "$license_dir/THIRD_PARTY_NOTICES" "$backup_notices" || fail 'cannot preserve existing THIRD_PARTY_NOTICES'
else
    backup_notices=
fi
if [ -e "$license_dir/.native-install" ] || [ -L "$license_dir/.native-install" ]; then
    had_marker=1
    mv "$license_dir/.native-install" "$backup_marker" || fail 'cannot preserve existing native-install marker'
else
    backup_marker=
fi
installed_license=1
mv -f "$staged_license" "$license_dir/LICENSE" || fail 'cannot install LICENSE'
staged_license=
installed_notices=1
mv -f "$staged_notices" "$license_dir/THIRD_PARTY_NOTICES" || fail 'cannot install THIRD_PARTY_NOTICES'
staged_notices=
installed_marker=1
mv -f "$staged_marker" "$license_dir/.native-install" || fail 'cannot install native-install marker'
staged_marker=
installed_binary=1
mv -f "$staged_binary" "$install_dir/pave" || fail 'cannot install pave executable'
staged_binary=
published=1
stop_animation
if [ "${PAVE_UPDATE_OUTPUT:-}" != 1 ]; then
    [ "$fancy" = 0 ] || printf '%s(=^w^=)%s  ' "$face_color" "$reset_color"
    printf 'Installed pave to %s/pave\n' "$install_dir"
    case ":${PATH:-}:" in
        *":$install_dir:"*) ;;
        *) printf 'Add %s to your PATH to run pave.\n' "$install_dir" ;;
    esac
fi
