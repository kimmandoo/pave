#!/bin/sh
set -eu

if [ "$#" -lt 2 ]; then
    echo "usage: check_release_dependencies.sh darwin|linux ARTIFACT..." >&2
    exit 2
fi
os=$1
shift
case "$os" in
    darwin|linux) ;;
    *) echo "unsupported release dependency platform: $os" >&2; exit 2 ;;
esac

tmp_base=${TMPDIR:-/tmp}
tmp=$(mktemp -d "${tmp_base%/}/pave-deps.XXXXXX")
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

if [ "$os" = darwin ]; then
    cat > "$tmp/load-system-framework.c" <<'EOF'
#include <dlfcn.h>
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: load-system-framework INSTALL_NAME\n");
        return 2;
    }
    void *handle = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (handle == NULL) {
        fprintf(stderr, "%s\n", dlerror());
        return 1;
    }
    dlclose(handle);
    return 0;
}
EOF
    cc "$tmp/load-system-framework.c" -o "$tmp/load-system-framework"
fi

is_linux_loader() {
    case "$1" in
        /lib64/ld-linux-x86-64.so.2|"/lib64/ld-linux-x86-64.so.2 "*) return 0 ;;
        /usr/lib64/ld-linux-x86-64.so.2|"/usr/lib64/ld-linux-x86-64.so.2 "*) return 0 ;;
        /lib/ld-linux-x86-64.so.2|"/lib/ld-linux-x86-64.so.2 "*) return 0 ;;
        /usr/lib/ld-linux-x86-64.so.2|"/usr/lib/ld-linux-x86-64.so.2 "*) return 0 ;;
        /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2|"/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 "*) return 0 ;;
        /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2|"/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 "*) return 0 ;;
        /lib/ld-linux-aarch64.so.1|"/lib/ld-linux-aarch64.so.1 "*) return 0 ;;
        /usr/lib/ld-linux-aarch64.so.1|"/usr/lib/ld-linux-aarch64.so.1 "*) return 0 ;;
        /lib64/ld-linux-aarch64.so.1|"/lib64/ld-linux-aarch64.so.1 "*) return 0 ;;
        /usr/lib64/ld-linux-aarch64.so.1|"/usr/lib64/ld-linux-aarch64.so.1 "*) return 0 ;;
        /lib/aarch64-linux-gnu/ld-linux-aarch64.so.1|"/lib/aarch64-linux-gnu/ld-linux-aarch64.so.1 "*) return 0 ;;
        /usr/lib/aarch64-linux-gnu/ld-linux-aarch64.so.1|"/usr/lib/aarch64-linux-gnu/ld-linux-aarch64.so.1 "*) return 0 ;;
        *) return 1 ;;
    esac
}

check_dependencies() {
    artifact=$1
    if [ "$os" = darwin ]; then
        otool -L "$artifact" > "$tmp/otool-output"
        awk 'NR > 1 { print $1 }' "$tmp/otool-output" > "$tmp/dependencies"
        while IFS= read -r dependency; do
            case "$dependency" in
                /usr/lib/libSystem.B.dylib|/usr/lib/libobjc.A.dylib|/usr/lib/libc++.1.dylib|/usr/lib/libiconv.2.dylib) ;;
                /System/Library/Frameworks/*)
                    # System frameworks can live only in dyld's cache.
                    # Probe the native loader without inherited DYLD overrides.
                    if ! /usr/bin/env -i PATH=/usr/bin:/bin:/usr/sbin:/sbin \
                        "$tmp/load-system-framework" "$dependency" \
                        > "$tmp/loader-output" 2>&1; then
                        echo "missing system dependency in $artifact: $dependency" >&2
                        cat "$tmp/loader-output" >&2
                        return 1
                    fi
                    ;;
                *)
                    echo "forbidden or non-system dependency in $artifact: $dependency" >&2
                    return 1
                    ;;
            esac
        done < "$tmp/dependencies"
    else
        ldd "$artifact" > "$tmp/ldd-output"
        while IFS= read -r line; do
            leading_whitespace=${line%%[![:space:]]*}
            line=${line#"$leading_whitespace"}
            if is_linux_loader "$line"; then
                continue
            fi
            case "$line" in
                *"not found"*)
                    echo "missing dependency in $artifact: $line" >&2
                    return 1
                    ;;
                linux-vdso.so.1|linux-vdso.so.1\ *) ;;
                *"=>"*)
                    dependency=${line%%=>*}
                    dependency=$(printf '%s' "$dependency" | tr -d '[:space:]')
                    resolved=${line#*=>}
                    resolved=$(printf '%s' "$resolved" | awk '{print $1}')
                    case "$dependency:$resolved" in
                        libc.so.6:/lib/*|libc.so.6:/usr/lib/*|libm.so.6:/lib/*|libm.so.6:/usr/lib/*|libpthread.so.0:/lib/*|libpthread.so.0:/usr/lib/*|libdl.so.2:/lib/*|libdl.so.2:/usr/lib/*|librt.so.1:/lib/*|librt.so.1:/usr/lib/*|libutil.so.1:/lib/*|libutil.so.1:/usr/lib/*|libgcc_s.so.1:/lib/*|libgcc_s.so.1:/usr/lib/*|ld-linux-*.so.*:/lib/*|ld-linux-*.so.*:/usr/lib/*|ld-linux-*.so.*:/lib64/*|ld-linux-*.so.*:/usr/lib64/*) ;;
                        *)
                            echo "forbidden or non-system dependency in $artifact: $line" >&2
                            return 1
                            ;;
                    esac
                    ;;
                '') ;;
                *)
                    echo "unrecognized dependency record in $artifact: $line" >&2
                    return 1
                    ;;
            esac
        done < "$tmp/ldd-output"
    fi
}

expect_rejected() {
    artifact=$1
    expected=$2
    label=$3
    if check_dependencies "$artifact" > "$tmp/check-output" 2>&1; then
        echo "dependency policy accepted $label fixture: $artifact" >&2
        exit 1
    fi
    output=$(cat "$tmp/check-output")
    case "$output" in
        *"$expected"*) ;;
        *)
            echo "dependency policy rejected $label fixture without $expected diagnostic: $output" >&2
            exit 1
            ;;
    esac
}

for artifact do
    test -f "$artifact" || { echo "missing release artifact: $artifact" >&2; exit 1; }
    check_dependencies "$artifact"
done

cat > "$tmp/dependency.c" <<'EOF'
int pave_fixture_value(void) { return 0; }
EOF
cat > "$tmp/main.c" <<'EOF'
int pave_fixture_value(void);
int main(void) { return pave_fixture_value(); }
EOF

if [ "$os" = darwin ]; then
    # Exercise allowed frameworks even when the release artifacts use only libSystem.
    cc "$tmp/main.c" "$tmp/dependency.c" -framework Foundation \
        -o "$tmp/system-framework"
    check_dependencies "$tmp/system-framework"

    cc -dynamiclib "$tmp/dependency.c" \
        -Wl,-install_name,"$tmp/libpavefixture.dylib" \
        -o "$tmp/libpavefixture.dylib"
    cc "$tmp/main.c" "$tmp/libpavefixture.dylib" -o "$tmp/forbidden"
    expect_rejected "$tmp/forbidden" "forbidden or non-system dependency" "non-system"

    cc -dynamiclib "$tmp/dependency.c" \
        -Wl,-install_name,/System/Library/Frameworks/PaveFixtureMissing.framework/PaveFixtureMissing \
        -o "$tmp/libpavemissing.dylib"
    cc "$tmp/main.c" "$tmp/libpavemissing.dylib" -o "$tmp/missing"
    rm "$tmp/libpavemissing.dylib"
else
    cc -fPIC -shared "$tmp/dependency.c" -Wl,-soname,libpavefixture.so \
        -o "$tmp/libpavefixture.so"
    cc "$tmp/main.c" -L"$tmp" -Wl,-rpath,"$tmp" -lpavefixture \
        -o "$tmp/forbidden"
    expect_rejected "$tmp/forbidden" "forbidden or non-system dependency" "non-system"

    cc -fPIC -shared "$tmp/dependency.c" -Wl,-soname,libpavemissing.so \
        -o "$tmp/libpavemissing.so"
    cc "$tmp/main.c" -L"$tmp" -Wl,-rpath,"$tmp" -lpavemissing \
        -o "$tmp/missing"
    rm "$tmp/libpavemissing.so"
fi
if [ "$os" = darwin ]; then
    expect_rejected "$tmp/missing" "missing system dependency" "missing"
else
    expect_rejected "$tmp/missing" "missing dependency" "missing"
fi
printf 'Release dependency policy passed for %s\n' "$os"
