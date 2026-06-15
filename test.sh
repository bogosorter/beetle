# This, of couse, was Chat-GPT generated...

set -u

TEST_DIR="./tests"

passed=0
failed=0
verbose=0

clear_line() {
    printf "\r\033[K"
}

fail() {
    local file="$1"
    local reason="$2"

    clear_line
    echo "failed: $file"

    if [[ $verbose -eq 1 ]]; then
        echo
        echo "$reason"
        echo
    fi

    ((failed++))
}

run_test() {
    local btl_file="$1"

    local base="${btl_file%.btl}"
    local exe="$base"
    local expected="${base}.out"

    printf "\rRunning: %s" "$btl_file"

    #
    # Compile
    #

    local compile_output
    if ! compile_output="$(beetle "$btl_file" 2>&1)"; then
        fail "$btl_file" \
"compile error:
$compile_output"
        return
    fi

    #
    # Executable exists
    #

    if [[ ! -x "$exe" ]]; then
        fail "$btl_file" \
"reason: executable not produced

expected executable:
$exe"
        return
    fi

    #
    # Expected output exists
    #

    if [[ ! -f "$expected" ]]; then
        fail "$btl_file" \
"reason: missing expected output file

expected file:
$expected"
        return
    fi

    #
    # Run executable
    #

    local actual_output
    actual_output="$("$exe" 2>&1)"
    local exit_code=$?

    local expected_output
    expected_output="$(cat "$expected")"

    #
    # Runtime failure
    #

    if [[ $exit_code -ne 0 ]]; then
        fail "$btl_file" \
"runtime error

exit code:
$exit_code

program output:
$actual_output"
        return
    fi

    #
    # Output mismatch
    #

    if [[ "$actual_output" != "$expected_output" ]]; then
        local diff_output
        diff_output="$(
            diff -u \
                <(printf "%s\n" "$expected_output") \
                <(printf "%s\n" "$actual_output") || true
        )"

        fail "$btl_file" \
"output mismatch

expected:
----------------------------------------
$expected_output
----------------------------------------

actual:
----------------------------------------
$actual_output
----------------------------------------
"
        return
    fi

    clear_line
    ((passed++))
}

#
# Single test mode
#

if [[ $# -eq 1 ]]; then
    verbose=1

    test_file="$1"

    if [[ "$test_file" != *.btl ]]; then
        test_file="${test_file}.btl"
    fi

    if [[ ! -f "$test_file" ]]; then
        found="$(find "$TEST_DIR" -type f -name "$(basename "$test_file")" | head -n 1)"

        if [[ -z "$found" ]]; then
            echo "test not found: $test_file"
            exit 1
        fi

        test_file="$found"
    fi

    run_test "$test_file"

    echo "passed: $passed"
    echo "failed: $failed"

    [[ $failed -ne 0 ]] && exit 1
    exit 0
fi

#
# Batch mode
#

while IFS= read -r -d '' btl_file; do
    run_test "$btl_file"
done < <(find "$TEST_DIR" -type f -name '*.btl' -print0)

[[ $failed -gt 0 ]] && echo
echo "passed: $passed"
echo "failed: $failed"

[[ $failed -ne 0 ]] && exit 1
exit 0
