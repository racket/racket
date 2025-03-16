#!/usr/bin/env bash

set -euo pipefail
HERE="$(dirname "$0")"
RACKET="racket"
RACO="raco"

echo Using `which "$RACKET"`
"$RACKET" -v

CPUS="$("$RACKET" -e '(processor-count)')"

echo Installing dt-test and racket-test using `which "$RACO"`
"$RACO" pkg install --auto --skip-installed db-test racket-test

do_test() {
    "$RACO" test --timeout 300 -j "$CPUS" "$@"
}


# What Gets Tested
# ~~~~~~~~~~~~~~~~
# These tests run in GitHub Actions, where the environment isn't suited
# to run all the tests. This script tries to run most of the tests that
# can easily run in that environment. The rest of the tests are run by
# DrDr[1] whenever changes are made to the master branch.
#
# [1]: http://drdr.racket-lang.org/

# Core Tests.
# ~~~~~~~~~~~~~~~~
# The core test suite of Racket itself.

printf "\n\n\n\n == Testing collection 'tests/racket/test' ==\n"
do_test -l "tests/racket/test"


# Collection Tests
# ~~~~~~~~~~~~~~~~
# Tests where `raco test` can discover and run all the tests.

COLLECTIONS_TO_TEST=(
  tests/file
  tests/future
  tests/generic
  tests/json
  tests/match
  tests/net
  tests/setup
  tests/stxparse
  tests/syntax
  tests/units
  tests/utils
  tests/xml
)

for collection in "${COLLECTIONS_TO_TEST[@]}"; do
    printf "\n\n\n\n == Testing collection '%s' ==\n" "$collection"
    do_test -c "$collection"
done


# Module Tests
# ~~~~~~~~~~~~
# Tests where a central module controls what gets tested.

MODULES_TO_TEST=(
  tests/db/all-tests
  tests/openssl/basic
  tests/openssl/https
  tests/zo-path
)

for mpath in "${MODULES_TO_TEST[@]}"; do
    printf "\n\n\n\n == Testing module path '%s' ==\n" "$mpath"
    do_test -l "$mpath"
done


# Special Cases
# ~~~~~~~~~~~~~
# Tests that don't fit in the previous two buckets.

printf "\n\n == Testing tests/racket/contract/all ==\n"

"$RACKET" -l tests/racket/contract/all
