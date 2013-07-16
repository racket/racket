#!r6rs
(import (tests r6rs arithmetic fixnums)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs arithmetic fixnums)\n")
(run-arithmetic-fixnums-tests)
(report-test-results)
