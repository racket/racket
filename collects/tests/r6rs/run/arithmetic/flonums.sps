#!r6rs
(import (tests r6rs arithmetic flonums)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs arithmetic flonums)\n")
(run-arithmetic-flonums-tests)
(report-test-results)
