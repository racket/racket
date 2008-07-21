#!r6rs
(import (tests r6rs reader)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs reader)\n")
(run-reader-tests)
(report-test-results)
