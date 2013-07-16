#!r6rs
(import (tests r6rs base)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs base)\n")
(run-base-tests)
(report-test-results)
