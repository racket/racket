#!r6rs
(import (tests r6rs lists)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs lists)\n")
(run-lists-tests)
(report-test-results)
