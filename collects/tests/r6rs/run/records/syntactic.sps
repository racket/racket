#!r6rs
(import (tests r6rs records syntactic)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs records syntactic)\n")
(run-records-syntactic-tests)
(report-test-results)
