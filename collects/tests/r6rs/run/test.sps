#!r6rs
(import (tests r6rs test)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs test)\n")
(run-test-tests)
(report-test-results)
