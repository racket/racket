#!r6rs
(import (tests r6rs sorting)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs sorting)\n")
(run-sorting-tests)
(report-test-results)
