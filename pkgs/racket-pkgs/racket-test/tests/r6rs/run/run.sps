#!r6rs
(import (tests r6rs run)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs run)\n")
(run-run-tests)
(report-test-results)
