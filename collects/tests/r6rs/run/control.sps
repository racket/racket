#!r6rs
(import (tests r6rs control)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs control)\n")
(run-control-tests)
(report-test-results)
