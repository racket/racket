#!r6rs
(import (tests r6rs eval)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs eval)\n")
(run-eval-tests)
(report-test-results)
