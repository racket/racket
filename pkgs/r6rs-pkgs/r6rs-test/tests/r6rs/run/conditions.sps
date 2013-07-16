#!r6rs
(import (tests r6rs conditions)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs conditions)\n")
(run-conditions-tests)
(report-test-results)
