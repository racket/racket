#!r6rs
(import (tests r6rs exceptions)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs exceptions)\n")
(run-exceptions-tests)
(report-test-results)
