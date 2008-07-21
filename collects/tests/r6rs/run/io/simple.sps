#!r6rs
(import (tests r6rs io simple)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs io simple)\n")
(run-io-simple-tests)
(report-test-results)
