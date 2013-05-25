#!r6rs
(import (tests r6rs io ports)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs io ports)\n")
(run-io-ports-tests)
(report-test-results)
