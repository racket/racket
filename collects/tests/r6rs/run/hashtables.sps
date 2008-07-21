#!r6rs
(import (tests r6rs hashtables)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs hashtables)\n")
(run-hashtables-tests)
(report-test-results)
