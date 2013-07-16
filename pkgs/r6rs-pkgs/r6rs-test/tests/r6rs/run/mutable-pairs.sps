#!r6rs
(import (tests r6rs mutable-pairs)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs mutable-pairs)\n")
(run-mutable-pairs-tests)
(report-test-results)
