#!r6rs
(import (tests r6rs mutable-strings)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs mutable-strings)\n")
(run-mutable-strings-tests)
(report-test-results)
