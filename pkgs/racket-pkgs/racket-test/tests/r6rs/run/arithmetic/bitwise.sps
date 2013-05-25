#!r6rs
(import (tests r6rs arithmetic bitwise)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs arithmetic bitwise)\n")
(run-arithmetic-bitwise-tests)
(report-test-results)
