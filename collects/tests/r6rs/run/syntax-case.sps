#!r6rs
(import (tests r6rs syntax-case)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs syntax-case)\n")
(run-syntax-case-tests)
(report-test-results)
