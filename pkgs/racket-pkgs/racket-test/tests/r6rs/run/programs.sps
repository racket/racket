#!r6rs
(import (tests r6rs programs)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs programs)\n")
(run-programs-tests)
(report-test-results)
