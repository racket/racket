#!r6rs
(import (tests r6rs bytevectors)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs bytevectors)\n")
(run-bytevectors-tests)
(report-test-results)
