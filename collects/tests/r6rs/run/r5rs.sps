#!r6rs
(import (tests r6rs r5rs)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs r5rs)\n")
(run-r5rs-tests)
(report-test-results)
