#!r6rs
(import (tests r6rs unicode)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs unicode)\n")
(run-unicode-tests)
(report-test-results)
