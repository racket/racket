#!r6rs
(import (tests r6rs records procedural)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs records procedural)\n")
(run-records-procedural-tests)
(report-test-results)
