#!r6rs
(import (tests r6rs enums)
        (tests r6rs test)
        (rnrs io simple))
(display "Running tests for (rnrs enums)\n")
(run-enums-tests)
(report-test-results)
