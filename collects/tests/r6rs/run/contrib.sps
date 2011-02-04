#!r6rs
(import (tests r6rs contrib)
        (tests r6rs test)
        (rnrs io simple))
(display "Running contributed tests\n")
(run-contrib-tests)
(report-test-results)
