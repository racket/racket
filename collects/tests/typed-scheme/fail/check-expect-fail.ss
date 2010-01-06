#lang typed/scheme

(require test-engine/scheme-tests)
(check-expect 3 (+ 1 'foo))

(test)
