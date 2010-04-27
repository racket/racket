#lang typed/scheme

(require typed/test-engine/scheme-tests)
(check-expect 3 (+ 1 'foo))

(test)
