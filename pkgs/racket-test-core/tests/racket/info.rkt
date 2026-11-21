#lang info

(define test-timeouts '(("test.rkt" 1500)))

;; test-pack.rkt adds and removes a "packed" collection in the main
;; collects directory, which can make `raco setup` fail in pkg tests
(define test-lock-names '(("test-pack.rkt" "pkg-test")))
