#lang racket/base

(require rackunit
         rackunit/text-ui
         "all-rackunit-tests.rkt")

(run-tests all-rackunit-tests)

;; These tests should all error, so we switch the meaning of correct and incorrect. If the error display changes significantly, DrDr will catch it
(parameterize ([current-error-port (current-output-port)]
               [current-output-port (current-error-port)])
  (run-tests failure-tests))
