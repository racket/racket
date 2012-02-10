#lang racket/base

(provide phase-1-eval-tests)

;; Check that eval at phase 1 doesn't use errortrace.
(define (phase-1-eval-tests)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require '(for-syntax (only racket/base + eval quote #%app #%datum)))
    (dynamic-require 'errortrace #f)
    (eval '(begin-for-syntax (eval '(+ 1 2))))))

