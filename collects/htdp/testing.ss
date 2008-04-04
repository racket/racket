#lang scheme/base

(require test-engine/scheme-tests)

(define (generate-report) (void))

(provide (all-from-out test-engine/scheme-tests)
         generate-report)
