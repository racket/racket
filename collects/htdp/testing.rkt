#lang scheme/base

(require test-engine/racket-tests)

(define (generate-report) (test)#;(void))

(provide (all-from-out test-engine/racket-tests)
         generate-report)
