#lang racket/base

(require srfi/11 rackunit)
(provide srfi-11-tests)

(define srfi-11-tests
  (test-suite
   "Tests for SRFI 11"
   (check-equal? (let-values ((x (values 1 2 3))) x) '(1 2 3) "PR 12147")))
