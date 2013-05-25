#lang typed/racket

(require (except-in typed/rackunit check-equal?))

(provide (all-from-out typed/rackunit)
         check-equal?)

;; This gets around the fact that typed/rackunit can no longer test higher-order values for equality,
;; since TR has firmed up its rules on passing `Any' types in and out of untyped code
(define-syntax-rule (check-equal? a b . message)
  (check-true (equal? a b) . message))
