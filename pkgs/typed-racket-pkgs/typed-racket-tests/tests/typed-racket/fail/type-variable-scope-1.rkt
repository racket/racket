#;
(exn:pred (lambda (e) (regexp-match? "Expected 1 type variables" e)))
#lang typed/racket

;; Testing type variable scope

;; This should fail because of the type variable arities
(: f (All (b) (b -> b)))
(define f
  (plambda: (a b) ([x : b]) x))
