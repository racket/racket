#;
(exn-pred #rx"type variables bound in different scopes")
#lang typed/racket

;; Test that the error message in this case mentions
;; that the type variables look the same but are
;; different

(: f (All (a) (a -> a)))
(define (f x)
  (: g (All (a) (a -> a)))
  (define (g y) x)
  x)

