#lang typed/racket

(define-struct:  x ([a : Any]))
(define-struct: (A) (y x) ([b : A]))

(: f : (y Any) -> String)
(define (f v)
  (if (string? (y-b v))
      (y-b v)
      "foo"))
