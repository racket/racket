#;
(exn-pred 2)
#lang typed/racket

(: foo (Nonnegative-Float -> Nonnegative-Float))
(define (foo x)
  (cond [(> x 0.0)  1.0]
        [else  (ann x String)]))

(foo 0.0)
