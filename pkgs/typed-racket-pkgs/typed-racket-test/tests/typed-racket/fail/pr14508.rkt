#;
(exn-pred 1)
#lang typed/racket

(: f (All (a ...) (-> a ... a (values a ... a))))
(define (f . xs)
  (apply values 3))
