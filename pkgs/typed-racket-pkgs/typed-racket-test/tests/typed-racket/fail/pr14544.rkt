#;
(exn-pred (regexp-quote "in: (define (f x y z) (+ y z))"))
#lang typed/racket

(: f (-> Number Number Number))
(define (f x y z) (+ y z))
