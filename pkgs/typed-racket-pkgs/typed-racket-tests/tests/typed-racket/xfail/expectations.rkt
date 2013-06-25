#lang typed/racket


(: f1 ((U 4 'four) -> Boolean : Number))
(define (f1 x) (number? x))
(f1 4)

(: f2 (All (b ...) (b ... b -> Number)))
(define (f2 . y)
  (: f2-inner (4 b ... b -> Number))
  (define (f2-inner x . z) 5)
  (apply f2-inner 4 y))
