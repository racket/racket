#lang typed-scheme

;; I don't believe the below should work, but it points out where that internal error is coming from.

(: f (All (a ...) ((a ... a -> Integer) -> (a ... a -> Integer))))
(define (f x) x)

(: g (All (b ...) ( -> (b ... b -> Integer))))
(define (g) (lambda xs 0))

(f (g))
