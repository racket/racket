#lang typed-scheme

(: f (All (a ...) ((a ... a -> Integer) -> (a ... a -> Integer))))
(define (f x) x)

(: y (Integer Integer -> Integer))
(define (y a b) (+ a b))

#{(f y) :: (Integer Integer -> Integer)}

(: z (Integer * -> Integer))
(define (z . xs) (apply + xs))

((f z) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)

#; #{(f z) :: (Integer * -> Integer)}

;; I don't believe the below should work, but it points out where that internal error is coming from.

(: g (All (b ...) ( -> (b ... b -> Integer))))
(define (g) (lambda xs 0))

(f (g))