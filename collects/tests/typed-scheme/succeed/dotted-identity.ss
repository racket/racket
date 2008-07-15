#lang typed-scheme

(: f (All (a ...) ((a ... a -> Integer) -> (a ... a -> Integer))))
(define (f x) x)

(: y (Integer Integer -> Integer))
(define (y a b) (+ a b))

#{(f y) :: (Integer Integer -> Integer)}

(: z (Integer * -> Integer))
(define (z . xs) (apply + xs))

((f z) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)

(f z)

#{(f z) :: (Integer * -> Integer)}
