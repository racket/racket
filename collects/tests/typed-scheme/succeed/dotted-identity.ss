#lang typed-scheme

(: f (All (a ...) ((a ... a -> Integer) -> (a ... a -> Integer))))
(define (f x) x)

(: y (Integer Integer -> Integer))
(define (y a b) (+ a b))

#{(f y) :: (Integer Integer -> Integer)}

