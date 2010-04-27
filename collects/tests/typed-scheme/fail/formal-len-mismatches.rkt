#lang typed-scheme

(: f (Integer Integer Integer * -> Integer))
(define (f x)
  (+ #\c x))

(: f2 (Integer Integer * -> Integer))
(define (f2 x y . z)
  (apply + #\c x y z))

(: f4 (Integer Integer -> Integer))
(define (f4 x y w . z)
  (apply + #\c x y w z))

(: f3 (Integer Integer -> Integer))
(define (f3 x . z)
  (apply + #\c x z))
