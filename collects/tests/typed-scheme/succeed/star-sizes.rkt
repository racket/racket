#lang typed-scheme

(: f (All (a) ((Integer a * -> Integer) -> Integer)))
(define (f g) 0)

(f +)
