#lang typed-scheme

(require typed-scheme/private/extra-procs)

(: f (Integer Integer -> Integer))
(define (f x y) (+ x y))

(map* f (list 1 2 3) (list 10 20 30))

#;(map* + (list 1 2 3) (list 10 20 30) (list 10 20 30))