#lang typed-scheme

(require typed-racket/base-env/extra-procs)

(: f (Integer Integer -> Integer))
(define (f x y) (+ x y))

(map f (list 1 2 3) (list 10 20 30))

(map + (list 1 2 3) (list 10 20 30) (list 10 20 30))

(map + (list 1 2 3) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30) (list 10 20 30))

(: h (Integer Integer Integer * -> Integer))
(define (h x y . z) (apply + (cons x (cons y z))))

(map h (list 1 2 3) (list 4 5 6))
(map h (list 1 2 3) (list 4 5 6) (list 4 5 6))
