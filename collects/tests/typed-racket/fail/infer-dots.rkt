#lang typed-scheme

(require typed-racket/base-env/extra-procs)

(map + (list 1 2 3) (list 10 20 30) (list 'a 'b 'c))

;; Arity mismatch.
(: g (Integer Integer Integer -> Integer))
(define (g x y z) 0)

(map g (list 1 2 3) (list 4 5 6))

(: h (Integer Integer Integer * -> Integer))
(define (h x y . z) 0)

(map h (list 1 2 3))
