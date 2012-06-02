#lang typed/racket

(: f (case-> 
      (Integer [#:k Integer] -> Integer)
      (Integer String [#:k Integer] -> Integer)))
(define f
  (lambda (x [z 2] #:k [y 1]) (+ x y)))

(: f2 (case-> 
       (Integer [#:k Integer] -> Integer)
       (Integer String [#:k Integer] -> Integer)))
(define (f2 x [z 2] #:k [y 1]) (+ x y))

(f 0)
(f 0 "s")
(f 0 #:k 1)
(f 0 "s" #:k 1)
(f 0 #:k 1 "s")

(f2 0)
(f2 0 "s")
(f2 0 #:k 1)
(f2 0 "s" #:k 1)
(f2 0 #:k 1 "s")

(: g (Integer #:k Integer -> Integer))
(define g
  (lambda (x #:k y) (+ x y)))

(: g2 (Integer #:k Integer -> Integer))
(define (g2 x #:k y) (+ x y))

(g 0 #:k 1)
(g2 0 #:k 1)
