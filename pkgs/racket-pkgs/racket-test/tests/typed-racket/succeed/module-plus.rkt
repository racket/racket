#lang typed/racket

(: f : Integer -> Integer)
(define (f x) x)

(module+ main
  (: g : Integer -> Integer)
  (f (+ 3 5)))

(module+ main
  (define (g x) (add1 x)))

(module+ main
  (g (f 7)))
