#lang racket/load

(module m typed/racket
  (provide #%module-begin)
  (: f : Integer -> Integer)
  (define (f x) (add1 x))
  (provide add1 #%datum #%app f))

(module n 'm
  5
  (add1 (f 7)))
