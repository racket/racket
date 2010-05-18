
#lang typed/scheme/base

(define (f x)
 (: g (Integer -> Integer))
 (define (g x)
   (+ x 2))
 (g x))
