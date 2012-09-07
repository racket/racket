#lang typed/racket
(provide list-sum list-product)

(: list-sum : (Listof Integer) -> Integer)
(define (list-sum xs)
  (define s 0)
  (for: ([x (in-list xs)])
    (set! s (+ x s)))
  s)

(: list-product : (Listof Integer) -> Integer)
(define (list-product xs)
  (define p 1)
  (for: ([x (in-list xs)])
    (set! p (* x p)))
  p)


