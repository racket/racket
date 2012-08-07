#lang typed/racket
(provide sum product)

(: sum : (Listof Integer) -> Integer)
(define (sum xs)
  (define s 0)
  (for: ([x (in-list xs)])
    (set! s (+ x s)))
  s)

(: product : (Listof Integer) -> Integer)
(define (product xs)
  (define p 1)
  (for: ([x (in-list xs)])
    (set! p (* x p)))
  p)

