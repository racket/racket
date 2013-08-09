#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 255)

(define (f b) 17)
(define (g l f)
  ((lambda (x) (f (first l)))
   1))
(g '(3) f)