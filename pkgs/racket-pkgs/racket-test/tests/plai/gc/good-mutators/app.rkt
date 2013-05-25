#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 28)

(define (app f)
  (lambda (x)
    (f x)))

(define plus (app add1))

(plus 23)
(plus 5)
