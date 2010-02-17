#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 28)

(define (app f)
  (lambda (x)
    (f x)))

(define plus (app add1))

(plus 23)
(plus 5)
