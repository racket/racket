#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 28)

(define (app f)
  (lambda (x)
    (f x)))

(define plus (app (λ (x) (add1 x))))

(plus 23)
(plus 5)
