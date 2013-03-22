#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 40)

(define (f x y)
  (values (+ x 11) (+ y 3)))

(let-values ([(x y) (values 1 2)])
  (let-values ([(a b) (f x y)])
    (let-values ([(a b) (f a b)])
      (+ a b))))

(values 1 2)
(values 3 4 5 6 7)