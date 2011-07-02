#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 68)

(define (gen-circular)
  (let ([x (cons 3 4)])
    (let ([y (cons 2 x)])
      (set-rest! x y)
      x)))

(define x (gen-circular))
(test/location=? x (rest (rest x)))
