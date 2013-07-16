#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 400)
(define (do-one i) (/ (- i 1)))
(define (loop i)
  (or (= 1 i)
      (and (do-one i)
           (loop (- i 1)))))
(loop 50)
