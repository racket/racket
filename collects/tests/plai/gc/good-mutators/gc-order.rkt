#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 58)

(define x 3)
(cons (begin (set! x 2)
             1)
      (begin (set! x 3)
             1))
(test/value=? x 3)
