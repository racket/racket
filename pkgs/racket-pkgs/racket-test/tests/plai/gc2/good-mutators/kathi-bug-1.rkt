#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 84)
(define L (cons 3 empty))
(test/value=? L '(3))
