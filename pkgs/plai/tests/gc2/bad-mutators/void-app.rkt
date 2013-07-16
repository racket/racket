#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 100)
(define x (cons 1 2))
((set-first! x 2) 1)
