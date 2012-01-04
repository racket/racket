#lang plai/gc2mutator
(allocator-setup "../good-collectors/good-collector.rkt" 100)
(define x (cons 1 2))
((set-first! x 2) 1)
