#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 58)

(define x 'initial)

(eq? x x)
(eq? x 'initial)
(eq? 5 4)
