#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 58)

(define x 'intial)
(test/value=? x 'intial)
