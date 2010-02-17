#lang plai/mutator
(allocator-setup "../bad-collectors/broken-collector.ss" 12)

50
60
70
80
(define x (cons 1 2))

(set-first! x x)