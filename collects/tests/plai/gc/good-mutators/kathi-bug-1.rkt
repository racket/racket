#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 84)
(define L (cons 3 empty))
(test/value=? L '(3))