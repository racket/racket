#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 28)

(halt-on-errors #t)
(test/value=? 12 12)
